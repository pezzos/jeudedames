unit Unit3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls,
  ExtCtrls, Menus, Types, ComCtrls;

type
  TPion = (Vide,J1,J2,D1,D2);
  { TForm3 }
  TForm3 = class(TForm)
    Button1: TButton;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    LabeledEdit3: TLabeledEdit;
    LabeledEdit4: TLabeledEdit;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    StringGrid1: TStringGrid;
    StatusBar1: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure ButtonOnClick(Sender: TObject);
    procedure StringGrid1Click(Sender: TObject);
    procedure StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
  private
    procedure DessinerPion(Col, Row:integer; Couleur:TColor);
    procedure SurlignerCase(Col, Row:integer; Couleur:TColor);
    function ValiderDeplacement(DepartCol,DepartRow,ArriveeCol,ArriveeRow:integer): Boolean;
    procedure VerifierCapture(DepartCol,DepartRow,ArriveeCol,ArriveeRow:integer);
    procedure VerifierPromotion(ArriveeCol,ArriveeRow:integer);
    procedure FinDuTour;
    procedure VerifierFinPartie;
  end;

var
  Form3: TForm3;
  CaseDepartSelectionnee :boolean;
  Plateau: array[0..9,0..10] of TPion;
  PionsMangesJ1,PionsMangesJ2,CaseDepartCol,CaseDepartRow,CaseArriveeCol,CaseArriveeRow,Tour:integer;
  //Nombretour:integer;
implementation
USES Unit1,Unit2;
{$R *.lfm}

{ TForm3 }
procedure TForm3.FormCreate(Sender: TObject);
var i,j:integer;
  begin
    
// Initialisation du damier
  StringGrid1.RowCount := 10;
  StringGrid1.ColCount := 10;
// Initialisation du plateau et des cases
for i := 0 to 9 do
    for j := 0 to 9 do 
    begin
      Plateau[i + 1, j + 1] := Vide; // Initialisation des cases vides
      if (i + j) mod 2 = 0 then
        StringGrid1.Cells[j, i] := 'B'  // Case blanche
      else
        StringGrid1.Cells[j, i] := 'N'; // Case noire
    end;


  // Placement des pions
  for i := 0 to 3 do
    for j := 0 to 9 do
      if (i + j) mod 2 = 1 then
        begin
         // DessinerPion(j, i, clWhite); // Remove redundant drawing call
         Plateau[i + 1, j + 1] := J1;
        end;

  for i := 6 to 9 do
    for j := 0 to 9 do
      if (i + j) mod 2 = 1 then
        begin
         // DessinerPion(j, i, clGray); // Remove redundant drawing call
         Plateau[i + 1, j + 1] := J2;
        end;

  // Force placement of pawn at 1-based [9, 10] as a workaround
  Plateau[9, 10] := J2;

  // Initialisation des variables
  Tour := 1;
  PionsMangesJ1 := 0;
  PionsMangesJ2 := 0;
  LabeledEdit1.Text := IntToStr(Tour);
  LabeledEdit2.Text := IntToStr(PionsMangesJ1);
  LabeledEdit3.Text := IntToStr(PionsMangesJ2);

  CaseDepartSelectionnee:=false;

  // Force redraw of the grid after initialization
  StringGrid1.Invalidate;
  end;
procedure TForm3.ButtonOnClick(Sender: TObject);
  begin
   
  // Vérifier que les cases de départ et d'arrivée ont été sélectionnées
  if (CaseDepartCol = 0) or (CaseDepartRow = 0) or (CaseArriveeCol = 0) or (CaseArriveeRow = 0) then
  begin
    StatusBar1.SimpleText := 'Veuillez sélectionner une case de départ et une case d''arrivée.';
    Exit;
  end;
   
  // Afficher les coordonnées pour débogage
  StatusBar1.SimpleText := 'Déplacement de Col=' + IntToStr(CaseDepartCol) + ', Row=' + IntToStr(CaseDepartRow) +
              ' à Col=' + IntToStr(CaseArriveeCol) + ', Row=' + IntToStr(CaseArriveeRow);

  // Valider le déplacement
  ValiderDeplacement(CaseDepartCol, CaseDepartRow, CaseArriveeCol, CaseArriveeRow);

  // Vérifier la capture
  VerifierCapture(CaseDepartCol, CaseDepartRow, CaseArriveeCol, CaseArriveeRow);

  // Vérifier la promotion
  VerifierPromotion(CaseArriveeCol, CaseArriveeRow);

  // Fin du tour
  FinDuTour;

  // Réinitialiser les coordonnées des cases de départ et d'arrivée
  CaseDepartCol := 0;
  CaseDepartRow := 0;
  CaseArriveeCol := 0;
  CaseArriveeRow := 0;
  StringGrid1.Invalidate;
  end;

procedure TForm3.StringGrid1Click(Sender: TObject);
var
  ClickedCol, ClickedRow: Integer;
  PieceType: TPion;
begin
  // Get clicked cell coordinates
  ClickedCol := StringGrid1.Col;
  ClickedRow := StringGrid1.Row;

  // Debug: Check if the button click is still needed - it shouldn't be
  // if Button1.Visible then
  // begin
  //   ShowMessage('Veuillez d''abord cliquer sur Valider.');
  //   Exit;
  // end;

  if (ClickedCol = 0) or (ClickedRow = 0) then // Ignore clicks on headers
    Exit;

  // Check if a start square is already selected
  if not CaseDepartSelectionnee then
  begin
    // First click: Selecting the start square
    // Debugging: Display selected start square
    // ShowMessage('Case départ sélectionnée : Col=' + IntToStr(ClickedCol) + ', Row=' + IntToStr(ClickedRow));

    // Check if the clicked square is valid (black and contains current player's piece)
    if ((ClickedRow + ClickedCol) mod 2 <> 0) then // Black square check
    begin
        PieceType := Plateau[ClickedRow, ClickedCol]; // Read from logical board
        // Check if the piece belongs to the current player based on Tour
        if ((Tour mod 2 <> 0) and (PieceType in [J1, D1])) or ((Tour mod 2 = 0) and (PieceType in [J2, D2])) then
        begin
            CaseDepartCol := ClickedCol;
            CaseDepartRow := ClickedRow;
            CaseDepartSelectionnee := True;
            // Highlight the selected square (optional, can be done in DrawCell)
            StringGrid1.InvalidateCell(CaseDepartCol, CaseDepartRow); // Force redraw for selection feedback
             StatusBar1.SimpleText := 'Case départ : Col=' + IntToStr(CaseDepartCol) + ', Row=' + IntToStr(CaseDepartRow);
        end
        else
        begin
            // Invalid selection: wrong player's piece or empty square
             StatusBar1.SimpleText := 'La case de départ doit contenir un pion du joueur actuel.';
            Exit;
        end;
    end
    else
    begin
        // Invalid selection: white square
         StatusBar1.SimpleText := 'Vous ne pouvez cliquer que sur les cases noires.';
        Exit;
    end;

  end
  else
  begin
    // Second click: Selecting the destination square
    // Debugging: Display selected arrival square
    // ShowMessage('Case arrivée sélectionnée : Col=' + IntToStr(ClickedCol) + ', Row=' + IntToStr(ClickedRow));

    CaseArriveeCol := ClickedCol;
    CaseArriveeRow := ClickedRow;

    // Check if the destination square is valid (black)
    if ((CaseArriveeRow + CaseArriveeCol) mod 2 <> 0) then
    begin
        // Get the type of the piece being moved
        PieceType := Plateau[CaseDepartRow, CaseDepartCol];

        // --- Validation Logic ---
        // 1. Is the destination square empty?
        if Plateau[CaseArriveeRow, CaseArriveeCol] = Vide then
        begin
            // 2. Is the move diagonal by one step? (Pawn Move)
            if (Abs(CaseArriveeCol - CaseDepartCol) = 1) and (Abs(CaseArriveeRow - CaseDepartRow) = 1) then
            begin
                // 3. Is the move direction correct for the pawn?
                if (PieceType = J1) and (CaseArriveeRow < CaseDepartRow) then
                begin
                     StatusBar1.SimpleText := 'Les pions blancs (J1) doivent se déplacer en descendant.';
                    Exit; // Invalid move
                end;
                if (PieceType = J2) and (CaseArriveeRow > CaseDepartRow) then
                begin
                     StatusBar1.SimpleText := 'Les pions noirs (J2) doivent se déplacer en montant.';
                    Exit; // Invalid move
                end;

                // --- Valid Pawn Move ---
                 StatusBar1.SimpleText := 'Déplacement de Col=' + IntToStr(CaseDepartCol) + ', Row=' + IntToStr(CaseDepartRow) +
                                       ' vers Col=' + IntToStr(CaseArriveeCol) + ', Row=' + IntToStr(CaseArriveeRow);
                // Update logical board
                Plateau[CaseArriveeRow, CaseArriveeCol] := PieceType;
                Plateau[CaseDepartRow, CaseDepartCol] := Vide;

                // End Turn Logic
                Tour := Tour + 1;
                LabeledEdit1.Text := IntToStr(Tour);
                CaseDepartSelectionnee := False;
                // Reset variables for next turn
                CaseDepartCol := 0; CaseDepartRow := 0; CaseArriveeCol := 0; CaseArriveeRow := 0;
                // Redraw the entire grid
                StringGrid1.Invalidate;
            end
            else
            begin
                // --- Not a simple pawn move, check other possibilities (capture, king move?) ---
                // For now, just consider it invalid if not a single diagonal step
                 StatusBar1.SimpleText := 'Mouvement non valide (pour l'instant).'; // Placeholder message
                // Keep selection active to allow choosing a different destination
                CaseArriveeCol := 0; CaseArriveeRow := 0;
                 Exit;
            end;
        end
        else
        begin
             // Destination square is not empty
             StatusBar1.SimpleText := 'La case d''arrivée doit être vide.';
             // Allow user to select a different destination
             CaseArriveeCol := 0; CaseArriveeRow := 0;
             Exit;
        end;
    end
    else
    begin
        // Invalid destination: white square
         StatusBar1.SimpleText := 'Vous ne pouvez cliquer que sur les cases noires pour la destination.';
        // Allow user to select a different destination
        CaseArriveeCol := 0; CaseArriveeRow := 0;
        Exit;
    end;
  end;
end;


// --- Obsolete Button Handler ---
procedure TForm3.Button1Click(Sender: TObject);
begin
 // This handler is no longer needed as moves happen directly on the second click.
 // We keep it here temporarily but will remove it (Task 1).
 // if (CaseDepartRow = 0) or (CaseDepartCol = 0) or (CaseArriveeRow = 0) or (CaseArriveeCol = 0) then
 // begin
 //   ShowMessage('Veuillez sélectionner une case de départ et une case d''arrivée.');
 //   Exit;
 // end;

 // ShowMessage('Déplacement de Col=' + IntToStr(CaseDepartCol) + ', Row=' + IntToStr(CaseDepartRow) +
 //   ' vers Col=' + IntToStr(CaseArriveeCol) + ', Row=' + IntToStr(CaseArriveeRow) + ' (Validation via bouton)');

 // Call the old validation logic (to be replaced/integrated into StringGrid1Click)
 // if ValiderDeplacement(CaseDepartCol, CaseDepartRow, CaseArriveeCol, CaseArriveeRow) then
 // begin
 //   // Move the piece on the logical board
 //   Plateau[CaseArriveeRow, CaseArriveeCol] := Plateau[CaseDepartRow, CaseDepartCol];
 //   Plateau[CaseDepartRow, CaseDepartCol] := Vide;

 //   // Check and handle capture (to be integrated/refined)
 //   // VerifierCapture(CaseDepartCol, CaseDepartRow, CaseArriveeCol, CaseArriveeRow);

 //   // Check and handle promotion (to be integrated/refined)
 //   // VerifierPromotion(CaseArriveeCol, CaseArriveeRow);

 //   // End Turn
 //   Tour := Tour + 1;
 //   LabeledEdit1.Text := IntToStr(Tour);

 //   // Reset selection for next turn
 //   CaseDepartSelectionnee := False;
 //   CaseDepartCol := 0;
 //   CaseDepartRow := 0;
 //   CaseArriveeCol := 0;
 //   CaseArriveeRow := 0;

 //   // Redraw the grid
 //   StringGrid1.Invalidate;
 // end;
 // // No else needed here, ValiderDeplacement already showed messages
end;


// --- Obsolete Helper Functions (to be removed or integrated) ---

function TForm3.ValiderDeplacement(DepartCol, DepartRow, ArriveeCol, ArriveeRow: Integer): Boolean;
var
  TypePiece: TPion;
begin
  Result := False; // Default to invalid move

  // Basic checks
  if (DepartCol = 0) or (DepartRow = 0) or (ArriveeCol = 0) or (ArriveeRow = 0) then
  begin
    // This should not happen if selection logic is correct
    Exit;
  end;

  // 1. Check if destination is diagonal
  if Abs(ArriveeCol - DepartCol) <> Abs(ArriveeRow - DepartRow) then
  begin
     StatusBar1.SimpleText := 'Le déplacement doit être diagonal.';
    Exit;
  end;

  // 2. Check if destination is empty
  if Plateau[ArriveeRow, ArriveeCol] <> Vide then
  begin
     StatusBar1.SimpleText := 'La case d''arrivée doit être vide.';
    Exit;
  end;

  // 3. Specific Pawn Movement Logic (only single step for now)
  TypePiece := Plateau[DepartRow, DepartCol];
  if (TypePiece = J1) or (TypePiece = J2) then
  begin
      // Check direction for pawns
      if (TypePiece = J1) and (ArriveeRow < DepartRow) then
      begin
          StatusBar1.SimpleText := 'Les pions blancs doivent se déplacer en descendant.';
          Exit;
      end;
      if (TypePiece = J2) and (ArriveeRow > DepartRow) then
      begin
          StatusBar1.SimpleText := 'Les pions noirs doivent se déplacer en montant.';
          Exit;
      end;

      // Check if it's a single step move
       if Abs(ArriveeCol - DepartCol) = 1 then // Already checked ArriveeRow diff implicitly with diagonal check
       begin
           // Valid single step pawn move
           Result := True;
           StatusBar1.SimpleText := 'Déplacement validé de (' + IntToStr(DepartCol) + ', ' + IntToStr(DepartRow) + ') à (' + IntToStr(ArriveeCol) + ', ' + IntToStr(ArriveeRow) + ')';
       end
       // else Check for capture jump (Abs = 2) - Add later
  end;

  // 4. King Movement Logic (Add later)
  // if (TypePiece = D1) or (TypePiece = D2) then
  // begin
  //   // Allow forward/backward, single/multi step, capture etc.
  // end;

end;

procedure TForm3.StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin
if (aRow + aCol) mod 2 = 0 then
    StringGrid1.Canvas.Brush.Color := clWhite
  else
    StringGrid1.Canvas.Brush.Color := clBlack;
    StringGrid1.Canvas.FillRect(aRect);

  if Plateau[aRow + 1, aCol + 1] = J1 then
    DessinerPion(aCol, aRow, clWhite)
  else if Plateau[aRow + 1, aCol + 1] = J2 then
    DessinerPion(aCol, aRow, clGray)
  else if Plateau[aRow + 1, aCol + 1] = D1 then
    DessinerPion(aCol, aRow, clWhite)
  else if Plateau[aRow + 1, aCol + 1] = D2 then
    DessinerPion(aCol, aRow, clGray);
 // Surligner la case départ
if (aCol = CaseDepartCol-1) and (aRow = CaseDepartRow-1) then
    SurlignerCase(aCol, aRow, clBlue);

  //SurlignerCase(CaseDepartCol-1, CaseDepartRow-1, clBlue);

 // Surligner la case arrivée
 if (aCol = CaseArriveeCol-1) and (aRow = CaseArriveeRow-1) then
     SurlignerCase(aCol, aRow, clBlue);

  //SurlignerCase(CaseArriveeCol-1, CaseArriveeRow-1, clBlue);
end;

procedure TForm3.DessinerPion(Col, Row: Integer; Couleur: TColor);
var
  CellRect: TRect;
  CenterX, CenterY, Radius: Integer;
begin
  CellRect := StringGrid1.CellRect(Col, Row);
  CenterX := (CellRect.Left + CellRect.Right) div 2;
  CenterY := (CellRect.Top + CellRect.Bottom) div 2;
  Radius := (CellRect.Right - CellRect.Left) div 3;
  StringGrid1.Canvas.Brush.Color := Couleur;
  StringGrid1.Canvas.Ellipse(CenterX - Radius, CenterY - Radius, CenterX + Radius, CenterY + Radius);
end;

procedure TForm3.SurlignerCase(Col, Row: Integer; Couleur: TColor);
begin
  StringGrid1.Canvas.Brush.Color := Couleur;
  StringGrid1.Canvas.FillRect(StringGrid1.CellRect(Col, Row));

if Plateau[Row + 1, Col + 1] = J1 then
    DessinerPion(Col, Row, clWhite)
  else if Plateau[Row + 1, Col + 1] = J2 then
    DessinerPion(Col, Row, clGray)
  else if Plateau[Row + 1, Col + 1] = D1 then
    DessinerPion(Col, Row, clWhite)
  else if Plateau[Row + 1, Col + 1] = D2 then
    DessinerPion(Col, Row, clGray);

end;

procedure TForm3.VerifierCapture(DepartCol, DepartRow, ArriveeCol, ArriveeRow: Integer);
var
  CapturedCol, CapturedRow: Integer;
begin
  // Vérifier si un pion adverse est capturé
  CapturedCol := (DepartCol + ArriveeCol) div 2;
  CapturedRow := (DepartRow + ArriveeRow) div 2;
  if (Plateau[CapturedRow, CapturedCol] = J1) or (Plateau[CapturedRow, CapturedCol] = J2) then
  begin
    Plateau[CapturedRow, CapturedCol] := Vide;
    StringGrid1.Canvas.FillRect(StringGrid1.CellRect(CapturedCol-1, CapturedRow-1));
    if RadioButton1.Checked then
      Inc(PionsMangesJ1)
    else
      Inc(PionsMangesJ2);
    LabeledEdit2.Text := IntToStr(PionsMangesJ1);
    LabeledEdit3.Text := IntToStr(PionsMangesJ2);
  end;
end;


procedure TForm3.VerifierPromotion(ArriveeCol, ArriveeRow: Integer);
begin
  // Vérifier si un pion atteint le bord du plateau pour devenir une dame
  if (ArriveeRow = 0) and (Plateau[ArriveeRow, ArriveeCol] = J1) then
  begin
    Plateau[ArriveeRow, ArriveeCol] := D1;
    DessinerPion(ArriveeCol, ArriveeRow, clWhite);
  end
  else if (ArriveeRow = 9) and (Plateau[ArriveeRow, ArriveeCol] = J2) then
  begin
    Plateau[ArriveeRow, ArriveeCol] := D2;
    DessinerPion(ArriveeCol, ArriveeRow, clGray);
  end;
end;

procedure TForm3.FinDuTour;
begin
  // Alternance des joueurs
  if RadioButton1.Checked then
  begin
    RadioButton1.Checked := False;
    RadioButton2.Checked := True;
  end
  else
  begin
    RadioButton1.Checked := True;
    RadioButton2.Checked := False;
  end;

  // Incrémentation du numéro de tour
  Inc(Tour);
  LabeledEdit1.Text := IntToStr(Tour);

  // Vérification de la victoire
  if PionsMangesJ1 = 20 then
    StatusBar1.SimpleText := 'Joueur 1 a gagné !'
  else if PionsMangesJ2 = 20 then
    StatusBar1.SimpleText := 'Joueur 2 a gagné !';
end;

procedure TForm3.MenuItem6Click(Sender: TObject);
begin
  form2.Show;
end;

procedure TForm3.MenuItem7Click(Sender: TObject);
begin
   //showmessage('cette action vous feras perdre la partie en cours');
 form3.hide;
 form1.Show;
end;

procedure TForm3.VerifierFinPartie;
begin
  // Basic check: if one player has no pawns left
  if PionsMangesJ2 = 10 then // Assuming 10 pawns initially for J2
  begin
     StatusBar1.SimpleText := 'Joueur 1 a gagné !';
    // Optionally disable further moves: StringGrid1.Enabled := False;
  end
  else if PionsMangesJ1 = 10 then // Assuming 10 pawns initially for J1
  begin
     StatusBar1.SimpleText := 'Joueur 2 a gagné !';
    // Optionally disable further moves: StringGrid1.Enabled := False;
  end;

  // More complex checks needed:
  // - No legal moves for the current player (stalemate/loss)
end;

procedure TForm3.MenuItem2Click(Sender: TObject);
begin
  //showmessage('cette action vous feras perdre la partie en cours');
  // Call FormCreate using Self
  Self.FormCreate(Sender);
end;

end.
