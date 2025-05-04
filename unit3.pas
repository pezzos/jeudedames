unit Unit3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls,
  ExtCtrls, Menus, Types;

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
    procedure ValiderDeplacement(DepartCol,DepartRow,ArriveeCol,ArriveeRow:integer);
    procedure VerifierCapture(DepartCol,DepartRow,ArriveeCol,ArriveeRow:integer);
    procedure VerifierPromotion(ArriveeCol,ArriveeRow:integer);
    procedure FinDuTour;
  end;

var
  Form3: TForm3;
  CaseDepartSelectionnee :boolean;
  Plateau: array[0..9,0..9] of TPion;
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
         DessinerPion(j, i, clWhite); // Pion blanc
         Plateau[i + 1, j + 1] := J1;
        end;

  for i := 6 to 9 do
    for j := 0 to 9 do
      if (i + j) mod 2 = 1 then
        begin
         DessinerPion(j, i, clGray); // Pion gris
         Plateau[i + 1, j + 1] := J2;
        end;


  // Initialisation des variables
  Tour := 1;
  PionsMangesJ1 := 0;
  PionsMangesJ2 := 0;
  LabeledEdit1.Text := IntToStr(Tour);
  LabeledEdit2.Text := IntToStr(PionsMangesJ1);
  LabeledEdit3.Text := IntToStr(PionsMangesJ2);

  CaseDepartSelectionnee:=false;

  end;
procedure TForm3.ButtonOnClick(Sender: TObject);
  begin
   
  // Vérifier que les cases de départ et d'arrivée ont été sélectionnées
  if (CaseDepartCol = 0) or (CaseDepartRow = 0) or (CaseArriveeCol = 0) or (CaseArriveeRow = 0) then
  begin
    ShowMessage('Veuillez sélectionner une case de départ et une case d''arrivée.');
    Exit;
  end;
   
  // Afficher les coordonnées pour débogage
  ShowMessage('Déplacement de Col=' + IntToStr(CaseDepartCol) + ', Row=' + IntToStr(CaseDepartRow) +
              ' à Col=' + IntToStr(CaseArriveeCol) + ', Row=' + IntToStr(CaseArriveeRow));

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
begin
  
if not CaseDepartSelectionnee then
  begin
    // Récupérer les coordonnées de la case départ
    CaseDepartCol := StringGrid1.Col + 1;
    CaseDepartRow := StringGrid1.Row + 1;

    // Afficher les coordonnées pour débogage
    ShowMessage('Case départ : Col=' + IntToStr(CaseDepartCol) + ', Row=' + IntToStr(CaseDepartRow));

    // Vérifier si la case est noire
    if (CaseDepartRow + CaseDepartCol) mod 2 = 0 then
    begin
      ShowMessage('Vous ne pouvez cliquer que sur les cases noires.');
      Exit;
    end;

    // Vérifier si la case contient un pion du joueur actuel
    if (Plateau[CaseDepartRow, CaseDepartCol] <> J1) and (Plateau[CaseDepartRow, CaseDepartCol] <> J2) then
    begin
      ShowMessage('La case de départ doit contenir un pion du joueur actuel.');
      Exit;
    end;

    // Surligner la case départ
    SurlignerCase(CaseDepartCol - 1, CaseDepartRow - 1, clBlue);

    // Marquer la case de départ comme sélectionnée
    CaseDepartSelectionnee := True;
  end
  else
  begin
    // Récupérer les coordonnées de la case arrivée
    CaseArriveeCol := StringGrid1.Col + 1;
    CaseArriveeRow := StringGrid1.Row + 1;

    // Afficher les coordonnées pour débogage
    ShowMessage('Case arrivée : Col=' + IntToStr(CaseArriveeCol) + ', Row=' + IntToStr(CaseArriveeRow));

    // Vérifier si la case est noire
    if (CaseArriveeRow + CaseArriveeCol) mod 2 = 0 then
    begin
      ShowMessage('Vous ne pouvez cliquer que sur les cases noires.');
      Exit;
    end;

    // Vérifier si la case d'arrivée est vide
    if (Plateau[CaseArriveeRow, CaseArriveeCol] <> Vide) then
    begin
      // Si la case contient un pion du joueur actuel, elle devient la nouvelle case de départ
      if (Plateau[CaseArriveeRow, CaseArriveeCol] = J1) or (Plateau[CaseArriveeRow, CaseArriveeCol] = J2) then
      begin
        CaseDepartCol := CaseArriveeCol;
        CaseDepartRow := CaseArriveeRow;
        SurlignerCase(CaseDepartCol - 1, CaseDepartRow - 1, clBlue);
        ShowMessage('La case d''arrivée contient un pion du joueur actuel. Choisissez une nouvelle case d''arrivée.');
        Exit;
      end
      else
      begin
        ShowMessage('La case d''arrivée doit être vide.');
        Exit;
      end;
    end;

    // Vérifier le déplacement diagonal
    if Abs(CaseArriveeCol - CaseDepartCol) <> Abs(CaseArriveeRow - CaseDepartRow) then
    begin
      ShowMessage('Le déplacement doit être diagonal.');
      Exit;
    end;

    // Vérifier le sens du déplacement (descendant pour les blancs, montant pour les noirs)
    if (Plateau[CaseDepartRow, CaseDepartCol] = J1) and (CaseArriveeRow <= CaseDepartRow) then
    begin
      ShowMessage('Les pions blancs doivent se déplacer en descendant.');
      Exit;
    end
    else if (Plateau[CaseDepartRow, CaseDepartCol] = J2) and (CaseArriveeRow >= CaseDepartRow) then
    begin
      ShowMessage('Les pions noirs doivent se déplacer en montant.');
      Exit;
    end;

    // Surligner la case arrivée
    SurlignerCase(CaseArriveeCol - 1, CaseArriveeRow - 1, clBlue);

    // Réinitialiser la sélection de la case de départ
    CaseDepartSelectionnee := False;
  end;

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


procedure TForm3.ValiderDeplacement(DepartCol, DepartRow, ArriveeCol, ArriveeRow: Integer);
begin
  // Vérifier que le déplacement est diagonal et dans les limites du plateau
  if Abs(ArriveeCol - DepartCol) <> Abs(ArriveeRow - DepartRow) then
  begin
    ShowMessage('Le déplacement doit être diagonal.');
    Exit;
  end;

  // Vérifier que la case d'arrivée est vide
  if (Plateau[ArriveeRow, ArriveeCol] <> Vide) then
  begin
    ShowMessage('La case d''arrivée doit être vide.');
    Exit;
  end;

  // Mettre à jour le plateau
  ShowMessage('Déplacement validé de (' + IntToStr(DepartCol) + ', ' + IntToStr(DepartRow) + ') à (' + IntToStr(ArriveeCol) + ', ' + IntToStr(ArriveeRow) + ')');
  Plateau[ArriveeRow, ArriveeCol] := Plateau[DepartRow, DepartCol];
  Plateau[DepartRow, DepartCol] := Vide;
  StringGrid1.InvalidateCell(DepartCol-1, DepartRow-1);
  StringGrid1.InvalidateCell(ArriveeCol-1, ArriveeRow-1);

  //StringGrid1.Canvas.FillRect(StringGrid1.CellRect(DepartCol-1, DepartRow-1));
  // Dessiner le pion à la nouvelle position
  {if (Plateau[ArriveeRow,ArriveeCol] =J1) or (Plateau[ArriveeRow,ArriveeCol] =D1) then
     begin
     DessinerPion(ArriveeCol-1, ArriveeRow-1, clWhite);
     end
     else if (Plateau[ArriveeRow,ArriveeRow]=J2) or (Plateau[ArriveeRow,ArriveeCol] =D2) then
       begin
             DessinerPion(ArriveeCol-1,ArriveeRow-1,clGray);
       end; }

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
    ShowMessage('Joueur 1 a gagné !')
  else if PionsMangesJ2 = 20 then
    ShowMessage('Joueur 2 a gagné !');
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
end.
