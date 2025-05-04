unit Unit3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls,
  ExtCtrls, Menus, Types;

type
  // Type defining the possible states for each square on the board
  TPion = (Vide, // Empty square
           J1,   // Player 1 Pawn (e.g., White)
           J2,   // Player 2 Pawn (e.g., Black/Gray)
           D1,   // Player 1 King (Dame)
           D2);  // Player 2 King (Dame)

  { TForm3: Main form for the checkers game }
  TForm3 = class(TForm)
    Button1: TButton;       // Potentially the "Confirm Move" button (currently seems unused/mislinked)
    LabeledEdit1: TLabeledEdit; // Displays current turn number
    LabeledEdit2: TLabeledEdit; // Displays Player 1's captured pawns
    LabeledEdit3: TLabeledEdit; // Displays Player 2's captured pawns
    LabeledEdit4: TLabeledEdit; // Purpose unclear, currently unused
    MainMenu1: TMainMenu;     // Main menu bar
    MenuItem1: TMenuItem;     // Menu item (e.g., File)
    MenuItem2: TMenuItem;     // Menu item (e.g., New Game under File)
    MenuItem3: TMenuItem;     // Menu item (e.g., Separator)
    MenuItem4: TMenuItem;     // Menu item (e.g., Rules under Help)
    MenuItem5: TMenuItem;     // Menu item (e.g., Exit under File)
    MenuItem6: TMenuItem;     // Menu item (linked to MenuItem6Click) - Often "About" or "Help"
    MenuItem7: TMenuItem;     // Menu item (linked to MenuItem7Click) - Often "Exit" or similar
    RadioButton1: TRadioButton; // Unused radio button
    RadioButton2: TRadioButton; // Unused radio button
    StringGrid1: TStringGrid;   // The visual representation of the checkerboard
    // Event handler: Called once when the form is created
    procedure FormCreate(Sender: TObject);
    // Event handler: Called when MenuItem6 is clicked
    procedure MenuItem6Click(Sender: TObject);
    // Event handler: Called when MenuItem7 is clicked
    procedure MenuItem7Click(Sender: TObject);
    // Event handler: Likely intended for Button1 click, confirms the selected move
    procedure ButtonOnClick(Sender: TObject); // Naming mismatch with Button1?
    // Event handler: Called when a cell in the StringGrid is clicked
    procedure StringGrid1Click(Sender: TObject);
    // Event handler: Called whenever a cell in the StringGrid needs to be redrawn
    procedure StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
  private
    { Private methods of the form }
    // Helper procedure to draw a pawn/king visual in a cell
    procedure DessinerPion(Col, Row:integer; Couleur:TColor);
    // Helper procedure to highlight a specific cell (e.g., selected start/end)
    procedure SurlignerCase(Col, Row:integer; Couleur:TColor);
    // Procedure to validate the proposed move (seems incomplete/uncalled)
    procedure ValiderDeplacement(DepartCol,DepartRow,ArriveeCol,ArriveeRow:integer);
    // Procedure to check if the move results in a capture (seems incomplete/uncalled)
    procedure VerifierCapture(DepartCol,DepartRow,ArriveeCol,ArriveeRow:integer);
    // Procedure to check if a pawn reached the opposite end for promotion (seems incomplete/uncalled)
    procedure VerifierPromotion(ArriveeCol,ArriveeRow:integer);
    // Procedure to handle end-of-turn logic (change player, update scores) (seems incomplete/uncalled)
    procedure FinDuTour;
  end;

var
  Form3: TForm3; // Global instance of the game form
  CaseDepartSelectionnee :boolean; // Flag: True if the starting square for a move has been clicked
  // The core game state: 2D array representing the board. Indices 1..10 are used.
  Plateau: array[0..9,0..9] of TPion; // Note: Declared 0..9 but often accessed 1..10
  // Game state variables
  PionsMangesJ1, // Count of Player 1's pawns captured by Player 2
  PionsMangesJ2, // Count of Player 2's pawns captured by Player 1
  CaseDepartCol, // Column index (1-based) of the selected starting square
  CaseDepartRow, // Row index (1-based) of the selected starting square
  CaseArriveeCol,// Column index (1-based) of the selected destination square
  CaseArriveeRow,// Row index (1-based) of the selected destination square
  Tour:integer;    // Current turn number
  //Nombretour:integer; // Commented out variable

implementation
USES Unit1,Unit2; // Uses other units (Main Menu, Rules Form)
{$R *.lfm} // Includes the form's layout file

{ TForm3 Procedures }

// Procedure executed when the game form (Form3) is first created
procedure TForm3.FormCreate(Sender: TObject);
var i,j:integer; // Loop counters
  begin
    // Configure the StringGrid component to represent the 10x10 board
    StringGrid1.RowCount := 10;
    StringGrid1.ColCount := 10;
    // Disable default grid drawing and editing
    StringGrid1.Options := StringGrid1.Options - [goEditing, goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine];
    StringGrid1.DefaultColWidth := 50; // Set cell width
    StringGrid1.DefaultRowHeight := 50; // Set cell height

    // Initialize the logical game board (Plateau array) and set visual cell types (unused in DrawCell)
    for i := 0 to 9 do // Loop through rows (0-9)
      for j := 0 to 9 do // Loop through columns (0-9)
      begin
        // Initialize the logical board state for this square to Empty
        // NOTE: Using 1-based indexing for Plateau here (i+1, j+1)
        Plateau[i + 1, j + 1] := Vide;
        // Store 'B' or 'N' in the grid cell text (for debugging? Not used for drawing)
        if (i + j) mod 2 = 0 then // Check if sum of indices is even (White squares)
          StringGrid1.Cells[j, i] := 'B'  // Assign 'B' to white squares
        else
          StringGrid1.Cells[j, i] := 'N'; // Assign 'N' to black squares (playable)
      end;

    // Place Player 1's pawns (J1) on the board
    for i := 0 to 3 do // Rows 0-3
      for j := 0 to 9 do // All columns
        // Place pawn only on black squares ((i+j) mod 2 = 1)
        if (i + j) mod 2 = 1 then
          begin
           // Visually draw the pawn (redundant here, DrawCell handles it)
           // DessinerPion(j, i, clWhite); // Initial drawing handled by DrawCell later
           // Update the logical board state
           // NOTE: Using 1-based indexing for Plateau (i+1, j+1)
           Plateau[i + 1, j + 1] := J1;
          end;

    // Place Player 2's pawns (J2) on the board
    for i := 6 to 9 do // Rows 6-9
      for j := 0 to 9 do // All columns
        // Place pawn only on black squares ((i+j) mod 2 = 1)
        if (i + j) mod 2 = 1 then
          begin
           // Visually draw the pawn (redundant here, DrawCell handles it)
           // DessinerPion(j, i, clGray); // Initial drawing handled by DrawCell later
           // Update the logical board state
           // NOTE: Using 1-based indexing for Plateau (i+1, j+1)
           Plateau[i + 1, j + 1] := J2;
          end;


    // Initialize game state variables and UI display
    Tour := 1;                // Start at turn 1
    PionsMangesJ1 := 0;       // Player 1 starts with 0 captures
    PionsMangesJ2 := 0;       // Player 2 starts with 0 captures
    LabeledEdit1.Text := IntToStr(Tour);          // Display turn number
    LabeledEdit2.Text := IntToStr(PionsMangesJ1); // Display P1 captures
    LabeledEdit3.Text := IntToStr(PionsMangesJ2); // Display P2 captures

    CaseDepartSelectionnee := False; // Initially, no starting square is selected

    // Force the grid to redraw itself using the initialized Plateau state
    StringGrid1.Invalidate;
  end;

// Procedure to handle click on Button1 (intended for confirming move)
// NOTE: Seems disconnected from StringGrid1Click logic currently
procedure TForm3.ButtonOnClick(Sender: TObject);
  begin
    // Check if both start and end squares have been selected (via StringGrid1Click)
    if (CaseDepartCol = 0) or (CaseDepartRow = 0) or (CaseArriveeCol = 0) or (CaseArriveeRow = 0) then
    begin
      // Show error if selection is incomplete
      ShowMessage('Veuillez sélectionner une case de départ et une case d''arrivée.');
      Exit; // Stop processing
    end;

    // Show debug message with selected coordinates
    ShowMessage('Déplacement de Col=' + IntToStr(CaseDepartCol) + ', Row=' + IntToStr(CaseDepartRow) +
                ' à Col=' + IntToStr(CaseArriveeCol) + ', Row=' + IntToStr(CaseArriveeRow));

    // --- These validation/action steps are likely incomplete or bypassed ---
    // Call validation procedure (implementation needed)
    ValiderDeplacement(CaseDepartCol, CaseDepartRow, CaseArriveeCol, CaseArriveeRow);

    // Call capture check procedure (implementation needed)
    VerifierCapture(CaseDepartCol, CaseDepartRow, CaseArriveeCol, CaseArriveeRow);

    // Call promotion check procedure (implementation needed)
    VerifierPromotion(CaseArriveeCol, CaseArriveeRow);

    // Call end-of-turn procedure (implementation needed)
    FinDuTour;
    // --- End of incomplete section ---

    // Reset selection state after attempting move
    CaseDepartCol := 0;
    CaseDepartRow := 0;
    CaseArriveeCol := 0;
    CaseArriveeRow := 0;
    CaseDepartSelectionnee := False; // Reset selection flag too

    // Force the grid to redraw to reflect any changes (or lack thereof)
    StringGrid1.Invalidate;
  end;

// Procedure executed when a cell in the checkerboard grid is clicked
procedure TForm3.StringGrid1Click(Sender: TObject);
var
  ClickedCol, ClickedRow: Integer; // 0-based grid coordinates
begin
  // Get the 0-based column and row index of the clicked cell
  ClickedCol := StringGrid1.Col;
  ClickedRow := StringGrid1.Row;

  // Determine if this click is for selecting the starting square or the destination square
  if not CaseDepartSelectionnee then
  begin
    // --- Selecting the STARTING square ---

    // Store potential starting coordinates (convert to 1-based for Plateau)
    CaseDepartCol := ClickedCol + 1;
    CaseDepartRow := ClickedRow + 1;

    // Debug message
    //ShowMessage('Case départ : Col=' + IntToStr(CaseDepartCol) + ', Row=' + IntToStr(CaseDepartRow));

    // Basic Validation for Starting Square:

    // 1. Must be a black square (playable square)
    // Note: (Row + Col) mod 2 == 1 for black squares in standard boards (if 0,0 is white)
    // The check '(CaseDepartRow + CaseDepartCol) mod 2 = 0' seems inverted or assumes 0,0 is black. Let's assume it's correct for this specific layout.
    // Correction: Using 0-based grid coords (ClickedRow + ClickedCol) is more direct here.
    if (ClickedRow + ClickedCol) mod 2 = 0 then
    begin
      ShowMessage('Vous ne pouvez sélectionner une pièce que sur les cases noires.');
      // Reset potentially stored coordinates if invalid
      CaseDepartCol := 0;
      CaseDepartRow := 0;
      Exit; // Stop processing click
    end;

    // 2. Must contain a pawn belonging to the current player
    // Determine whose turn it is (assuming Tour 1=J1, Tour 2=J2, Tour 3=J1, etc.)
    // Note: Needs logic for Kings (D1, D2) as well!
    // Note: Accessing Plateau with 1-based coords (CaseDepartRow, CaseDepartCol)
    if Tour mod 2 = 1 then // Player 1's turn (J1 or D1)
    begin
      if (Plateau[CaseDepartRow, CaseDepartCol] <> J1) and (Plateau[CaseDepartRow, CaseDepartCol] <> D1) then
      begin
        ShowMessage('Tour du Joueur 1: Sélectionnez une pièce blanche.');
        CaseDepartCol := 0; CaseDepartRow := 0; // Reset selection
        Exit;
      end;
    end
    else // Player 2's turn (J2 or D2)
    begin
       if (Plateau[CaseDepartRow, CaseDepartCol] <> J2) and (Plateau[CaseDepartRow, CaseDepartCol] <> D2) then
      begin
        ShowMessage('Tour du Joueur 2: Sélectionnez une pièce grise.');
        CaseDepartCol := 0; CaseDepartRow := 0; // Reset selection
        Exit;
      end;
    end;

    // If validation passes:
    // Highlight the selected starting square (using 0-based grid coords)
    SurlignerCase(ClickedCol, ClickedRow, clLime); // Use Lime for start highlight

    // Mark that the starting square has been successfully selected
    CaseDepartSelectionnee := True;
  end
  else // CaseDepartSelectionnee is True
  begin
    // --- Selecting the DESTINATION square ---

    // Store potential destination coordinates (convert to 1-based for Plateau)
    CaseArriveeCol := ClickedCol + 1;
    CaseArriveeRow := ClickedRow + 1;

    // Debug message
    //ShowMessage('Case arrivée : Col=' + IntToStr(CaseArriveeCol) + ', Row=' + IntToStr(CaseArriveeRow));

    // Basic Validation for Destination Square:

    // 1. Check if the user clicked the *same* square again
    if (CaseArriveeCol = CaseDepartCol) and (CaseArriveeRow = CaseDepartRow) then
    begin
      // De-select the starting square
      CaseDepartSelectionnee := False;
      CaseDepartCol := 0; CaseDepartRow := 0;
      StringGrid1.Invalidate; // Redraw to remove highlight
      Exit; // Do nothing else
    end;

    // 2. Must be a black square
    // Using 0-based grid coords (ClickedRow + ClickedCol)
    if (ClickedRow + ClickedCol) mod 2 = 0 then
    begin
      ShowMessage('Vous ne pouvez vous déplacer que sur les cases noires.');
      // Don't reset arrival coords yet, let user try another destination
      Exit;
    end;

    // 3. Destination square must be empty
    // Note: Accessing Plateau with 1-based coords
    if Plateau[CaseArriveeRow, CaseArriveeCol] <> Vide then
    begin
       // Allow clicking on another *own* piece to change the starting piece
       if ((Tour mod 2 = 1) and ((Plateau[CaseArriveeRow, CaseArriveeCol] = J1) or (Plateau[CaseArriveeRow, CaseArriveeCol] = D1))) or
          ((Tour mod 2 = 0) and ((Plateau[CaseArriveeRow, CaseArriveeCol] = J2) or (Plateau[CaseArriveeRow, CaseArriveeCol] = D2))) then
       begin
         // Update starting square to the newly clicked one
         CaseDepartCol := CaseArriveeCol;
         CaseDepartRow := CaseArriveeRow;
         StringGrid1.Invalidate; // Redraw to update highlight
         SurlignerCase(ClickedCol, ClickedRow, clLime); // Highlight new start
         // Keep CaseDepartSelectionnee = True
         Exit; // Wait for next click (new destination)
       end
       else
       begin
         // Clicked on opponent's piece or invalid square that's not empty
         ShowMessage('La case d''arrivée doit être vide.');
         Exit; // Let user choose another destination
       end;
    end;

    // --- At this point, basic conditions met: start selected, dest is black & empty ---
    // --- NOW, perform the move logic (or prepare for ButtonOnClick confirmation) ---

    // TODO: Instead of ButtonOnClick, integrate full move logic here.
    // This includes:
    //    - Detailed move validation (diagonal, distance 1 for non-capture, distance 2 for capture, direction based on pawn/king)
    //    - Capture detection and removal of captured piece
    //    - Handling multi-jumps (requires more state)
    //    - Updating the Plateau array
    //    - Checking for promotion to King
    //    - Ending the turn (incrementing Tour, updating labels)
    //    - Checking for win/loss conditions

    // --- TEMPORARY: Assume move is valid for now and update board state ---
    // Get piece type from start position
    val PieceType : TPion := Plateau[CaseDepartRow, CaseDepartCol];

    // Move piece in the logical board
    Plateau[CaseArriveeRow, CaseArriveeCol] := PieceType;
    Plateau[CaseDepartRow, CaseDepartCol] := Vide;

    // --- Check for simple promotion (reaching the end row) ---
    // NOTE: Does not handle Kings moving back!
    if (PieceType = J1) and (CaseArriveeRow = 10) then // J1 reaches row 10 (index 9 in 0-based)
      Plateau[CaseArriveeRow, CaseArriveeCol] := D1; // Promote to King
    else if (PieceType = J2) and (CaseArriveeRow = 1) then // J2 reaches row 1 (index 0 in 0-based)
       Plateau[CaseArriveeRow, CaseArriveeCol] := D2; // Promote to King

    // --- Placeholder for Capture Logic ---
    // If Abs(CaseArriveeCol - CaseDepartCol) = 2 then // Jump implies capture
    // begin
    //   captureCol := (CaseArriveeCol + CaseDepartCol) div 2;
    //   captureRow := (CaseArriveeRow + CaseDepartRow) div 2;
    //   capturedPiece := Plateau[captureRow, captureCol];
    //   if capturedPiece = J1 or capturedPiece = D1 then Inc(PionsMangesJ2)
    //   else if capturedPiece = J2 or capturedPiece = D2 then Inc(PionsMangesJ1);
    //   Plateau[captureRow, captureCol] := Vide;
    //   LabeledEdit2.Text := IntToStr(PionsMangesJ1);
    //   LabeledEdit3.Text := IntToStr(PionsMangesJ2);
    // end;

    // --- End Turn Logic ---
    // TODO: Add check for further possible captures (multi-jump) before ending turn.
    Tour := Tour + 1;
    LabeledEdit1.Text := IntToStr(Tour);

    // --- Reset selection state ---
    CaseDepartSelectionnee := False;
    CaseDepartCol := 0; CaseDepartRow := 0;
    CaseArriveeCol:= 0; CaseArriveeRow := 0;

    // Force redraw of the entire grid to show the move
    StringGrid1.Invalidate;

    // --- Old Logic relying on Button ---
    (*
    // Highlight the selected destination square
    SurlignerCase(ClickedCol, ClickedRow, clAqua); // Use Aqua for destination highlight

    // Reset the start selection flag, ready for ButtonOnClick or next move selection
    // CaseDepartSelectionnee := False; // Keep true until move confirmed? Depends on final logic.

    // Now the user would theoretically click Button1 to confirm...
    // Or, ideally, remove the button and execute the move logic right here.
    *)

  end; // End if/else for selecting start vs destination

end;


// Procedure called by the VCL framework whenever a grid cell needs repainting
procedure TForm3.StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  PieceType: TPion;
  CellColor: TColor;
begin
  // 1. Draw the cell background color (White or Black)
  if (aRow + aCol) mod 2 = 0 then // Even sum = White square
    CellColor := clWhite
  else // Odd sum = Black square (playable)
    CellColor := clBlack;
  StringGrid1.Canvas.Brush.Color := CellColor; // Set brush color
  StringGrid1.Canvas.FillRect(aRect);          // Fill the cell rectangle

  // 2. Check the logical board state for this cell
  // NOTE: Accessing Plateau using 1-based indexing (aRow+1, aCol+1)
  PieceType := Plateau[aRow + 1, aCol + 1];

  // 3. Draw the pawn/king if the square is not empty
  if PieceType = J1 then
    DessinerPion(aCol, aRow, clWhite) // Draw Player 1 pawn
  else if PieceType = J2 then
    DessinerPion(aCol, aRow, clGray) // Draw Player 2 pawn
  else if PieceType = D1 then
    // TODO: Draw Player 1 King differently
    DessinerPion(aCol, aRow, clWhite) // Placeholder: Draw same as pawn
  else if PieceType = D2 then
    // TODO: Draw Player 2 King differently
    DessinerPion(aCol, aRow, clGray); // Placeholder: Draw same as pawn

  // 4. Draw highlight if this cell is the selected start or destination
  // Note: Uses 0-based grid coordinates (aCol, aRow) vs 1-based stored coords
  if CaseDepartSelectionnee then
  begin
     // Highlight starting square
     if (CaseDepartCol = aCol + 1) and (CaseDepartRow = aRow + 1) then
     begin
        SurlignerCase(aCol, aRow, clLime); // Lime for selected start
     end;
     (* // Only highlight destination if ButtonOnClick is used
     // Highlight destination square (if selected)
     else if (CaseArriveeCol = aCol + 1) and (CaseArriveeRow = aRow + 1) then
     begin
        SurlignerCase(aCol, aRow, clAqua); // Aqua for selected destination
     end;
     *)
  end;

  // 5. Draw grid lines (optional, currently disabled in FormCreate)
  // StringGrid1.Canvas.Pen.Color := clGray;
  // StringGrid1.Canvas.Rectangle(aRect);

end;

// Helper procedure to draw a circle representing a pawn/king
procedure TForm3.DessinerPion(Col, Row: integer; Couleur: TColor);
var
  Rect: TRect;
  CenterX, CenterY, Radius: Integer;
begin
  // Calculate the rectangle for the cell using grid coordinates
  Rect := StringGrid1.CellRect(Col, Row);
  // Calculate center and radius for the circle based on cell dimensions
  CenterX := Rect.Left + (Rect.Right - Rect.Left) div 2;
  CenterY := Rect.Top + (Rect.Bottom - Rect.Top) div 2;
  Radius := (Rect.Right - Rect.Left) div 2 - 4; // Leave a small margin

  // Set drawing properties
  StringGrid1.Canvas.Brush.Color := Couleur; // Fill color for the pawn
  StringGrid1.Canvas.Pen.Color := clBlack;   // Outline color
  StringGrid1.Canvas.Pen.Width := 1;         // Outline width

  // Draw the circle
  StringGrid1.Canvas.Ellipse(CenterX - Radius, CenterY - Radius, CenterX + Radius, CenterY + Radius);

  // TODO: Add indicator for Kings (e.g., draw a smaller inner circle or change outline)
  // if IsKing(...) then ...
end;

// Helper procedure to draw a highlight border around a cell
procedure TForm3.SurlignerCase(Col, Row: integer; Couleur: TColor);
var
  Rect: TRect;
begin
  // Get the rectangle coordinates of the cell
  Rect := StringGrid1.CellRect(Col, Row);

  // Set pen properties for the highlight border
  StringGrid1.Canvas.Pen.Color := Couleur; // Use the specified highlight color
  StringGrid1.Canvas.Pen.Width := 3;       // Make the border thick enough to be visible
  StringGrid1.Canvas.Brush.Style := bsClear; // Make the rectangle fill transparent

  // Draw the rectangle outline slightly inside the cell boundaries
  StringGrid1.Canvas.Rectangle(Rect.Left + 1, Rect.Top + 1, Rect.Right - 1, Rect.Bottom - 1);

  // Reset brush style if needed elsewhere
  StringGrid1.Canvas.Brush.Style := bsSolid;
  // Reset pen width if needed elsewhere
  StringGrid1.Canvas.Pen.Width := 1;
end;

// --- Incomplete Game Logic Procedures ---

// Placeholder for validating a move
procedure TForm3.ValiderDeplacement(DepartCol, DepartRow, ArriveeCol, ArriveeRow: integer);
begin
  // TODO: Implement full move validation logic here.
  // Check:
  // - If move is diagonal (Abs(ArriveeCol - DepartCol) = Abs(ArriveeRow - DepartRow))
  // - If distance is valid (1 for simple move, 2 for capture)
  // - If direction is valid (forward for pawns, any for kings)
  // - If path is clear for capture (opponent piece in middle, destination empty)
  ShowMessage('ValiderDeplacement: Not implemented');
end;

// Placeholder for checking and handling captures
procedure TForm3.VerifierCapture(DepartCol, DepartRow, ArriveeCol, ArriveeRow: integer);
begin
  // TODO: Implement capture logic.
  // Check if the move was a jump (distance = 2).
  // If so, calculate the captured square's coordinates.
  // Check if the captured square contained an opponent's piece.
  // If so, remove the piece from Plateau, update capture counts (PionsMangesJ1/J2), update labels.
  // Check for possibility of multi-jumps from the arrival square.
  ShowMessage('VerifierCapture: Not implemented');
end;

// Placeholder for checking and handling pawn promotion
procedure TForm3.VerifierPromotion(ArriveeCol, ArriveeRow: integer);
begin
  // TODO: Implement promotion logic.
  // Check if the piece that just moved reached the opponent's back rank.
  // (Row 10 for J1, Row 1 for J2 - using 1-based index).
  // If so, change the piece type in Plateau to the corresponding King (D1 or D2).
  ShowMessage('VerifierPromotion: Not implemented');
end;

// Placeholder for handling the end of a player's turn
procedure TForm3.FinDuTour;
begin
  // TODO: Implement end-of-turn logic.
  // Switch the active player (update 'Tour' variable? Or use a boolean flag?).
  // Update UI elements indicating whose turn it is.
  // Check for win/loss conditions (no pieces left, no legal moves left).
  Tour := Tour + 1; // Simple turn increment (used elsewhere already)
  LabeledEdit1.Text := IntToStr(Tour); // Update turn display
  ShowMessage('FinDuTour: Basic implementation (turn incremented)');
end;

// Event handler for MenuItem6 (e.g., Help/About)
procedure TForm3.MenuItem6Click(Sender: TObject);
begin
  ShowMessage('Jeu de Dames - Version Alpha');
  // Could potentially show Form2 (rules) here as well:
  // Form2.Show;
end;

// Event handler for MenuItem7 (e.g., Exit)
procedure TForm3.MenuItem7Click(Sender: TObject);
begin
  Close; // Close this form (Form3)
  // Alternatively, if this is the main game window and closing it should end the app:
  // Application.Terminate;
end;

end.
