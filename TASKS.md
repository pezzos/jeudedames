# Prioritized Task List for JeuDeDames

This list breaks down the TODOs into smaller, actionable steps, prioritized by importance.

## Priority: HIGH (Core Bugs & Functionality)

1.  **Task: Remove Confirmation Button & Handler**
    *   **Goal:** Eliminate the non-functional confirmation step.
    *   **Steps:**
        *   Remove `Button1` component from `Form3` (using Lazarus designer or by editing `.lfm` and `.pas`).
        *   Delete the `ButtonOnClick` procedure declaration and implementation from `Unit3.pas`.
        *   Remove references to the obsolete helper procedures (`ValiderDeplacement`, `VerifierCapture`, etc.) if `ButtonOnClick` was the only caller.
    *   **Impacted Files:** `Unit3.pas`, `Unit3.lfm`

2.  **Task: Verify/Fix Initial J2 Pawn Placement [COMPLETED - Workaround]**
    *   **Goal:** Ensure all black pawns are correctly placed at the start.
    *   **Steps:**
        *   **Re-Analysis & Debug:** Confirmed `Plateau[9,10]` was `Vide` after initial loops.
        *   **Fix Approach:** Implemented consistent 1-based indexing initially, but reverted due to visual shifts. The root cause likely remains the mixed indexing logic.
        *   **Final Workaround:** Explicitly added `Plateau[9, 10] := J2;` after the J2 initialization loop in `FormCreate` to force the pawn placement. This resolves the visual bug but is not ideal.
        *   **Cleanup Task Created:** See Task X (Refactor Indexing & Remove Workaround).
    *   **Impacted Files:** `Unit3.pas`

3.  **Task: Implement Basic Pawn Movement Logic in `StringGrid1Click`**
    *   **Goal:** Allow valid single-step diagonal moves for pawns directly on the second click.
    *   **Steps:**
        *   Locate the `else // CaseDepartSelectionnee is True` block in `StringGrid1Click`.
        *   Remove the existing temporary move logic (`Plateau[CaseArriveeRow, CaseArriveeCol] := PieceType; ...`).
        *   Implement validation checks **before** modifying `Plateau`:
            *   Is the move diagonal? (`Abs(CaseArriveeCol - CaseDepartCol) = 1` and `Abs(CaseArriveeRow - CaseDepartRow) = 1`).
            *   Is the destination square empty? (`Plateau[CaseArriveeRow, CaseArriveeCol] = Vide`).
            *   Is the move direction correct? (J1: `CaseArriveeRow > CaseDepartRow`, J2: `CaseArriveeRow < CaseDepartRow`).
        *   If all checks pass:
            *   Update the logical board: `Plateau[CaseArriveeRow, CaseArriveeCol] := Plateau[CaseDepartRow, CaseDepartCol];`
            *   Empty the starting square: `Plateau[CaseDepartRow, CaseDepartCol] := Vide;`
            *   Proceed to Task 4 (End Turn Logic).
        *   If checks fail, provide feedback (see Task 12) and do *not* change the board state or end the turn. Keep the start piece selected.
    *   **Impacted Files:** `Unit3.pas`

4.  **Task: Implement Basic Turn End Logic**
    *   **Goal:** Advance the turn and reset selection after a valid move.
    *   **Steps:**
        *   Place this logic *after* a successful move is validated and the `Plateau` is updated (within the `if all checks pass` block from Task 3).
        *   Increment the turn counter: `Tour := Tour + 1;`
        *   Update the turn display: `LabeledEdit1.Text := IntToStr(Tour);`
        *   Reset selection variables: `CaseDepartSelectionnee := False; CaseDepartCol := 0; CaseDepartRow := 0; CaseArriveeCol := 0; CaseArriveeRow := 0;`
        *   Force grid redraw: `StringGrid1.Invalidate;`
    *   **Impacted Files:** `Unit3.pas`

5.  **Task: Implement Basic Pawn Capture Logic**
    *   **Goal:** Allow single diagonal jumps over an opponent pawn.
    *   **Steps:**
        *   In `StringGrid1Click` (second click logic), add validation for capture moves *in addition* to simple moves:
            *   Is the move a diagonal jump? (`Abs(CaseArriveeCol - CaseDepartCol) = 2` and `Abs(CaseArriveeRow - CaseDepartRow) = 2`).
            *   Is the destination square empty?
            *   Calculate the jumped square coordinates: `JumpedCol := (CaseDepartCol + CaseArriveeCol) div 2; JumpedRow := (CaseDepartRow + CaseArriveeRow) div 2;`
            *   Does the jumped square contain an *opponent's* pawn? (Check `Plateau[JumpedRow, JumpedCol]` against J1/J2 based on `Tour`).
        *   If a valid capture is detected:
            *   Perform the move: Update `Plateau` for Arrivee and Depart squares.
            *   Remove the captured piece: `Plateau[JumpedRow, JumpedCol] := Vide;`
            *   Update capture counts: `Inc(PionsMangesJ1)` or `Inc(PionsMangesJ2)`.
            *   Update capture labels: `LabeledEdit2.Text := ...`, `LabeledEdit3.Text := ...`.
            *   Proceed to Task 6 (Promotion Check) *before* Task 4 (End Turn Logic).
        *   **Note:** Initially, do not implement mandatory captures or multi-jumps. Assume a capture ends the turn.
    *   **Impacted Files:** `Unit3.pas`

6.  **Task: Implement Basic Pawn Promotion**
    *   **Goal:** Promote pawns reaching the back rank to Kings.
    *   **Steps:**
        *   Place this logic *after* a successful move/capture updates the `Plateau` (Arrivee square now contains the piece) but *before* ending the turn.
        *   Check if the piece that just moved *to* `CaseArriveeRow`, `CaseArriveeCol` is a pawn (`J1` or `J2`).
        *   Check if it landed on the opponent's back rank (J1: `CaseArriveeRow = 10`, J2: `CaseArriveeRow = 1` - assuming 1-based logic for now).
        *   If promotion conditions met, update the piece type: `if Plateau[ArriveeRow, ArriveeCol] = J1 then Plateau[ArriveeRow, ArriveeCol] := D1` (and similarly for J2 to D2).
        *   The visual update will happen in `StringGrid1DrawCell` once Task 9 is done.
    *   **Impacted Files:** `Unit3.pas`

## Priority: MEDIUM (Rules Completion & Refactoring)

7.  **Task: Standardize Array Indexing**
    *   **Goal:** Use consistent 0-based or 1-based indexing for `Plateau` and grid access.
    *   **Steps:**
        *   **Decision:** Choose 0-based (recommended) or 1-based.
        *   **Apply:**
            *   **(0-based):** Change `Plateau` to `array[0..9, 0..9]`. Remove all `+1` adjustments when accessing `Plateau` using grid coordinates (`aCol`, `aRow`, `ClickedCol`, `ClickedRow`). Adjust loop bounds in `FormCreate` and logic if necessary (e.g., back rank check becomes `ArriveeRow = 9` for J1, `ArriveeRow = 0` for J2).
            *   **(1-based):** Change `Plateau` to `array[1..10, 1..10]`. Ensure *every* access using grid coordinates adds `+1` (e.g., `Plateau[ClickedRow + 1, ClickedCol + 1]`). Keep back rank checks as `10` and `1`.
        *   **Test:** Thoroughly re-test initial placement, movement, capture, and promotion after the change.
    *   **Impacted Files:** `Unit3.pas`

8.  **Task: Implement Basic King Movement**
    *   **Goal:** Allow Kings to move diagonally forward and backward one step.
    *   **Steps:**
        *   Modify the move validation logic (Task 3) to check the piece type (`Plateau[CaseDepartRow, CaseDepartCol]` after indexing standardization).
        *   If the piece is a King (`D1` or `D2`), allow movement where `Abs(CaseArriveeRow - CaseDepartRow) = 1` regardless of the direction (i.e., remove the J1/J2 direction check for Kings).
    *   **Impacted Files:** `Unit3.pas`

9.  **Task: Visual Distinction for Kings**
    *   **Goal:** Draw Kings differently from Pawns.
    *   **Steps:**
        *   Modify `DessinerPion` procedure:
            *   Add an `IsKing: Boolean` parameter OR check the piece type directly via `Plateau` before drawing.
            *   If it's a King, draw an additional visual element (e.g., `Canvas.Pen.Color := clRed; Canvas.Ellipse(CenterX - Radius div 2, ...)` for an inner circle, or change `Pen.Width` for the main circle).
        *   Update the calling code in `StringGrid1DrawCell` to pass the necessary information (e.g., `DessinerPion(aCol, aRow, clWhite, PieceType in [D1, D2])`) or ensure `DessinerPion` reads the `Plateau` itself.
    *   **Impacted Files:** `Unit3.pas`

10. **Task: Implement Basic King Capture**
    *   **Goal:** Allow Kings to capture by jumping one step diagonally forwards or backwards.
    *   **Steps:**
        *   Modify the capture validation logic (Task 5).
        *   If the moving piece is a King (`D1` or `D2`), allow capture jumps (`Abs(...) = 2`) where the jumped piece is an opponent, regardless of the jump direction.
    *   **Impacted Files:** `Unit3.pas`

11. **Task: Implement Multi-jump Logic**
    *   **Goal:** Force subsequent captures if available after an initial capture.
    *   **Steps:**
        *   Introduce global/form state variables: `MultiJumpInProgress: Boolean; PieceInMultiJumpCol, PieceInMultiJumpRow: Integer;`.
        *   After a successful capture (Task 5/10):
            *   Check if further captures are possible *from* the arrival square (`CaseArriveeCol`, `CaseArriveeRow`) with the same piece.
            *   If yes: Set `MultiJumpInProgress := True`, store `PieceInMultiJumpCol/Row`, **do not end the turn (skip Task 4)**, `StringGrid1.Invalidate` and exit the procedure.
            *   If no: Set `MultiJumpInProgress := False`, proceed to promotion check (Task 6) and turn end (Task 4).
        *   Modify `StringGrid1Click`:
            *   At the start of the first-click logic (`if not CaseDepartSelectionnee`), if `MultiJumpInProgress` is true, only allow selecting the piece at `PieceInMultiJumpCol/Row`. Ignore clicks elsewhere.
            *   At the start of the second-click logic (`else`), if `MultiJumpInProgress` is true, only allow selecting a valid *capture* destination for the required piece. Ignore non-capture moves.
    *   **Impacted Files:** `Unit3.pas`

12. **Task: Implement Mandatory Capture Rule**
    *   **Goal:** Force the player to take an available capture if one exists.
    *   **Steps:**
        *   Create a function `CanPlayerCapture(Player: TPionType): Boolean` (where TPionType is J1/D1 or J2/D2 based on `Tour`) that iterates through all of that player's pieces on the board and checks if any have a valid capture move available.
        *   In `StringGrid1Click`:
            *   **(First click):** If `CanPlayerCapture` is true, only allow selecting a piece that *has* a capture available.
            *   **(Second click):** If the selected piece *could* capture, only allow selecting a valid capture destination (disallow simple moves).
    *   **Impacted Files:** `Unit3.pas`

13. **Task: Implement Flying Kings (Full Movement/Capture)**
    *   **Goal:** Allow kings to move/capture over multiple squares diagonally.
    *   **Steps:**
        *   Refactor King movement validation: Check all squares along the chosen diagonal between start and end. They must all be `Vide`.
        *   Refactor King capture validation: Check all squares along the diagonal between start and end. Exactly one square must contain an opponent's piece, and the square(s) immediately beyond it (up to the destination) must be `Vide`.
    *   **Impacted Files:** `Unit3.pas`

## Priority: LOW (UI/UX & Refactoring)

14. **Task: Highlight Possible Moves**
    *   **Goal:** Show the user where the selected piece can legally move/capture.
    *   **Steps:**
        *   Create a function `GetValidMoves(StartCol, StartRow: Integer): TList<TPoint>` (use `TList<TPoint>` from `Generics.Collections` if available, or a dynamic array of records).
        *   This function performs all validation (simple/capture, pawn/king, mandatory/multi-jump states) for the piece at StartCol/Row and returns a list of valid ArriveeCol/Row coordinates.
        *   Store this list when a piece is selected (first click).
        *   Modify `StringGrid1DrawCell` to check if `aCol`, `aRow` is in the stored list and apply a highlight (e.g., `SurlignerCase(aCol, aRow, clYellow)`).
        *   Clear the list and invalidate the grid when selection changes or move completes.
    *   **Impacted Files:** `Unit3.pas`

15. **Task: Replace `ShowMessage` with `TStatusBar`**
    *   **Goal:** Provide non-interrupting feedback.
    *   **Steps:**
        *   Add `TStatusBar` to `Form3` in the designer.
        *   Search `Unit3.pas` for all `ShowMessage` calls used for game feedback/errors.
        *   Replace them with `StatusBar1.SimpleText := 'Your message here';`
    *   **Impacted Files:** `Unit3.pas`, `Unit3.lfm`

16. **Task: Remove Unused UI Components**
    *   **Goal:** Clean up the form.
    *   **Steps:**
        *   Identify unused components (`RadioButton1`, `RadioButton2`, `LabeledEdit4`).
        *   Delete them from the form using the Lazarus designer.
        *   Clean up any remaining references in `Unit3.pas` if necessary.
    *   **Impacted Files:** `Unit3.pas`, `Unit3.lfm`

17. **Task: Encapsulate Game State**
    *   **Goal:** Improve code structure and testability.
    *   **Steps:**
        *   Define `TJeuDeDamesEtat = class` or `record`.
        *   Move `Plateau`, `Tour`, `PionsMangesJ1`, `PionsMangesJ2` from global vars into this structure.
        *   Add `Form3.GameState: TJeuDeDamesEtat;` instance.
        *   Refactor all code currently using the global variables to access them via `GameState.Plateau`, `GameState.Tour`, etc.
        *   Consider moving core logic functions (`GetValidMoves`, move execution helpers) into methods of `TJeuDeDamesEtat`.
    *   **Impacted Files:** `Unit3.pas`

18. **Task: Implement Game End Conditions**
    *   **Goal:** Detect when the game is over.
    *   **Steps:**
        *   Create `Function IsGameOver: Boolean;` and `Function GetWinner: TPionType;` (could return Vide if draw/not over).
        *   Inside `IsGameOver`, check:
            *   Does Player 1 have any pieces left?
            *   Does Player 2 have any pieces left?
            *   Can the current player make *any* valid moves (check all pieces)?
        *   Call `IsGameOver` at the end of each turn.
        *   If true, display the winner/draw message (using `StatusBar`) and potentially disable `StringGrid1` clicks.
    *   **Impacted Files:** `Unit3.pas` 

X.  **Task: Refactor Board Indexing & Remove Workaround**
    *   **Priority:** HIGH
    *   **Goal:** Implement consistent 1-based indexing for `Plateau` throughout the code and remove the temporary fix for pawn placement.
    *   **Related Task:** Supersedes Task 7 (Standardize Array Indexing) and cleans up Task 2 workaround.
    *   **Steps:**
        *   Change `Plateau` declaration to `array[1..10, 1..10] of TPion`.
        *   Thoroughly review **all** accesses to `Plateau` in `Unit3.pas` (`FormCreate`, `StringGrid1Click`, `StringGrid1DrawCell`, `SurlignerCase`, `ValiderDeplacement`, `VerifierCapture`, `VerifierPromotion`, etc.).
        *   Adjust loop bounds and array accesses to use 1-based indices consistently for `Plateau`.
        *   Ensure logic relying on indices (e.g., promotion checks `ArriveeRow = 1/10`, placement condition `(i+j) mod 2`) is correct with 1-based indexing.
        *   Map 0-based grid coordinates (`aCol`, `aRow`, `ClickedCol`, `ClickedRow`) to 1-based `Plateau` coordinates correctly where needed (e.g., `Plateau[aRow + 1, aCol + 1]` remains valid if `Plateau` is 1-based).
        *   Remove the line `Plateau[9, 10] := J2;` from `FormCreate`.
        *   Test thoroughly: Initial placement, pawn/king movement, captures, promotions.
    *   **Impacted Files:** `Unit3.pas`