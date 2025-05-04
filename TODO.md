# TODO List for JeuDeDames

This file tracks suggested improvements and necessary implementations for the Delphi checkers game.

## Core Gameplay Logic

1.  **Refactor Move Handling and Execution:**
    *   **Problem:** Move selection, validation, and execution logic is currently split between `StringGrid1Click`, `ButtonOnClick`, and several unimplemented helper procedures (`ValiderDeplacement`, `VerifierCapture`, etc.). `ButtonOnClick` seems disconnected or redundant.
    *   **Task:** Consolidate all move-related logic. `StringGrid1Click` should handle only the selection of start (first click) and destination (second click) squares. Upon the second valid click (empty black square), it should call a single, comprehensive procedure (e.g., `ExecuterOuTenterDeplacement`). This new procedure will be responsible for:
        *   Performing full move validation (diagonal checks, distance, direction, pawn vs. king rules).
        *   Handling captures (detecting jumps, removing captured pieces, updating scores).
        *   Implementing multi-jump logic (mandatory subsequent captures within the same turn).
        *   Updating the `Plateau` array with the final piece position.
        *   Checking for and applying promotion to King.
        *   Ending the turn (updating `Tour`, switching player context).
        *   Checking for win/loss conditions.
    *   **Cleanup:** Remove the `Button1`/`ButtonOnClick` confirmation step and the now-obsolete helper procedures (`ValiderDeplacement`, `VerifierCapture`, `VerifierPromotion`, `FinDuTour` placeholders).

2.  **Implement Full Checkers Ruleset:**
    *   **Problem:** Current validation in `StringGrid1Click` is very basic. Critical rules like captures, mandatory jumps, multi-jumps, and king movement/capture are missing.
    *   **Task:** Implement the complete rules for international draughts (assuming 10x10 board):
        *   **Pawn Movement:** Forward diagonal, one step to empty square.
        *   **King Movement:** Any distance diagonally (forward/backward) to empty square.
        *   **Pawn Capture:** Mandatory forward diagonal jump over adjacent opponent to empty square beyond. Must take available captures. Implement logic for choosing between multiple captures if required by ruleset (e.g., longest sequence).
        *   **King Capture:** Mandatory diagonal jump (any distance, forward/backward) over opponent to empty square beyond (or further along the same diagonal if clear). Must take available captures. Implement choice logic.
        *   **Multi-jumps:** After a capture, if the landing square allows another capture, it *must* be taken in the same turn. State needs to be managed to track if the current turn involves an ongoing capture sequence.
        *   **Promotion:** Pawns reaching the back rank become Kings *at the end* of the move action.
        *   **Game End Conditions:** Detect win/loss (no pieces left, no legal moves).

## Code Structure and Maintainability

3.  **Standardize Array Indexing (Plateau vs. StringGrid):**
    *   **Problem:** `Plateau` is declared `array[0..9, 0..9]` but accessed as if 1-based (`[Row+1, Col+1]`), while `StringGrid` uses 0-based indices (`aCol`, `aRow`). This inconsistency is error-prone.
    *   **Task:** Choose *one* indexing convention and apply it consistently everywhere:
        *   **Recommended:** Modify all `Plateau` access logic (initialization, `StringGrid1Click`, `StringGrid1DrawCell`, move execution) to use **0-based indexing** (0 to 9). This aligns directly with `StringGrid` coordinates.
        *   **Alternative:** Change `Plateau` declaration to `array[1..10, 1..10]` and ensure all grid coordinates are adjusted (`+1`) before accessing `Plateau`.

4.  **Improve Game State Management:**
    *   **Problem:** Game state (`Plateau`, `Tour`, `PionsMangesJ1`, `PionsMangesJ2`, `CaseDepartSelectionnee`, etc.) relies on global variables, making the code harder to manage and test.
    *   **Task:** Encapsulate the game state within a dedicated class or record (e.g., `TJeuDeDamesEtat`). `TForm3` would hold an instance of this state object. Core game logic functions (like move validation, execution, turn management) would become methods of this state object, improving modularity.

## User Interface and Experience

5.  **Visual Distinction for Kings (Dames):**
    *   **Problem:** Kings (`D1`, `D2`) are currently drawn identically to pawns (`J1`, `J2`) in `DessinerPion`.
    *   **Task:** Modify `DessinerPion` (or create a new `DessinerRoi` procedure) to draw Kings differently. Add a visual indicator (e.g., a concentric inner circle, a thicker border, a small 'K' or crown symbol). Update `StringGrid1DrawCell` to call the correct drawing procedure based on whether the `PieceType` is `J1`/`J2` or `D1`/`D2`.

6.  **Enhance UI Feedback:**
    *   **Problem:** `ShowMessage` is used for debugging info and error messages, which interrupts gameplay.
    *   **Task:** Replace all `ShowMessage` calls related to game flow and validation with less intrusive methods. Use a `TStatusBar` component added to `Form3` to display status messages (e.g., "Invalid move: destination occupied", "Player 2's turn"). Alternatively, use a dedicated `TLabel`. Ensure highlighting (`SurlignerCase`) is clear and consistently applied/removed.

7.  **Remove Unused UI Components:**
    *   **Problem:** `RadioButton1`, `RadioButton2`, and potentially `LabeledEdit4` appear to be unused in the form's current logic.
    *   **Task:** Remove these components from `Form3` using the Lazarus Form Designer (or by manually editing `Unit3.pas` and `Unit3.lfm`) to clean up the interface and code.

8.  **Clarify or Remove Button1:**
    *   **Problem:** The purpose and connection of `Button1` and its `ButtonOnClick` handler are unclear, especially if move execution happens directly in `StringGrid1Click`.
    *   **Task:** Decide on the role of `Button1`. If moves are executed immediately on the second grid click (as recommended in Task 1), remove `Button1` and its `ButtonOnClick` handler entirely. If it serves another purpose (e.g., "Offer Draw", "Resign"), rename the button and handler appropriately and implement that functionality. Ensure the handler name (`ButtonOnClick`) matches the component's `OnClick` event assignment in the Object Inspector if kept. 