       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY9.
       AUTHOR. OK999.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
          SELECT INPUT-FILE
          ASSIGN TO "./inputs/example.txt"
          ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
          FD INPUT-FILE.
          01 INPUT-FILE-RECORD.
             03 INPUT-STR PIC X(60).
       WORKING-STORAGE SECTION.
          77 WS-DIGITS PIC 9(1) USAGE BINARY VALUE 8.
          77 WS-WIN PIC 9(2) USAGE BINARY VALUE 23.
          77 WS-DECREMENT PIC 9(1) USAGE BINARY VALUE 7.
          01 WS-INPUT-FILE-EOF PIC X(1) VALUE "N".
          01 WS-DUMMY PIC X(25).
          01 WS-TMP-1 PIC 9(18).
          01 WS-TMP-2 PIC 9(18).
          01 WS-TMP-3 PIC 9(18).
          01 WS-SETUP.
             03 WS-PLAYERS PIC 9(18) USAGE BINARY.
             03 WS-MARBLES PIC 9(18) USAGE BINARY.
          01 WS-LENGTH PIC 9(18) USAGE BINARY VALUE 1.
          01 WS-CURRENT PIC 9(18) USAGE BINARY VALUE 1.
          01 WS-PLAYER PIC 9(18) USAGE BINARY VALUE 1.
          01 WS-TO-PLACE PIC 9(18) USAGE BINARY VALUE 1.
          01 WS-SCORE-TABLE.
             03 WS-SCORE PIC 9(18) USAGE BINARY VALUE 0 OCCURS 1000.
          01 WS-MARBLE-TABLE.
             03 WS-MARBLE PIC 9(8) VALUE 0 OCCURS 8000000.
          01 WS-LIST-SIZE PIC 9(8) USAGE BINARY VALUE 1.
          01 WS-ROOT PIC 9(8) USAGE BINARY VALUE 1.
          01 WS-LIST OCCURS 8000000.
             03 WS-VALUE PIC 9(8) USAGE BINARY VALUE 0.
             03 WS-PREV PIC 9(8) USAGE BINARY VALUE 1.
             03 WS-NEXT PIC 9(8) USAGE BINARY VALUE 1.
          01 WS-IDX-1 PIC 9(8).
          01 WS-IDX-2 PIC 9(8) USAGE BINARY.
          01 WS-PART-1-RESULT PIC 9(18) USAGE BINARY VALUE 0.
          01 WS-PART-2-RESULT PIC 9(18) USAGE BINARY VALUE 0.
      *
       PROCEDURE DIVISION.
       PARSE-INPUT SECTION.
          OPEN INPUT INPUT-FILE.
          PERFORM UNTIL WS-INPUT-FILE-EOF = "Y"
             READ INPUT-FILE
                AT END
                   MOVE "Y" TO WS-INPUT-FILE-EOF
                NOT AT END
                   UNSTRING INPUT-STR
                      DELIMITED BY " "
                      INTO WS-TMP-1, WS-DUMMY, WS-DUMMY, WS-DUMMY,
                         WS-DUMMY, WS-DUMMY, WS-TMP-2, WS-DUMMY
                   MOVE WS-TMP-1 TO WS-PLAYERS
                   MOVE WS-TMP-2 TO WS-MARBLES
             END-READ
          END-PERFORM.
          CLOSE INPUT-FILE.
          PERFORM PART-1.
          INITIALIZE WS-SCORE-TABLE.
          MULTIPLY 100 BY WS-MARBLES.
          PERFORM PART-2.
          STOP RUN.
      *
       PART-1 SECTION.
          PERFORM VARYING WS-IDX-1 FROM 1 BY 1
          UNTIL WS-IDX-1 > WS-MARBLES
             PERFORM PLACE-MARBLE
             COMPUTE WS-PLAYER = FUNCTION MOD (WS-PLAYER WS-PLAYERS) + 1
          END-PERFORM.
          PERFORM VARYING WS-IDX-1 FROM 1 BY 1
          UNTIL WS-IDX-1 > WS-PLAYERS
             IF WS-SCORE (WS-IDX-1) GREATER THAN WS-PART-1-RESULT THEN
                MOVE WS-SCORE (WS-IDX-1) TO WS-PART-1-RESULT
             END-IF
          END-PERFORM.
          DISPLAY "PART 1: " WS-PART-1-RESULT.
          EXIT.
      *
       PART-2 SECTION.
          PERFORM VARYING WS-IDX-2 FROM 1 BY 1
          UNTIL WS-IDX-2 > WS-MARBLES
             IF FUNCTION MOD (WS-IDX-2 WS-WIN) = 0 THEN
                ADD WS-IDX-2 TO WS-SCORE (WS-PLAYER)
                PERFORM REMOVE-FROM-LIST
             ELSE
                PERFORM INSERT-TO-LIST
             END-IF 
             COMPUTE WS-PLAYER = FUNCTION MOD (WS-PLAYER WS-PLAYERS) + 1
          END-PERFORM.
          PERFORM VARYING WS-IDX-2 FROM 1 BY 1
          UNTIL WS-IDX-2 > WS-PLAYERS
             IF WS-SCORE (WS-IDX-2) GREATER THAN WS-PART-2-RESULT THEN
                MOVE WS-SCORE (WS-IDX-2) TO WS-PART-2-RESULT
             END-IF
          END-PERFORM.
          DISPLAY "PART 2: " WS-PART-2-RESULT.
          EXIT.
       PLACE-MARBLE SECTION.
          IF FUNCTION MOD(WS-IDX-1 WS-WIN) = 0 THEN
             PERFORM REMOVE-MARBLE
             EXIT SECTION
          END-IF.
          COMPUTE WS-TMP-1 = WS-CURRENT + 1.
          COMPUTE WS-TMP-1 = FUNCTION MOD (WS-TMP-1 WS-LENGTH) + 1
          COMPUTE WS-TMP-2 = WS-DIGITS * (WS-LENGTH - WS-TMP-1 + 1).
          COMPUTE WS-TMP-3 = WS-DIGITS * (WS-TMP-1 - 1) + 1.
          MOVE WS-MARBLE-TABLE (WS-TMP-3 : WS-TMP-2)
             TO WS-MARBLE-TABLE (WS-TMP-3 + WS-DIGITS : WS-TMP-2).
          MOVE WS-IDX-1 TO WS-MARBLE (WS-TMP-1).
          MOVE WS-TMP-1 TO WS-CURRENT.
          ADD 1 TO WS-LENGTH.
          EXIT.
      *
       REMOVE-MARBLE SECTION.
          IF WS-CURRENT LESS THAN OR EQUAL WS-DECREMENT THEN
             COMPUTE WS-TMP-1 = WS-LENGTH - WS-DECREMENT + WS-CURRENT
          ELSE
             COMPUTE WS-TMP-1 = WS-CURRENT - WS-DECREMENT
          END-IF.
          COMPUTE WS-TMP-2 = WS-DIGITS * (WS-LENGTH - WS-TMP-1).
          COMPUTE WS-TMP-3 = WS-DIGITS * (WS-TMP-1 - 1) + 1.
          ADD WS-MARBLE (WS-TMP-1) TO WS-SCORE (WS-PLAYER).
          ADD WS-IDX-1 TO WS-SCORE (WS-PLAYER).
          MOVE WS-MARBLE-TABLE (WS-TMP-3 + WS-DIGITS : WS-TMP-2)
             TO WS-MARBLE-TABLE (WS-TMP-3 : WS-TMP-2).
          MOVE WS-TMP-1 TO WS-CURRENT.
          SUBTRACT 1 FROM WS-LENGTH.
          EXIT.
      *
       INSERT-TO-LIST SECTION.
          ADD 1 TO WS-LIST-SIZE.
      *
          MOVE WS-NEXT (WS-ROOT) TO WS-TMP-1.
          MOVE WS-NEXT (WS-TMP-1) TO WS-TMP-2.
      *
          MOVE WS-IDX-2 TO WS-VALUE (WS-LIST-SIZE).
          MOVE WS-TMP-1 TO WS-PREV (WS-LIST-SIZE).
          MOVE WS-TMP-2 TO WS-NEXT (WS-LIST-SIZE).
      *
          MOVE WS-LIST-SIZE TO WS-NEXT (WS-TMP-1).
          MOVE WS-LIST-SIZE TO WS-PREV (WS-TMP-2).
      *
          MOVE WS-LIST-SIZE TO WS-ROOT.
      *
          EXIT.
      *
        REMOVE-FROM-LIST SECTION.
           MOVE WS-PREV (WS-ROOT) TO WS-TMP-1.
           MOVE WS-PREV (WS-TMP-1) TO WS-TMP-1.
           MOVE WS-PREV (WS-TMP-1) TO WS-TMP-1.
           MOVE WS-PREV (WS-TMP-1) TO WS-TMP-1.
           MOVE WS-PREV (WS-TMP-1) TO WS-TMP-1.
           MOVE WS-PREV (WS-TMP-1) TO WS-TMP-1.
           MOVE WS-PREV (WS-TMP-1) TO WS-TMP-1.
      *
           MOVE WS-PREV (WS-TMP-1) TO WS-TMP-2.
           MOVE WS-NEXT (WS-TMP-1) TO WS-TMP-3.
      *
           MOVE WS-TMP-3 TO WS-NEXT (WS-TMP-2).
           MOVE WS-TMP-2 TO WS-PREV (WS-TMP-3).
      *
           MOVE WS-TMP-3 TO WS-ROOT.
      *
           ADD WS-VALUE (WS-TMP-1) TO WS-SCORE (WS-PLAYER).
      *
           EXIT.


