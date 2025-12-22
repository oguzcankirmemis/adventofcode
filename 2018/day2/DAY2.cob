       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY2.
       AUTHOR. OK999.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
          SELECT INPUT-F
             ASSIGN TO "./inputs/example.txt"
             ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
          FD INPUT-F.
          01 INPUT-LINE PIC X(26).
       WORKING-STORAGE SECTION.
          01 WS-EOF PIC A(1) VALUE "N".
          01 WS-STR PIC X(26).
          01 WS-CHAR PIC X.
          01 WS-CODE PIC 9(3) USAGE COMP-3.
          01 WS-TABLE-STR.
             03 WS-TABLE OCCURS 128.
                05 WS-CHAR-COUNT PIC 9(2) USAGE DISPLAY VALUE 0.
          01 WS-IDX-1 PIC 9(3) USAGE COMP-3.
          01 WS-IDX-2 PIC 9(3) USAGE COMP-3.
          01 PART-1-RESULT.
             03 COUNT-TWO PIC 9(18) COMP-3 VALUE 0.
             03 COUNT-THREE PIC 9(18) COMP-3 VALUE 0.
             03 CHECKSUM PIC 9(18) COMP-3 VALUE 0.
          01 PART-2-RESULT.
             03 DIFF-COUNT PIC 9(2) VALUE 0.
             03 DIFF-IDX PIC 9(2) VALUE 0.
             03 PART-2-IDX PIC 9(3) VALUE 1.
             03 PART-2-TABLE OCCURS 250.
                05 INPUT-STR PIC X(26).

      *
       PROCEDURE DIVISION.
       PART-1 SECTION.
          OPEN INPUT INPUT-F.
          PERFORM UNTIL WS-EOF = "Y"
             INITIALIZE WS-TABLE-STR
             READ INPUT-F
                AT END
                   MOVE "Y" TO WS-EOF
                NOT AT END
                   MOVE INPUT-LINE TO INPUT-STR (PART-2-IDX)
                   ADD 1 TO PART-2-IDX
                   PERFORM COUNT-CHARS
                   PERFORM UPDATE-COUNTS
          END-PERFORM.
          CLOSE INPUT-F.
          COMPUTE CHECKSUM = COUNT-TWO * COUNT-THREE.
          DISPLAY "PART 1: " CHECKSUM.
      *
       PART-2 SECTION.
          PERFORM VARYING WS-IDX-1 FROM 1 BY 1 
          UNTIL WS-IDX-1 > 250
             COMPUTE WS-IDX-2 = WS-IDX-1 + 1
             PERFORM VARYING WS-IDX-2 FROM WS-IDX-2 BY 1 
             UNTIL WS-IDX-2 > 250
                PERFORM COUNT-DIFFS
                IF DIFF-COUNT = 1 THEN
                   GO TO PART-2-99
                END-IF
             END-PERFORM
          END-PERFORM.
       PART-2-99.
          MOVE INPUT-STR (WS-IDX-1) TO WS-STR.
          DISPLAY "PART 2: " WS-STR (1:DIFF-IDX - 1)
             WS-STR (DIFF-IDX + 1:26 - DIFF-IDX).
          STOP RUN.
      *
       COUNT-CHARS SECTION.
          PERFORM VARYING WS-IDX-1 FROM 1 BY 1
          UNTIL WS-IDX-1 > LENGTH OF INPUT-LINE
             MOVE INPUT-LINE (WS-IDX-1:1) TO WS-CHAR
             COMPUTE WS-CODE = FUNCTION ORD (WS-CHAR)
             ADD 1 TO WS-CHAR-COUNT (WS-CODE)
          END-PERFORM.
          EXIT.
      *
       UPDATE-COUNTS SECTION.
          PERFORM VARYING WS-IDX-1 FROM 98 BY 1 UNTIL WS-IDX-1 > 123
             IF WS-CHAR-COUNT (WS-IDX-1) = 2 THEN
                ADD 1 TO COUNT-TWO
                GO TO UPDATE-COUNTS-50
             END-IF
          END-PERFORM.
      *
       UPDATE-COUNTS-50.
          PERFORM VARYING WS-IDX-1 FROM 98 BY 1 UNTIL WS-IDX-1 > 123
             IF WS-CHAR-COUNT (WS-IDX-1) = 3 THEN
                ADD 1 TO COUNT-THREE
                GO TO UPDATE-COUNTS-99
             END-IF
          END-PERFORM.
      *
       UPDATE-COUNTS-99.
          EXIT.
      * 
       COUNT-DIFFS SECTION.
          MOVE 0 TO DIFF-COUNT.
          PERFORM VARYING PART-2-IDX FROM 1 BY 1
          UNTIL PART-2-IDX > LENGTH OF INPUT-STR (WS-IDX-1)
             IF NOT INPUT-STR (WS-IDX-1) (PART-2-IDX:1) 
                = INPUT-STR (WS-IDX-2) (PART-2-IDX:1)
             THEN
                ADD 1 TO DIFF-COUNT
                MOVE PART-2-IDX TO DIFF-IDX
             END-IF
          END-PERFORM.
          EXIT.

       