       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY13.
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
                03 INPUT-STR PIC X(200).
       WORKING-STORAGE SECTION.
          01 WS-INPUT-FILE-EOF PIC X(1) VALUE "N".
          01 WS-COLLISION-TABLE OCCURS 200.
             03 WS-COLLISION PIC X(1) OCCURS 200 VALUE " ".
                88 WS-CRASH VALUE "X".
          01 WS-MAP.
             03 WS-ROWS PIC 9(4) USAGE BINARY VALUE 0.
             03 WS-ROW OCCURS 200.
                05 WS-COL.
                   07 WS-M PIC X(1) OCCURS 200.
                05 WS-R REDEFINES WS-COL PIC X(200).
          01 WS-CARTS PIC 9(2) USAGE BINARY VALUE 0.
          01 WS-CART OCCURS 25.
             03 WS-COORD.
                05 WS-CRASHED PIC X(1) VALUE "X".
                   88 WS-REMOVED VALUE "X".
                05 WS-C-R PIC 9(4) VALUE 9999.
                05 FILLER PIC X(1) VALUE "#".
                05 WS-C-C PIC 9(4) VALUE 9999.
             03 WS-SORT-KEY REDEFINES WS-COORD PIC X(10).
             03 WS-DIR PIC 9(1).
                88 WS-LEFT VALUE 0.
                88 WS-UP VALUE 1.
                88 WS-RIGHT VALUE 2.
                88 WS-DOWN VALUE 3.
             03 WS-NEXT PIC 9(1).
                88 WS-TURN-LEFT VALUE 0.
                88 WS-GO-STRAIGHT VALUE 1.
                88 WS-TURN-RIGHT VALUE 2.
          01 WS-I-1 PIC 9(4) USAGE BINARY.
          01 WS-I-2 PIC 9(4) USAGE BINARY.
          01 WS-T-1 PIC 9(4) USAGE BINARY.
          01 WS-T-2 PIC 9(4) USAGE BINARY.
          01 WS-T-3 PIC 9(4) USAGE BINARY.
          01 WS-CRASHES PIC 9(4) USAGE BINARY.
          01 WS-PART-1-RESULT.
             03 WS-X-1 PIC 9(4) VALUE 9999.
             03 FILLER PIC X(1) VALUE ",".
             03 WS-Y-1 PIC 9(4) VALUE 9999.
          01 WS-PART-2-RESULT.
             03 WS-X-2 PIC 9(4) VALUE 9999.
             03 FILLER PIC X(1) VALUE ",".
             03 WS-Y-2 PIC 9(4) VALUE 9999.
      *
       PROCEDURE DIVISION.
       MAIN SECTION.
          PERFORM PARSE-INPUT.
          PERFORM PART-1.
          PERFORM PART-2.
          STOP RUN.
      *
       PARSE-INPUT SECTION.
          OPEN INPUT INPUT-FILE.
          PERFORM UNTIL WS-INPUT-FILE-EOF = "Y"
             READ INPUT-FILE
                AT END
                   MOVE "Y" TO WS-INPUT-FILE-EOF
                NOT AT END
                   ADD 1 TO WS-ROWS
                   MOVE INPUT-STR TO WS-R (WS-ROWS)
                   PERFORM VARYING WS-I-1 FROM 1 BY 1
                   UNTIL WS-I-1 > LENGTH OF INPUT-STR
                      EVALUATE WS-M (WS-ROWS, WS-I-1)
                         WHEN "<"
                            ADD 1 TO WS-CARTS
                            MOVE WS-ROWS TO WS-C-R (WS-CARTS)
                            MOVE WS-I-1 TO WS-C-C (WS-CARTS)
                            MOVE 0 TO WS-DIR (WS-CARTS)
                            MOVE 0 TO WS-NEXT (WS-CARTS)
                            MOVE " " TO WS-CRASHED (WS-CARTS)
                         WHEN "^"
                            ADD 1 TO WS-CARTS
                            MOVE WS-ROWS TO WS-C-R (WS-CARTS)
                            MOVE WS-I-1 TO WS-C-C (WS-CARTS)
                            MOVE 1 TO WS-DIR (WS-CARTS)
                            MOVE 0 TO WS-NEXT (WS-CARTS)
                            MOVE " " TO WS-CRASHED (WS-CARTS)
                         WHEN ">"
                            ADD 1 TO WS-CARTS
                            MOVE WS-ROWS TO WS-C-R (WS-CARTS)
                            MOVE WS-I-1 TO WS-C-C (WS-CARTS)
                            MOVE 2 TO WS-DIR (WS-CARTS)
                            MOVE 0 TO WS-NEXT (WS-CARTS)
                            MOVE " " TO WS-CRASHED (WS-CARTS)
                         WHEN "v"
                            ADD 1 TO WS-CARTS
                            MOVE WS-ROWS TO WS-C-R (WS-CARTS)
                            MOVE WS-I-1 TO WS-C-C (WS-CARTS)
                            MOVE 3 TO WS-DIR (WS-CARTS)
                            MOVE 0 TO WS-NEXT (WS-CARTS)
                            MOVE " " TO WS-CRASHED (WS-CARTS)
                      END-EVALUATE
                   END-PERFORM
             END-READ
          END-PERFORM.
          CLOSE INPUT-FILE.
          EXIT.
      *
       PART-1 SECTION.
          PERFORM UNTIL WS-X-1 < 9999 OR WS-Y-1 < 9999
             PERFORM TICK
          END-PERFORM.
          DISPLAY "PART 1: " WS-PART-1-RESULT.
          EXIT.
      *
       PART-2 SECTION.
          PERFORM UNTIL WS-CARTS = 1
             PERFORM TICK
          END-PERFORM.
          SORT WS-CART ON ASCENDING WS-SORT-KEY.
          COMPUTE WS-Y-2 = WS-C-R (1) - 1.
          COMPUTE WS-X-2 = WS-C-C (1) - 1.
          DISPLAY "PART 2: " WS-PART-2-RESULT.
          EXIT.
      *
       TICK SECTION.
          MOVE 0 TO WS-CRASHES.
          SORT WS-CART ON ASCENDING WS-SORT-KEY.
          PERFORM VARYING WS-I-1 FROM 1 BY 1
          UNTIL WS-I-1 > WS-CARTS
             PERFORM MOVE-CART
          END-PERFORM.
          SUBTRACT WS-CRASHES FROM WS-CARTS
          EXIT.
      *
       MOVE-CART SECTION.
          IF WS-REMOVED (WS-I-1) THEN
             EXIT SECTION
          END-IF.
          MOVE WS-C-R (WS-I-1) TO WS-T-1.
          MOVE WS-C-C (WS-I-1) TO WS-T-2.
          MOVE " " TO WS-COLLISION (WS-T-1, WS-T-2).
          IF WS-LEFT (WS-I-1) THEN
             SUBTRACT 1 FROM WS-T-2
          ELSE IF WS-UP (WS-I-1) THEN
             SUBTRACT 1 FROM WS-T-1
          ELSE IF WS-RIGHT (WS-I-1) THEN
             ADD 1 TO WS-T-2
          ELSE
             ADD 1 TO WS-T-1
          END-IF.
          IF WS-CRASH (WS-T-1, WS-T-2) THEN
             PERFORM HANDLE-CRASH
             EXIT SECTION
          END-IF.
          MOVE "X" TO WS-COLLISION (WS-T-1, WS-T-2).
          MOVE WS-T-1 TO WS-C-R (WS-I-1).
          MOVE WS-T-2 TO WS-C-C (WS-I-1).
          EVALUATE WS-M (WS-T-1, WS-T-2)
             WHEN "/"
                IF WS-LEFT (WS-I-1) OR WS-RIGHT (WS-I-1) THEN
                   COMPUTE WS-T-3 = WS-DIR (WS-I-1) + 3
                   COMPUTE WS-DIR (WS-I-1) = FUNCTION MOD (WS-T-3 4)
                ELSE
                   COMPUTE WS-T-3 = WS-DIR (WS-I-1) + 1
                   COMPUTE WS-DIR (WS-I-1) = FUNCTION MOD (WS-T-3 4)
                END-IF
             WHEN "\"
                IF WS-LEFT (WS-I-1) OR WS-RIGHT (WS-I-1) THEN
                   COMPUTE WS-T-3 = WS-DIR (WS-I-1) + 1
                   COMPUTE WS-DIR (WS-I-1) = FUNCTION MOD (WS-T-3 4)
                ELSE
                   COMPUTE WS-T-3 = WS-DIR (WS-I-1) + 3
                   COMPUTE WS-DIR (WS-I-1) = FUNCTION MOD (WS-T-3 4)
                END-IF
             WHEN "+"
                IF WS-TURN-LEFT (WS-I-1) THEN
                   COMPUTE WS-T-3 = WS-DIR (WS-I-1) + 3
                   COMPUTE WS-DIR (WS-I-1) = FUNCTION MOD (WS-T-3 4)
                END-IF
                IF WS-TURN-RIGHT (WS-I-1) THEN
                   COMPUTE WS-T-3 = WS-DIR (WS-I-1) + 1
                   COMPUTE WS-DIR (WS-I-1) = FUNCTION MOD (WS-T-3 4)
                END-IF
                COMPUTE WS-T-3 = WS-NEXT (WS-I-1) + 1
                COMPUTE WS-NEXT (WS-I-1) = FUNCTION MOD (WS-T-3 3)
          END-EVALUATE.
          EXIT.
      *
       HANDLE-CRASH SECTION.
          ADD 2 TO WS-CRASHES.
          IF WS-X-1 = 9999 AND WS-Y-1 = 9999 THEN
             COMPUTE WS-Y-1 = WS-T-1 - 1
             COMPUTE WS-X-1 = WS-T-2 - 1
          END-IF.
          MOVE "X" TO WS-CRASHED (WS-I-1).
          PERFORM VARYING WS-I-2 FROM 1 BY 1
          UNTIL WS-I-2 > WS-CARTS
             IF WS-C-R (WS-I-2) = WS-T-1 AND
                WS-C-C (WS-I-2) = WS-T-2 THEN
                   MOVE "X" TO WS-CRASHED (WS-I-2)
                   MOVE " " TO WS-COLLISION (WS-T-1, WS-T-2)
                   EXIT SECTION
             END-IF
          END-PERFORM.
          EXIT.
