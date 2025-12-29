       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY12.
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
             03 INPUT-STR PIC X(120).
             03 INPUT-INIT REDEFINES INPUT-STR.
                05 FILLER PIC X(15).
                05 INPUT-STATE PIC X(1) OCCURS 105.
             03 INPUT-RULE REDEFINES INPUT-STR.
                05 INPUT-FROM PIC X(5).
                05 FILLER PIC X(4).
                05 INPUT-TO PIC X(1).
                05 FILLER PIC X(110).
       WORKING-STORAGE SECTION.
          77 WS-PART-1-SIMULATIONS PIC 9(18) USAGE BINARY VALUE 20.
          77 WS-STABILIZATION-SIMULATIONS PIC 9(18) USAGE BINARY
             VALUE 200.
          77 WS-PART-2-SIMULATIONS PIC 9(18) USAGE BINARY
             VALUE 50000000000.
          01 WS-INPUT-FILE-EOF PIC X(1) VALUE "N".
          01 WS-RULES.
             03 WS-R OCCURS 32.
                05 WS-F PIC X(5).
                05 WS-T PIC X(1).
             03 WS-R-L PIC 9(18) USAGE BINARY VALUE 0. 
          01 WS-STATE OCCURS 2.
             03 WS-LI PIC S9(18) USAGE BINARY OCCURS 100000.
             03 WS-LE PIC 9(18) USAGE BINARY VALUE 0.
          01 WS-C-T PIC 9(18) USAGE BINARY VALUE 1.
          01 WS-N-T PIC 9(18) USAGE BINARY VALUE 2.
          01 WS-TMP PIC 9(18) USAGE BINARY.
          01 WS-C-S.
             03 WS-S PIC X(1) OCCURS 5 VALUE ".".
          01 WS-S-STR REDEFINES WS-C-S PIC X(5).
          01 WS-I-1 PIC 9(18) USAGE BINARY.
          01 WS-I-2 PIC 9(18) USAGE BINARY.
          01 WS-C-P PIC S9(18) USAGE BINARY.
          01 WS-SIMULATIONS PIC S9(18) USAGE BINARY VALUE 0.
          01 WS-PART-1-RESULT PIC S9(18) USAGE BINARY VALUE 0.
          01 WS-PART-2-RESULT PIC S9(18) USAGE BINARY VALUE 0.
      *
       PROCEDURE DIVISION.
       MAIN SECTION.
          PERFORM PARSE-INPUT.
          PERFORM PART-1.
          PERFORM PART-2.
          STOP RUN.
      *
       PART-1 SECTION.
          PERFORM VARYING WS-SIMULATIONS FROM WS-SIMULATIONS BY 1
          UNTIL WS-SIMULATIONS = WS-PART-1-SIMULATIONS
             PERFORM SIMULATE
          END-PERFORM.
          PERFORM VARYING WS-I-1 FROM 1 BY 1
          UNTIL WS-I-1 > WS-LE (WS-C-T)
             ADD WS-LI (WS-C-T, WS-I-1) TO WS-PART-1-RESULT
          END-PERFORM.
          DISPLAY "PART 1: " WS-PART-1-RESULT.
          EXIT.
      *
       PART-2 SECTION.
          PERFORM VARYING WS-SIMULATIONS FROM WS-SIMULATIONS BY 1
          UNTIL WS-SIMULATIONS = WS-STABILIZATION-SIMULATIONS
             PERFORM SIMULATE
          END-PERFORM.
          PERFORM VARYING WS-I-1 FROM 1 BY 1
          UNTIL WS-I-1 > WS-LE (WS-C-T)
             ADD WS-LI (WS-C-T, WS-I-1) TO WS-PART-2-RESULT
          END-PERFORM.
          COMPUTE WS-PART-2-RESULT = WS-PART-2-RESULT +
             (WS-PART-2-SIMULATIONS - WS-STABILIZATION-SIMULATIONS) *
             WS-LE (WS-C-T).
          DISPLAY "PART 2: " WS-PART-2-RESULT.
          EXIT.
      *
       PARSE-INPUT SECTION.
          OPEN INPUT INPUT-FILE.
          READ INPUT-FILE.
          PERFORM VARYING WS-I-1 FROM 1 BY 1
          UNTIL INPUT-STATE (WS-I-1) = " "
             IF INPUT-STATE (WS-I-1) = "#" THEN
                ADD 1 TO WS-LE (WS-C-T)
                MOVE WS-LE (WS-C-T) TO WS-TMP
                COMPUTE WS-LI (WS-C-T, WS-TMP) = WS-I-1 - 1
             END-IF
          END-PERFORM.
          READ INPUT-FILE.
          PERFORM UNTIL WS-INPUT-FILE-EOF = "Y"
             READ INPUT-FILE
                AT END
                   MOVE "Y" TO WS-INPUT-FILE-EOF
                NOT AT END
                   ADD 1 TO WS-R-L
                   MOVE INPUT-FROM TO WS-F (WS-R-L)
                   MOVE INPUT-TO TO WS-T (WS-R-L)
             END-READ
          END-PERFORM.
          CLOSE INPUT-FILE.
          EXIT.
      *
       SIMULATE SECTION.
          MOVE 0 TO WS-LE (WS-N-T).
          PERFORM VARYING WS-I-1 FROM 1 BY 1
          UNTIL WS-I-1 > WS-LE (WS-C-T)
             PERFORM SIMULATE-ONE
          END-PERFORM.
          MOVE WS-N-T TO WS-TMP.
          MOVE WS-C-T TO WS-N-T.
          MOVE WS-TMP TO WS-C-T.
          EXIT.
      *
       SIMULATE-ONE SECTION.
          COMPUTE WS-C-P = WS-LI (WS-C-T, WS-I-1) - 2.
          IF WS-I-1 > 1 AND WS-LI (WS-C-T, WS-I-1 - 1) >= WS-C-P THEN
             GO TO SIMULATE-ONE-30
          END-IF.
          MOVE WS-LE (WS-N-T) TO WS-TMP.
          IF WS-TMP > 0 AND WS-LI (WS-N-T, WS-TMP) >= WS-C-P THEN
             GO TO SIMULATE-ONE-30
          END-IF.
          MOVE "....." TO WS-S-STR.
          COMPUTE WS-I-2 = WS-I-1 - 1.
          PERFORM SEARCH-BACKWARD.
          MOVE WS-I-1 TO WS-I-2.
          PERFORM SEARCH-FORWARD.
          PERFORM APPLY-RULE.
       SIMULATE-ONE-30.
          ADD 1 TO WS-C-P.
          IF WS-I-1 > 1 AND WS-LI (WS-C-T, WS-I-1 - 1) >= WS-C-P THEN
             GO TO SIMULATE-ONE-50
          END-IF.
          MOVE WS-LE (WS-N-T) TO WS-TMP.
          IF WS-TMP > 0 AND WS-LI (WS-N-T, WS-TMP) >= WS-C-P THEN
             GO TO SIMULATE-ONE-50
          END-IF.
          MOVE "....." TO WS-S-STR.
          COMPUTE WS-I-2 = WS-I-1 - 1.
          PERFORM SEARCH-BACKWARD.
          MOVE WS-I-1 TO WS-I-2.
          PERFORM SEARCH-FORWARD.
          PERFORM APPLY-RULE.
       SIMULATE-ONE-50.
          ADD 1 TO WS-C-P.
          MOVE "..#.." TO WS-S-STR.
          COMPUTE WS-I-2 = WS-I-1 - 1.
          PERFORM SEARCH-BACKWARD.
          COMPUTE WS-I-2 = WS-I-1 + 1.
          PERFORM SEARCH-FORWARD.
          PERFORM APPLY-RULE.
       SIMULATE-ONE-70.
          ADD 1 TO WS-C-P.
          IF WS-I-1 < WS-LE (WS-C-T) AND
             WS-LI (WS-C-T, WS-I-1 + 1) <= WS-C-P THEN
                GO TO SIMULATE-ONE-90
          END-IF.
          MOVE "....." TO WS-S-STR.
          MOVE WS-I-1 TO WS-I-2.
          PERFORM SEARCH-BACKWARD.
          COMPUTE WS-I-2 = WS-I-1 + 1.
          PERFORM SEARCH-FORWARD.
          PERFORM APPLY-RULE.
       SIMULATE-ONE-90.
          ADD 1 TO WS-C-P.
          IF WS-I-1 < WS-LE (WS-C-T) AND
             WS-LI (WS-C-T, WS-I-1 + 1) <= WS-C-P THEN
                GO TO SIMULATE-ONE-99
          END-IF.
          MOVE "....." TO WS-S-STR.
          MOVE WS-I-1 TO WS-I-2.
          PERFORM SEARCH-BACKWARD.
          COMPUTE WS-I-2 = WS-I-1 + 1.
          PERFORM SEARCH-FORWARD.
          PERFORM APPLY-RULE.
       SIMULATE-ONE-99.
          EXIT.
      *
       SEARCH-BACKWARD SECTION.
          IF WS-I-2 = 0 THEN
             EXIT SECTION
          END-IF.
          IF WS-C-P - WS-LI (WS-C-T, WS-I-2) = 1 THEN
             MOVE "#" TO WS-S (2)
          END-IF.
          IF WS-C-P - WS-LI (WS-C-T, WS-I-2) = 2 THEN
             MOVE "#" TO WS-S (1)
          END-IF.
          SUBTRACT 1 FROM WS-I-2.
          IF WS-I-2 = 0 THEN
             EXIT SECTION
          END-IF.
          IF WS-C-P - WS-LI (WS-C-T, WS-I-2) = 2 THEN
             MOVE "#" TO WS-S (1)
          END-IF.
          EXIT.
      *
       SEARCH-FORWARD SECTION.
          IF WS-I-2 > WS-LE (WS-C-T) THEN
             EXIT SECTION
          END-IF.
          IF WS-LI (WS-C-T, WS-I-2) - WS-C-P = 1 THEN
             MOVE "#" TO WS-S (4)
          END-IF.
          IF WS-LI (WS-C-T, WS-I-2) - WS-C-P = 2 THEN
             MOVE "#" TO WS-S (5)
          END-IF.
          ADD 1 TO WS-I-2.
          IF WS-I-2 > WS-LE (WS-C-T) THEN
             EXIT SECTION
          END-IF.
          IF WS-LI (WS-C-T, WS-I-2) - WS-C-P = 2 THEN
             MOVE "#" TO WS-S (5)
          END-IF.
          EXIT.
      *
       APPLY-RULE SECTION.
          PERFORM VARYING WS-I-2 FROM 1 BY 1
          UNTIL WS-I-2 > WS-R-L
             IF WS-S-STR = WS-F (WS-I-2) THEN
                IF WS-T (WS-I-2) = "#" THEN
                   ADD 1 TO WS-LE (WS-N-T)
                   MOVE WS-LE (WS-N-T) TO WS-TMP
                   MOVE WS-C-P TO WS-LI (WS-N-T, WS-TMP)
                END-IF
                EXIT SECTION
             END-IF
          END-PERFORM.
          EXIT.
          
       