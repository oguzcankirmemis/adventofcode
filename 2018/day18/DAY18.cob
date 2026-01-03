       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY18.
       AUTHOR. OK999.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
          SELECT INPUT-FILE
          ASSIGN TO "./inputs/example.txt"
          ORGANIZATION IS LINE SEQUENTIAL.
      * 
       DATA DIVISION.
       FILE SECTION.
          FD INPUT-FILE.
          01 INPUT-FILE-RECORD.
             03 INPUT-STR PIC X(100).
       WORKING-STORAGE SECTION.
          77 WS-STABILIZATION PIC 9(18) USAGE COMP-5 VALUE 20000.
          77 WS-T-THRESHOLD PIC 9(18) USAGE COMP-5 VALUE 2.
          77 WS-L-THRESHOLD PIC 9(18) USAGE COMP-5 VALUE 2.
          77 WS-L-T-THRESHOLD PIC 9(18) USAGE COMP-5 VALUE 0.
          77 WS-L-L-THRESHOLD PIC 9(18) USAGE COMP-5 VALUE 0.
          77 WS-O PIC X(1) VALUE ".".
          77 WS-T PIC X(1) VALUE "|".
          77 WS-L PIC X(1) VALUE "#".
          77 WS-PART-1-MINUTES PIC 9(18) USAGE COMP-5 VALUE 10.
          77 WS-PART-2-MINUTES PIC 9(18) USAGE COMP-5 VALUE 1000000000.
          01 WS-INPUT-FILE-EOF PIC X(1) VALUE "N".
          01 WS-ROWS PIC 9(18) USAGE COMP-5 VALUE 0.
          01 WS-COLS PIC 9(18) USAGE COMP-5 VALUE 0.
          01 WS-DP OCCURS 2.
             03 WS-R OCCURS 50.
                05 WS-C.
                   07 WS-M PIC X(1) OCCURS 50.
                      88 WS-OPEN VALUE ".".
                      88 WS-TREE VALUE "|".
                      88 WS-LUMBER VALUE "#".
                05 WS-C-STR REDEFINES WS-C PIC X(50).
          01 WS-METRICS.
             03 WS-OPEN-COUNT PIC 9(18) USAGE COMP-5.
             03 WS-TREE-COUNT PIC 9(18) USAGE COMP-5.
             03 WS-LUMBER-COUNT PIC 9(18) USAGE COMP-5.
          01 WS-I-1 PIC 9(18) USAGE COMP-5.
          01 WS-I-2 PIC 9(18) USAGE COMP-5.
          01 WS-T-1 PIC S9(18) USAGE COMP-5.
          01 WS-T-2 PIC S9(18) USAGE COMP-5.
          01 WS-T-3 PIC S9(18) USAGE COMP-5.
          01 WS-T-4 PIC S9(18) USAGE COMP-5.
          01 WS-I-R PIC 9(18) USAGE COMP-5.
          01 WS-I-C PIC 9(18) USAGE COMP-5.
          01 WS-MINUTES PIC 9(18) USAGE COMP-5 VALUE 0.
          01 WS-HASHMAP PIC 9(18) USAGE COMP-5 VALUE 0 OCCURS 2000000.
          01 WS-PART-1-RESULT PIC 9(18) USAGE COMP-5.
          01 WS-PART-2-RESULT PIC 9(18) USAGE COMP-5.
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
                   MOVE INPUT-STR TO WS-C-STR (1, WS-ROWS)
          END-PERFORM.
          CLOSE INPUT-FILE.
          PERFORM VARYING WS-I-1 FROM 1 BY 1
          UNTIL WS-M (1, WS-ROWS, WS-I-1) = " "
             MOVE WS-I-1 TO WS-COLS
          END-PERFORM.
          MOVE 1 TO WS-I-1.
          MOVE 2 TO WS-I-2.
          EXIT.
      *
       PART-1 SECTION.
          PERFORM VARYING WS-MINUTES FROM 0 BY 1
          UNTIL WS-MINUTES = WS-PART-1-MINUTES
             PERFORM ITERATE
          END-PERFORM.
          PERFORM CHECK-ALL-METRICS.
          COMPUTE WS-PART-1-RESULT = WS-TREE-COUNT * WS-LUMBER-COUNT.
          DISPLAY "PART 1: " WS-PART-1-RESULT.
          EXIT.
      *
       PART-2 SECTION.
          PERFORM VARYING WS-MINUTES FROM WS-MINUTES BY 1
          UNTIL WS-MINUTES = WS-STABILIZATION
             PERFORM ITERATE
          END-PERFORM.
          PERFORM FOREVER
             PERFORM ITERATE
             ADD 1 TO WS-MINUTES
             PERFORM CHECK-ALL-METRICS
             COMPUTE WS-T-1 = WS-TREE-COUNT * WS-LUMBER-COUNT
             IF NOT WS-HASHMAP (WS-T-1) = 0 THEN
                COMPUTE WS-T-2 = WS-MINUTES - WS-HASHMAP (WS-T-1)
                COMPUTE WS-T-3 = WS-PART-2-MINUTES - WS-MINUTES
                COMPUTE WS-T-4 = FUNCTION MOD (WS-T-3, WS-T-2)
                COMPUTE WS-MINUTES = WS-PART-2-MINUTES - WS-T-4
                EXIT PERFORM
             END-IF
             MOVE WS-MINUTES TO WS-HASHMAP (WS-T-1)
          END-PERFORM.
          PERFORM VARYING WS-MINUTES FROM WS-MINUTES BY 1
          UNTIL WS-MINUTES = WS-PART-2-MINUTES
             PERFORM ITERATE
          END-PERFORM.
          PERFORM CHECK-ALL-METRICS.
          COMPUTE WS-PART-2-RESULT = WS-TREE-COUNT * WS-LUMBER-COUNT.
          DISPLAY "PART 2: " WS-PART-2-RESULT.
          EXIT.
      *
       CHECK-NEIGHBOUR-METRICS SECTION.
          MOVE 0 TO WS-OPEN-COUNT.
          MOVE 0 TO WS-TREE-COUNT.
          MOVE 0 TO WS-LUMBER-COUNT.
          PERFORM VARYING WS-T-1 FROM -1 BY 1
          UNTIL WS-T-1 > 1
             PERFORM VARYING WS-T-2 FROM -1 BY 1
             UNTIL WS-T-2 > 1
                PERFORM CHECK-NEIGHBOUR
             END-PERFORM
          END-PERFORM.
          EXIT.
      *
       CHECK-NEIGHBOUR SECTION.
          IF WS-T-1 = 0 AND WS-T-2 = 0 THEN
             EXIT SECTION
          END-IF.
          COMPUTE WS-T-3 = WS-I-R + WS-T-1.
          IF WS-T-3 < 1 OR WS-T-3 > WS-ROWS THEN
             EXIT SECTION
          END-IF.
          COMPUTE WS-T-4 = WS-I-C + WS-T-2.
          IF WS-T-4 < 1 OR WS-T-4 > WS-COLS THEN
             EXIT SECTION
          END-IF.
          IF WS-OPEN (WS-I-1, WS-T-3, WS-T-4) THEN
             ADD 1 TO WS-OPEN-COUNT
          END-IF.
          IF WS-TREE (WS-I-1, WS-T-3, WS-T-4) THEN
             ADD 1 TO WS-TREE-COUNT
          END-IF.
          IF WS-LUMBER (WS-I-1, WS-T-3, WS-T-4) THEN
             ADD 1 TO WS-LUMBER-COUNT
          END-IF.
          EXIT.
      *
       CHECK-ALL-METRICS SECTION.
          MOVE 0 TO WS-OPEN-COUNT.
          MOVE 0 TO WS-TREE-COUNT.
          MOVE 0 TO WS-LUMBER-COUNT.
          PERFORM VARYING WS-I-R FROM 1 BY 1
          UNTIL WS-I-R > WS-ROWS
             PERFORM VARYING WS-I-C FROM 1 BY 1
             UNTIL WS-I-C > WS-COLS
                IF WS-OPEN (WS-I-1, WS-I-R, WS-I-C) THEN
                   ADD 1 TO WS-OPEN-COUNT
                END-IF
                IF WS-TREE (WS-I-1, WS-I-R, WS-I-C) THEN
                   ADD 1 TO WS-TREE-COUNT
                END-IF
                IF WS-LUMBER (WS-I-1, WS-I-R, WS-I-C) THEN
                   ADD 1 TO WS-LUMBER-COUNT
                END-IF
             END-PERFORM
          END-PERFORM.
      *
       ITERATE SECTION.
          PERFORM VARYING WS-I-R FROM 1 BY 1
          UNTIL WS-I-R > WS-ROWS
             PERFORM VARYING WS-I-C FROM 1 BY 1
             UNTIL WS-I-C > WS-COLS
                PERFORM ITERATE-ONE
             END-PERFORM
          END-PERFORM.
          MOVE WS-I-1 TO WS-T-3.
          MOVE WS-I-2 TO WS-I-1.
          MOVE WS-T-3 TO WS-I-2.
          EXIT.
      *
       ITERATE-ONE SECTION.
          PERFORM CHECK-NEIGHBOUR-METRICS.
          IF WS-OPEN (WS-I-1, WS-I-R, WS-I-C) THEN
             IF WS-TREE-COUNT > WS-T-THRESHOLD THEN
                MOVE WS-T TO WS-M (WS-I-2, WS-I-R, WS-I-C)
             ELSE
                MOVE WS-O TO WS-M (WS-I-2, WS-I-R, WS-I-C)
             END-IF
          END-IF.
          IF WS-TREE (WS-I-1, WS-I-R, WS-I-C) THEN
             IF WS-LUMBER-COUNT > WS-L-THRESHOLD THEN
                MOVE WS-L TO WS-M (WS-I-2, WS-I-R, WS-I-C)
             ELSE
                MOVE WS-T TO WS-M (WS-I-2, WS-I-R, WS-I-C)
             END-IF
          END-IF.
          IF WS-LUMBER (WS-I-1, WS-I-R, WS-I-C) THEN
             IF WS-TREE-COUNT > WS-L-T-THRESHOLD AND
                WS-LUMBER-COUNT > WS-L-L-THRESHOLD THEN
                   MOVE WS-L TO WS-M (WS-I-2, WS-I-R, WS-I-C)
             ELSE
                   MOVE WS-O TO WS-M (WS-I-2, WS-I-R, WS-I-C)
             END-IF
          END-IF.
          EXIT.
