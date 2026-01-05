       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY22.
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
             03 INPUT-STR PIC X(20).
       WORKING-STORAGE SECTION.
          77 WS-E-NEITHER PIC 9(18) USAGE COMP-5 VALUE 0.
          77 WS-E-TORCH PIC 9(18) USAGE COMP-5 VALUE 1.
          77 WS-E-CLIMBING-GEAR PIC 9(18) USAGE COMP-5 VALUE 2.
          01 WS-TMP.
             03 WS-T-STR-1 PIC X(20).
             03 WS-T-STR-2 PIC X(20).
             03 WS-TMP-1 PIC 9(18).
             03 WS-TMP-2 PIC 9(18).
             03 WS-T-1 PIC S9(18) USAGE COMP-5.
             03 WS-T-2 PIC S9(18) USAGE COMP-5.
          01 WS-CAVE.
             03 WS-DEPTH PIC 9(18) USAGE COMP-5.
             03 WS-TARGET-X PIC 9(18) USAGE COMP-5.
             03 WS-TARGET-Y PIC 9(18) USAGE COMP-5.
             03 WS-R OCCURS 1000.
                05 WS-C OCCURS 1000.
                   07 WS-G PIC 9(18) USAGE COMP-5 VALUE 0.
                   07 WS-E PIC 9(18) USAGE COMP-5 VALUE 0.
                   07 WS-M PIC X(1) VALUE "#".
                      88 WS-ROCKY VALUE ".".
                      88 WS-WET VALUE "=".
                      88 WS-NARROW VALUE "|".
          01 WS-EXPLORED.
             03 WS-E-X OCCURS 1000.
                05 WS-E-Y OCCURS 1000.
                   07 WS-E-E OCCURS 3.
                      10 WS-E-M PIC 9(18) USAGE COMP-5 VALUE 999999.
          01 WS-STACK.
             03 WS-S-L PIC 9(18) USAGE COMP-5 VALUE 0.
             03 WS-S-ELEMENT OCCURS 10000000.
                05 WS-S-X PIC 9(18) USAGE COMP-5.
                05 WS-S-Y PIC 9(18) USAGE COMP-5.
                05 WS-S-E PIC 9(18) USAGE COMP-5.
                05 WS-S-M PIC 9(18) USAGE COMP-5.
          01 WS-I.
             03 WS-I-X PIC 9(18) USAGE COMP-5.
             03 WS-I-Y PIC 9(18) USAGE COMP-5.
             03 WS-I-E PIC 9(18) USAGE COMP-5.
                88 WS-NEITHER VALUE 0.
                88 WS-TORCH VALUE 1.
                88 WS-CLIMBING-GEAR VALUE 2.
             03 WS-I-M PIC 9(18) USAGE COMP-5.
          01 WS-J.
             03 WS-J-X PIC 9(18) USAGE COMP-5.
             03 WS-J-Y PIC 9(18) USAGE COMP-5.
             03 WS-J-E PIC 9(18) USAGE COMP-5.
             03 WS-J-M PIC 9(18) USAGE COMP-5.
          01 WS-PART-1-RESULT PIC 9(18) USAGE COMP-5 VALUE 0.
          01 WS-PART-2-RESULT PIC 9(18) USAGE COMP-5 VALUE 1200.
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
          READ INPUT-FILE.
          UNSTRING INPUT-STR DELIMITED BY ": "
             INTO WS-T-STR-1, WS-TMP-1.
          MOVE WS-TMP-1 TO WS-DEPTH.
          READ INPUT-FILE.
          UNSTRING INPUT-STR DELIMITED BY ": "
             INTO WS-T-STR-1, WS-T-STR-2.
          UNSTRING WS-T-STR-2 DELIMITED BY ","
             INTO WS-TMP-1, WS-TMP-2.
          MOVE WS-TMP-1 TO WS-TARGET-X.
          MOVE WS-TMP-2 TO WS-TARGET-Y.
          CLOSE INPUT-FILE.
          EXIT.
      *
       PART-1 SECTION.
          PERFORM COMPUTE-ALL.
          DISPLAY "PART 1: " WS-PART-1-RESULT.
          EXIT.
      *
       PART-2 SECTION.
          PERFORM DFS.
          DISPLAY "PART 2: " WS-PART-2-RESULT.
          EXIT.
      *
       DFS SECTION.
          ADD 1 TO WS-S-L.
          MOVE 0 TO WS-S-X (WS-S-L).
          MOVE 0 TO WS-S-Y (WS-S-L).
          MOVE WS-E-TORCH TO WS-S-E (WS-S-L).
          MOVE 0 TO WS-S-M (WS-S-L).
          PERFORM UNTIL WS-S-L = 0
             MOVE WS-S-X (WS-S-L) TO WS-I-X
             MOVE WS-S-Y (WS-S-L) TO WS-I-Y
             MOVE WS-S-E (WS-S-L) TO WS-I-E
             MOVE WS-S-M (WS-S-L) TO WS-I-M
             SUBTRACT 1 FROM WS-S-L
             PERFORM DFS-ITERATION
          END-PERFORM.
          EXIT.
      *
       DFS-ITERATION SECTION.
          IF WS-I-M >= WS-PART-2-RESULT OR
             WS-I-M >= WS-E-M (WS-I-Y + 1, WS-I-X + 1, WS-I-E) THEN
                EXIT SECTION
          END-IF.
          MOVE WS-I-M TO WS-E-M (WS-I-Y + 1, WS-I-X + 1, WS-I-E)
          COMPUTE WS-T-1 = WS-I-X - WS-TARGET-X.
          COMPUTE WS-T-1 = FUNCTION ABS (WS-T-1).
          COMPUTE WS-T-2 = WS-I-Y - WS-TARGET-Y.
          COMPUTE WS-T-2 = FUNCTION ABS (WS-T-2).
          IF WS-I-M + WS-T-1 + WS-T-2 >= WS-PART-2-RESULT THEN
                EXIT SECTION
          END-IF.
          IF WS-I-X = WS-TARGET-X AND WS-I-Y = WS-TARGET-Y THEN
             IF NOT WS-TORCH THEN
                COMPUTE WS-I-M = WS-I-M + 7
             END-IF
             MOVE WS-E-TORCH TO WS-I-E
             IF WS-I-M < WS-PART-2-RESULT THEN
                MOVE WS-I-M TO WS-E-M (WS-I-Y + 1, WS-I-X + 1, WS-I-E)
                MOVE WS-I-M TO WS-PART-2-RESULT
             END-IF
             EXIT SECTION
          END-IF
      *
          MOVE WS-I-Y TO WS-J-Y.
          COMPUTE WS-J-X = WS-I-X - 1.
          PERFORM MOVE-REGION.
      *    
          COMPUTE WS-J-X = WS-I-X + 1.
          PERFORM MOVE-REGION.
      *
          COMPUTE WS-J-Y = WS-I-Y - 1.
          MOVE WS-I-X TO WS-J-X.
          PERFORM MOVE-REGION.
      *
          COMPUTE WS-J-Y = WS-I-Y + 1.
          PERFORM MOVE-REGION.
      *
          EXIT.
      *
       MOVE-REGION SECTION.
      *   BOUNDARIES AND WS-PART-2-RESULT SHOULD BE ADJUSTED
      *   WITH ROUGH ESTIMATES SO THAT THE ALGORITHM CAN
      *   RESPOND IN A REASONABLE TIME.
      *
      *   I WAS TOO LAZY FOR A* OR DJIKSTRA (PRIORITY QUEUE)
      *   I ALSO AVOID IMPLEMENTING NEW DATA STRUCTURES AS MUCH 
      *   AS I CAN FOR THIS AOC TO HAVE THE MOST ACCURATE
      *   COBOL DEVELOPER EXPERIENCE AND STAY AS BARE METAL
      *   AS POSSIBLE. :)
          IF WS-J-Y < 0 OR
             WS-J-Y >= 1000 OR
             WS-J-X < 0 OR
             WS-J-X >= 100 THEN
                EXIT SECTION
          END-IF.
          MOVE WS-I-E TO WS-J-E. 
          COMPUTE WS-J-M = WS-I-M + 1.
          IF WS-ROCKY (WS-J-Y + 1, WS-J-X + 1) AND
             WS-NEITHER THEN
                IF WS-WET (WS-I-Y + 1, WS-I-X + 1) THEN
                   MOVE WS-E-CLIMBING-GEAR TO WS-J-E
                END-IF
                IF WS-NARROW (WS-I-Y + 1, WS-I-X + 1) THEN
                   MOVE WS-E-TORCH TO WS-J-E
                END-IF
                COMPUTE WS-J-M = WS-I-M + 8
          END-IF.
          IF WS-WET (WS-J-Y + 1, WS-J-X + 1) AND
             WS-TORCH THEN
                IF WS-ROCKY (WS-I-Y + 1, WS-I-X + 1) THEN
                   MOVE WS-E-CLIMBING-GEAR TO WS-J-E
                END-IF
                IF WS-NARROW (WS-I-Y + 1, WS-I-X + 1) THEN
                   MOVE WS-E-NEITHER TO WS-J-E
                END-IF
                COMPUTE WS-J-M = WS-I-M + 8
          END-IF.
          IF WS-NARROW (WS-J-Y + 1, WS-J-X + 1) AND
             WS-CLIMBING-GEAR THEN
                IF WS-ROCKY (WS-I-Y + 1, WS-I-X + 1) THEN
                   MOVE WS-E-TORCH TO WS-J-E
                END-IF
                IF WS-WET (WS-I-Y + 1, WS-I-X + 1) THEN
                   MOVE WS-E-NEITHER TO WS-J-E
                END-IF
                COMPUTE WS-J-M = WS-I-M + 8
          END-IF.
          PERFORM PUSH-DFS-ELEMENT.
          EXIT.
      *
       CHANGE-EQUIPMENT SECTION.
          MOVE WS-I-X TO WS-J-X
          MOVE WS-I-Y TO WS-J-Y
          COMPUTE WS-J-M = WS-I-M + 7
          IF WS-ROCKY (WS-I-Y + 1, WS-I-X + 1) THEN
             IF WS-TORCH THEN
                MOVE WS-E-CLIMBING-GEAR TO WS-J-E
                PERFORM PUSH-DFS-ELEMENT
             END-IF
             IF WS-CLIMBING-GEAR THEN
                MOVE WS-E-TORCH TO WS-J-E
                PERFORM PUSH-DFS-ELEMENT
             END-IF
          END-IF.
          IF WS-WET (WS-I-Y + 1, WS-I-X + 1) THEN
             IF WS-CLIMBING-GEAR THEN
                MOVE WS-E-NEITHER TO WS-J-E
                PERFORM PUSH-DFS-ELEMENT
             END-IF
             IF WS-NEITHER THEN
                MOVE WS-E-CLIMBING-GEAR TO WS-J-E
                PERFORM PUSH-DFS-ELEMENT
             END-IF
          END-IF.
          IF WS-NARROW (WS-I-Y + 1, WS-I-X + 1) THEN
             IF WS-TORCH THEN
                MOVE WS-E-NEITHER TO WS-J-E
                PERFORM PUSH-DFS-ELEMENT
             END-IF
             IF WS-NEITHER THEN
                MOVE WS-E-TORCH TO WS-J-E
                PERFORM PUSH-DFS-ELEMENT
             END-IF
          END-IF.
          EXIT.
      *
       PUSH-DFS-ELEMENT SECTION.
          ADD 1 TO WS-S-L.
          MOVE WS-J-X TO WS-S-X (WS-S-L).
          MOVE WS-J-Y TO WS-S-Y (WS-S-L).
          MOVE WS-J-E TO WS-S-E (WS-S-L).
          MOVE WS-J-M TO WS-S-M (WS-S-L).
          EXIT.
      *
       COMPUTE-ALL SECTION.
          PERFORM VARYING WS-I-Y FROM 0 BY 1
          UNTIL WS-I-Y >= 1000
             PERFORM VARYING WS-I-X FROM 0 BY 1
             UNTIL WS-I-X >= 1000
                PERFORM COMPUTE-GEOLOGIC-INDEX
                PERFORM COMPUTE-EROSION-LEVEL
                PERFORM COMPUTE-TYPE
                PERFORM COMPUTE-RISK
             END-PERFORM
          END-PERFORM.
          EXIT.
      *
       COMPUTE-GEOLOGIC-INDEX SECTION.
          IF WS-I-X = 0 AND WS-I-Y = 0 THEN
             MOVE 0 TO WS-G (WS-I-Y + 1, WS-I-X + 1)
             EXIT SECTION
          END-IF.
          IF WS-I-X = WS-TARGET-X AND WS-I-Y = WS-TARGET-Y THEN
             MOVE 0 TO WS-G (WS-I-Y + 1, WS-I-X + 1)
             EXIT SECTION
          END-IF.
          IF WS-I-Y = 0 THEN
             COMPUTE WS-G (WS-I-Y + 1, WS-I-X + 1) = 16807 * WS-I-X
             EXIT SECTION
          END-IF.
          IF WS-I-X = 0 THEN
             COMPUTE WS-G (WS-I-Y + 1, WS-I-X + 1) = 48271 * WS-I-Y
             EXIT SECTION
          END-IF.
          COMPUTE WS-G (WS-I-Y + 1, WS-I-X + 1) =
             WS-E (WS-I-Y, WS-I-X + 1) * WS-E (WS-I-Y + 1, WS-I-X).
          EXIT.
      *
       COMPUTE-EROSION-LEVEL SECTION.
          COMPUTE WS-E (WS-I-Y + 1, WS-I-X + 1) = FUNCTION MOD
             (WS-G (WS-I-Y + 1, WS-I-X + 1) + WS-DEPTH, 20183).
          EXIT.
      *
       COMPUTE-TYPE SECTION.
          EVALUATE FUNCTION MOD (WS-E (WS-I-Y + 1, WS-I-X + 1), 3)
             WHEN 0
                MOVE "." TO WS-M (WS-I-Y + 1, WS-I-X + 1)
             WHEN 1
                MOVE "=" TO WS-M (WS-I-Y + 1, WS-I-X + 1)
             WHEN 2
                MOVE "|" TO WS-M (WS-I-Y + 1, WS-I-X + 1)
          END-EVALUATE.
          EXIT.
      *
       COMPUTE-RISK SECTION.
          IF WS-I-X > WS-TARGET-X OR WS-I-Y > WS-TARGET-Y THEN
             EXIT SECTION
          END-IF.
          IF WS-WET (WS-I-Y + 1, WS-I-X + 1) THEN
             ADD 1 TO WS-PART-1-RESULT  
          END-IF.
          IF WS-NARROW (WS-I-Y + 1, WS-I-X + 1) THEN
             ADD 2 TO WS-PART-1-RESULT
          END-IF.
          EXIT.
