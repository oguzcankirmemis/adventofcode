       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY15.
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
             03 INPUT-STR PIC X(200).
       WORKING-STORAGE SECTION.
          01 WS-INPUT-FILE-EOF PIC X(1) VALUE "N".
          01 WS-ROUNDS PIC 9(18) USAGE BINARY VALUE 0.
          01 WS-C-PLAYER-COUNT PIC 9(4) USAGE BINARY.
          01 WS-PLAYER-COUNT PIC 9(4) USAGE BINARY VALUE 0.
          01 WS-C-ELF-COUNT PIC 9(4) USAGE BINARY.
          01 WS-ELF-COUNT PIC 9(4) USAGE BINARY VALUE 0.
          01 WS-C-GOBLIN-COUNT PIC 9(4) USAGE BINARY.
          01 WS-GOBLIN-COUNT PIC 9(4) USAGE BINARY VALUE 0.
          01 WS-C-PLAYER-LIST.
             03 WS-C-PLAYERS OCCURS 30.
                05 WS-C-COORD.
                   07 WS-C-P-R PIC 9(4).
                   07 WS-C-P-C PIC 9(4).
                05 WS-C-TYPE PIC X(1).
                05 WS-C-AP PIC S9(4) USAGE BINARY.
                05 WS-C-HP PIC S9(4) USAGE BINARY.
          01 WS-PLAYER-LIST.
             03 WS-PLAYERS OCCURS 30.
                05 WS-COORD.
                   07 WS-P-R PIC 9(4) VALUE 9999.
                   07 WS-P-C PIC 9(4) VALUE 9999.
                05 WS-SORT-KEY REDEFINES WS-COORD PIC 9(8).
                05 WS-TYPE PIC X(1).
                   88 WS-ELF VALUE "E".
                   88 WS-GOBLIN VALUE "G".
                05 WS-AP PIC S9(4) USAGE BINARY VALUE 3.
                05 WS-HP PIC S9(4) USAGE BINARY VALUE 200.
          01 WS-C-MAP.
             03 WS-C-ROW-L PIC S9(4) USAGE BINARY.
             03 WS-C-ROW OCCURS 200.
                05 WS-C-COLUMN.
                   07 WS-C-COL OCCURS 200.
                      10 WS-C-UNIT PIC X(1).
                05 WS-C-COL-STR REDEFINES WS-C-COLUMN PIC X(200).
          01 WS-MAP.
             03 WS-ROW-L PIC S9(4) USAGE BINARY VALUE 0.
             03 WS-ROW OCCURS 200.
                05 WS-COLUMN.
                   07 WS-COL OCCURS 200.
                      10 WS-UNIT PIC X(1).
                         88 WS-WALL VALUE "#".
                         88 WS-EMPTY VALUE ".".
                         88 WS-SPACE VALUE " ".
                05 WS-COL-STR REDEFINES WS-COLUMN PIC X(200).
          01 WS-BFS.
             03 WS-Q-H PIC 9(4) USAGE BINARY VALUE 0.
             03 WS-Q-T PIC 9(4) USAGE BINARY VALUE 0.
             03 WS-Q OCCURS 100000.
                05 WS-Q-R PIC 9(4).
                05 WS-Q-C PIC 9(4).
                05 WS-Q-D PIC 9(4) USAGE BINARY.
                05 WS-Q-M PIC 9(1) USAGE BINARY.
                   88 WS-UNDETERMINED VALUE 0.
                   88 WS-UP VALUE 1.
                   88 WS-LEFT VALUE 2.
                   88 WS-RIGHT VALUE 3.
                   88 WS-DOWN VALUE 4.
             03 WS-E-ROW OCCURS 200.
                05 WS-E-COL OCCURS 200.
                   07 WS-FOR-WHOM PIC 9(4) USAGE BINARY VALUE 0.
                   07 WS-ROUND PIC 9(18) USAGE BINARY
                      VALUE 999999999999999999.
                   07 WS-STOP PIC X(1) VALUE " ".
                      88 WS-DEST VALUE "X".
          01 WS-I-1 PIC 9(4) USAGE BINARY VALUE 0.
          01 WS-I-2 PIC 9(4) USAGE BINARY VALUE 0.
          01 WS-T-1 PIC S9(4) USAGE BINARY VALUE 0.
          01 WS-T-2 PIC S9(4) USAGE BINARY VALUE 0.
          01 WS-T-3 PIC S9(4) USAGE BINARY VALUE 0.
          01 WS-T-4 PIC S9(4) USAGE BINARY VALUE 0.
          01 WS-T-5 PIC S9(4) USAGE BINARY VALUE 0.
          01 WS-T-6 PIC S9(4) USAGE BINARY VALUE 0.
          01 WS-ATTACK.
             03 WS-A-D PIC 9(4) USAGE BINARY VALUE 0.
             03 WS-A-HP PIC 9(4) USAGE BINARY VALUE 0.
             03 WS-A-I PIC 9(4) USAGE BINARY VALUE 0.
          01 WS-TARGETS.
             03 WS-T-L PIC 9(4) USAGE BINARY VALUE 0.
             03 WS-T-COORD OCCURS 200.
                05 WS-T-R PIC 9(4).
                05 WS-T-C PIC 9(4).
          01 WS-MIN-ELF-AP PIC S9(4) USAGE BINARY VALUE 3.
          01 WS-PART-1-RESULT PIC 9(18) USAGE BINARY VALUE 0.
          01 WS-PART-2-RESULT PIC 9(18) USAGE BINARY VALUE 0.
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
                   ADD 1 TO WS-ROW-L
                   MOVE INPUT-STR TO WS-ROW (WS-ROW-L)
                   PERFORM VARYING WS-I-1 FROM 1 BY 1
                   UNTIL WS-SPACE (WS-ROW-L, WS-I-1)
                      PERFORM PARSE-UNIT
                   END-PERFORM
             END-READ
          END-PERFORM.
          MOVE WS-MAP TO WS-C-MAP.
          MOVE WS-PLAYER-LIST TO WS-C-PLAYER-LIST.
          MOVE WS-PLAYER-COUNT TO WS-C-PLAYER-COUNT.
          MOVE WS-ELF-COUNT TO WS-C-ELF-COUNT.
          MOVE WS-GOBLIN-COUNT TO WS-C-GOBLIN-COUNT.
          CLOSE INPUT-FILE.
          EXIT.
      *
       PARSE-UNIT SECTION.
          IF WS-WALL (WS-ROW-L, WS-I-1) OR
             WS-EMPTY (WS-ROW-L, WS-I-1) THEN
             EXIT SECTION
          END-IF.
          ADD 1 TO WS-PLAYER-COUNT.
          MOVE WS-ROW-L TO WS-P-R (WS-PLAYER-COUNT).
          MOVE WS-I-1 TO WS-P-C (WS-PLAYER-COUNT).
          MOVE WS-UNIT (WS-ROW-L, WS-I-1) TO WS-TYPE (WS-PLAYER-COUNT).
          IF WS-ELF (WS-PLAYER-COUNT) THEN
             ADD 1 TO WS-ELF-COUNT
          ELSE
             ADD 1 TO WS-GOBLIN-COUNT
          END-IF.
          EXIT.
      *
       RESET-MAP SECTION.
          MOVE WS-C-MAP TO WS-MAP.
          MOVE WS-C-PLAYER-LIST TO WS-PLAYER-LIST.
          MOVE WS-C-PLAYER-COUNT TO WS-PLAYER-COUNT.
          MOVE WS-C-GOBLIN-COUNT TO WS-GOBLIN-COUNT.
          MOVE WS-C-ELF-COUNT TO WS-ELF-COUNT.
          MOVE 0 TO WS-ROUNDS.
          EXIT.
      *
       PART-1 SECTION.
          PERFORM UNTIL WS-ELF-COUNT = 0 OR WS-GOBLIN-COUNT = 0
             PERFORM ROUND
          END-PERFORM.
          SORT WS-PLAYERS ON ASCENDING WS-SORT-KEY.
          PERFORM VARYING WS-I-1 FROM 1 BY 1
          UNTIL WS-I-1 > WS-PLAYER-COUNT
             ADD WS-HP (WS-I-1) TO WS-PART-1-RESULT
          END-PERFORM.
          MULTIPLY WS-ROUNDS BY WS-PART-1-RESULT.
          DISPLAY "PART 1: " WS-PART-1-RESULT.
          EXIT.
      *
       PART-2 SECTION.
          PERFORM UNTIL WS-ELF-COUNT = WS-C-ELF-COUNT
             PERFORM RESET-MAP
             ADD 1 TO WS-MIN-ELF-AP
             PERFORM VARYING WS-I-1 FROM 1 BY 1
             UNTIL WS-I-1 > WS-PLAYER-COUNT
                IF WS-ELF (WS-I-1) THEN
                   MOVE WS-MIN-ELF-AP TO WS-AP (WS-I-1)
                END-IF
             END-PERFORM
             PERFORM UNTIL WS-GOBLIN-COUNT = 0
                PERFORM ROUND
                IF NOT WS-ELF-COUNT = WS-C-ELF-COUNT THEN
                   EXIT PERFORM
                END-IF
             END-PERFORM
          END-PERFORM.
          SORT WS-PLAYERS ON ASCENDING WS-SORT-KEY.
          PERFORM VARYING WS-I-1 FROM 1 BY 1
          UNTIL WS-I-1 > WS-PLAYER-COUNT
             ADD WS-HP (WS-I-1) TO WS-PART-2-RESULT
          END-PERFORM.
          MULTIPLY WS-ROUNDS BY WS-PART-2-RESULT.
          DISPLAY "PART 2: " WS-PART-2-RESULT.
          EXIT.
      *
       ROUND SECTION.
          SORT WS-PLAYERS ON ASCENDING WS-SORT-KEY.
          PERFORM VARYING WS-I-1 FROM 1 BY 1
          UNTIL WS-I-1 > WS-PLAYER-COUNT
             IF WS-ELF-COUNT = 0 OR WS-GOBLIN-COUNT = 0 THEN
                COMPUTE WS-PLAYER-COUNT = WS-ELF-COUNT + WS-GOBLIN-COUNT
                EXIT SECTION
             END-IF
             PERFORM PLAYER-TURN
          END-PERFORM.
          COMPUTE WS-PLAYER-COUNT = WS-ELF-COUNT + WS-GOBLIN-COUNT.
          ADD 1 TO WS-ROUNDS.
          EXIT.
       PLAYER-TURN SECTION.
          IF WS-HP (WS-I-1) LESS THAN OR EQUAL TO 0 THEN
             EXIT SECTION
          END-IF.
          PERFORM ATTACK.
          IF WS-A-I NOT EQUAL TO 0 THEN
             EXIT SECTION
          END-IF.
          PERFORM MOVEMENT.
          PERFORM ATTACK.
          EXIT.
      *
       ATTACK SECTION.
          MOVE 0 TO WS-A-I.
          PERFORM VARYING WS-I-2 FROM 1 BY 1
          UNTIL WS-I-2 > WS-PLAYER-COUNT
             IF WS-HP (WS-I-1) > 0 AND
                WS-TYPE (WS-I-1) NOT EQUAL TO WS-TYPE (WS-I-2) THEN
                   COMPUTE WS-T-1 = WS-P-R (WS-I-1) - WS-P-R (WS-I-2)
                   COMPUTE WS-T-2 = WS-P-C (WS-I-1) - WS-P-C (WS-I-2)
                   COMPUTE WS-T-1 = FUNCTION ABS (WS-T-1)
                   COMPUTE WS-T-2 = FUNCTION ABS (WS-T-2)
                   COMPUTE WS-A-D = WS-T-1 + WS-T-2
                   IF WS-A-D = 1 THEN
                      IF WS-A-I = 0 THEN
                         MOVE WS-I-2 TO WS-A-I
                         MOVE WS-HP (WS-I-2) TO WS-A-HP
                      END-IF
                      IF WS-HP (WS-I-2) < WS-A-HP THEN
                         MOVE WS-I-2 TO WS-A-I
                         MOVE WS-HP (WS-I-2) TO WS-A-HP
                      END-IF
                   END-IF
             END-IF
          END-PERFORM.
          IF WS-A-I = 0 THEN
             EXIT SECTION
          END-IF.
          SUBTRACT WS-AP (WS-I-1) FROM WS-HP (WS-A-I).
          IF WS-HP (WS-A-I) LESS THAN OR EQUAL TO 0 THEN
             IF WS-ELF (WS-A-I) THEN
                SUBTRACT 1 FROM WS-ELF-COUNT
             ELSE
                SUBTRACT 1 FROM WS-GOBLIN-COUNT
             END-IF
             MOVE WS-P-R (WS-A-I) TO WS-T-1
             MOVE WS-P-C (WS-A-I) TO WS-T-2
             MOVE "." TO WS-UNIT (WS-T-1, WS-T-2)
             MOVE 9999 TO WS-P-R (WS-A-I)
             MOVE 9999 TO WS-P-C (WS-A-I)
          END-IF.
          EXIT.
      *
       MOVEMENT SECTION.
          PERFORM SEARCH-TARGETS.
      *
          MOVE WS-P-R (WS-I-1) TO WS-T-1.
          MOVE WS-P-C (WS-I-1) TO WS-T-2.
          MOVE WS-I-1 TO WS-FOR-WHOM (WS-T-1, WS-T-2).
          MOVE 1 TO WS-Q-T.
          MOVE 1 TO WS-Q-H.
          MOVE WS-T-1 TO WS-Q-R (WS-Q-T).
          MOVE WS-T-2 TO WS-Q-C (WS-Q-T).
          MOVE 0 TO WS-Q-D (WS-Q-T).
          MOVE 0 TO WS-Q-M (WS-Q-T).
          MOVE WS-I-1 TO WS-FOR-WHOM (WS-T-1, WS-T-2).
          MOVE WS-ROUNDS TO WS-ROUND (WS-T-1, WS-T-2).
      *
          PERFORM UNTIL WS-Q-H > WS-Q-T
             MOVE WS-Q-R (WS-Q-H) TO WS-T-1
             MOVE WS-Q-C (WS-Q-H) TO WS-T-2
             MOVE WS-Q-D (WS-Q-H) TO WS-T-3
             MOVE WS-Q-M (WS-Q-H) TO WS-T-4
      *
             IF WS-DEST (WS-T-1, WS-T-2) THEN
      *
                COMPUTE WS-I-2 = WS-Q-H + 1
                PERFORM VARYING WS-I-2 FROM WS-I-2 BY 1 
                UNTIL WS-I-2 > WS-Q-T OR WS-Q-D (WS-I-2) > WS-T-3
                   MOVE WS-Q-R (WS-I-2) TO WS-T-5
                   MOVE WS-Q-C (WS-I-2) TO WS-T-6
                   IF WS-DEST (WS-T-5, WS-T-6) THEN
                      IF WS-T-5 < WS-T-1 THEN
                         MOVE WS-Q-R (WS-I-2) TO WS-T-1
                         MOVE WS-Q-C (WS-I-2) TO WS-T-2
                         MOVE WS-Q-D (WS-I-2) TO WS-T-3
                         MOVE WS-Q-M (WS-I-2) TO WS-T-4 
                      END-IF
                      IF WS-T-5 = WS-T-1 AND WS-T-6 < WS-T-2 THEN
                         MOVE WS-Q-R (WS-I-2) TO WS-T-1
                         MOVE WS-Q-C (WS-I-2) TO WS-T-2
                         MOVE WS-Q-D (WS-I-2) TO WS-T-3
                         MOVE WS-Q-M (WS-I-2) TO WS-T-4 
                      END-IF
                   END-IF
                END-PERFORM
      *
                MOVE WS-P-R (WS-I-1) TO WS-T-1
                MOVE WS-P-C (WS-I-1) TO WS-T-2
                MOVE "." TO WS-UNIT (WS-T-1, WS-T-2)
                IF WS-UP (WS-Q-H) THEN
                   SUBTRACT 1 FROM WS-T-1
                END-IF
                IF WS-LEFT (WS-Q-H) THEN
                   SUBTRACT 1 FROM WS-T-2
                END-IF
                IF WS-RIGHT (WS-Q-H) THEN
                   ADD 1 TO WS-T-2
                END-IF
                IF WS-DOWN (WS-Q-H) THEN
                   ADD 1 TO WS-T-1
                END-IF
                IF WS-ELF (WS-I-1) THEN
                   MOVE "E" TO WS-UNIT (WS-T-1, WS-T-2)
                ELSE
                   MOVE "G" TO WS-UNIT (WS-T-1, WS-T-2)
                END-IF
                MOVE WS-T-1 TO WS-P-R (WS-I-1)
                MOVE WS-T-2 TO WS-P-C (WS-I-1)
                GO TO MOVEMENT-90
             END-IF
      *
             ADD 1 TO WS-T-3
      *
             IF WS-UNDETERMINED (WS-Q-H) THEN
                MOVE 1 TO WS-T-4
             END-IF
             SUBTRACT 1 FROM WS-T-1
             PERFORM QUEUE-NEIGHBOUR
      *
             IF WS-UNDETERMINED (WS-Q-H) THEN
                MOVE 2 TO WS-T-4
             END-IF
             ADD 1 TO WS-T-1
             SUBTRACT 1 FROM WS-T-2
             PERFORM QUEUE-NEIGHBOUR
      *
             IF WS-UNDETERMINED (WS-Q-H) THEN
                MOVE 3 TO WS-T-4
             END-IF
             ADD 2 TO WS-T-2
             PERFORM QUEUE-NEIGHBOUR
      *
             IF WS-UNDETERMINED (WS-Q-H) THEN
                MOVE 4 TO WS-T-4
             END-IF
             SUBTRACT 1 FROM WS-T-2
             ADD 1 TO WS-T-1
             PERFORM QUEUE-NEIGHBOUR
      *
             ADD 1 TO WS-Q-H
          END-PERFORM.
      *
       MOVEMENT-90.
          PERFORM VARYING WS-I-2 FROM 1 BY 1
          UNTIL WS-I-2 > WS-T-L
             MOVE WS-T-R (WS-I-2) TO WS-T-1
             MOVE WS-T-C (WS-I-2) TO WS-T-2
             MOVE " " TO WS-STOP (WS-T-1, WS-T-2)
          END-PERFORM.
          EXIT.
      *
       QUEUE-NEIGHBOUR SECTION.
          IF NOT WS-EMPTY (WS-T-1, WS-T-2) THEN
             EXIT SECTION
          END-IF.
          IF WS-ROUNDS = WS-ROUND (WS-T-1, WS-T-2) AND
             WS-I-1 = WS-FOR-WHOM (WS-T-1, WS-T-2) THEN
                EXIT SECTION
          END-IF.
          MOVE WS-ROUNDS TO WS-ROUND (WS-T-1, WS-T-2).
          MOVE WS-I-1 TO WS-FOR-WHOM (WS-T-1, WS-T-2).
          ADD 1 TO WS-Q-T.
          MOVE WS-T-1 TO WS-Q-R (WS-Q-T).
          MOVE WS-T-2 TO WS-Q-C (WS-Q-T).
          MOVE WS-T-3 TO WS-Q-D (WS-Q-T).
          MOVE WS-T-4 TO WS-Q-M (WS-Q-T).
          EXIT.
      *
       SEARCH-TARGETS SECTION.
          MOVE 0 TO WS-T-L.
          PERFORM VARYING WS-I-2 FROM 1 BY 1
          UNTIL WS-I-2 > WS-PLAYER-COUNT
             IF WS-HP (WS-I-2) > 0 AND
                WS-TYPE (WS-I-1) NOT EQUAL TO WS-TYPE (WS-I-2) THEN
                   PERFORM CHECK-NEIGHBOURS
              END-IF
          END-PERFORM.
          EXIT.
      *
       CHECK-NEIGHBOURS SECTION.
          COMPUTE WS-T-1 = WS-P-R (WS-I-2) - 1.
          MOVE WS-P-C (WS-I-2) TO WS-T-2.
          PERFORM CHECK-NEIGHBOUR.
          MOVE WS-P-R (WS-I-2) TO WS-T-1.
          COMPUTE WS-T-2 = WS-P-C (WS-I-2) - 1.
          PERFORM CHECK-NEIGHBOUR.
          MOVE WS-P-R (WS-I-2) TO WS-T-1.
          COMPUTE WS-T-2 = WS-P-C (WS-I-2) + 1.
          PERFORM CHECK-NEIGHBOUR.
          COMPUTE WS-T-1 = WS-P-R (WS-I-2) + 1.
          MOVE WS-P-C (WS-I-2) TO WS-T-2.
          PERFORM CHECK-NEIGHBOUR.
          EXIT.
      *
       CHECK-NEIGHBOUR SECTION.
          IF WS-EMPTY (WS-T-1, WS-T-2) THEN
             ADD 1 TO WS-T-L
             MOVE WS-T-1 TO WS-T-R (WS-T-L)
             MOVE WS-T-2 TO WS-T-C (WS-T-L)
             MOVE "X" TO WS-STOP (WS-T-1, WS-T-2)
          END-IF.
          EXIT.


       