       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY17.
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
             03 INPUT-STR PIC X(40).
       WORKING-STORAGE SECTION.
          77 WS-WATER-Y PIC 9(18) USAGE COMP-5 VALUE 1.
          77 WS-WATER-X PIC 9(18) USAGE COMP-5 VALUE 500.
          77 WS-GO-DOWN PIC 9(18) USAGE COMP-5 VALUE 0.
          77 WS-GO-LEFT PIC 9(18) USAGE COMP-5 VALUE 1.
          77 WS-GO-RIGHT PIC 9(18) USAGE COMP-5 VALUE 2.
          01 WS-INPUT-FILE-EOF PIC X(1) VALUE "N".
          01 WS-BOUNDARIES.
             03 WS-MIN-Y PIC 9(18) USAGE COMP-5
                VALUE 999999999999999999.
             03 WS-MAX-Y PIC 9(18) USAGE COMP-5 VALUE 0.
          01 WS-MAP.
             03 WS-Y OCCURS 4000.
                05 WS-X.
                   07 WS-C PIC X(1) VALUE "." OCCURS 4000.
                      88 WS-SAND VALUE ".".
                      88 WS-CLAY VALUE "#".
                      88 WS-SOURCE VALUE "+".
                      88 WS-FALL VALUE "|".
                      88 WS-STABLE VALUE "~".
                05 WS-X-STR REDEFINES WS-X PIC X(4000).
          01 WS-PARSE-1.
             03 WS-AXIS-1 PIC X(1).
             03 FILLER PIC X(1).
             03 WS-REST-1 PIC X(38).
          01 WS-T-S-1 REDEFINES WS-PARSE-1 PIC X(40).
          01 WS-PARSE-2.
             03 WS-AXIS-2 PIC X(1).
             03 FILLER PIC X(1).
             03 WS-REST-2 PIC X(38).
          01 WS-T-S-2 REDEFINES WS-PARSE-2 PIC X(40).
          01 WS-T-1 PIC 9(18).
          01 WS-Y-RANGE PIC X(1).
          01 WS-I-1 PIC 9(18) USAGE COMP-5.
          01 WS-I-2 PIC 9(18) USAGE COMP-5.
          01 WS-I-3 PIC 9(18) USAGE COMP-5.
          01 WS-STACK.
             03 WS-S-L PIC 9(18) USAGE COMP-5 VALUE 0.
             03 WS-E OCCURS 1000000.
                05 WS-S-Y PIC 9(18) USAGE COMP-5.
                05 WS-S-X PIC 9(18) USAGE COMP-5.
          01 WS-LEFT-STABLE PIC X(1).
          01 WS-RIGHT-STABLE PIC X(1).
          01 WS-PART-1-RESULT PIC 9(18) USAGE COMP-5 VALUE 0.
          01 WS-PART-2-RESULT PIC 9(18) USAGE COMP-5 VALUE 0.
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
                   UNSTRING INPUT-STR DELIMITED BY ", "
                      INTO WS-T-S-1, WS-T-S-2
                   PERFORM PARSE-FIRST-PART
                   PERFORM PARSE-SECOND-PART
                   PERFORM FILL-MAP
          END-PERFORM.
          CLOSE INPUT-FILE.
          EXIT.
      *
       PARSE-FIRST-PART SECTION.
          IF WS-AXIS-1 = "x" THEN
             MOVE "N" TO WS-Y-RANGE
          ELSE
             MOVE "Y" TO WS-Y-RANGE
          END-IF.
          MOVE WS-REST-1 TO WS-T-1.
          MOVE WS-T-1 TO WS-I-1.
          EXIT.
      *
       PARSE-SECOND-PART SECTION.
          UNSTRING WS-REST-2 DELIMITED BY ".."
             INTO WS-T-S-1, WS-T-S-2.
          MOVE WS-T-S-1 TO WS-T-1.
          MOVE WS-T-1 TO WS-I-2.
          MOVE WS-T-S-2 TO WS-T-1.
          MOVE WS-T-1 TO WS-I-3.
          EXIT.
      *
       FILL-MAP SECTION.
          PERFORM VARYING WS-I-2 FROM WS-I-2 BY 1
          UNTIL WS-I-2 > WS-I-3
             IF WS-Y-RANGE = "Y" THEN
                IF WS-I-1 > WS-MAX-Y THEN
                   MOVE WS-I-1 TO WS-MAX-Y
                END-IF
                IF WS-I-1 < WS-MIN-Y THEN
                   MOVE WS-I-1 TO WS-MIN-Y
                END-IF
                MOVE "#" TO WS-C (WS-I-1, WS-I-2)
             ELSE
                IF WS-I-3 > WS-MAX-Y THEN
                   MOVE WS-I-3 TO WS-MAX-Y
                END-IF
                IF WS-I-2 < WS-MIN-Y THEN
                   MOVE WS-I-2 TO WS-MIN-Y
                END-IF
                MOVE "#" TO WS-C (WS-I-2, WS-I-1)
             END-IF
          END-PERFORM.
          EXIT.
      *
       PART-1 SECTION.
          PERFORM DFS.
          DISPLAY "PART 1: " WS-PART-1-RESULT.
          EXIT.
      *
       PART-2 SECTION.
          DISPLAY "PART 2: " WS-PART-2-RESULT.
          EXIT.
      *
       DFS SECTION.
          ADD 1 TO WS-S-L.
          MOVE WS-WATER-Y TO WS-S-Y (WS-S-L).
          MOVE WS-WATER-X TO WS-S-X (WS-S-L).
          PERFORM UNTIL WS-S-L = 0
             MOVE WS-S-Y (WS-S-L) TO WS-I-1
             MOVE WS-S-X (WS-S-L) TO WS-I-2
             SUBTRACT 1 FROM WS-S-L
             IF WS-I-1 > WS-MAX-Y THEN
                CONTINUE
             ELSE IF WS-SAND (WS-I-1, WS-I-2) THEN
                PERFORM SAND-VISIT
             ELSE IF WS-FALL (WS-I-1, WS-I-2) THEN
                PERFORM FALL-VISIT
             END-IF
          END-PERFORM.
          EXIT.
      *
       SAND-VISIT SECTION.
          IF NOT WS-I-1 < WS-MIN-Y THEN
             ADD 1 TO WS-PART-1-RESULT
          END-IF.
          MOVE "|" TO WS-C (WS-I-1, WS-I-2).
          IF WS-SAND (WS-I-1 + 1, WS-I-2) THEN
             ADD 1 TO WS-S-L
             COMPUTE WS-S-Y (WS-S-L) = WS-I-1 + 1
             MOVE WS-I-2 TO WS-S-X (WS-S-L)
             EXIT SECTION
          END-IF.
          IF WS-FALL (WS-I-1 + 1, WS-I-2) THEN
             EXIT SECTION
          END-IF.
          PERFORM CHECK-LEFT.
          PERFORM CHECK-RIGHT.
          IF WS-LEFT-STABLE = "Y" AND WS-RIGHT-STABLE = "Y" THEN
             PERFORM FILL-ROW
          END-IF.
          EXIT.
      *
       FALL-VISIT SECTION.
          IF NOT WS-CLAY (WS-I-1 + 1, WS-I-2) AND
             NOT WS-STABLE (WS-I-1 + 1, WS-I-2) THEN
                EXIT SECTION
          END-IF.
          PERFORM CHECK-LEFT.
          PERFORM CHECK-RIGHT.
          IF WS-LEFT-STABLE = "Y" AND WS-RIGHT-STABLE = "Y" THEN
             PERFORM FILL-ROW
          END-IF.
          EXIT.
      *
       CHECK-LEFT SECTION.
          MOVE "Y" TO WS-LEFT-STABLE.
          COMPUTE WS-I-3 = WS-I-2 - 1.
          PERFORM VARYING WS-I-3 FROM WS-I-3 BY -1
          UNTIL WS-CLAY (WS-I-1, WS-I-3)
             IF WS-SAND (WS-I-1, WS-I-3) THEN
                ADD 1 TO WS-PART-1-RESULT
                MOVE "|" TO WS-C (WS-I-1, WS-I-3)
             END-IF
             IF NOT WS-CLAY (WS-I-1 + 1, WS-I-3) AND
                NOT WS-STABLE (WS-I-1 + 1, WS-I-3) THEN
                   MOVE "N" TO WS-LEFT-STABLE
                   IF WS-SAND (WS-I-1 + 1, WS-I-3) THEN
                      ADD 1 TO WS-S-L
                      COMPUTE WS-S-Y (WS-S-L) = WS-I-1 + 1
                      MOVE WS-I-3 TO WS-S-X (WS-S-L)
                   END-IF
                   EXIT SECTION
             END-IF
          END-PERFORM.
          EXIT.
      *
       CHECK-RIGHT SECTION.
          MOVE "Y" TO WS-RIGHT-STABLE.
          COMPUTE WS-I-3 = WS-I-2 + 1.
          PERFORM VARYING WS-I-3 FROM WS-I-3 BY 1
          UNTIL WS-CLAY (WS-I-1, WS-I-3)
             IF WS-SAND (WS-I-1, WS-I-3) THEN
                ADD 1 TO WS-PART-1-RESULT
                MOVE "|" TO WS-C (WS-I-1, WS-I-3)
             END-IF
             IF NOT WS-CLAY (WS-I-1 + 1, WS-I-3) AND
                NOT WS-STABLE (WS-I-1 + 1, WS-I-3) THEN
                   MOVE "N" TO WS-RIGHT-STABLE
                   IF WS-SAND (WS-I-1 + 1, WS-I-3) THEN
                      ADD 1 TO WS-S-L
                      COMPUTE WS-S-Y (WS-S-L) = WS-I-1 + 1
                      MOVE WS-I-3 TO WS-S-X (WS-S-L)
                   END-IF
                   EXIT SECTION
             END-IF
          END-PERFORM.
          EXIT.
      *
       FILL-ROW SECTION.
          COMPUTE WS-I-3 = WS-I-2 - 1.
          PERFORM VARYING WS-I-3 FROM WS-I-3 BY -1
          UNTIL NOT WS-FALL (WS-I-1, WS-I-3)
             ADD 1 TO WS-PART-2-RESULT
             MOVE "~" TO WS-C (WS-I-1, WS-I-3)
             IF WS-FALL (WS-I-1 - 1, WS-I-3) THEN
                ADD 1 TO WS-S-L
                COMPUTE WS-S-Y (WS-S-L) = WS-I-1 - 1
                MOVE WS-I-2 TO WS-S-X (WS-S-L)
             END-IF
          END-PERFORM.
          MOVE WS-I-2 TO WS-I-3.
          PERFORM VARYING WS-I-3 FROM WS-I-3 BY 1
          UNTIL NOT WS-FALL (WS-I-1, WS-I-3)
             ADD 1 TO WS-PART-2-RESULT
             MOVE "~" TO WS-C (WS-I-1, WS-I-3)
             IF WS-FALL (WS-I-1 - 1, WS-I-3) THEN
                ADD 1 TO WS-S-L
                COMPUTE WS-S-Y (WS-S-L) = WS-I-1 - 1
                MOVE WS-I-2 TO WS-S-X (WS-S-L)
             END-IF
          END-PERFORM.
          EXIT.

