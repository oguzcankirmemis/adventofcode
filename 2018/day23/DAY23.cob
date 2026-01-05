       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY23.
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
             03 FILLER PIC X(5).
             03 INPUT-STR PIC X(55).
       WORKING-STORAGE SECTION.
          01 WS-INPUT-FILE-EOF PIC X(1) VALUE "N".
          01 WS-NANOBOTS.
             03 WS-NANOBOTS-LEN PIC 9(18) USAGE COMP-5.
             03 WS-NANOBOT OCCURS 1000.
                05 WS-POSITION.
                   07 WS-X PIC S9(18) USAGE COMP-5.
                   07 WS-Y PIC S9(18) USAGE COMP-5.
                   07 WS-Z PIC S9(18) USAGE COMP-5.
                05 WS-RADIUS PIC 9(18) USAGE COMP-5.
          01 WS-TMP.
             03 WS-T-STR-1 PIC X(60).
             03 WS-T-D-1 PIC S9(18).
             03 WS-T-D-2 PIC S9(18).
             03 WS-T-D-3 PIC S9(18).
             03 WS-T-1 PIC S9(18) USAGE COMP-5.
             03 WS-T-2 PIC S9(18) USAGE COMP-5.
             03 WS-T-3 PIC S9(18) USAGE COMP-5.
             03 WS-T-4 PIC S9(18) USAGE COMP-5.
             03 WS-T-5 PIC S9(18) USAGE COMP-5.
             03 WS-T-6 PIC S9(18) USAGE COMP-5.
             03 WS-T-7 PIC S9(18) USAGE COMP-5.
             03 WS-T-8 PIC S9(18) USAGE COMP-5.
          01 WS-IDX.
             03 WS-I-1 PIC 9(18) USAGE COMP-5.
             03 WS-I-2 PIC 9(18) USAGE COMP-5.
          01 WS-STRONGEST-I PIC 9(18) USAGE COMP-5 VALUE 1.
          01 WS-DISTANCE PIC 9(18) USAGE COMP-5.
          01 WS-PART-1-RESULT PIC 9(18) USAGE COMP-5 VALUE 0.
          01 WS-P.
             03 WS-P-X PIC S9(18) USAGE COMP-5.
             03 WS-P-Y PIC S9(18) USAGE COMP-5.
             03 WS-P-Z PIC S9(18) USAGE COMP-5.
             03 WS-P-O-D PIC 9(18) USAGE COMP-5.
             03 WS-P-I-R PIC 9(18) USAGE COMP-5.
          01 WS-PART-2.
             03 WS-BEST-POSITION.
                05 WS-BEST-X PIC S9(18) USAGE COMP-5 VALUE 0.
                05 WS-BEST-Y PIC S9(18) USAGE COMP-5 VALUE 0.
                05 WS-BEST-Z PIC S9(18) USAGE COMP-5 VALUE 0.
             03 WS-BEST-IN-RANGE PIC 9(18) USAGE COMP-5 VALUE 0.
             03 WS-BEST-ORIGIN-DISTANCE PIC 9(18) USAGE COMP-5 VALUE 0.
             03 WS-PART-2-RESULT PIC 9(18) USAGE COMP-5 VALUE 0.
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
                   ADD 1 TO WS-NANOBOTS-LEN
                   UNSTRING INPUT-STR DELIMITED BY ">, r="
                      INTO WS-T-STR-1, WS-T-D-1
                   MOVE WS-T-D-1 TO WS-RADIUS (WS-NANOBOTS-LEN)
                   UNSTRING WS-T-STR-1 DELIMITED BY ","
                      INTO WS-T-D-1, WS-T-D-2, WS-T-D-3
                   MOVE WS-T-D-1 TO WS-X (WS-NANOBOTS-LEN)
                   MOVE WS-T-D-2 TO WS-Y (WS-NANOBOTS-LEN)
                   MOVE WS-T-D-3 TO WS-Z (WS-NANOBOTS-LEN) 
          END-PERFORM.
          CLOSE INPUT-FILE.
          EXIT.
      *
       PART-1 SECTION.
          PERFORM FIND-STRONGEST.
          MOVE WS-STRONGEST-I TO WS-I-1.
          PERFORM VARYING WS-I-2 FROM 1 bY 1
          UNTIL WS-I-2 > WS-NANOBOTS-LEN
             PERFORM COMPUTE-MANHATTAN-DISTANCE
             IF WS-DISTANCE <= WS-RADIUS (WS-I-1) THEN
                ADD 1 TO WS-PART-1-RESULT
             END-IF
          END-PERFORM.
          DISPLAY "PART 1: " WS-PART-1-RESULT.
          EXIT.
      *
       PART-2 SECTION.
      *   ADJUST ITERATION COUNTS (WS-T-1, WS-T-2) AND STARTING POINT
      *   ACCORDING TO THE INPUT. HERE THE STARTING POINT IS ONE OF
      *   NANOBOTS CHOSEN RANDOMLY.
      *
      *   THIS METHOD CAN GET TRAPPED IN LOCAL MINIMAS AND REQUIRES
      *   PROGRAMMER'S GUIDANCE TO CIRCUMVENT THEM, SEE BELOW FOR AN
      *   EXAMPLE.
      *
      *   AS ALWAYS WITH THIS CHALLENGE, I WANTED TO AVOID FANCY 
      *   STUFF, SAW SOMEONE'S APPROACH WITH BINARY SEARCH, THOUGHT
      *   WOULD BE SUITABLE FOR COBOL. :) 
          MOVE -29620959 TO WS-BEST-X.
          MOVE 40130696 TO WS-BEST-Y.
          MOVE 41844171 TO WS-BEST-Z.
          COMPUTE WS-BEST-ORIGIN-DISTANCE =
             FUNCTION ABS (WS-BEST-X) +
             FUNCTION ABS (WS-BEST-Y) +
             FUNCTION ABS (WS-BEST-Z).
          MOVE WS-BEST-X TO WS-P-X.
          MOVE WS-BEST-Y TO WS-P-Y.
          MOVE WS-BEST-Z TO WS-P-Z.
          PERFORM FIND-IN-RANGE.
          MOVE WS-P-I-R TO WS-BEST-IN-RANGE
          PERFORM VARYING WS-T-1 FROM 1 BY 1
          UNTIL WS-T-1 > 5
      *      WS-T-2 = 2^24
             MOVE 16777216 TO WS-T-2
             PERFORM UNTIL WS-T-2 = 0
                PERFORM BINARY-SEARCH-ITERATION
                DIVIDE WS-T-2 BY 2 GIVING WS-T-2
             END-PERFORM 
          END-PERFORM.
          MOVE WS-BEST-ORIGIN-DISTANCE TO WS-PART-2-RESULT.
          DISPLAY "PART 2: " WS-PART-2-RESULT.
          EXIT.
      *
       BINARY-SEARCH-ITERATION SECTION.
      *   THE SEARCH BOUNDARY CAN BE FINE-TUNED AS WELL.
          PERFORM VARYING WS-T-3 FROM -5 BY 1
          UNTIL WS-T-3 > 3
             PERFORM VARYING WS-T-4 FROM -5 BY 1
             UNTIL WS-T-4 > 3
                PERFORM VARYING WS-T-5 FROM -5 BY 1
                UNTIL WS-T-5 > 3
                   COMPUTE WS-P-X = WS-BEST-X + WS-T-3 * WS-T-2
                   COMPUTE WS-P-Y = WS-BEST-Y + WS-T-4 * WS-T-2
                   COMPUTE WS-P-Z = WS-BEST-Z + WS-T-5 * WS-T-2
                   COMPUTE WS-P-O-D = FUNCTION ABS (WS-P-X) +
                      FUNCTION ABS (WS-P-Y) +
                      FUNCTION ABS (WS-P-Z)
                   PERFORM FIND-IN-RANGE
      *            ONE-OFF ESCAPE OF LOCAL MINIMA TO CIRCUMVENT.
                   IF WS-P-I-R = 901 AND
                      WS-BEST-IN-RANGE = 901 AND
                      WS-P-O-D > WS-BEST-ORIGIN-DISTANCE THEN
                         MOVE WS-P-X TO WS-BEST-X
                         MOVE WS-P-Y TO WS-BEST-Y
                         MOVE WS-P-Z TO WS-BEST-Z
                         MOVE WS-P-I-R TO WS-BEST-IN-RANGE
                         MOVE WS-P-O-D TO WS-BEST-ORIGIN-DISTANCE
      *            FOR THIS LOCAL MINIMA EXPLORE IN THE OTHER DIRECTION,
      *            SEE EXTRA CHECK BELOW.
                   ELSE IF WS-P-I-R = 936 AND
                      WS-BEST-IN-RANGE = 936 AND
                      WS-P-O-D > WS-BEST-ORIGIN-DISTANCE THEN
                         MOVE WS-P-X TO WS-BEST-X
                         MOVE WS-P-Y TO WS-BEST-Y
                         MOVE WS-P-Z TO WS-BEST-Z
                         MOVE WS-P-I-R TO WS-BEST-IN-RANGE
                         MOVE WS-P-O-D TO WS-BEST-ORIGIN-DISTANCE
      *            EXPLORE IN THE OTHER DIRECTION FOR LOCAL MINIMA 936.
                   ELSE IF NOT WS-P-I-R = 936 AND
                      WS-P-I-R = WS-BEST-IN-RANGE AND
                      WS-P-O-D < WS-BEST-ORIGIN-DISTANCE THEN
                         MOVE WS-P-X TO WS-BEST-X
                         MOVE WS-P-Y TO WS-BEST-Y
                         MOVE WS-P-Z TO WS-BEST-Z
                         MOVE WS-P-I-R TO WS-BEST-IN-RANGE
                         MOVE WS-P-O-D TO WS-BEST-ORIGIN-DISTANCE         
                   END-IF
                   IF WS-P-I-R > WS-BEST-IN-RANGE THEN
                      MOVE WS-P-X TO WS-BEST-X
                      MOVE WS-P-Y TO WS-BEST-Y
                      MOVE WS-P-Z TO WS-BEST-Z
                      MOVE WS-P-I-R TO WS-BEST-IN-RANGE
                      MOVE WS-P-O-D TO WS-BEST-ORIGIN-DISTANCE
                   END-IF
                END-PERFORM
             END-PERFORM
          END-PERFORM.
          EXIT.
      *
       FIND-IN-RANGE SECTION.
          MOVE 0 TO WS-P-I-R.
          PERFORM VARYING WS-I-1 FROM 1 BY 1
          UNTIL WS-I-1 > WS-NANOBOTS-LEN
             COMPUTE WS-T-6 = FUNCTION ABS (WS-X (WS-I-1) - WS-P-X)
             COMPUTE WS-T-7 = FUNCTION ABS (WS-Y (WS-I-1) - WS-P-Y)
             COMPUTE WS-T-8 = FUNCTION ABS (WS-Z (WS-I-1) - WS-P-Z)
             COMPUTE WS-DISTANCE = WS-T-6 + WS-T-7 + WS-T-8
             IF WS-DISTANCE <= WS-RADIUS (WS-I-1) THEN
                ADD 1 TO WS-P-I-R
             END-IF
          END-PERFORM.
          EXIT.
      *
       FIND-STRONGEST SECTION.
          PERFORM VARYING WS-I-1 FROM 1 BY 1
          UNTIL WS-I-1 > WS-NANOBOTS-LEN
             IF WS-RADIUS (WS-I-1) > WS-RADIUS (WS-STRONGEST-I) THEN
                MOVE WS-I-1 TO WS-STRONGEST-I
             END-IF
          END-PERFORM.
          EXIT.
      *
       COMPUTE-MANHATTAN-DISTANCE SECTION.
          COMPUTE WS-T-1 = WS-X (WS-I-1) - WS-X (WS-I-2).
          COMPUTE WS-T-1 = FUNCTION ABS (WS-T-1).
          COMPUTE WS-T-2 = WS-Y (WS-I-1) - WS-Y ( WS-I-2).
          COMPUTE WS-T-2 = FUNCTION ABS (WS-T-2).
          COMPUTE WS-T-3 = WS-Z (WS-I-1) - WS-Z (WS-I-2).
          COMPUTE WS-T-3 = FUNCTION ABS (WS-T-3).
          COMPUTE WS-DISTANCE = WS-T-1 + WS-T-2 + WS-T-3.
          EXIT.
