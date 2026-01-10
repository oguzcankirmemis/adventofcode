       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY23A.
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
          01 WS-IDX.
             03 WS-I-1 PIC 9(18) USAGE COMP-5.
             03 WS-I-2 PIC 9(18) USAGE COMP-5.
          01 WS-STRONGEST-I PIC 9(18) USAGE COMP-5 VALUE 1.
          01 WS-DISTANCE PIC 9(18) USAGE COMP-5.
          01 WS-CUBE-IN-RANGE PIC X(1).
          01 WS-POINT.
             03 WS-P-X PIC S9(18) USAGE COMP-5.
             03 WS-P-Y PIC S9(18) USAGE COMP-5.
             03 WS-P-Z PIC S9(18) USAGE COMP-5.
          01 WS-CURRENT-CUBE.
      *      BOUNDS INITIALIZED TO -/+2^32
             03 WS-CURRENT-C-X-1 PIC S9(18) USAGE COMP-5
                VALUE -4294967296.
             03 WS-CURRENT-C-X-2 PIC S9(18) USAGE COMP-5
                VALUE 4294967296.
             03 WS-CURRENT-C-Y-1 PIC S9(18) USAGE COMP-5
                VALUE -4294967296.
             03 WS-CURRENT-C-Y-2 PIC S9(18) USAGE COMP-5
                VALUE 4294967296.
             03 WS-CURRENT-C-Z-1 PIC S9(18) USAGE COMP-5
                VALUE -4294967296.
             03 WS-CURRENT-C-Z-2 PIC S9(18) USAGE COMP-5
                VALUE 4294967296.
          01 WS-CURRENT-LEVEL PIC 9(18) USAGE COMP-5.
          01 WS-BEST-P PIC 9(18) USAGE COMP-5 VALUE 0.
          01 WS-OCTREE.
             03 WS-OCTREE-LEN PIC 9(18) USAGE COMP-5 VALUE 0.
             03 WS-O-E OCCURS 10000.
                05 WS-CUBE.
                   07 WS-C-X-1 PIC S9(18) USAGE COMP-5.
                   07 WS-C-X-2 PIC S9(18) USAGE COMP-5.
                   07 WS-C-Y-1 PIC S9(18) USAGE COMP-5.
                   07 WS-C-Y-2 PIC S9(18) USAGE COMP-5.
                   07 WS-C-Z-1 PIC S9(18) USAGE COMP-5.
                   07 WS-C-Z-2 PIC S9(18) USAGE COMP-5.
                05 WS-I-R PIC 9(18) USAGE COMP-5.
                05 WS-O-D PIC 9(18) USAGE COMP-5.
                05 WS-LEVEL PIC 9(18) USAGE COMP-5.
          01 WS-QUEUE.
             03 WS-QUEUE-HEAD PIC 9(18) USAGE COMP-5 VALUE 0.
             03 WS-QUEUE-TAIL PIC 9(18) USAGE COMP-5 VALUE 0.
             03 WS-QUEUE-I PIC 9(18) USAGE COMP-5 OCCURS 10000.
          01 WS-LEVEL-BESTS OCCURS 64.
             03 WS-L-B-I-R PIC 9(18) USAGE COMP-5 VALUE 0.
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
      *
          ADD 1 TO WS-OCTREE-LEN.
          MOVE WS-CURRENT-CUBE TO WS-CUBE (WS-OCTREE-LEN).
          MOVE 1 TO WS-LEVEL (WS-OCTREE-LEN).
          MOVE 1 TO WS-I-1.
          PERFORM COUNT-BOTS-IN-RANGE.
          PERFORM COMPUTE-CUBE-DISTANCE-TO-ORIGIN.
          MOVE WS-I-R (WS-OCTREE-LEN) TO WS-L-B-I-R (1).
          ADD 1 TO WS-QUEUE-TAIL.
          MOVE 1 TO WS-QUEUE-I (WS-QUEUE-TAIL).
      *
          PERFORM UNTIL WS-QUEUE-HEAD > WS-QUEUE-TAIL
      *
             MOVE WS-QUEUE-I (WS-QUEUE-HEAD) TO WS-I-1
             ADD 1 TO WS-QUEUE-HEAD
             MOVE WS-CUBE (WS-I-1) TO WS-CURRENT-CUBE
             MOVE WS-LEVEL (WS-I-1) TO WS-CURRENT-LEVEL
      *
             IF WS-CURRENT-C-X-1 = WS-CURRENT-C-X-2 AND
                WS-CURRENT-C-Y-1 = WS-CURRENT-C-Y-2 AND
                WS-CURRENT-C-Z-1 = WS-CURRENT-C-Z-2 THEN
                   IF WS-BEST-P EQUAL TO 0 THEN
                         MOVE WS-I-1 TO WS-BEST-P
                   ELSE IF WS-I-R (WS-I-1) > WS-I-R (WS-BEST-P) THEN
                         MOVE WS-I-1 TO WS-BEST-P
                   ELSE IF WS-I-R (WS-I-1) = WS-I-R (WS-BEST-P) AND
      *               FIND THE ONE CLOSEST TO ORIGIN AMONG THE LEVEL BESTS
                      WS-O-D (WS-I-1) < WS-O-D (WS-BEST-P) THEN
                         MOVE WS-I-1 TO WS-BEST-P
                   END-IF
             ELSE IF WS-I-R (WS-I-1) <
      *         SKIP IF A BETTER COUNT IN THE LEVEL IS ALREADY FOUND
                WS-L-B-I-R (WS-CURRENT-LEVEL) THEN
                   CONTINUE
             ELSE
                   PERFORM SPLIT-CUBE
                   COMPUTE WS-I-1 = WS-OCTREE-LEN - 7
                   PERFORM VARYING WS-I-1 FROM WS-I-1 BY 1
                   UNTIL WS-I-1 > WS-OCTREE-LEN
                      PERFORM COUNT-BOTS-IN-RANGE
                      PERFORM COMPUTE-CUBE-DISTANCE-TO-ORIGIN
                      COMPUTE WS-LEVEL (WS-I-1) = WS-CURRENT-LEVEL + 1
                      IF WS-I-R (WS-I-1) >=
                         WS-L-B-I-R (WS-CURRENT-LEVEL + 1) THEN
      *                     ONLY EXPLORE SOLUTIONS THAT ARE AT LEAST AS
      *                     GOOD AS LEVEL BEST
                            ADD 1 TO WS-QUEUE-TAIL
                            MOVE WS-I-1 TO WS-QUEUE-I (WS-QUEUE-TAIL)
                            MOVE WS-I-R (WS-I-1) TO
                               WS-L-B-I-R (WS-CURRENT-LEVEL + 1)
                      END-IF
                   END-PERFORM
             END-IF
      *
          END-PERFORM.
          MOVE WS-O-D (WS-BEST-P) TO WS-PART-2-RESULT.
          DISPLAY "PART 2: " WS-PART-2-RESULT.
          EXIT.
      *
       SPLIT-CUBE SECTION.
          COMPUTE WS-T-1 = WS-CURRENT-C-X-1 +
             (WS-CURRENT-C-X-2 - WS-CURRENT-C-X-1) / 2.
          COMPUTE WS-T-2 = WS-CURRENT-C-Y-1 +
             (WS-CURRENT-C-Y-2 - WS-CURRENT-C-Y-1) / 2.
          COMPUTE WS-T-3 = WS-CURRENT-C-Z-1 +
             (WS-CURRENT-C-Z-2 - WS-CURRENT-C-Z-1) / 2.
      *
          ADD 1 TO WS-OCTREE-LEN.
          MOVE WS-CURRENT-C-X-1 TO WS-C-X-1 (WS-OCTREE-LEN).
          MOVE WS-CURRENT-C-Y-1 TO WS-C-Y-1 (WS-OCTREE-LEN).
          MOVE WS-CURRENT-C-Z-1 TO WS-C-Z-1 (WS-OCTREE-LEN).
          MOVE WS-T-1 TO WS-C-X-2 (WS-OCTREE-LEN).
          MOVE WS-T-2 TO WS-C-Y-2 (WS-OCTREE-LEN).
          MOVE WS-T-3 TO WS-C-Z-2 (WS-OCTREE-LEN).
      *
          ADD 1 TO WS-OCTREE-LEN.
          COMPUTE WS-C-X-1 (WS-OCTREE-LEN) = WS-T-1 + 1.
          MOVE WS-CURRENT-C-Y-1 TO WS-C-Y-1 (WS-OCTREE-LEN).
          MOVE WS-CURRENT-C-Z-1 TO WS-C-Z-1 (WS-OCTREE-LEN).
          MOVE WS-CURRENT-C-X-2 TO WS-C-X-2 (WS-OCTREE-LEN).
          MOVE WS-T-2 TO WS-C-Y-2 (WS-OCTREE-LEN).
          MOVE WS-T-3 TO WS-C-Z-2 (WS-OCTREE-LEN).
      *
          ADD 1 TO WS-OCTREE-LEN.
          MOVE WS-CURRENT-C-X-1 TO WS-C-X-1 (WS-OCTREE-LEN).
          COMPUTE WS-C-Y-1 (WS-OCTREE-LEN) = WS-T-2 + 1.
          MOVE WS-CURRENT-C-Z-1 TO WS-C-Z-1 (WS-OCTREE-LEN).
          MOVE WS-T-1 TO WS-C-X-2 (WS-OCTREE-LEN).
          MOVE WS-CURRENT-C-Y-2 TO WS-C-Y-2 (WS-OCTREE-LEN).
          MOVE WS-T-3 TO WS-C-Z-2 (WS-OCTREE-LEN).
      *
          ADD 1 TO WS-OCTREE-LEN.
          COMPUTE WS-C-X-1 (WS-OCTREE-LEN) = WS-T-1 + 1.
          COMPUTE WS-C-Y-1 (WS-OCTREE-LEN) = WS-T-2 + 1.
          MOVE WS-CURRENT-C-Z-1 TO WS-C-Z-1 (WS-OCTREE-LEN).
          MOVE WS-CURRENT-C-X-2 TO WS-C-X-2 (WS-OCTREE-LEN).
          MOVE WS-CURRENT-C-Y-2 TO WS-C-Y-2 (WS-OCTREE-LEN).
          MOVE WS-T-3 TO WS-C-Z-2 (WS-OCTREE-LEN).
      *
          ADD 1 TO WS-OCTREE-LEN.
          MOVE WS-CURRENT-C-X-1 TO WS-C-X-1 (WS-OCTREE-LEN).
          MOVE WS-CURRENT-C-Y-1 TO WS-C-Y-1 (WS-OCTREE-LEN).
          COMPUTE WS-C-Z-1 (WS-OCTREE-LEN) = WS-T-3 + 1.
          MOVE WS-T-1 TO WS-C-X-2 (WS-OCTREE-LEN).
          MOVE WS-T-2 TO WS-C-Y-2 (WS-OCTREE-LEN).
          MOVE WS-CURRENT-C-Z-2 TO WS-C-Z-2 (WS-OCTREE-LEN).
      *
          ADD 1 TO WS-OCTREE-LEN.
          COMPUTE WS-C-X-1 (WS-OCTREE-LEN) = WS-T-1 + 1.
          MOVE WS-CURRENT-C-Y-1 TO WS-C-Y-1 (WS-OCTREE-LEN).
          COMPUTE WS-C-Z-1 (WS-OCTREE-LEN) = WS-T-3 + 1.
          MOVE WS-CURRENT-C-X-2 TO WS-C-X-2 (WS-OCTREE-LEN).
          MOVE WS-T-2 TO WS-C-Y-2 (WS-OCTREE-LEN).
          MOVE WS-CURRENT-C-Z-2 TO WS-C-Z-2 (WS-OCTREE-LEN).
      *
          ADD 1 TO WS-OCTREE-LEN.
          MOVE WS-CURRENT-C-X-1 TO WS-C-X-1 (WS-OCTREE-LEN).
          COMPUTE WS-C-Y-1 (WS-OCTREE-LEN) = WS-T-2 + 1.
          COMPUTE WS-C-Z-1 (WS-OCTREE-LEN) = WS-T-3 + 1.
          MOVE WS-T-1 TO WS-C-X-2 (WS-OCTREE-LEN).
          MOVE WS-CURRENT-C-Y-2 TO WS-C-Y-2 (WS-OCTREE-LEN).
          MOVE WS-CURRENT-C-Z-2 TO WS-C-Z-2 (WS-OCTREE-LEN).
      *
          ADD 1 TO WS-OCTREE-LEN.
          COMPUTE WS-C-X-1 (WS-OCTREE-LEN) = WS-T-1 + 1.
          COMPUTE WS-C-Y-1 (WS-OCTREE-LEN) = WS-T-2 + 1.
          COMPUTE WS-C-Z-1 (WS-OCTREE-LEN) = WS-T-3 + 1.
          MOVE WS-CURRENT-C-X-2 TO WS-C-X-2 (WS-OCTREE-LEN).
          MOVE WS-CURRENT-C-Y-2 TO WS-C-Y-2 (WS-OCTREE-LEN).
          MOVE WS-CURRENT-C-Z-2 TO WS-C-Z-2 (WS-OCTREE-LEN).
      *
          EXIT.
      *
       COMPUTE-CUBE-DISTANCE-TO-ORIGIN.
          COMPUTE WS-P-X = WS-C-X-1 (WS-I-1) +
             (WS-C-X-2 (WS-I-1) - WS-C-X-1 (WS-I-1)) / 2.
          COMPUTE WS-P-Y = WS-C-Y-1 (WS-I-1) +
             (WS-C-Y-2 (WS-I-1) - WS-C-Y-1 (WS-I-1)) / 2.
          COMPUTE WS-P-Z = WS-C-Z-1 (WS-I-1) +
             (WS-C-Z-2 (WS-I-1) - WS-C-Z-1 (WS-I-1)) / 2.
          COMPUTE WS-O-D (WS-I-1) = FUNCTION ABS (WS-P-X) +
             FUNCTION ABS (WS-P-Y) +
             FUNCTION ABS (WS-P-Z).
          EXIT.
      *
       COUNT-BOTS-IN-RANGE SECTION.
          MOVE 0 TO WS-I-R (WS-I-1).
          PERFORM VARYING WS-I-2 FROM 1 BY 1
          UNTIL WS-I-2 > WS-NANOBOTS-LEN
             PERFORM CHECK-CUBE-IN-RANGE
             IF WS-CUBE-IN-RANGE = "Y" THEN
                ADD 1 TO WS-I-R (WS-I-1)
             END-IF
          END-PERFORM.
          EXIT.
      *
       CHECK-CUBE-IN-RANGE SECTION.
          MOVE "Y" TO WS-CUBE-IN-RANGE.
          PERFORM COMPUTE-CLOSEST-POINT-ON-CUBE.
          COMPUTE WS-T-1 = WS-X (WS-I-2) - WS-P-X.
          COMPUTE WS-T-2 = WS-Y (WS-I-2) - WS-P-Y.
          COMPUTE WS-T-3 = WS-Z (WS-I-2) - WS-P-Z.
          COMPUTE WS-DISTANCE = FUNCTION ABS (WS-T-1) +
             FUNCTION ABS (WS-T-2) +
             FUNCTION ABS (WS-T-3).
          IF WS-DISTANCE > WS-RADIUS (WS-I-2) THEN
             MOVE "N" TO WS-CUBE-IN-RANGE
          END-IF
          EXIT.
      *
       COMPUTE-CLOSEST-POINT-ON-CUBE SECTION.
          IF WS-X (WS-I-2) > WS-C-X-2 (WS-I-1) THEN
             MOVE WS-C-X-2 (WS-I-1) TO WS-P-X
          ELSE IF WS-X (WS-I-2) < WS-C-X-1 (WS-I-1) THEN
             MOVE WS-C-X-1 (WS-I-1) TO WS-P-X
          ELSE
             MOVE WS-X (WS-I-2) TO WS-P-X
          END-IF.
          IF WS-Y (WS-I-2) > WS-C-Y-2 (WS-I-1) THEN
             MOVE WS-C-Y-2 (WS-I-1) TO WS-P-Y
          ELSE IF WS-Y (WS-I-2) < WS-C-Y-1 (WS-I-1) THEN
             MOVE WS-C-Y-1 (WS-I-1) TO WS-P-Y
          ELSE
             MOVE WS-Y (WS-I-2) TO WS-P-Y
          END-IF.
          IF WS-Z (WS-I-2) > WS-C-Z-2 (WS-I-1) THEN
             MOVE WS-C-Z-2 (WS-I-1) TO WS-P-Z
          ELSE IF WS-Z (WS-I-2) < WS-C-Z-1 (WS-I-1) THEN
             MOVE WS-C-Z-1 (WS-I-1) TO WS-P-Z
          ELSE
             MOVE WS-Z (WS-I-2) TO WS-P-Z
          END-IF.
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
