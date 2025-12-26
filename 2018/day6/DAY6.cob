       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY6.
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
             03 INPUT-STR PIC X(10).
       WORKING-STORAGE SECTION.
          77 WS-HEIGHT PIC 9(5) VALUE 500.
          77 WS-WIDTH PIC 9(5) VALUE 500.
          77 WS-PART-2-BOUNDS PIC 9(5) VALUE 9999.
          77 WS-MORE-THAN-ONE PIC 9(5) VALUE 1000.
          01 WS-INPUT-FILE-EOF PIC A(1) VALUE "N".
          01 WS-POINTS.
             03 WS-POINTS-LENGTH PIC 9(5) USAGE BINARY VALUE 0.
             03 WS-POINTS-TABLE OCCURS 0 TO 1000
             DEPENDING ON WS-POINTS-LENGTH.
                05 WS-X PIC 9(5).
                05 WS-Y PIC 9(5).
                05 WS-AREA PIC 9(18) USAGE BINARY VALUE 0.
          01 WS-MAP.
             03 WS-ROWS OCCURS 500.
                05 WS-COLS OCCURS 500.
                   07 WS-CLOSEST PIC 9(4) VALUE 0.
                      88 MORE-THAN-ONE VALUE 1000.
                   07 WS-DISTANCE PIC 9(18) USAGE BINARY
                      VALUE 999999999999999999.
          01 WS-IDX-1 PIC 9(5) USAGE BINARY.
          01 WS-IDX-2 PIC 9(5) USAGE BINARY.
          01 WS-IDX-3 PIC 9(5) USAGE BINARY.
          01 WS-TMP-1 PIC S9(18) USAGE BINARY.
          01 WS-TMP-2 PIC S9(18) USAGE BINARY.
          01 WS-TMP-3 PIC S9(18) USAGE BINARY.
          01 WS-TMP-4 PIC S9(18) USAGE BINARY.
          01 WS-INFINITE PIC A(1) VALUE "N".
          01 WS-PART-1-RESULT PIC 9(18) USAGE BINARY VALUE 0.
          01 WS-PART-2-RESULT PIC 9(18) USAGE BINARY VALUE 0.
      *
       PROCEDURE DIVISION.
       INPUT-PARSE SECTION.
          OPEN INPUT INPUT-FILE.
          PERFORM UNTIL WS-INPUT-FILE-EOF = "Y"
             READ INPUT-FILE
                AT END
                   MOVE "Y" TO WS-INPUT-FILE-EOF
                NOT AT END
                   ADD 1 TO WS-POINTS-LENGTH
                   UNSTRING INPUT-STR DELIMITED BY ", " INTO
                      WS-X (WS-POINTS-LENGTH), WS-Y (WS-POINTS-LENGTH)
             END-READ
          END-PERFORM
          CLOSE INPUT-FILE.
          PERFORM PART-1.
          PERFORM PART-2.
          STOP RUN.
      *
       PART-1 SECTION.
          PERFORM VARYING WS-IDX-1 FROM 1 BY 1
          UNTIL WS-IDX-1 > WS-HEIGHT
             PERFORM VARYING WS-IDX-2 FROM 1 BY 1
             UNTIL WS-IDX-2 > WS-WIDTH
                PERFORM VARYING WS-IDX-3 FROM 1 BY 1
                UNTIL WS-IDX-3 > WS-POINTS-LENGTH
                   PERFORM COMPUTE-MANHATTAN-DISTANCE
                END-PERFORM
             END-PERFORM
          END-PERFORM.
          PERFORM VARYING WS-IDX-1 FROM 1 BY 1
          UNTIL WS-IDX-1 > WS-HEIGHT
             PERFORM VARYING WS-IDX-2 FROM 1 BY 1
             UNTIL WS-IDX-2 > WS-WIDTH
                IF NOT MORE-THAN-ONE (WS-IDX-1, WS-IDX-2) THEN
                   MOVE WS-CLOSEST (WS-IDX-1, WS-IDX-2) TO WS-IDX-3
                   ADD 1 TO WS-AREA (WS-IDX-3)
                END-IF
             END-PERFORM
          END-PERFORM.
          PERFORM VARYING WS-IDX-1 FROM 1 BY 1
          UNTIL WS-IDX-1 > WS-POINTS-LENGTH
             PERFORM CHECK-EDGES
             IF NOT WS-INFINITE = "Y" AND
                WS-AREA (WS-IDX-1) > WS-PART-1-RESULT THEN
                   MOVE WS-AREA (WS-IDX-1) TO WS-PART-1-RESULT
             END-IF
          END-PERFORM.
          DISPLAY "PART 1: " WS-PART-1-RESULT.
          EXIT.
      *
       PART-2 SECTION.
          PERFORM VARYING WS-IDX-1 FROM 0 BY 1
          UNTIL WS-IDX-1 > WS-PART-2-BOUNDS
             PERFORM VARYING WS-IDX-2 FROM 0 BY 1
             UNTIL WS-IDX-2 > WS-PART-2-BOUNDS - WS-IDX-1
                IF WS-IDX-1 = 0 AND WS-IDX-2 = 0 THEN
                   MOVE WS-Y (1) TO WS-TMP-1
                   MOVE WS-X (1) TO WS-TMP-2
                   PERFORM CHECK-POINT
                ELSE IF WS-IDX-1 = 0 THEN
                   MOVE WS-Y (1) TO WS-TMP-1
                   COMPUTE WS-TMP-2 = WS-X (1) - WS-IDX-2
                   PERFORM CHECK-POINT
                   COMPUTE WS-TMP-2 = WS-X (1) + WS-IDX-2
                   PERFORM CHECK-POINT
                ELSE IF WS-IDX-2 = 0 THEN
                   MOVE WS-X (1) TO WS-TMP-2
                   COMPUTE WS-TMP-1 = WS-Y (1) - WS-IDX-1
                   PERFORM CHECK-POINT
                   COMPUTE WS-TMP-1 = WS-Y (1) + WS-IDX-1
                   PERFORM CHECK-POINT
                ELSE
                   COMPUTE WS-TMP-1 = WS-Y (1) - WS-IDX-1
                   COMPUTE WS-TMP-2 = WS-X (1) - WS-IDX-2
                   PERFORM CHECK-POINT
                   COMPUTE WS-TMP-1 = WS-Y (1) + WS-IDX-1
                   COMPUTE WS-TMP-2 = WS-X (1) - WS-IDX-2
                   PERFORM CHECK-POINT
                   COMPUTE WS-TMP-1 = WS-Y (1) - WS-IDX-1
                   COMPUTE WS-TMP-2 = WS-X (1) + WS-IDX-2
                   PERFORM CHECK-POINT
                   COMPUTE WS-TMP-1 = WS-Y (1) + WS-IDX-1
                   COMPUTE WS-TMP-2 = WS-X (1) + WS-IDX-2
                   PERFORM CHECK-POINT
                END-IF
             END-PERFORM
          END-PERFORM.
          DISPLAY "PART 2: " WS-PART-2-RESULT.
          EXIT.
      *
       COMPUTE-MANHATTAN-DISTANCE SECTION.
          COMPUTE WS-TMP-1 = WS-IDX-1 - WS-Y (WS-IDX-3) - 1.
          MOVE FUNCTION ABS (WS-TMP-1) TO WS-TMP-1.
          COMPUTE WS-TMP-2 = WS-IDX-2 - WS-X (WS-IDX-3) - 1.
          MOVE FUNCTION ABS (WS-TMP-2) TO WS-TMP-2.
          ADD WS-TMP-2 TO WS-TMP-1.
          IF WS-TMP-1 < WS-DISTANCE (WS-IDX-1, WS-IDX-2) THEN
             MOVE WS-TMP-1 TO WS-DISTANCE (WS-IDX-1, WS-IDX-2)
             MOVE WS-IDX-3 TO WS-CLOSEST (WS-IDX-1, WS-IDX-2)
          ELSE IF WS-TMP-1 = WS-DISTANCE (WS-IDX-1, WS-IDX-2) THEN
             MOVE WS-MORE-THAN-ONE TO WS-CLOSEST (WS-IDX-1, WS-IDX-2)
          END-IF.
          EXIT.
      *
       CHECK-EDGES SECTION.
          MOVE "N" TO WS-INFINITE.
          PERFORM VARYING WS-IDX-2 FROM 1 BY 1
          UNTIL WS-IDX-2 > WS-HEIGHT
             IF WS-CLOSEST (WS-IDX-2, 1) = WS-IDX-1 OR
                WS-CLOSEST (WS-IDX-2, WS-WIDTH) = WS-IDX-1 THEN
                   MOVE "Y" TO WS-INFINITE
                   EXIT SECTION
             END-IF
          END-PERFORM.
          PERFORM VARYING WS-IDX-2 FROM 1 BY 1
          UNTIL WS-IDX-2 > WS-WIDTH
             IF WS-CLOSEST (1, WS-IDX-2) = WS-IDX-1 OR
                WS-CLOSEST (WS-HEIGHT, WS-IDX-2) = WS-IDX-1 THEN
                   MOVE "Y" TO WS-INFINITE
                   EXIT SECTION
             END-IF
          END-PERFORM.
          EXIT.
      *
       CHECK-POINT SECTION.
          MOVE 0 TO WS-TMP-4.
          PERFORM VARYING WS-IDX-3 FROM 1 BY 1
          UNTIL WS-IDX-3 > WS-POINTS-LENGTH
             COMPUTE WS-TMP-3 = WS-Y (WS-IDX-3) - WS-TMP-1
             ADD FUNCTION ABS (WS-TMP-3) TO WS-TMP-4
             COMPUTE WS-TMP-3 = WS-X (WS-IDX-3) - WS-TMP-2
             ADD FUNCTION ABS (WS-TMP-3) TO WS-TMP-4 
             IF WS-TMP-4 > WS-PART-2-BOUNDS THEN
                EXIT SECTION
             END-IF
          END-PERFORM.
          ADD 1 TO WS-PART-2-RESULT
          EXIT.
       