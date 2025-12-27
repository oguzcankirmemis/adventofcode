       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY10.
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
             03 INPUT-STR PIC X(80).
       WORKING-STORAGE SECTION.
          77 WS-ALIGN-THRESHOLD PIC S9(18) VALUE 5000.
          77 WS-DISPLAY-ROWS PIC S9(18) VALUE 10.
          01 WS-INPUT-FILE-EOF PIC X(1) VALUE "N".
          01 WS-TMP-STR-1 PIC X(40).
          01 WS-TMP-STR-2 PIC X(40).
          01 WS-TMP-1 PIC S9(18).
          01 WS-TMP-2 PIC S9(18).
          01 WS-LENGTH PIC S9(18) USAGE BINARY VALUE 0.
          01 WS-ALIGNED PIC X(1) VALUE "N".
             88 STARS-ALIGNED VALUE "Y".
          01 WS-IDX-1 PIC S9(18) USAGE BINARY VALUE 0.
          01 WS-IDX-2 PIC S9(18) USAGE BINARY VALUE 0.
          01 WS-SECONDS PIC S9(18) USAGE BINARY VALUE 0.
          01 WS-MAP.
             03 WS-ROW OCCURS 10.
                05 WS-COLS.
                   07 WS-COL PIC X(1) VALUE "." OCCURS 62.
                05 WS-ROW-STR REDEFINES WS-COLS PIC X(62).
          01 WS-TABLE OCCURS 1000.
             03 WS-POSITION.
                05 WS-P-X PIC S9(18) USAGE BINARY.
                05 WS-P-Y PIC S9(18) USAGE BINARY.
             03 WS-VELOCITY.
                05 WS-V-X PIC S9(18) USAGE BINARY.
                05 WS-V-Y PIC S9(18) USAGE BINARY.
          01 WS-METRICS.
             03 WS-MIN-X PIC S9(18) USAGE BINARY
                VALUE 999999999999999999.
             03 WS-MIN-Y PIC S9(18) USAGE BINARY
                VALUE 999999999999999999.
      *
       PROCEDURE DIVISION.
       PARSE-INPUT SECTION.
          OPEN INPUT INPUT-FILE.
          PERFORM UNTIL WS-INPUT-FILE-EOF = "Y"
             READ INPUT-FILE
                AT END
                   MOVE "Y" TO WS-INPUT-FILE-EOF
                NOT AT END
                   ADD 1 TO WS-LENGTH
                   UNSTRING INPUT-STR
                      DELIMITED BY "position=<"
                      INTO WS-TMP-STR-2, WS-TMP-STR-1
                   UNSTRING WS-TMP-STR-1
                      DELIMITED BY ">"
                      INTO WS-TMP-STR-1
                   UNSTRING WS-TMP-STR-1
                      DELIMITED BY ", "
                      INTO WS-TMP-1, WS-TMP-2
                   MOVE WS-TMP-1 TO WS-P-X (WS-LENGTH)
                   MOVE WS-TMP-2 TO WS-P-Y (WS-LENGTH)
                   UNSTRING INPUT-STR
                      DELIMITED BY "velocity=<"
                      INTO WS-TMP-STR-2, WS-TMP-STR-1
                   UNSTRING WS-TMP-STR-1
                      DELIMITED BY ">"
                      INTO WS-TMP-STR-1
                   UNSTRING WS-TMP-STR-1
                      DELIMITED BY ", "
                      INTO WS-TMP-1, WS-TMP-2
                   MOVE WS-TMP-1 TO WS-V-X (WS-LENGTH)
                   MOVE WS-TMP-2 TO WS-V-Y (WS-LENGTH)
             END-READ
          END-PERFORM.
          CLOSE INPUT-FILE.
          PERFORM PART-1.
          PERFORM PART-2.
          STOP RUN.
      *
       PART-1 SECTION.
          PERFORM FOREVER
             ADD 1 TO WS-SECONDS
             PERFORM SIMULATE
             PERFORM CHECK-ALIGNED
             IF STARS-ALIGNED THEN
                EXIT PERFORM
             END-IF
          END-PERFORM
          DISPLAY "PART 1:".
          PERFORM DISPLAY-STARS.
          EXIT.
      *
       PART-2 SECTION.
          DISPLAY SPACE.
          DISPLAY "PART 2: " WS-SECONDS.
          EXIT.
      *
       SIMULATE SECTION.
          PERFORM VARYING WS-IDX-1 FROM 1 BY 1
          UNTIL WS-IDX-1 > WS-LENGTH
             COMPUTE WS-P-X (WS-IDX-1) =
                WS-P-X (WS-IDX-1) + WS-V-X (WS-IDX-1)
             COMPUTE WS-P-Y (WS-IDX-1) =
                WS-P-Y (WS-IDX-1) + WS-V-Y (WS-IDX-1)
          END-PERFORM.
          EXIT.
      *
       CHECK-ALIGNED SECTION.
          MOVE 0 TO WS-TMP-1.
          PERFORM VARYING WS-IDX-1 FROM 1 BY 1
          UNTIL WS-IDX-1 > WS-LENGTH
             COMPUTE WS-TMP-2 = WS-IDX-1 + 1
             PERFORM VARYING WS-IDX-2 FROM WS-TMP-2 BY 1
             UNTIL WS-IDX-2 > WS-LENGTH
                IF WS-P-X (WS-IDX-1) = WS-P-X (WS-IDX-2) OR
                   WS-P-Y (WS-IDX-1) = WS-P-Y (WS-IDX-2) THEN
                      ADD 1 TO WS-TMP-1
                END-IF
             END-PERFORM
          END-PERFORM.
          IF WS-TMP-1 > WS-ALIGN-THRESHOLD THEN
             MOVE "Y" TO WS-ALIGNED
          END-IF.
          EXIT.
      *
       DISPLAY-STARS SECTION.
          PERFORM VARYING WS-IDX-1 FROM 1 BY 1
          UNTIL WS-IDX-1 > WS-LENGTH
             IF WS-P-X (WS-IDX-1) < WS-MIN-X THEN
                MOVE WS-P-X (WS-IDX-1) TO WS-MIN-X
             END-IF
             IF WS-P-Y (WS-IDX-1) < WS-MIN-Y THEN
                MOVE WS-P-Y (WS-IDX-1) TO WS-MIN-Y
             END-IF
          END-PERFORM.
          PERFORM VARYING WS-IDX-1 FROM 1 BY 1
          UNTIL WS-IDX-1 > WS-LENGTH
             COMPUTE WS-TMP-1 = WS-P-X (WS-IDX-1) - WS-MIN-X + 1
             COMPUTE WS-TMP-2 = WS-P-Y (WS-IDX-1) - WS-MIN-Y + 1
             MOVE "#" TO WS-COL (WS-TMP-2, WS-TMP-1)
          END-PERFORM.
          PERFORM VARYING WS-IDX-1 FROM 1 BY 1
          UNTIL WS-IDX-1 > WS-DISPLAY-ROWS
             DISPLAY WS-ROW-STR (WS-IDX-1)
          END-PERFORM.
          EXIT.
