       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY25.
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
          01 WS-INPUT-FILE-EOF PIC X(1) VALUE "N".
          01 WS-UNION-FIND-LENGTH PIC 9(18) USAGE COMP-5 VALUE 0.
          01 WS-M-D PIC 9(18) USAGE COMP-5.
          01 WS-P-I PIC 9(18) USAGE COMP-5.
          01 WS-U-I-1 PIC 9(18) USAGE COMP-5.
          01 WS-U-I-2 PIC 9(18) USAGE COMP-5.
          01 WS-UNION-FIND OCCURS 10000.
             03 WS-PARENT PIC 9(18) USAGE COMP-5. 
             03 WS-SIZE PIC 9(18) USAGE COMP-5.
             03 WS-W PIC S9(18) USAGE COMP-5.
             03 WS-X PIC S9(18) USAGE COMP-5.
             03 WS-Y PIC S9(18) USAGE COMP-5.
             03 WS-Z PIC S9(18) USAGE COMP-5.
          01 WS-IDX.
             03 WS-I-1 PIC 9(18) USAGE COMP-5.
             03 WS-I-2 PIC 9(18) USAGE COMP-5.
          01 WS-TMP.
             03 WS-T-D-1 PIC S9(18).
             03 WS-T-D-2 PIC S9(18).
             03 WS-T-D-3 PIC S9(18).
             03 WS-T-D-4 PIC S9(18).
             03 WS-T-1 PIC S9(18) USAGE COMP-5.
          01 WS-PART-1-RESULT PIC 9(18) USAGE COMP-5 VALUE 0.
      *
       PROCEDURE DIVISION.
       MAIN SECTION.
          PERFORM PARSE-INPUT.
          PERFORM PART-1.
          STOP RUN.
      *
       PARSE-INPUT SECTION.
          OPEN INPUT INPUT-FILE.
          PERFORM UNTIL WS-INPUT-FILE-EOF = "Y"
             READ INPUT-FILE
                AT END
                   MOVE "Y" TO WS-INPUT-FILE-EOF
                NOT AT END
                   ADD 1 TO WS-UNION-FIND-LENGTH
                   ADD 1 TO WS-PART-1-RESULT
                   UNSTRING INPUT-STR DELIMITED BY ","
                      INTO WS-T-D-1, WS-T-D-2, WS-T-D-3, WS-T-D-4
                   MOVE WS-T-D-1 TO WS-W (WS-UNION-FIND-LENGTH)
                   MOVE WS-T-D-2 TO WS-X (WS-UNION-FIND-LENGTH)
                   MOVE WS-T-D-3 TO WS-Y (WS-UNION-FIND-LENGTH)
                   MOVE WS-T-D-4 TO WS-Z (WS-UNION-FIND-LENGTH)
                   MOVE 1 TO WS-SIZE (WS-UNION-FIND-LENGTH)
                   MOVE WS-UNION-FIND-LENGTH
                      TO WS-PARENT (WS-UNION-FIND-LENGTH)
          END-PERFORM.
          CLOSE INPUT-FILE.
          EXIT.
      *
       PART-1 SECTION.
          PERFORM VARYING WS-I-1 FROM 1 BY 1
          UNTIL WS-I-1 = WS-UNION-FIND-LENGTH
             PERFORM VARYING WS-I-2 FROM 1 BY 1
             UNTIL WS-I-2 > WS-UNION-FIND-LENGTH
                PERFORM COMPUTE-MANHATTAN-DISTANCE
                IF WS-M-D <= 3 THEN
                   MOVE WS-I-1 TO WS-U-I-1
                   MOVE WS-I-2 TO WS-U-I-2
                   PERFORM UNION
                END-IF
             END-PERFORM
          END-PERFORM.
          DISPLAY "PART 1: " WS-PART-1-RESULT.
          EXIT.
      *
       FIND SECTION.
          PERFORM UNTIL WS-PARENT (WS-P-I) = WS-P-I
             MOVE WS-PARENT (WS-P-I) TO WS-T-1
             MOVE WS-PARENT (WS-PARENT (WS-P-I)) TO WS-PARENT (WS-P-I)
             MOVE WS-T-1 TO WS-P-I
          END-PERFORM.
          EXIT.
      *
       UNION SECTION.
          MOVE WS-U-I-1 TO WS-P-I.
          PERFORM FIND.
          MOVE WS-P-I TO WS-U-I-1.
          MOVE WS-U-I-2 TO WS-P-I.
          PERFORM FIND.
          MOVE WS-P-I TO WS-U-I-2.
          IF WS-U-I-1 = WS-U-I-2 THEN
             EXIT SECTION
          END-IF.
          IF WS-SIZE (WS-U-I-1) < WS-SIZE (WS-U-I-2) THEN
             MOVE WS-U-I-1 TO WS-T-1
             MOVE WS-U-I-2 TO WS-U-I-1
             MOVE WS-T-1 TO WS-U-I-2
          END-IF.
          MOVE WS-U-I-1 TO WS-PARENT (WS-U-I-2).
          ADD WS-SIZE (WS-U-I-2) TO WS-SIZE (WS-U-I-1).
          SUBTRACT 1 FROM WS-PART-1-RESULT.
          EXIT.
      *
       COMPUTE-MANHATTAN-DISTANCE SECTION.
          MOVE 0 TO WS-M-D.
          COMPUTE WS-T-1 = WS-W (WS-I-1) - WS-W (WS-I-2).
          ADD FUNCTION ABS (WS-T-1) TO WS-M-D.
          COMPUTE WS-T-1 = WS-X (WS-I-1) - WS-X (WS-I-2).
          ADD FUNCTION ABS (WS-T-1) TO WS-M-D.
          COMPUTE WS-T-1 = WS-Y (WS-I-1) - WS-Y (WS-I-2).
          ADD FUNCTION ABS (WS-T-1) TO WS-M-D.
          COMPUTE WS-T-1 = WS-Z (WS-I-1) - WS-Z (WS-I-2).
          ADD FUNCTION ABS (WS-T-1) TO WS-M-D.
          EXIT.
