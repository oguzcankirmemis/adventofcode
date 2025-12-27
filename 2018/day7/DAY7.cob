       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY7.
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
             03 INPUT-DEPENDENCY PIC X(1).
             03 FILLER PIC X(30).
             03 INPUT-DEPENDANT PIC X(1).
             03 FILLER PIC X(13).
       WORKING-STORAGE SECTION.
          01 WS-INPUT-FILE-EOF PIC X(1) VALUE "N".
          01 WS-STEPS OCCURS 26.
             03 WS-STEP-NAME PIC X(1) VALUE "~".
             03 WS-COMPLETED PIC X(1) VALUE "N".
                88 WS-STEP-COMPLETED VALUE "Y".
             03 WS-IN-WORK PIC X(1) VALUE "N".
                88 WS-STEP-IN-WORK VALUE "Y".
             03 WS-DEPENDENCIES-LENGTH PIC 9(2) USAGE BINARY VALUE 0.
             03 WS-DEPENDENCIES OCCURS 26.
                05 WS-STEP-IDX PIC 9(2) USAGE BINARY VALUE 0.
          01 WS-IDX-1 PIC 9(2) USAGE BINARY.
          01 WS-IDX-2 PIC 9(2) USAGE BINARY.
          01 WS-IDX-3 PIC 9(2) USAGE BINARY.
          01 WS-IDX-4 PIC 9(2) USAGE BINARY.
          01 WS-PART-1-TABLE-LENGTH PIC 9(2) USAGE BINARY VALUE 0.
          01 WS-PART-1-RESULT.

             03 WS-PART-1-TABLE OCCURS 26.
                05 WS-PART-1-CHAR PIC X(1) VALUE " ".
          01 WS-PART-1-TABLE-STR REDEFINES WS-PART-1-RESULT PIC X(26).
          01 WS-WORKER OCCURS 5.
             03 WS-WORK-IDX PIC 9(2) USAGE BINARY VALUE 0.
             03 WS-TIME PIC 9(2) USAGE BINARY VALUE 0.
          01 WS-STATE PIC X(1) VALUE "N".
             88 WS-STOP VALUE "Y".
          01 WS-PART-2-RESULT PIC 9(4) USAGE BINARY VALUE 0.
      *
       PROCEDURE DIVISION.
       PARSE-INPUT SECTION.
          OPEN INPUT INPUT-FILE.
          PERFORM UNTIL WS-INPUT-FILE-EOF = "Y"
             READ INPUT-FILE
                AT END
                   MOVE "Y" TO WS-INPUT-FILE-EOF
                NOT AT END
                   COMPUTE WS-IDX-1 =
                      FUNCTION ORD (INPUT-DEPENDANT) - 65
                   ADD 1 TO WS-DEPENDENCIES-LENGTH (WS-IDX-1)
                   COMPUTE WS-IDX-2 =
                      FUNCTION ORD (INPUT-DEPENDENCY) - 65
                   MOVE WS-DEPENDENCIES-LENGTH (WS-IDX-1) TO WS-IDX-3
                   MOVE WS-IDX-2
                      TO WS-STEP-IDX (WS-IDX-1, WS-IDX-3)
                   MOVE INPUT-DEPENDANT TO WS-STEP-NAME (WS-IDX-1)
                   MOVE INPUT-DEPENDENCY TO WS-STEP-NAME (WS-IDX-2)
             END-READ
          END-PERFORM.
          CLOSE INPUT-FILE.
          PERFORM PART-1.
          PERFORM RESET-STEPS.
          PERFORM PART-2.
          STOP RUN.
      *
       PART-1 SECTION.
          PERFORM UNTIL WS-STOP
             PERFORM PART-1-STEP
          END-PERFORM.
          DISPLAY "PART 1: " WS-PART-1-TABLE-STR.
          EXIT.
       PART-2 SECTION.
          PERFORM UNTIL WS-STOP
             PERFORM PART-2-STEP
          END-PERFORM.
          DISPLAY "PART 2: " WS-PART-2-RESULT.
          EXIT.
      *
       PART-1-STEP SECTION.
          MOVE "Y" TO WS-STATE.
          MOVE 1 TO WS-IDX-1.
       PART-1-STEP-10.
          IF WS-IDX-1 > 26 THEN
             GO TO PART-1-STEP-99
          END-IF.
          IF WS-STEP-NAME (WS-IDX-1) = "~"
             OR WS-STEP-COMPLETED (WS-IDX-1) THEN
                GO TO PART-1-STEP-70
          END-IF.
          PERFORM VARYING WS-IDX-2 FROM 1 BY 1 
          UNTIL WS-IDX-2 > WS-DEPENDENCIES-LENGTH (WS-IDX-1)
             MOVE WS-STEP-IDX (WS-IDX-1, WS-IDX-2) TO WS-IDX-3
             IF NOT WS-STEP-COMPLETED (WS-IDX-3) THEN
                GO TO PART-1-STEP-70
             END-IF
          END-PERFORM.
          MOVE "N" TO WS-STATE.
          MOVE "Y" TO WS-COMPLETED (WS-IDX-1).
          ADD 1 TO WS-PART-1-TABLE-LENGTH.
          MOVE WS-STEP-NAME (WS-IDX-1) 
             TO WS-PART-1-TABLE (WS-PART-1-TABLE-LENGTH).
          GO TO PART-1-STEP-99.
       PART-1-STEP-70.
          ADD 1 TO WS-IDX-1.
          GO TO PART-1-STEP-10.
       PART-1-STEP-99.
          EXIT.
      *
       RESET-STEPS SECTION.
          MOVE "N" TO WS-STATE.
          PERFORM VARYING WS-IDX-1 FROM 1 BY 1
          UNTIL WS-IDX-1 > 26
             MOVE "N" TO WS-COMPLETED (WS-IDX-1)
          END-PERFORM.
          EXIT.
      *
       PART-2-STEP SECTION.
          MOVE "Y" TO WS-STATE.
          PERFORM VARYING WS-IDX-1 FROM 1 BY 1 UNTIL WS-IDX-1 > 5
             IF WS-TIME (WS-IDX-1) = 0
                AND NOT WS-WORK-IDX (WS-IDX-1) = 0 THEN
                   MOVE WS-WORK-IDX (WS-IDX-1) TO WS-IDX-2
                   MOVE "Y" TO WS-COMPLETED (WS-IDX-2)
                   MOVE 0 TO WS-WORK-IDX (WS-IDX-1)
             END-IF
          END-PERFORM.
          PERFORM VARYING WS-IDX-1 FROM 1 BY 1 UNTIL WS-IDX-1 > 5
             IF WS-TIME (WS-IDX-1) = 0 THEN
                PERFORM ASSIGN-WORK
             ELSE
                SUBTRACT 1 FROM WS-TIME (WS-IDX-1)
             END-IF
          END-PERFORM.
          PERFORM VARYING WS-IDX-1 FROM 1 BY 1 UNTIL WS-IDX-1 > 5
             IF NOT WS-WORK-IDX (WS-IDX-1) = 0 THEN
                MOVE "N" TO WS-STATE
             END-IF
          END-PERFORM.
          IF NOT WS-STOP THEN
             ADD 1 TO WS-PART-2-RESULT
          END-IF.
          EXIT.
      *
       ASSIGN-WORK SECTION.
          MOVE 1 TO WS-IDX-2.
       ASSIGN-WORK-10.
          IF WS-IDX-2 > 26 THEN
             GO TO ASSIGN-WORK-99
          END-IF.
          IF WS-STEP-NAME (WS-IDX-2) = "~"
             OR WS-STEP-COMPLETED (WS-IDX-2)
             OR WS-STEP-IN-WORK (WS-IDX-2) THEN
                GO TO ASSIGN-WORK-70
          END-IF.
          PERFORM VARYING WS-IDX-3 FROM 1 BY 1 
          UNTIL WS-IDX-3 > WS-DEPENDENCIES-LENGTH (WS-IDX-2)
             MOVE WS-STEP-IDX (WS-IDX-2, WS-IDX-3) TO WS-IDX-4
             IF NOT WS-STEP-COMPLETED (WS-IDX-4) THEN
                GO TO ASSIGN-WORK-70
             END-IF
          END-PERFORM.
          MOVE "Y" TO WS-IN-WORK (WS-IDX-2)
          MOVE WS-IDX-2 TO WS-WORK-IDX (WS-IDX-1)
          COMPUTE WS-TIME (WS-IDX-1) = 60 + WS-IDX-2 - 1
          GO TO ASSIGN-WORK-99.
       ASSIGN-WORK-70.
          ADD 1 TO WS-IDX-2.
          GO TO ASSIGN-WORK-10.
       ASSIGN-WORK-99.
          EXIT.
