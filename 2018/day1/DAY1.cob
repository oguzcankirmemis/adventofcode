       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY1.
       AUTHOR. OK999.
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
             03 INPUT-VARIABLE PIC X(18).
       WORKING-STORAGE SECTION.
          01 WS-INPUT-FILE-EOF PIC A(1) VALUE "N".
          01 TMP-VAR PIC S9(18) USAGE COMP-3.
          01 TMP-IDX PIC 9(6) VALUE 1.
          01 PART-1-VARS.
             03 PART-1-SUM PIC S9(18) USAGE COMP-3 VALUE 0.
          01 PART-2-VARS.
             03 PART-2-FOUND PIC A(1) VALUE "N".
             03 PART-2-CURRENT-SUM PIC S9(18) USAGE COMP-3 VALUE 0.
             03 PART-2-HISTORY OCCURS 999999.
                05 PART-2-POSITIVE PIC X(1) VALUE "N".
                05 PART-2-NEGATIVE PIC X(1) VALUE "N".
      * 
       PROCEDURE DIVISION.
       PART-1 SECTION.
          OPEN INPUT INPUT-FILE.
             PERFORM UNTIL WS-INPUT-FILE-EOF="Y"
                READ INPUT-FILE
                   AT END
                      MOVE "Y" TO WS-INPUT-FILE-EOF
                   NOT AT END
                      MOVE INPUT-VARIABLE TO TMP-VAR
                      ADD TMP-VAR TO PART-1-SUM
                END-READ
             END-PERFORM.
          CLOSE INPUT-FILE.
          DISPLAY "PART 1: " PART-1-SUM.
      *    
       PART-2 SECTION.
           MOVE "Y" TO PART-2-POSITIVE (1).
       PART-2-10.
           MOVE "N" TO WS-INPUT-FILE-EOF.
           OPEN INPUT INPUT-FILE.
             PERFORM UNTIL WS-INPUT-FILE-EOF="Y"
                READ INPUT-FILE
                   AT END
                      MOVE "Y" TO WS-INPUT-FILE-EOF
                   NOT AT END
                      MOVE INPUT-VARIABLE TO TMP-VAR
                      ADD TMP-VAR TO PART-2-CURRENT-SUM
                      PERFORM SEARCH-SUM
                      IF PART-2-FOUND = "Y" THEN
                         GO TO PART-2-99
                      END-IF
                END-READ
             END-PERFORM.
           CLOSE INPUT-FILE.
           GO TO PART-2-10.
       PART-2-99.
           CLOSE INPUT-FILE.
           DISPLAY "PART 2: " PART-2-CURRENT-SUM.
           STOP RUN.
      *
      * SIMPLE HASHSET WITH HASH FUNCTION: x -> |x| + 1
      * DOES NOT HANDLE OUT OF BOUNDS.
       SEARCH-SUM SECTION.
           COMPUTE TMP-VAR = FUNCTION ABS (PART-2-CURRENT-SUM) + 1.
           IF PART-2-CURRENT-SUM < 0 THEN
               IF PART-2-NEGATIVE (TMP-VAR) = "Y" THEN
                   MOVE "Y" TO PART-2-FOUND
               END-IF
               MOVE "Y" TO PART-2-NEGATIVE (TMP-VAR)
           ELSE
               IF PART-2-POSITIVE (TMP-VAR) = "Y" THEN
                   MOVE "Y" TO PART-2-FOUND
               END-IF
               MOVE "Y" TO PART-2-POSITIVE (TMP-VAR)
           END-IF.
           EXIT.
