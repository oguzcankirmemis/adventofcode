       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY5.
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
             03 INPUT-STR PIC X(60000).
       WORKING-STORAGE SECTION.
          01 WS-INPUT-FILE-EOF PIC A(1) VALUE "N".
          01 WS-INPUT.
             03 WS-INPUT-TABLE OCCURS 60000.
                05 WS-INPUT-CHAR PIC X(1).
          01 WS-INPUT-STR REDEFINES WS-INPUT PIC X(60000).
          01 WS-TMP.
             03 WS-TMP-TABLE OCCURS 60000.
                05 WS-TMP-CHAR PIC X(1).
          01 WS-TMP-STR REDEFINES WS-TMP PIC X(60000).
          01 WS-MID-RESULT.
             03 WS-MID-RESULT-TABLE OCCURS 60000.
                05 WS-MID-RESULT-CHAR PIC X(1).
          01 WS-LENGTH PIC 9(5).
          01 WS-MIN-LENGTH PIC 9(5) VALUE 99999.
          01 WS-IDX-1 PIC 9(5).
          01 WS-IDX-2 PIC 9(5).
          01 WS-COUNTER PIC 9(5).
          01 WS-CHAR-1 PIC X(1).
          01 WS-CHAR-2 PIC X(1).
          01 WS-STATE PIC X(1) VALUE "N".
      *
       PROCEDURE DIVISION.
       PART-1 SECTION.
          OPEN INPUT INPUT-FILE.
          PERFORM UNTIL WS-INPUT-FILE-EOF = "Y"
             READ INPUT-FILE
                AT END
                   MOVE "Y" TO WS-INPUT-FILE-EOF
                NOT AT END
                   MOVE INPUT-STR TO WS-INPUT-STR
          END-PERFORM.
          CLOSE INPUT-FILE.
          PERFORM FOREVER
             PERFORM REACT
             IF WS-STATE = "Y" THEN
                EXIT PERFORM
             END-IF
          END-PERFORM.
          DISPLAY "PART 1: " WS-LENGTH.
      *
       PART-2 SECTION.
          MOVE WS-INPUT TO WS-MID-RESULT.
          PERFORM VARYING WS-COUNTER FROM 1 BY 1
          UNTIL WS-COUNTER > 26
             MOVE FUNCTION CHAR (WS-COUNTER + 96) TO WS-CHAR-1
             PERFORM REMOVE-CHAR
             PERFORM FOREVER
                PERFORM REACT
                IF WS-STATE = "Y" THEN
                   EXIT PERFORM
                END-IF
             END-PERFORM
             IF WS-LENGTH < WS-MIN-LENGTH THEN
                MOVE WS-LENGTH TO WS-MIN-LENGTH
             END-IF
          END-PERFORM.
          DISPLAY "PART 2: " WS-MIN-LENGTH.
          STOP RUN.
      *
       REACT SECTION.
          INITIALIZE WS-TMP.
          MOVE "Y" TO WS-STATE.
          MOVE 0 TO WS-IDX-2.
          PERFORM VARYING WS-IDX-1 FROM 1 BY 1
          UNTIL WS-INPUT-CHAR (WS-IDX-1) = " "
             MOVE WS-INPUT-CHAR (WS-IDX-1) TO WS-CHAR-1
             MOVE WS-INPUT-CHAR (WS-IDX-1 + 1) TO WS-CHAR-2
             IF WS-CHAR-1 NOT EQUAL WS-CHAR-2 AND
                FUNCTION UPPER-CASE (WS-CHAR-1) = 
                FUNCTION UPPER-CASE (WS-CHAR-2) THEN
                   ADD 1 TO WS-IDX-1
                   MOVE "N" TO WS-STATE
             ELSE
                ADD 1 TO WS-IDX-2
                MOVE WS-CHAR-1 TO WS-TMP-CHAR (WS-IDX-2)
             END-IF
          END-PERFORM.
          MOVE WS-TMP TO WS-INPUT.
          MOVE WS-IDX-2 TO WS-LENGTH.
          EXIT.
      *
       REMOVE-CHAR SECTION.
          INITIALIZE WS-INPUT.
          MOVE 0 TO WS-IDX-2.
          PERFORM VARYING WS-IDX-1 FROM 1 BY 1
          UNTIL WS-MID-RESULT-CHAR (WS-IDX-1) = " "
             MOVE WS-MID-RESULT-CHAR (WS-IDX-1) TO WS-CHAR-2
             IF NOT WS-CHAR-1 = FUNCTION LOWER-CASE (WS-CHAR-2) THEN
                ADD 1 TO WS-IDX-2
                MOVE WS-CHAR-2 TO WS-INPUT-CHAR (WS-IDX-2)
             END-IF
          END-PERFORM.
          EXIT.

        
       