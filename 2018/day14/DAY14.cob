       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY14.
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
             03 INPUT-OFFSET PIC 9(6).
             03 INPUT-OFFSET-STR REDEFINES INPUT-OFFSET PIC X(6).
       WORKING-STORAGE SECTION.
          01 WS-SCOREBOARD.
             03 WS-NUMS PIC 9(1) OCCURS 100000000.
          01 WS-NUMS-STR REDEFINES WS-SCOREBOARD PIC X(1000000).
          01 WS-LEN PIC 9(18) USAGE BINARY VALUE 0.
          01 WS-P1 PIC 9(18) USAGE BINARY VALUE 0.
          01 WS-P2 PIC 9(18) USAGE BINARY VALUE 0.
          01 WS-TMP PIC 9(2).
          01 WS-TMP-DIGITS REDEFINES WS-TMP.
             03 WS-TEN-DIGIT PIC 9(1).
             03 WS-ONE-DIGIT PIC 9(1).
          01 WS-PART-1-RESULT PIC 9(10).
          01 WS-PART-2-RESULT PIC 9(10).
      *
       PROCEDURE DIVISION.
       MAIN SECTION.
          PERFORM PARSE-INPUT.
          PERFORM RESET-SCOREBOARD.
          PERFORM PART-1.
          PERFORM RESET-SCOREBOARD.
          PERFORM PART-2.
          STOP RUN.
      *
       PARSE-INPUT SECTION.
          OPEN INPUT INPUT-FILE.
          READ INPUT-FILE.
          CLOSE INPUT-FILE.
          EXIT.
      *
       PART-1 SECTION.
          PERFORM UNTIL WS-LEN >= INPUT-OFFSET + 10
             PERFORM ADD-RECIPE
          END-PERFORM.
          MOVE WS-NUMS-STR (INPUT-OFFSET + 1 : 10) TO WS-PART-1-RESULT.
          DISPLAY "PART 1: " WS-PART-1-RESULT.
          EXIT.
      *
       PART-2 SECTION.
          PERFORM FOREVER
             IF WS-LEN > 5 AND
                WS-NUMS-STR (WS-LEN - 5 : 6) = INPUT-OFFSET-STR  
                   EXIT PERFORM
             END-IF
             IF WS-LEN > 6 AND
                WS-NUMS-STR (WS-LEN - 6 : 6) = INPUT-OFFSET-STR
                   EXIT PERFORM
             END-IF
             PERFORM ADD-RECIPE
          END-PERFORM.
          IF WS-NUMS-STR (WS-LEN - 5 : 6) = INPUT-OFFSET THEN
             COMPUTE WS-PART-2-RESULT = WS-LEN - 6
          ELSE
             COMPUTE WS-PARt-2-RESULT = WS-LEN - 7
          END-IF.
          DISPLAY "PART 2: " WS-PART-2-RESULT.
          EXIT.
      *
       ADD-RECIPE SECTION.
          COMPUTE WS-TMP = WS-NUMS (WS-P1) + WS-NUMS (WS-P2).
          IF WS-TEN-DIGIT NOT = 0 THEN
             ADD 1 TO WS-LEN
             MOVE WS-TEN-DIGIT TO WS-NUMS (WS-LEN)
          END-IF.
          ADD 1 TO WS-LEN.
          MOVE WS-ONE-DIGIT TO WS-NUMS (WS-LEN).
          COMPUTE WS-P1 = WS-P1 + WS-NUMS (WS-P1).
          COMPUTE WS-P1 = FUNCTION MOD (WS-P1 WS-LEN) + 1.
          COMPUTE WS-P2 = WS-P2 + WS-NUMS (WS-P2).
          COMPUTE WS-P2 = FUNCTION MOD (WS-P2 WS-LEN) + 1.
          EXIT.
      *
       RESET-SCOREBOARD SECTION.
          MOVE 3 TO WS-NUMS (1).
          MOVE 7 TO WS-NUMS (2).
          MOVE 1 TO WS-P1.
          MOVE 2 TO WS-P2.
          MOVE 2 TO WS-LEN.
