       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY4.
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
          01 INPUT-FILE-RECORD PIC X(50).
      *
       WORKING-STORAGE SECTION.
          01 WS-INPUT-FILE-EOF PIC X(1) VALUE "N".
          01 WS-LOG-TABLE OCCURS 10000.
             03 WS-TIME-STR PIC X(19) VALUE "[9999-99-99 99:99] ".
             03 WS-TIME REDEFINES WS-TIME-STR.
                05 FILLER PIC X(1).
                05 WS-YEAR PIC 9(4).
                05 FILLER PIC X(1).
                05 WS-MONTH PIC 9(2).
                05 FILLER PIC X(1).
                05 WS-DAY PIC 9(2).
                05 FILLER PIC X(1).
                05 WS-HOUR PIC 9(2).
                05 FILLER PIC X(1).
                05 WS-MINUTE PIC 9(2).
                05 FILLER PIC X(2).
             03 WS-TYPE PIC X(1).
                88 WS-BEGIN VALUE 'G'.
                88 WS-ASLEEP VALUE 'f'.
                88 WS-AWAKE VALUE 'w'.
             03 FILLER PIC X(6).
             03 WS-STATEMENT PIC X(20).
             03 WS-GUARD-ID PIC 9(4).
          01 WS-TABLE-LENGTH PIC 9(5) VALUE 0.
          01 WS-GUARD-TABLE OCCURS 10000.
             03 WS-GUARD-ASLEEP PIC 9(18) USAGE BINARY VALUE 0.
          01 WS-TIME-TABLE OCCURS 60.
             03 WS-GUARD-TIME-TABLE OCCURS 10000.
                05 WS-TIME-TABLE-GUARD-ID PIC 9(4).
                05 WS-ASLEEP-COUNT PIC 9(18) USAGE BINARY VALUE 0.
          01 WS-IDX PIC 9(4) VALUE 1.
          01 WS-TMP-1 PIC 9(4) VALUE 0.
          01 WS-TMP-2 PIC 9(4) VALUE 0.
          01 WS-TMP-3 PIC 9(4) VALUE 0.
          01 PART-1-RESULT.
             03 PART-1-GUARD PIC 9(4) VALUE 1.
             03 PART-1-MINUTE PIC 9(4) VALUE 1.
             03 PART-1-STRATEGY-RESULT PIC 9(18).
          01 PART-2-RESULT.
             03 PART-2-GUARD PIC 9(4) VALUE 1.
             03 PART-2-MINUTE PIC 9(4) VALUE 1.
             03 PART-2-STRATEGY-RESULT PIC 9(18).
      *
       PROCEDURE DIVISION.
       INPUT-PARSE SECTION.
          OPEN INPUT INPUT-FILE.
             PERFORM UNTIL WS-INPUT-FILE-EOF = "Y"
                READ INPUT-FILE
                   AT END
                      MOVE "Y" TO WS-INPUT-FILE-EOF
                   NOT AT END
                      ADD 1 TO WS-TABLE-LENGTH
                      MOVE INPUT-FILE-RECORD 
                         TO WS-LOG-TABLE (WS-TABLE-LENGTH)
             END-PERFORM.
          CLOSE INPUT-FILE.
          SORT WS-LOG-TABLE ASCENDING KEY WS-TIME-STR.
          PERFORM VARYING WS-IDX FROM 1 BY 1
          UNTIL WS-IDX > WS-TABLE-LENGTH
             IF WS-BEGIN (WS-IDX) THEN
                UNSTRING WS-STATEMENT (WS-IDX)
                   DELIMITED BY " "
                   INTO WS-GUARD-ID (WS-IDX)
             ELSE
                MOVE WS-GUARD-ID (WS-IDX - 1) TO WS-GUARD-ID (WS-IDX)
             END-IF
          END-PERFORM.
          PERFORM PART-1.
          PERFORM PART-2.
          STOP RUN.
      *
       PART-1 SECTION.
          PERFORM VARYING WS-IDX FROM 1 BY 1
          UNTIL WS-IDX > WS-TABLE-LENGTH
             IF WS-ASLEEP (WS-IDX) THEN
                MOVE WS-IDX TO WS-TMP-1
             END-IF
             IF WS-AWAKE (WS-IDX) THEN
                MOVE WS-GUARD-ID (WS-IDX) TO WS-TMP-2
                COMPUTE WS-GUARD-ASLEEP (WS-TMP-2) =
                   WS-GUARD-ASLEEP (WS-TMP-2) +
                   WS-MINUTE (WS-IDX) - WS-MINUTE (WS-TMP-1)
                MOVE 0 TO WS-TMP-1
             END-IF
             IF WS-BEGIN (WS-IDX) AND WS-TMP-1 NOT = 0 THEN
                MOVE WS-GUARD-ID (WS-IDX) TO WS-TMP-2
                COMPUTE WS-GUARD-ASLEEP (WS-TMP-2) = 
                   WS-GUARD-ASLEEP (WS-TMP-2) +
                   60 - WS-MINUTE (WS-TMP-1)
                MOVE 0 TO WS-TMP-1
             END-IF
          END-PERFORM.
          PERFORM VARYING WS-IDX FROM 1 BY 1
          UNTIL WS-IDX > WS-TABLE-LENGTH
             MOVE WS-GUARD-ID (WS-IDX) TO WS-TMP-1
             IF WS-GUARD-ASLEEP (WS-TMP-1) 
                > WS-GUARD-ASLEEP (PART-1-GUARD) THEN
                   MOVE WS-TMP-1 TO PART-1-GUARD
             END-IF
          END-PERFORM.
          PERFORM VARYING WS-IDX FROM 1 BY 1
          UNTIL WS-IDX > WS-TABLE-LENGTH
             IF WS-ASLEEP (WS-IDX) THEN
                PERFORM COMPUTE-ASLEEP
             END-IF
          END-PERFORM.
          PERFORM VARYING WS-IDX FROM 1 BY 1
          UNTIL WS-IDX > 60
             IF WS-ASLEEP-COUNT (WS-IDX, PART-1-GUARD)
                > WS-ASLEEP-COUNT (PART-1-MINUTE, PART-1-GUARD) THEN
                   MOVE WS-IDX TO PART-1-MINUTE
          END-PERFORM.
          COMPUTE PART-1-STRATEGY-RESULT = 
             PART-1-GUARD * (PART-1-MINUTE - 1).
          DISPLAY "PART 1: " PART-1-STRATEGY-RESULT.
          EXIT.
      * 
       COMPUTE-ASLEEP SECTION.
          MOVE WS-MINUTE (WS-IDX) TO WS-TMP-1.
          IF WS-IDX = WS-TABLE-LENGTH OR WS-BEGIN (WS-IDX + 1) THEN
             MOVE 60 TO WS-TMP-2
          ELSE IF WS-AWAKE (WS-IDX + 1) THEN
             MOVE WS-MINUTE (WS-IDX + 1) TO WS-TMP-2
          END-IF.
          PERFORM VARYING WS-TMP-1 FROM WS-TMP-1 BY 1
          UNTIL WS-TMP-1 = WS-TMP-2
             MOVE WS-GUARD-ID (WS-IDX) TO WS-TMP-3
             MOVE WS-TMP-3
                TO WS-TIME-TABLE-GUARD-ID (WS-TMP-1 + 1, WS-TMP-3)
             ADD 1 TO WS-ASLEEP-COUNT (WS-TMP-1 + 1, WS-TMP-3)
          END-PERFORM.
          EXIT.
      * 
       PART-2 SECTION.
          PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 60
             SORT WS-GUARD-TIME-TABLE (WS-IDX)
                DESCENDING KEY WS-ASLEEP-COUNT
             IF WS-ASLEEP-COUNT (WS-IDX, 1) >
                WS-ASLEEP-COUNT (WS-IDX, 2) AND 
                WS-ASLEEP-COUNT (WS-IDX, 1) >
                WS-ASLEEP-COUNT (PART-2-MINUTE, 1) THEN
                   MOVE WS-TIME-TABLE-GUARD-ID (WS-IDX, 1) 
                      TO PART-2-GUARD
                   MOVE WS-IDX TO PART-2-MINUTE
             END-IF 
          END-PERFORM.
          COMPUTE PART-2-STRATEGY-RESULT =
             PART-2-GUARD * (PART-2-MINUTE - 1).
          DISPLAY "PART 2: " PART-2-STRATEGY-RESULT.
          EXIT.

