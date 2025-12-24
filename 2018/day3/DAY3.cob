       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY3.
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
             03 INPUT-STR PIC X(30).
       WORKING-STORAGE SECTION.
          01 WS-INPUT-FILE-EOF PIC A(1) VALUE "N".
          01 WS-TMP1 PIC X(30).
          01 WS-TMP2 PIC X(30).
          01 WS-IDX-ROW PIC 9(4) USAGE BINARY.
          01 WS-IDX-COL PIC 9(4) USAGE BINARY.
          01 WS-IDX-1 PIC 9(4) USAGE BINARY.
          01 WS-IDX-2 PIC 9(4) USAGE BINARY.
          01 WS-TABLE.
             03 WS-ROW OCCURS 1000 TIMES.
                05 WS-COL OCCURS 1000 TIMES.
                   07 WS-COUNT PIC 9(4) USAGE BINARY VALUE 0.
          01 WS-CLAIM.
             03 CLAIM-ID.
                05 HEADER PIC X(1).
                05 ID-NUM PIC 9(4).
             03 LEFT-PAD-STR PIC 9(4).
             03 LEFT-PAD PIC 9(4) USAGE BINARY.
             03 TOP-PAD-STR PIC 9(4).
             03 TOP-PAD PIC 9(4) USAGE BINARY.
             03 WIDTH-STR PIC 9(4).
             03 WIDTH PIC 9(4) USAGE BINARY.
             03 HEIGHT-STR PIC 9(4).
             03 HEIGHT PIC 9(4) USAGE BINARY.
          01 PART-1-RESULT PIC 9(18) USAGE BINARY VALUE 0.
          01 PART-2-RESULT PIC 9(4) VALUE 0.
      *       
       PROCEDURE DIVISION.
       PART-1 SECTION.
          OPEN INPUT INPUT-FILE.
             PERFORM UNTIL WS-INPUT-FILE-EOF = "Y"
                READ INPUT-FILE
                   AT END
                      MOVE "Y" TO WS-INPUT-FILE-EOF
                   NOT AT END
                      PERFORM PARSE-INPUT
                      PERFORM COUNT-CELLS
             END-PERFORM.
          CLOSE INPUT-FILE.
          DISPLAY "PART 1: " PART-1-RESULT.
      *
       PART-2 SECTION.
          MOVE "N" TO WS-INPUT-FILE-EOF.
          OPEN INPUT INPUT-FILE.
             PERFORM UNTIL PART-2-RESULT NOT = 0
                READ INPUT-FILE
                   AT END
                      MOVE "Y" TO WS-INPUT-FILE-EOF
                   NOT AT END
                      PERFORM PARSE-INPUT
                      PERFORM CHECK-CLAIM
             END-PERFORM
          CLOSE INPUT-FILE.
          DISPLAY "PART 2: " PART-2-RESULT.
          STOP RUN.
      *
       PARSE-INPUT SECTION.
          UNSTRING INPUT-STR
             DELIMITED BY " @ "
             INTO WS-TMP1 WS-TMP2.
          MOVE WS-TMP1 TO CLAIM-ID.
          UNSTRING WS-TMP2
             DELIMITED BY ": "
             INTO WS-TMP1 WS-TMP2.
          UNSTRING WS-TMP1
             DELIMITED BY ","
             INTO LEFT-PAD-STR TOP-PAD-STR.
          UNSTRING WS-TMP2
             DELIMITED BY "x"
             INTO WIDTH-STR HEIGHT-STR.
          MOVE LEFT-PAD-STR TO LEFT-PAD.
          MOVE TOP-PAD-STR TO TOP-PAD.
          MOVE WIDTH-STR TO WIDTH.
          MOVE HEIGHT-STR TO HEIGHT.
          EXIT.
      *
       CHECK-CLAIM SECTION.
          MOVE ID-NUM TO PART-2-RESULT.
          COMPUTE WS-IDX-ROW = TOP-PAD + 1.
          COMPUTE WS-IDX-COL = LEFT-PAD + 1.
          PERFORM VARYING WS-IDX-1 FROM 0 BY 1
          UNTIL WS-IDX-1 = HEIGHT
             COMPUTE WS-IDX-ROW = TOP-PAD + 1 + WS-IDX-1
             PERFORM VARYING WS-IDX-2 FROM 0 BY 1
             UNTIL WS-IDX-2 = WIDTH 
                COMPUTE WS-IDX-COL = LEFT-PAD + 1 + WS-IDX-2
                IF NOT WS-COUNT (WS-IDX-ROW, WS-IDX-COl) = 1 THEN
                   MOVE 0 TO PART-2-RESULT
                END-IF
             END-PERFORM
          END-PERFORM.
          EXIT.
      *
       COUNT-CELLS SECTION.
          COMPUTE WS-IDX-ROW = TOP-PAD + 1.
          COMPUTE WS-IDX-COL = LEFT-PAD + 1.
          PERFORM VARYING WS-IDX-1 FROM 0 BY 1
          UNTIL WS-IDX-1 = HEIGHT
             COMPUTE WS-IDX-ROW = TOP-PAD + 1 + WS-IDX-1
             PERFORM VARYING WS-IDX-2 FROM 0 BY 1
             UNTIL WS-IDX-2 = WIDTH 
                COMPUTE WS-IDX-COL = LEFT-PAD + 1 + WS-IDX-2
                ADD 1 TO WS-COUNT (WS-IDX-ROW, WS-IDX-COL)
                IF WS-COUNT (WS-IDX-ROW, WS-IDX-COl) = 2 THEN
                   ADD 1 TO PART-1-RESULT
                END-IF
             END-PERFORM
          END-PERFORM.
          EXIT.

        

       