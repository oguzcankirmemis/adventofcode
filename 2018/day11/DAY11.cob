       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY11.
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
             03 INPUT-SERIAL-NUMBER-STR PIC X(20).
       WORKING-STORAGE SECTION.
          77 WS-ROWS USAGE BINARY PIC 9(18) VALUE 300.
          77 WS-COLS USAGE BINARY PIC 9(18) VALUE 300.
          77 WS-SIZES USAGE BINARY PIC 9(18) VALUE 300.
          01 WS-INPUT-FILE-EOF PIC X(1) VALUE "N".
          01 WS-TMP-1 PIC S9(18).
          01 WS-TMP-2 REDEFINES WS-TMP-1.
             03 FILLER PIC 9(15).
             03 WS-HUNDREDS-DIGIT PIC 9(1).
             03 FILLER PIC 9(2).
          01 WS-SERIAL-NUMBER USAGE BINARY PIC S9(18).
          01 WS-ROW USAGE BINARY PIC 9(18).
          01 WS-COL USAGE BINARY PIC 9(18).
          01 WS-SIZE USAGE BINARY PIC 9(18).
          01 WS-FUEL-ROW USAGE BINARY PIC 9(18).
          01 WS-FUEL-COL USAGE BINARY PIC 9(18).
          01 WS-TOTAL-POWER USAGE BINARY PIC S9(18).
          01 WS-TABLE.
             03 WS-R OCCURS 301.
                05 WS-C OCCURS 301.
                   07 WS-S OCCURS 301.
                      10 WS-P USAGE BINARY PIC S9(18)
                         VALUE -999999999999999999.
                         88 WS-INVALID VALUE -999999999999999999.
          01 WS-FUEL-CELL.
             03 WS-RACK-ID PIC S9(18) USAGE BINARY.
             03 WS-POWER-LEVEL PIC S9(18) USAGE BINARY.
          01 WS-PART-1-RESULT.
             03 WS-MAX-POWER-1 PIC S9(18) USAGE BINARY
                VALUE -999999999999999999.
             03 WS-X-1 PIC 9(18).
             03 WS-Y-1 PIC 9(18).
          01 WS-PART-2-RESULT.
             03 WS-MAX-POWER-2 PIC S9(18) USAGE BINARY
                VALUE -999999999999999999.
             03 WS-X-2 USAGE BINARY PIC 9(18).
             03 WS-Y-2 USAGE BINARY PIC 9(18).
             03 WS-L USAGE BINARY PIC 9(18).
      *
       PROCEDURE DIVISION.
       MAIN SECTION.
          PERFORM PARSE-INPUT.
          PERFORM COMPUTE-GRID.
          PERFORM COMPUTE-DYNAMIC.
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
                   MOVE INPUT-SERIAL-NUMBER-STR TO WS-TMP-1
                   MOVE WS-TMP-1 TO WS-SERIAL-NUMBER
             END-READ
          END-PERFORM.
          CLOSE INPUT-FILE.
          EXIT.
      *
       PART-1 SECTION.
          DISPLAY "PART 1: " WS-X-1 "," WS-Y-1.
          EXIT.
      *
       PART-2 SECTION.
          DISPLAY "PART 2: " WS-X-2 "," WS-Y-2 "," WS-L
          EXIT.
      *
       COMPUTE-DYNAMIC SECTION.
          PERFORM VARYING WS-ROW FROM WS-ROWS BY -1
          UNTIL WS-ROW < 1
             PERFORM VARYING WS-COL FROM WS-COLS BY -1
             UNTIL WS-COL < 1
                MOVE WS-P (WS-ROW, WS-COL, 1) TO WS-TMP-1
                PERFORM VARYING WS-SIZE FROM 1 BY 1
                UNTIL WS-SIZE > WS-SIZES
                   IF WS-INVALID (WS-ROW + 1, WS-COL + 1, WS-SIZE) THEN
                      EXIT PERFORM
                   END-IF
                   ADD WS-P (WS-ROW + WS-SIZE, WS-COL, 1) TO WS-TMP-1
                   ADD WS-P (WS-ROW, WS-COL + WS-SIZE, 1) TO WS-TMP-1
                   COMPUTE WS-P (WS-ROW, WS-COL, WS-SIZE + 1) =
                      WS-P (WS-ROW + 1, WS-COL + 1, WS-SIZE) + WS-TMP-1
                   IF WS-SIZE = 2 AND
                      WS-P (WS-ROW, WS-COL, WS-SIZE + 1) >
                      WS-MAX-POWER-1 THEN
                         MOVE WS-P (WS-ROW, WS-COL, WS-SIZE + 1)
                            TO WS-MAX-POWER-1
                         MOVE WS-ROW TO WS-Y-1
                         MOVE WS-COL TO WS-X-1
                   END-IF
                   IF WS-P (WS-ROW, WS-COL, WS-SIZE + 1) >
                      WS-MAX-POWER-2 THEN
                         MOVE WS-P (WS-ROW, WS-COL, WS-SIZE + 1)
                            TO WS-MAX-POWER-2
                         MOVE WS-ROW TO WS-Y-2
                         MOVE WS-COL TO WS-X-2
                         COMPUTE WS-L = WS-SIZE + 1
                   END-IF
                END-PERFORM 
             END-PERFORM
          END-PERFORM.
     *
       COMPUTE-GRID SECTION.
          PERFORM VARYING WS-ROW FROM 1 BY 1
          UNTIL WS-ROW > WS-ROWS
             PERFORM VARYING WS-COL FROM 1 BY 1
             UNTIL WS-COL > WS-COLS
                PERFORM COMPUTE-FUEL-CELL
                MOVE WS-POWER-LEVEL TO WS-P (WS-ROW, WS-COL, 1)
                IF WS-POWER-LEVEL > WS-MAX-POWER-2 THEN
                   MOVE WS-POWER-LEVEL TO WS-MAX-POWER-2
                   MOVE WS-ROW TO WS-Y-2
                   MOVE WS-COL TO WS-X-2
                   MOVE 1 TO WS-L
                END-IF
             END-PERFORM
          END-PERFORM.
          EXIT.
      *
       COMPUTE-FUEL-CELL SECTION.
          COMPUTE WS-RACK-ID = WS-COL + 10.
          COMPUTE WS-POWER-LEVEL = WS-RACK-ID * WS-ROW.
          ADD WS-SERIAL-NUMBER TO WS-POWER-LEVEL.
          MULTIPLY WS-RACK-ID BY WS-POWER-LEVEL.
          MOVE WS-POWER-LEVEL TO WS-TMP-1.
          MOVE WS-HUNDREDS-DIGIT TO WS-POWER-LEVEL.
          SUBTRACT 5 FROM WS-POWER-LEVEL.
          EXIT.
       