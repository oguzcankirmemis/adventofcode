       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY19.
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
          77 WS-CRITICAL-SECTION PIC S9(18) USAGE COMP-5 VALUE 3.
          77 WS-CRITICAL-REGISTER PIC 9(18) USAGE COMP-5 VALUE 3.
          01 WS-INPUT-FILE-EOF PIC X(1) VALUE "N".
          01 WS-REGISTERS.
             03 WS-R PIC S9(18) USAGE COMP-5 VALUE 0 OCCURS 6.
          01 WS-INSTRUCTION.
             03 WS-OPCODE PIC X(4).
             03 WS-OP-1 PIC S9(18) USAGE COMP-5.
             03 WS-OP-2 PIC S9(18) USAGE COMP-5.
             03 WS-RES PIC S9(18) USAGE COMP-5.
          01 WS-PROGRAM.
             03 WS-IP PIC 9(18) USAGE COMP-5.
             03 WS-PROGRAM-LEN PIC 9(18) USAGE COMP-5 VALUE 0.
             03 WS-PR-INSTR OCCURS 100.
                05 WS-PR-OPCODE PIC X(4).
                05 WS-PR-OP-1 PIC S9(18) USAGE COMP-5.
                05 WS-PR-OP-2 PIC S9(18) USAGE COMP-5.
                05 WS-PR-RES PIC S9(18) USAGE COMP-5.
          01 WS-TMP.
             03 WS-TMP-STR-1 PIC X(4).
             03 WS-TMP-1 PIC 9(18).
             03 WS-TMP-2 PIC 9(18).
             03 WS-TMP-3 PIC 9(18).
             03 WS-T-1 PIC S9(18) USAGE COMP-5.
             03 WS-T-2 PIC S9(18) USAGE COMP-5.
          01 WS-PART-1-RESULT PIC S9(18) USAGE COMP-5.
          01 WS-PART-2-RESULT PIC S9(18) USAGE COMP-5.
      *
       PROCEDURE DIVISION.
       MAIN SECTION.
          PERFORM PARSE-INPUT.
          PERFORM PART-1.
          PERFORM PART-2.
          STOP RUN.
      *
       PARSE-INPUT SECTION.
          OPEN INPUT INPUT-FILE.
          READ INPUT-FILE.
          UNSTRING INPUT-STR DELIMITED BY " "
             INTO WS-TMP-STR-1, WS-TMP-1.
          COMPUTE WS-IP = WS-TMP-1 + 1.
          PERFORM UNTIL WS-INPUT-FILE-EOF = "Y"
             READ INPUT-FILE
                AT END
                   MOVE "Y" TO WS-INPUT-FILE-EOF
                NOT AT END
                   ADD 1 TO WS-PROGRAM-LEN
                   UNSTRING INPUT-STR DELIMITED BY " "
                      INTO WS-TMP-STR-1, WS-TMP-1, WS-TMP-2, WS-TMP-3
                   MOVE WS-TMP-STR-1 TO WS-PR-OPCODE (WS-PROGRAM-LEN)
                   MOVE WS-TMP-1 TO WS-PR-OP-1 (WS-PROGRAM-LEN)
                   MOVE WS-TMP-2 TO WS-PR-OP-2 (WS-PROGRAM-LEN)
                   MOVE WS-TMP-3 TO WS-PR-RES (WS-PROGRAM-LEN)
          END-PERFORM.
          CLOSE INPUT-FILE.
          EXIT.
      *
       PART-1 SECTION.
          PERFORM RUN-PROGRAM.
          MOVE WS-R (1) TO WS-PART-1-RESULT.
          DISPLAY "PART 1: " WS-PART-1-RESULT.
          EXIT.
      *
       PART-2 SECTION.
          PERFORM RUN-MODIFIED-PROGRAM.
          MOVE WS-R (WS-CRITICAL-REGISTER) TO WS-T-2.
          PERFORM VARYING WS-T-1 FROM 1 BY 1
          UNTIL WS-T-1 > WS-T-2
             IF FUNCTION MOD (WS-T-2, WS-T-1) = 0 THEN
                ADD WS-T-1 TO WS-PART-2-RESULT
             END-IF
          END-PERFORM.
          DISPLAY "PART 2: " WS-PART-2-RESULT.
          EXIT.
      *
       RUN-PROGRAM SECTION.
          MOVE 0 TO WS-R (1).
          MOVE 0 TO WS-R (2).
          MOVE 0 TO WS-R (3).
          MOVE 0 TO WS-R (4).
          MOVE 0 TO WS-R (5).
          MOVE 0 TO WS-R (6).
          PERFORM FOREVER
             COMPUTE WS-T-1 = WS-R (WS-IP) + 1
             MOVE WS-PR-OPCODE (WS-T-1) TO WS-OPCODE
             MOVE WS-PR-OP-1 (WS-T-1) TO WS-OP-1
             MOVE WS-PR-OP-2 (WS-T-1) TO WS-OP-2
             MOVE WS-PR-RES (WS-T-1) TO WS-RES
             PERFORM RUN-INSTRUCTION
             ADD 1 TO WS-R (WS-IP)
             IF WS-R (WS-IP) < 0 OR WS-R (WS-IP) >= WS-PROGRAM-LEN THEN
                SUBTRACT 1 FROM WS-R (WS-IP)
                EXIT PERFORM
             END-IF
          END-PERFORM.
          EXIT.
      *
       RUN-MODIFIED-PROGRAM SECTION.
          MOVE 1 TO WS-R (1).
          MOVE 0 TO WS-R (2).
          MOVE 0 TO WS-R (3).
          MOVE 0 TO WS-R (4).
          MOVE 0 TO WS-R (5).
          MOVE 0 TO WS-R (6).
          PERFORM UNTIL WS-R (WS-IP) = WS-CRITICAL-SECTION
             COMPUTE WS-T-1 = WS-R (WS-IP) + 1
             MOVE WS-PR-OPCODE (WS-T-1) TO WS-OPCODE
             MOVE WS-PR-OP-1 (WS-T-1) TO WS-OP-1
             MOVE WS-PR-OP-2 (WS-T-1) TO WS-OP-2
             MOVE WS-PR-RES (WS-T-1) TO WS-RES
             PERFORM RUN-INSTRUCTION
             ADD 1 TO WS-R (WS-IP)
          END-PERFORM.
          EXIT.
      *
       RUN-INSTRUCTION SECTION.
          EVALUATE WS-OPCODE
             WHEN "addr"
                PERFORM ADDR
             WHEN "addi"
                PERFORM ADDI
             WHEN "mulr"
                PERFORM MULR
             WHEN "muli"
                PERFORM MULI
             WHEN "banr"
                PERFORM BANR
             WHEN "bani"
                PERFORM BANI
             WHEN "borr"
                PERFORM BORR
             WHEN "bori"
                PERFORM BORI
             WHEN "setr"
                PERFORM SETR
             WHEN "seti"
                PERFORM SETI
             WHEN "gtir"
                PERFORM GTIR
             WHEN "gtri"
                PERFORM GTRI
             WHEN "gtrr"
                PERFORM GTRR
             WHEN "eqir"
                PERFORM EQIR
             WHEN "eqri"
                PERFORM EQRI
             WHEN "eqrr"
                PERFORM EQRR
          END-EVALUATE.
          EXIT.
      *
       ADDR SECTION.
          COMPUTE WS-R (WS-RES + 1) =
             WS-R (WS-OP-1 + 1) + WS-R (WS-OP-2 + 1).
          EXIT.
      *
       ADDI SECTION.
          COMPUTE WS-R (WS-RES + 1) =
             WS-R (WS-OP-1 + 1) + WS-OP-2.
          EXIT.
      *
       MULR SECTION.
          COMPUTE WS-R (WS-RES + 1) =
             WS-R (WS-OP-1 + 1) * WS-R (WS-OP-2 + 1).
          EXIT.
      *
       MULI SECTION.
          COMPUTE WS-R (WS-RES + 1) =
             WS-R (WS-OP-1 + 1) * WS-OP-2.
          EXIT.
      *
       BANR SECTION.
          COMPUTE WS-R (WS-RES + 1) =
             WS-R (WS-OP-1 + 1) B-AND WS-R (WS-OP-2 + 1).
          EXIT.
      *
       BANI SECTION.
          COMPUTE WS-R (WS-RES + 1) =
             WS-R (WS-OP-1 + 1) B-AND WS-OP-2.
          EXIT.
      *
       BORR SECTION.
          COMPUTE WS-R (WS-RES + 1) =
             WS-R (WS-OP-1 + 1) B-OR WS-R (WS-OP-2 + 1).
          EXIT.
      *
       BORI SECTION.
          COMPUTE WS-R (WS-RES + 1) =
             WS-R (WS-OP-1 + 1) B-OR WS-OP-2.
          EXIT.
      *
       SETR SECTION.
          MOVE WS-R (WS-OP-1 + 1) TO WS-R (WS-RES + 1).
          EXIT.
      *
       SETI SECTION.
          MOVE WS-OP-1 TO WS-R (WS-RES + 1).
          EXIT.
      *
       GTIR SECTION.
          IF WS-OP-1 > WS-R (WS-OP-2 + 1) THEN
             MOVE 1 TO WS-R (WS-RES + 1)
          ELSE
             MOVE 0 TO WS-R (WS-RES + 1)
          END-IF.
          EXIT.
      *
       GTRI SECTION.
          IF WS-R (WS-OP-1 + 1) > WS-OP-2 THEN
             MOVE 1 TO WS-R (WS-RES + 1)
          ELSE
             MOVE 0 TO WS-R (WS-RES + 1)
          END-IF.
          EXIT.
      *
       GTRR SECTION.
          IF WS-R (WS-OP-1 + 1) > WS-R (WS-OP-2 + 1) THEN
             MOVE 1 TO WS-R (WS-RES + 1)
          ELSE
             MOVE 0 TO WS-R (WS-RES + 1)
          END-IF.
          EXIT.
      *
       EQIR SECTION.
          IF WS-OP-1 = WS-R (WS-OP-2 + 1) THEN
             MOVE 1 TO WS-R (WS-RES + 1)
          ELSE
             MOVE 0 TO WS-R (WS-RES + 1)
          END-IF.
          EXIT.
      *
       EQRI SECTION.
          IF WS-R (WS-OP-1 + 1) = WS-OP-2 THEN
             MOVE 1 TO WS-R (WS-RES + 1)
          ELSE
             MOVE 0 TO WS-R (WS-RES + 1)
          END-IF.
          EXIT.
      *
       EQRR SECTION.
          IF WS-R (WS-OP-1 + 1) = WS-R (WS-OP-2 + 1) THEN
             MOVE 1 TO WS-R (WS-RES + 1)
          ELSE
             MOVE 0 TO WS-R (WS-RES + 1)
          END-IF.
          EXIT.
