       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY16.
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
             03 INPUT-STR PIC X(40).
       WORKING-STORAGE SECTION.
          77 WS-ADDR PIC 9(18) USAGE COMP-5 VALUE 1.
          77 WS-ADDI PIC 9(18) USAGE COMP-5 VALUE 2.
          77 WS-MULR PIC 9(18) USAGE COMP-5 VALUE 3.
          77 WS-MULI PIC 9(18) USAGE COMP-5 VALUE 4.
          77 WS-BANR PIC 9(18) USAGE COMP-5 VALUE 5.
          77 WS-BANI PIC 9(18) USAGE COMP-5 VALUE 6.
          77 WS-BORR PIC 9(18) USAGE COMP-5 VALUE 7.
          77 WS-BORI PIC 9(18) USAGE COMP-5 VALUE 8.
          77 WS-SETR PIC 9(18) USAGE COMP-5 VALUE 9.
          77 WS-SETI PIC 9(18) USAGE COMP-5 VALUE 10.
          77 WS-GTIR PIC 9(18) USAGE COMP-5 VALUE 11.
          77 WS-GTRI PIC 9(18) USAGE COMP-5 VALUE 12.
          77 WS-GTRR PIC 9(18) USAGE COMP-5 VALUE 13.
          77 WS-EQIR PIC 9(18) USAGE COMP-5 VALUE 14.
          77 WS-EQRI PIC 9(18) USAGE COMP-5 VALUE 15.
          77 WS-EQRR PIC 9(18) USAGE COMP-5 VALUE 16.
          77 WS-INSTRUCTIONS PIC 9(18) USAGE COMP-5 VALUE 16.
          77 WS-PART-1-THRESHOLD PIC 9(18) USAGE COMP-5 VALUE 2.
          01 WS-TMP.
             03 WS-TMP-STR-1 PIC X(40).
             03 WS-TMP-STR-2 PIC X(40).
             03 WS-TMP-1 PIC 9(18).
             03 WS-TMP-2 PIC 9(18).
             03 WS-TMP-3 PIC 9(18).
             03 WS-TMP-4 PIC 9(18).
          01 WS-INPUT-FILE-EOF PIC X(1) VALUE "N".
          01 WS-PUZZLE-LEN PIC 9(18) USAGE COMP-5 VALUE 0.
          01 WS-PUZZLE OCCURS 4000.
             03 WS-BEFORE.
                05 WS-B-REG-1 PIC S9(18) USAGE COMP-5 VALUE 0.
                05 WS-B-REG-2 PIC S9(18) USAGE COMP-5 VALUE 0.
                05 WS-B-REG-3 PIC S9(18) USAGE COMP-5 VALUE 0.
                05 WS-B-REG-4 PIC S9(18) USAGE COMP-5 VALUE 0.
             03 WS-PZ-INSTR.
                05 WS-PZ-OPCODE PIC S9(18) USAGE COMP-5 VALUE 0.
                05 WS-PZ-OP-1 PIC S9(18) USAGE COMP-5 VALUE 0.
                05 WS-PZ-OP-2 PIC S9(18) USAGE COMP-5 VALUE 0.
                05 WS-PZ-RES PIC S9(18) USAGE COMP-5 VALUE 0.
             03 WS-AFTER.
                05 WS-A-REG-1 PIC S9(18) USAGE COMP-5 VALUE 0.
                05 WS-A-REG-2 PIC S9(18) USAGE COMP-5 VALUE 0.
                05 WS-A-REG-3 PIC S9(18) USAGE COMP-5 VALUE 0.
                05 WS-A-REG-4 PIC S9(18) USAGE COMP-5 VALUE 0.
          01 WS-I-1 PIC 9(18) USAGE COMP-5.
          01 WS-I-2 PIC 9(18) USAGE COMP-5.
          01 WS-I-3 PIC 9(18) USAGE COMP-5.
          01 WS-REGISTERS.
             03 WS-R-1 PIC S9(18) USAGE COMP-5 VALUE 0.
             03 WS-R-2 PIC S9(18) USAGE COMP-5 VALUE 0.
             03 WS-R-3 PIC S9(18) USAGE COMP-5 VALUE 0.
             03 WS-R-4 PIC S9(18) USAGE COMP-5 VALUE 0.
          01 WS-REGISTERS-ARR REDEFINES WS-REGISTERS.
             03 WS-R PIC S9(18) USAGE COMP-5 OCCURS 4.
          01 WS-INSTRUCTION.
             03 WS-OPCODE PIC S9(18) USAGE COMP-5 VALUE 0.
             03 WS-OP-1 PIC S9(18) USAGE COMP-5 VALUE 0.
             03 WS-OP-2 PIC S9(18) USAGE COMP-5 VALUE 0.
             03 WS-RES PIC S9(18) USAGE COMP-5 VALUE 0.
          01 WS-SOLVED PIC X(1) VALUE "N".
          01 WS-OPCODE-TABLE OCCURS 16.
             03 WS-SPECULATIONS PIC X(1) OCCURS 16 VALUE "Y".
             03 WS-SPECULATIONS-COUNT PIC 9(18) USAGE COMP-5 VALUE 16.
             03 WS-OPCODE-MAP PIC 9(18) USAGE COMP-5 VALUE 0.
          01 WS-PROGRAM.
             03 WS-PROGRAM-LEN PIC 9(18) USAGE COMP-5 VALUE 0.
             03 WS-PR-INSTR OCCURS 4000.
                05 WS-PR-OPCODE PIC S9(18) USAGE COMP-5 VALUE 0.
                05 WS-PR-OP-1 PIC S9(18) USAGE COMP-5 VALUE 0.
                05 WS-PR-OP-2 PIC S9(18) USAGE COMP-5 VALUE 0.
                05 WS-PR-RES PIC S9(18) USAGE COMP-5 VALUE 0.
          01 WS-PART-1-RESULT PIC 9(18) USAGE COMP-5 VALUE 0.
          01 WS-PART-2-RESULT PIC 9(18) USAGE COMP-5 VALUE 0.
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
          PERFORM UNTIL WS-INPUT-FILE-EOF = "Y"
             READ INPUT-FILE
                AT END
                   MOVE "Y" TO WS-INPUT-FILE-EOF
                NOT AT END
                   IF INPUT-STR (1 : 7) = "Before:" THEN
                      ADD 1 TO WS-PUZZLE-LEN
                      PERFORM PARSE-PUZZLE-BEFORE
                      READ INPUT-FILE
                      PERFORM PARSE-PUZZLE-INSTRUCTION
                      READ INPUT-FILE
                      PERFORM PARSE-PUZZLE-AFTER
                   ELSE
                      IF NOT INPUT-STR (1 : 1) = " " THEN
                         ADD 1 TO WS-PROGRAM-LEN
                         PERFORM PARSE-INSTRUCTION
                      END-IF
                   END-IF
             END-READ
          END-PERFORM.
          CLOSE INPUT-FILE.
          EXIT.
      *
       PARSE-PUZZLE-BEFORE SECTION.
          UNSTRING INPUT-STR DELIMITED BY "[" 
             INTO WS-TMP-STR-1, WS-TMP-STR-2.
          UNSTRING WS-TMP-STR-2 DELIMITED BY "]" INTO WS-TMP-STR-1.
          UNSTRING WS-TMP-STR-1 DELIMITED BY ", "
             INTO WS-TMP-1, WS-TMP-2, WS-TMP-3, WS-TMP-4.
          MOVE WS-TMP-1 TO WS-B-REG-1 (WS-PUZZLE-LEN).
          MOVE WS-TMP-2 TO WS-B-REG-2 (WS-PUZZLE-LEN).
          MOVE WS-TMP-3 TO WS-B-REG-3 (WS-PUZZLE-LEN).
          MOVE WS-TMP-4 TO WS-B-REG-4 (WS-PUZZLE-LEN).
          EXIT.
      *
       PARSE-PUZZLE-INSTRUCTION SECTION.
          UNSTRING INPUT-STR DELIMITED BY ALL " "
             INTO WS-TMP-1, WS-TMP-2, WS-TMP-3, WS-TMP-4.
          MOVE WS-TMP-1 TO WS-PZ-OPCODE (WS-PUZZLE-LEN).
          MOVE WS-TMP-2 TO WS-PZ-OP-1 (WS-PUZZLE-LEN).
          MOVE WS-TMP-3 TO WS-PZ-OP-2 (WS-PUZZLE-LEN).
          MOVE WS-TMP-4 TO WS-PZ-RES (WS-PUZZLE-LEN).
          EXIT.
      *
       PARSE-PUZZLE-AFTER SECTION.
          UNSTRING INPUT-STR DELIMITED BY "[" 
             INTO WS-TMP-STR-1, WS-TMP-STR-2.
          UNSTRING WS-TMP-STR-2 DELIMITED BY "]" INTO WS-TMP-STR-1.
          UNSTRING WS-TMP-STR-1 DELIMITED BY ", "
             INTO WS-TMP-1, WS-TMP-2, WS-TMP-3, WS-TMP-4.
          MOVE WS-TMP-1 TO WS-A-REG-1 (WS-PUZZLE-LEN).
          MOVE WS-TMP-2 TO WS-A-REG-2 (WS-PUZZLE-LEN).
          MOVE WS-TMP-3 TO WS-A-REG-3 (WS-PUZZLE-LEN).
          MOVE WS-TMP-4 TO WS-A-REG-4 (WS-PUZZLE-LEN).
          EXIT.
      *
       PARSE-INSTRUCTION SECTION.
          UNSTRING INPUT-STR DELIMITED BY ALL " "
             INTO WS-TMP-1, WS-TMP-2, WS-TMP-3, WS-TMP-4.
          MOVE WS-TMP-1 TO WS-PR-OPCODE (WS-PROGRAM-LEN).
          MOVE WS-TMP-2 TO WS-PR-OP-1 (WS-PROGRAM-LEN).
          MOVE WS-TMP-3 TO WS-PR-OP-2 (WS-PROGRAM-LEN).
          MOVE WS-TMP-4 TO WS-PR-RES (WS-PROGRAM-LEN).
          EXIT.
      *
       PART-1 SECTION.
          PERFORM VARYING WS-I-1 FROM 1 BY 1
          UNTIL WS-I-1 > WS-PUZZLE-LEN
             MOVE 0 TO WS-I-3
             MOVE WS-PZ-INSTR (WS-I-1) TO WS-INSTRUCTION
             PERFORM VARYING WS-I-2 FROM 1 BY 1
             UNTIL WS-I-2 > WS-INSTRUCTIONS
                PERFORM RESET-REGISTERS
                EVALUATE WS-I-2
                   WHEN WS-ADDR
                      PERFORM ADDR
                   WHEN WS-ADDI
                      PERFORM ADDI
                   WHEN WS-MULR
                      PERFORM MULR
                   WHEN WS-MULI
                      PERFORM MULI
                   WHEN WS-BANR
                      PERFORM BANR
                   WHEN WS-BANI
                      PERFORM BANI
                   WHEN WS-BORR
                      PERFORM BORR
                   WHEN WS-BORI
                      PERFORM BORI
                   WHEN WS-SETR
                      PERFORM SETR
                   WHEN WS-SETI
                      PERFORM SETI
                   WHEN WS-GTIR
                      PERFORM GTIR
                   WHEN WS-GTRI
                      PERFORM GTRI
                   WHEN WS-GTRR
                      PERFORM GTRR
                   WHEN WS-EQIR
                      PERFORM EQIR
                   WHEN WS-EQRI
                      PERFORM EQRI
                   WHEN WS-EQRR
                      PERFORM EQRR
                END-EVALUATE
                PERFORM CHECK-REGISTERS
             END-PERFORM
             IF WS-I-3 > WS-PART-1-THRESHOLD THEN
                ADD 1 TO WS-PART-1-RESULT
             END-IF
          END-PERFORM.
          DISPLAY "PART 1: " WS-PART-1-RESULT.
          EXIT.
      *
       PART-2 SECTION.
          PERFORM UNTIL WS-SOLVED = "Y"
             PERFORM VARYING WS-I-1 FROM 1 BY 1
             UNTIL WS-I-1 > WS-INSTRUCTIONS
                IF WS-SPECULATIONS-COUNT (WS-I-1) > 1 THEN
                   PERFORM CHECK-SPECULATIONS
                END-IF
             END-PERFORM
             PERFORM CHECK-SOLVED
          END-PERFORM.
          PERFORM VARYING WS-I-1 FROM 1 BY 1
          UNTIL WS-I-1 > WS-INSTRUCTIONS
             PERFORM VARYING WS-I-2 FROM 1 BY 1
             UNTIL WS-I-2 > WS-INSTRUCTIONS
                IF WS-SPECULATIONS (WS-I-1, WS-I-2) = "Y" THEN
                   MOVE WS-I-2 TO WS-OPCODE-MAP (WS-I-1)
                END-IF
             END-PERFORM
          END-PERFORM
          MOVE 0 TO WS-R-1.
          MOVE 0 TO WS-R-2.
          MOVE 0 TO WS-R-3.
          MOVE 0 TO WS-R-4.
          PERFORM VARYING WS-I-1 FROM 1 BY 1
          UNTIL WS-I-1 > WS-PROGRAM-LEN
             MOVE WS-PR-INSTR (WS-I-1) TO WS-INSTRUCTION
             EVALUATE WS-OPCODE-MAP (WS-OPCODE + 1)
                WHEN WS-ADDR
                   PERFORM ADDR
                WHEN WS-ADDI
                   PERFORM ADDI
                WHEN WS-MULR
                   PERFORM MULR
                WHEN WS-MULI
                   PERFORM MULI
                WHEN WS-BANR
                   PERFORM BANR
                WHEN WS-BANI
                   PERFORM BANI
                WHEN WS-BORR
                   PERFORM BORR
                WHEN WS-BORI
                   PERFORM BORI
                WHEN WS-SETR
                   PERFORM SETR
                WHEN WS-SETI
                   PERFORM SETI
                WHEN WS-GTIR
                   PERFORM GTIR
                WHEN WS-GTRI
                   PERFORM GTRI
                WHEN WS-GTRR
                   PERFORM GTRR
                WHEN WS-EQIR
                   PERFORM EQIR
                WHEN WS-EQRI
                   PERFORM EQRI
                WHEN WS-EQRR
                   PERFORM EQRR
             END-EVALUATE
          END-PERFORM.
          MOVE WS-R-1 TO WS-PART-2-RESULT.
          DISPLAY "PART 2: " WS-PART-2-RESULT.
          EXIT.
      *
       CHECK-SPECULATIONS SECTION.
          PERFORM VARYING WS-I-2 FROM 1 BY 1
          UNTIL WS-I-2 > WS-INSTRUCTIONS
             IF WS-SPECULATIONS (WS-I-1, WS-I-2) = "Y" THEN
                PERFORM VARYING WS-I-3 FROM 1 BY 1
                UNTIL WS-I-3 > WS-INSTRUCTIONS
                   IF WS-I-3 NOT EQUAL TO WS-I-1 AND 
                      WS-SPECULATIONS-COUNT (WS-I-3) = 1 AND
                      WS-SPECULATIONS (WS-I-3, WS-I-2) = "Y" THEN
                         MOVE "N" TO WS-SPECULATIONS (WS-I-1, WS-I-2)
                         SUBTRACT 1 FROM WS-SPECULATIONS-COUNT (WS-I-1)
                         EXIT PERFORM
                   END-IF
                END-PERFORM
             END-IF
          END-PERFORM.
          EXIT.
      *
       CHECK-SOLVED SECTION.
          MOVE "Y" TO WS-SOLVED
          PERFORM VARYING WS-I-1 FROM 1 BY 1
          UNTIL WS-I-1 > WS-INSTRUCTIONS
             IF NOT WS-SPECULATIONS-COUNT (WS-I-1) = 1 THEN
                MOVE "N" TO WS-SOLVED
             END-IF
          END-PERFORM.
          EXIT.
      *
       RESET-REGISTERS SECTION.
          MOVE WS-B-REG-1 (WS-I-1) TO WS-R-1.
          MOVE WS-B-REG-2 (WS-I-1) TO WS-R-2.
          MOVE WS-B-REG-3 (WS-I-1) TO WS-R-3.
          MOVE WS-B-REG-4 (WS-I-1) TO WS-R-4.
          EXIT.
      *
       CHECK-REGISTERS SECTION.
          IF WS-A-REG-1 (WS-I-1) = WS-R-1 AND
             WS-A-REG-2 (WS-I-1) = WS-R-2 AND
             WS-A-REG-3 (WS-I-1) = WS-R-3 AND
             WS-A-REG-4 (WS-I-1) = WS-R-4 THEN
                ADD 1 TO WS-I-3
          ELSE
                IF WS-SPECULATIONS
                   (WS-PZ-OPCODE (WS-I-1) + 1, WS-I-2) = "Y" THEN
                      MOVE "N" TO WS-SPECULATIONS
                         (WS-PZ-OPCODE (WS-I-1) + 1, WS-I-2)
                      SUBTRACT 1 FROM WS-SPECULATIONS-COUNT
                         (WS-PZ-OPCODE (WS-I-1) + 1)
                END-IF
          END-IF.
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


