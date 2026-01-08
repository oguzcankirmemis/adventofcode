       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY24.
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
             03 INPUT-STR PIC X(200).
       WORKING-STORAGE SECTION.
          01 WS-INPUT-FILE-EOF PIC X(1) VALUE "N".
          01 WS-GROUPS-LEN PIC 9(18) USAGE COMP-5 VALUE 0.
          01 WS-SORT-TABLE OCCURS 20.
             03 WS-SORT-KEY.
                05 WS-SORT-EFFECTIVE-POWER PIC 9(18) VALUE 0.
                05 WS-SORT-INITIATIVE PIC 9(2) VALUE 0.
             03 WS-SORT-KEY-STR REDEFINES WS-SORT-KEY PIC X(20).
             03 WS-GROUPS-I PIC 9(18) USAGE COMP-5.
          01 WS-INITIAL-INFECTION-COUNT PIC 9(18) USAGE COMP-5.
          01 WS-INFECTION-COUNT PIC 9(18) USAGE COMP-5 VALUE 0.
          01 WS-INITIAL-IMMUNE-SYSTEM-COUNT PIC 9(18) USAGE COMP-5.
          01 WS-IMMUNE-SYSTEM-COUNT PIC 9(18) USAGE COMP-5 VALUE 0.
          01 WS-INITIAL.
             03 FILLER OCCURS 20.
                05 FILLER PIC 9(18).
                05 FILLER PIC 9(2).
                05 FILLER PIC X(10).
                05 FILLER PIC S9(18) USAGE COMP-5.
                05 FILLER PIC S9(18) USAGE COMP-5.
                05 FILLER PIC S9(18) USAGE COMP-5.
                05 FILLER PIC X(15).
                05 FILLER PIC 9(18) USAGE COMP-5.
                05 FILLER OCCURS 5.
                   07 FILLER PIC X(15).
                05 FILLER PIC 9(18) USAGE COMP-5.
                05 FILLER OCCURS 5.
                   07 FILLER PIC X(15).
                05 FILLER PIC 9(18) USAGE COMP-5.
                05 FILLER PIC 9(18) USAGE COMP-5.
                05 FILLER PIC 9(18) USAGE COMP-5.
          01 WS-WORKING.
             03 WS-GROUPS OCCURS 20.
                05 WS-EFFECTIVE-POWER PIC 9(18).
                05 WS-INITIATIVE PIC 9(2).
                05 WS-TYPE PIC X(10).
                   88 WS-INFECTION VALUE "infection".
                   88 WS-IMMUNE-SYSTEM VALUE "immune".
                05 WS-UNITS PIC S9(18) USAGE COMP-5.
                05 WS-HITPOINTS PIC S9(18) USAGE COMP-5.
                05 WS-DAMAGE PIC S9(18) USAGE COMP-5.
                05 WS-DAMAGE-TYPE PIC X(15).
                05 WS-WEAKNESSES-LEN PIC 9(18) USAGE COMP-5 VALUE 0.
                05 WS-WEAKNESSES OCCURS 5.
                   07 WS-WEAKNESS PIC X(15).
                05 WS-IMMUNITIES-LEN PIC 9(18) USAGE COMP-5 VALUE 0.
                05 WS-IMMUNITIES OCCURS 5.
                   07 WS-IMMUNITY PIC X(15).
                05 WS-TARGET PIC 9(18) USAGE COMP-5 VALUE 0.
                05 WS-TARGET-DAMAGE PIC 9(18) USAGE COMP-5 VALUE 0.
                05 WS-ATTACKED-BY PIC 9(18) USAGE COMP-5 VALUE 0.
          01 WS-TMP.
             03 WS-T-STR-1 PIC X(200).
             03 WS-T-STR-2 PIC X(200).
             03 WS-T-1 PIC 9(18) USAGE COMP-5.
          01 WS-IDX.
             03 WS-I-1 PIC 9(18) USAGE COMP-5.
             03 WS-I-2 PIC 9(18) USAGE COMP-5.
             03 WS-I-3 PIC 9(18) USAGE COMP-5.
             03 WS-I-4 PIC 9(18) USAGE COMP-5.
          01 WS-BINARY-SEARCH.
             03 WS-LEFT PIC 9(18) USAGE COMP-5 VALUE 0.
             03 WS-MIDDLE PIC 9(18) USAGE COMP-5.
             03 WS-RIGHT PIC 9(18) USAGE COMP-5
                VALUE 999999.
             03 WS-SMALLEST-BOOST PIC 9(18) USAGE COMP-5.
          01 WS-COMBAT-DEADLOCK PIC X(1) VALUE "N".
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
          READ INPUT-FILE.
          PERFORM UNTIL INPUT-STR = SPACES
             READ INPUT-FILE
             IF NOT INPUT-STR = SPACES THEN
                ADD 1 TO WS-IMMUNE-SYSTEM-COUNT
                PERFORM PARSE-IMMUNE-SYSTEM
             END-IF
          END-PERFORM.
          READ INPUT-FILE
          PERFORM UNTIL WS-INPUT-FILE-EOF = "Y"
             READ INPUT-FILE
                AT END
                   MOVE "Y" TO WS-INPUT-FILE-EOF
                NOT AT END
                   ADD 1 TO WS-INFECTION-COUNT
                   PERFORM PARSE-INFECTION
          END-PERFORM.
          CLOSE INPUT-FILE.
          SORT WS-GROUPS DESCENDING WS-INITIATIVE.
          PERFORM VARYING WS-I-1 FROM 1 BY 1
          UNTIL WS-I-1 > WS-GROUPS-LEN
             MOVE WS-EFFECTIVE-POWER (WS-I-1)
                TO WS-SORT-EFFECTIVE-POWER (WS-I-1)
             MOVE WS-INITIATIVE (WS-I-1)
                TO WS-SORT-INITIATIVE (WS-I-1)
             MOVE WS-I-1 TO WS-GROUPS-I (WS-I-1)
          END-PERFORM.
          SORT WS-SORT-TABLE DESCENDING WS-SORT-KEY-STR.
          MOVE WS-IMMUNE-SYSTEM-COUNT TO WS-INITIAL-IMMUNE-SYSTEM-COUNT.
          MOVE WS-INFECTION-COUNT TO WS-INITIAL-INFECTION-COUNT.
          MOVE WS-WORKING TO WS-INITIAL.
          EXIT.
      *
       PARSE-IMMUNE-SYSTEM SECTION.
          ADD 1 TO WS-GROUPS-LEN.
          MOVE "immune" TO WS-TYPE (WS-GROUPS-LEN).
          PERFORM PARSE-GROUP.
          EXIT.
      *
       PARSE-INFECTION SECTION.
          ADD 1 TO WS-GROUPS-LEN.
          MOVE "infection" TO WS-TYPE (WS-GROUPS-LEN).
          PERFORM PARSE-GROUP.
          EXIT.
      *
       PARSE-GROUP SECTION.
          UNSTRING INPUT-STR DELIMITED BY " units each with "
             INTO WS-UNITS (WS-GROUPS-LEN), WS-T-STR-1.
          UNSTRING WS-T-STR-1 DELIMITED BY " hit points "
             INTO WS-HITPOINTS (WS-GROUPS-LEN), WS-T-STR-1.
          IF WS-T-STR-1 (1 : 1) = "(" THEN
             UNSTRING WS-T-STR-1 DELIMITED BY "("
                INTO WS-T-STR-2, WS-T-STR-1
             IF WS-T-STR-1 (1 : 6) = "immune" THEN
                PERFORM PARSE-IMMUNITIES
             ELSE
                PERFORM PARSE-WEAKNESSES
             END-IF
             IF WS-T-STR-1 (1 : 6) = "immune" THEN
                PERFORM PARSE-IMMUNITIES
             END-IF
             IF WS-T-STR-1 (1 : 4) = "weak" THEN
                PERFORM PARSE-WEAKNESSES
             END-IF
          END-IF.
          UNSTRING WS-T-STR-1 DELIMITED BY "that does "
             INTO WS-T-STR-2, WS-T-STR-1.
          UNSTRING WS-T-STR-1 DELIMITED BY " damage at initiative "
             INTO WS-T-STR-2, WS-INITIATIVE (WS-GROUPS-LEN)
          UNSTRING WS-T-STR-2 DELIMITED BY " "
             INTO WS-DAMAGE (WS-GROUPS-LEN),
                WS-DAMAGE-TYPE (WS-GROUPS-LEN).
          COMPUTE WS-EFFECTIVE-POWER (WS-GROUPS-LEN) =
             WS-UNITS (WS-GROUPS-LEN) * WS-DAMAGE (WS-GROUPS-LEN)
          EXIT.
      *
       PARSE-IMMUNITIES SECTION.
          UNSTRING WS-T-STR-1 DELIMITED BY "immune to "
             INTO WS-T-STR-2, WS-T-STR-1.
          MOVE 0 TO WS-T-1.
          UNSTRING WS-T-STR-1 DELIMITED BY "; "
             INTO WS-T-STR-2, WS-T-STR-1
             TALLYING IN WS-T-1.
          IF WS-T-1 = 1 THEN
             UNSTRING WS-T-STR-1 DELIMITED BY ")"
                INTO WS-T-STR-2, WS-T-STR-1
          END-IF.
          UNSTRING WS-T-STR-2 DELIMITED BY ", "
             INTO WS-IMMUNITY (WS-GROUPS-LEN, 1),
                WS-IMMUNITY (WS-GROUPS-LEN, 2),
                WS-IMMUNITY (WS-GROUPS-LEN, 3),
                WS-IMMUNITY (WS-GROUPS-LEN, 4),
                WS-IMMUNITY (WS-GROUPS-LEN, 5)
             TALLYING IN WS-IMMUNITIES-LEN (WS-GROUPS-LEN).
          EXIT.
      *
       PARSE-WEAKNESSES SECTION.
          UNSTRING WS-T-STR-1 DELIMITED BY "weak to "
             INTO WS-T-STR-2, WS-T-STR-1.
          MOVE 0 TO WS-T-1.
          UNSTRING WS-T-STR-1 DELIMITED BY "; "
             INTO WS-T-STR-2, WS-T-STR-1
             TALLYING IN WS-T-1.
          IF WS-T-1 = 1 THEN
             UNSTRING WS-T-STR-1 DELIMITED BY ")"
                INTO WS-T-STR-2, WS-T-STR-1
          END-IF.
          UNSTRING WS-T-STR-2 DELIMITED BY ", "
             INTO WS-WEAKNESS (WS-GROUPS-LEN, 1),
                WS-WEAKNESS (WS-GROUPS-LEN, 2),
                WS-WEAKNESS (WS-GROUPS-LEN, 3),
                WS-WEAKNESS (WS-GROUPS-LEN, 4),
                WS-WEAKNESS (WS-GROUPS-LEN, 5)
             TALLYING IN WS-WEAKNESSES-LEN (WS-GROUPS-LEN).
          EXIT.
      *
       PART-1 SECTION.
          PERFORM SIMULATE.
          PERFORM VARYING WS-I-1 FROM 1 BY 1
          UNTIL WS-I-1 > WS-GROUPS-LEN
             ADD WS-UNITS (WS-I-1) TO WS-PART-1-RESULT
          END-PERFORM.
          DISPLAY "PART 1: " WS-PART-1-RESULT.
          EXIT.
      *
       PART-2 SECTION.
          PERFORM RESET-GROUPS
          MOVE 23 TO WS-MIDDLE
          PERFORM SIMULATE
          PERFORM UNTIL WS-LEFT > WS-RIGHT
             COMPUTE WS-MIDDLE = WS-RIGHT - WS-LEFT
             DIVIDE WS-MIDDLE BY 2 GIVING WS-MIDDLE
             ADD WS-LEFT TO WS-MIDDLE
             PERFORM RESET-GROUPS
             PERFORM BOOST-IMMUNE-SYSTEM
             PERFORM SIMULATE
             IF WS-INFECTION-COUNT = 0 THEN
                MOVE WS-MIDDLE TO WS-SMALLEST-BOOST
                MOVE 0 TO WS-PART-2-RESULT
                PERFORM VARYING WS-I-1 FROM 1 BY 1
                UNTIL WS-I-1 > WS-GROUPS-LEN
                   ADD WS-UNITS (WS-I-1) TO WS-PART-2-RESULT
                END-PERFORM
                COMPUTE WS-RIGHT = WS-MIDDLE - 1
             ELSE
                COMPUTE WS-LEFT = WS-MIDDLE + 1
             END-IF
          END-PERFORM.
          DISPLAY "PART 2: " WS-PART-2-RESULT.
          EXIT.
      *
       BOOST-IMMUNE-SYSTEM SECTION.
          PERFORM VARYING WS-I-1 FROM 1 BY 1
          UNTIL WS-I-1 > WS-GROUPS-LEN
             IF WS-IMMUNE-SYSTEM (WS-I-1) THEN
                ADD WS-MIDDLE TO WS-DAMAGE (WS-I-1)
                COMPUTE WS-EFFECTIVE-POWER (WS-I-1) =
                   WS-UNITS (WS-I-1) * WS-DAMAGE (WS-I-1)
             END-IF
             MOVE WS-EFFECTIVE-POWER (WS-I-1)
                TO WS-SORT-EFFECTIVE-POWER (WS-I-1)
             MOVE WS-INITIATIVE (WS-I-1)
                TO WS-SORT-INITIATIVE (WS-I-1)
             MOVE WS-I-1 TO WS-GROUPS-I (WS-I-1)
          END-PERFORM.
          SORT WS-SORT-TABLE DESCENDING WS-SORT-KEY-STR.
          EXIT.
      *
       RESET-GROUPS SECTION.
          MOVE WS-INITIAL TO WS-WORKING.
          MOVE WS-INITIAL-INFECTION-COUNT TO WS-INFECTION-COUNT.
          MOVE WS-INITIAL-IMMUNE-SYSTEM-COUNT TO WS-IMMUNE-SYSTEM-COUNT.
          EXIT.
      *
       SIMULATE SECTION.
          PERFORM UNTIL WS-INFECTION-COUNT = 0 OR
             WS-IMMUNE-SYSTEM-COUNT = 0 OR
             WS-COMBAT-DEADLOCK = "Y"
                PERFORM TARGET-SELECTION
                PERFORM ATTACK
          END-PERFORM.
          EXIT.
      *
       ATTACK SECTION.
          MOVE "Y" TO WS-COMBAT-DEADLOCK.
          PERFORM VARYING WS-I-1 FROM 1 BY 1
          UNTIL WS-I-1 > WS-GROUPS-LEN
             IF WS-UNITS (WS-I-1) = 0 OR
                WS-TARGET (WS-I-1) = 0 THEN
                   CONTINUE
             ELSE
                   MOVE WS-TARGET (WS-I-1) TO WS-I-2
                   PERFORM ATTACK-TARGET
             END-IF
          END-PERFORM.
          PERFORM VARYING WS-I-1 FROM 1 BY 1
          UNTIL WS-I-1 > WS-GROUPS-LEN
             MOVE WS-GROUPS-I (WS-I-1) TO WS-I-2
             MOVE WS-EFFECTIVE-POWER (WS-I-2)
                TO WS-SORT-EFFECTIVE-POWER (WS-I-1)
             MOVE 0 TO WS-ATTACKED-BY (WS-I-2)
             MOVE 0 TO WS-TARGET (WS-I-2)
             MOVE 0 TO WS-TARGET-DAMAGE (WS-I-2)
          END-PERFORM.
          SORT WS-SORT-TABLE DESCENDING WS-SORT-KEY-STR.
          EXIT.
      *
       ATTACK-TARGET SECTION.
          IF WS-UNITS (WS-I-2) = 0 THEN
             EXIT SECTION
          END-IF.
          DIVIDE WS-TARGET-DAMAGE (WS-I-1) BY WS-HITPOINTS (WS-I-2)
             GIVING WS-T-1.
          IF WS-T-1 > 0 THEN
             MOVE "N" TO WS-COMBAT-DEADLOCK
          END-IF.
          SUBTRACT WS-T-1 FROM WS-UNITS (WS-I-2).
          IF WS-UNITS (WS-I-2) <= 0 THEN
             MOVE 0 TO WS-UNITS (WS-I-2)
             IF WS-INFECTION (WS-I-2) THEN
                SUBTRACT 1 FROM WS-INFECTION-COUNT
             ELSE
                SUBTRACT 1 FROM WS-IMMUNE-SYSTEM-COUNT
             END-IF
          END-IF.
          COMPUTE WS-EFFECTIVE-POWER (WS-I-2) =
                WS-UNITS (WS-I-2) * WS-DAMAGE (WS-I-2).
          IF NOT WS-TARGET (WS-I-2) = 0 THEN
             MOVE WS-TARGET (WS-I-2) TO WS-I-3
             PERFORM COMPUTE-DAMAGE
             MOVE WS-T-1 TO WS-TARGET-DAMAGE (WS-I-2)
          END-IF.
          EXIT.
      *
       TARGET-SELECTION SECTION.
          PERFORM VARYING WS-I-1 FROM 1 BY 1
          UNTIL WS-I-1 > WS-GROUPS-LEN
             MOVE WS-GROUPS-I (WS-I-1) TO WS-I-2
             PERFORM FIND-TARGET
          END-PERFORM.
          EXIT.
      *
       FIND-TARGET SECTION.
          PERFORM VARYING WS-I-3 FROM 1 BY 1
          UNTIL WS-I-3 > WS-GROUPS-LEN
             IF WS-TYPE (WS-I-2) NOT EQUAL TO WS-TYPE (WS-I-3) AND
                WS-UNITS (WS-I-3) NOT EQUAL TO 0 AND
                WS-ATTACKED-BY (WS-I-3) EQUAL TO 0 THEN
                   PERFORM COMPUTE-DAMAGE
                   PERFORM CHECK-TARGET
             END-IF 
          END-PERFORM.
          IF WS-TARGET (WS-I-2) NOT EQUAL TO 0 THEN
             MOVE WS-TARGET (WS-I-2) TO WS-I-3
             MOVE WS-I-2 TO WS-ATTACKED-BY (WS-I-3)
          END-IF.
          EXIT.
      *
       CHECK-TARGET SECTION.
          IF WS-T-1 EQUAL TO 0 THEN
             EXIT SECTION
          END-IF.
          IF WS-TARGET (WS-I-2) = 0 THEN
             MOVE WS-I-3 TO WS-TARGET (WS-I-2)
             MOVE WS-T-1 TO WS-TARGET-DAMAGE (WS-I-2)
             EXIT SECTION
          END-IF.
          IF WS-T-1 < WS-TARGET-DAMAGE (WS-I-2) THEN
             EXIT SECTION
          END-IF.
          IF WS-T-1 = WS-TARGET-DAMAGE (WS-I-2) THEN
             MOVE WS-TARGET (WS-I-2) TO WS-I-4
             IF WS-EFFECTIVE-POWER (WS-I-3) <
                WS-EFFECTIVE-POWER (WS-I-4) THEN
                   EXIT SECTION
             END-IF
             IF WS-EFFECTIVE-POWER (WS-I-3) =
                WS-EFFECTIVE-POWER (WS-I-4) THEN
                   IF WS-INITIATIVE (WS-I-3) <
                      WS-INITIATIVE (WS-I-4) THEN
                         EXIT SECTION
                   END-IF
             END-IF
          END-IF.
          MOVE WS-I-3 TO WS-TARGET (WS-I-2).
          MOVE WS-T-1 TO WS-TARGET-DAMAGE (WS-I-2).
          EXIT.
      *
       COMPUTE-DAMAGE SECTION.
          PERFORM VARYING WS-I-4 FROM 1 BY 1
          UNTIL WS-I-4 > WS-IMMUNITIES-LEN (WS-I-3)
             IF WS-DAMAGE-TYPE (WS-I-2) = WS-IMMUNITY (WS-I-3, WS-I-4)
             THEN
                MOVE 0 TO WS-T-1
                EXIT SECTION
             END-IF
          END-PERFORM.
          PERFORM VARYING WS-I-4 FROM 1 BY 1
          UNTIL WS-I-4 > WS-WEAKNESSES-LEN (WS-I-3)
             IF WS-DAMAGE-TYPE (WS-I-2) = WS-WEAKNESS (WS-I-3, WS-I-4)
             THEN
                COMPUTE WS-T-1 = 2 * WS-EFFECTIVE-POWER (WS-I-2)
                EXIT SECTION
             END-IF
          END-PERFORM.
          MOVE WS-EFFECTIVE-POWER (WS-I-2) TO WS-T-1
          EXIT.
