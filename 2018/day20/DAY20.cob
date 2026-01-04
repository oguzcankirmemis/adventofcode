       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY20.
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
             03 INPUT-STR PIC X(15000).
       WORKING-STORAGE SECTION.
          77 WS-PART-2-THRESHOLD PIC 9(18) USAGE COMP-5 VALUE 1000.
          77 WS-DFS-CACHE-LEN PIC 9(18) USAGE COMP-5 VALUE 10.
          77 WS-P-WALL PIC X(1) VALUE "#".
          77 WS-P-H-DOOR PIC X(1) VALUE "-".
          77 WS-P-V-DOOR PIC X(1) VALUE "|".
          77 WS-P-ROOM PIC X(1) VALUE ".".
          77 WS-P-VISIT PIC X(1) VALUE "X".
          77 WS-S-R PIC 9(18) USAGE COMP-5 VALUE 500.
          77 WS-S-C PIC 9(18) USAGE COMP-5 VALUE 500.
          01 WS-INPUT-FILE-EOF PIC X(1) VALUE "N".
          01 WS-REGEXP.
             03 WS-R PIC X(1) OCCURS 15000.
                88 WS-WEST VALUE "W".
                88 WS-EAST VALUE "E".
                88 WS-SOUTH VALUE "S".
                88 WS-NORTH VALUE "N".
                88 WS-END VALUE "$".
                88 WS-START VALUE "^".
                88 WS-LEFT-P VALUE "(".
                88 WS-RIGHT-P VALUE ")".
                88 WS-PIPE VALUE "|".
          01 WS-REGEXP-STR REDEFINES WS-REGEXP PIC X(15000).
          01 WS-MAP.
             03 WS-R OCCURS 1000.
                05 WS-C.
                   07 WS-M PIC X(1) VALUE "#" OCCURS 1000.
                      88 WS-WALL VALUE "#".
                      88 WS-H-DOOR VALUE "-".
                      88 WS-V-DOOR VALUE "|".
                      88 WS-ROOM VALUE ".".
                      88 WS-EXPLORED VALUE "X".
                05 WS-C-STR REDEFINES WS-C PIC X(1000).
          01 WS-DFS-EXPLORED-ROW OCCURS 1000.
             03 WS-DFS-EXPLORED-COL OCCURS 1000.
                05 WS-D-E-L PIC 9(18) USAGE COMP-5 VALUE 0.
                05 WS-D-E-I PIC 9(18) USAGE COMP-5 OCCURS 100.
          01 WS-DFS-EXPLORED PIC X(1) VALUE "N".
          01 WS-PARANTHESIS OCCURS 15000.
             03 WS-PARA-END PIC 9(18) USAGE COMP-5 VALUE 0.
             03 WS-PIPES-LEN PIC 9(18) USAGE COMP-5 VALUE 0.
             03 WS-PIPES PIC 9(18) USAGE COMP-5 OCCURS 2000.
          01 WS-PARA-STACK-LEN PIC 9(18) USAGE COMP-5 VALUE 0.
          01 WS-PARA-STACK PIC 9(18) USAGE COMP-5 OCCURS 2000.
          01 WS-I-1 PIC 9(18) USAGE COMP-5.
          01 WS-I-2 PIC 9(18) USAGE COMP-5.
          01 WS-I-3 PIC 9(18) USAGE COMP-5.
          01 WS-I-4 PIC 9(18) USAGE COMP-5.
          01 WS-I-5 PIC 9(18) USAGE COMP-5.
          01 WS-STACK-LEN PIC 9(18) USAGE COMP-5 VALUE 0.
          01 WS-STACK OCCURS 15000.
             03 WS-D-R PIC 9(18) USAGE COMP-5.
             03 WS-D-C PIC 9(18) USAGE COMP-5.
             03 WS-D-I PIC 9(18) USAGE COMP-5.
          01 WS-QUEUE-HEAD PIC 9(18) USAGE COMP-5 VALUE 0.
          01 WS-QUEUE-TAIL PIC 9(18) USAGE COMP-5 VALUE 0.
          01 WS-QUEUE OCCURS 15000.
             03 WS-Q-R PIC 9(18) USAGE COMP-5.
             03 WS-Q-C PIC 9(18) USAGE COMP-5.
             03 WS-Q-D PIC 9(18) USAGE COMP-5.
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
          MOVE INPUT-STR TO WS-REGEXP-STR.
          CLOSE INPUT-FILE.
          MOVE WS-P-ROOM TO WS-M (WS-S-R, WS-S-C).
          EXIT.
      *
       PART-1 SECTION.
          PERFORM PROCESS-REGEXP.
          PERFORM DFS.
          PERFORM BFS.
          DISPLAY "PART 1: " WS-PART-1-RESULT.
          EXIT.
      *
       PART-2 SECTION.
          DISPLAY "PART 2: " WS-PART-2-RESULT.
          EXIT.
      *
       PROCESS-REGEXP SECTION.
          PERFORM VARYING WS-I-1 FROM 1 BY 1
          UNTIL WS-END (WS-I-1)
             IF WS-LEFT-P (WS-I-1) THEN
                ADD 1 TO WS-PARA-STACK-LEN
                MOVE WS-I-1 TO WS-PARA-STACK (WS-PARA-STACK-LEN)
                ADD 1 TO WS-PIPES-LEN (WS-I-1)
                MOVE WS-PIPES-LEN (WS-I-1) TO WS-I-2
                COMPUTE WS-PIPES (WS-I-1, WS-I-2) = WS-I-1 + 1
             END-IF
             IF WS-RIGHT-P (WS-I-1) THEN
                MOVE WS-PARA-STACK (WS-PARA-STACK-LEN) TO WS-I-2
                MOVE WS-PIPES-LEN (WS-I-2) TO WS-I-3
                PERFORM VARYING WS-I-3 FROM 2 BY 1
                UNTIL WS-I-3 > WS-PIPES-LEN (WS-I-2)
                   COMPUTE WS-I-4 = WS-PIPES (WS-I-2, WS-I-3) - 1
                   COMPUTE WS-PARA-END (WS-I-4) = WS-I-1 + 1
                END-PERFORM
                COMPUTE WS-PARA-END (WS-PARA-STACK-LEN) = WS-I-1 + 1
                SUBTRACT 1 FROM WS-PARA-STACK-LEN
             END-IF
             IF WS-PIPE (WS-I-1) THEN
                MOVE WS-PARA-STACK (WS-PARA-STACK-LEN) TO WS-I-2
                ADD 1 TO WS-PIPES-LEN (WS-I-2)
                MOVE WS-PIPES-LEN (WS-I-2) TO WS-I-3
                COMPUTE WS-PIPES (WS-I-2, WS-I-3) = WS-I-1 + 1
             END-IF
          END-PERFORM.
          EXIT.
      *
       DFS SECTION.
          ADD 1 TO WS-STACK-LEN.
          MOVE WS-S-R TO WS-D-R (WS-STACK-LEN).
          MOVE WS-S-C TO WS-D-C (WS-STACK-LEN).
          MOVE 1 TO WS-D-I (WS-STACK-LEN).
          PERFORM UNTIL WS-STACK-LEN = 0
             MOVE WS-D-R (WS-STACK-LEN) TO WS-I-1
             MOVE WS-D-C (WS-STACK-LEN) TO WS-I-2
             MOVE WS-D-I (WS-STACK-LEN) TO WS-I-3
             SUBTRACT 1 FROM WS-STACK-LEN
             PERFORM CHECK-EXPLORED
             IF WS-DFS-EXPLORED = "N" THEN
                PERFORM MARK-EXPLORED
                PERFORM CHECK-START
                PERFORM CHECK-CHAR
                PERFORM CHECK-LEFT-PARA
                PERFORM CHECK-PIPE
                PERFORM CHECK-RIGHT-PARA
                PERFORM CHECK-END             
             END-IF
          END-PERFORM.
          EXIT.
      *
       CHECK-EXPLORED SECTION.             
          MOVE "N" TO WS-DFS-EXPLORED
          IF NOT WS-WEST (WS-I-3) AND
             NOT WS-EAST (WS-I-3) AND
             NOT WS-SOUTH (WS-I-3) AND
             NOT WS-NORTH (WS-I-3) THEN
                EXIT SECTION
          END-IF.
          PERFORM VARYING WS-I-4 FROM 1 BY 1
          UNTIL WS-I-4 > WS-D-E-L (WS-I-1, WS-I-2)
             IF WS-D-E-I (WS-I-1, WS-I-2, WS-I-4) = WS-I-3 THEN                
                MOVE "Y" TO WS-DFS-EXPLORED
                EXIT SECTION
             END-IF
          END-PERFORM.
          EXIT.
       MARK-EXPLORED SECTION.
          IF NOT WS-WEST (WS-I-3) AND
             NOT WS-EAST (WS-I-3) AND
             NOT WS-SOUTH (WS-I-3) AND
             NOT WS-NORTH (WS-I-3) THEN
                EXIT SECTION
          END-IF.
          ADD 1 TO WS-D-E-L (WS-I-1, WS-I-2)
          MOVE WS-D-E-L (WS-I-1, WS-I-2) TO WS-I-4
          IF WS-I-4 <= WS-DFS-CACHE-LEN THEN
             MOVE WS-I-3 TO WS-D-E-I (WS-I-1, WS-I-2, WS-I-4)
          ELSE
             DISPLAY "WARNING: DFS CACHE LIMIT REACHED"
          END-IF.
          EXIT.
      *
       CHECK-START SECTION.
          IF NOT WS-START (WS-I-3) THEN
             EXIT SECTION
          END-IF.
          COMPUTE WS-I-4 = WS-I-3 + 1.
          ADD 1 TO WS-STACK-LEN.
          MOVE WS-I-1 TO WS-D-R (WS-STACK-LEN).
          MOVE WS-I-2 TO WS-D-C (WS-STACK-LEN).
          MOVE WS-I-4 TO WS-D-I (WS-STACK-LEN).
          EXIT.
      *
       CHECK-CHAR SECTION.
          IF WS-WEST (WS-I-3) THEN
             MOVE WS-P-V-DOOR TO WS-M (WS-I-1, WS-I-2 - 1)
             MOVE WS-P-ROOM TO WS-M (WS-I-1, WS-I-2 - 2)
             SUBTRACT 2 FROM WS-I-2
          ELSE IF WS-EAST (WS-I-3) THEN
             MOVE WS-P-V-DOOR TO WS-M (WS-I-1, WS-I-2 + 1)
             MOVE WS-P-ROOM TO WS-M (WS-I-1, WS-I-2 + 2)
             ADD 2 TO WS-I-2
          ELSE IF WS-SOUTH (WS-I-3) THEN
             MOVE WS-P-H-DOOR TO WS-M (WS-I-1 + 1, WS-I-2)
             MOVE WS-P-ROOM TO WS-M (WS-I-1 + 2, WS-I-2)
             ADD 2 TO WS-I-1
          ELSE IF WS-NORTH (WS-I-3) THEN
             MOVE WS-P-H-DOOR TO WS-M (WS-I-1 - 1, WS-I-2)
             MOVE WS-P-ROOM TO WS-M (WS-I-1 - 2, WS-I-2)
             SUBTRACT 2 FROM WS-I-1
          ELSE
             EXIT SECTION
          END-IF.
          ADD 1 TO WS-STACK-LEN.
          COMPUTE WS-I-4 = WS-I-3 + 1.
          MOVE WS-I-1 TO WS-D-R (WS-STACK-LEN).
          MOVE WS-I-2 TO WS-D-C (WS-STACK-LEN).
          MOVE WS-I-4 TO WS-D-I (WS-STACK-LEN).
          EXIT.
      *
       CHECK-PIPE SECTION.
          IF NOT WS-PIPE (WS-I-3) THEN
             EXIT SECTION
          END-IF.
          MOVE WS-PARA-END (WS-I-3) TO WS-I-4.
          ADD 1 TO WS-STACK-LEN.
          MOVE WS-I-1 TO WS-D-R (WS-STACK-LEN).
          MOVE WS-I-2 TO WS-D-C (WS-STACK-LEN).
          MOVE WS-I-4 TO WS-D-I (WS-STACK-LEN).
          EXIT.
      *
       CHECK-LEFT-PARA SECTION.
          IF NOT WS-LEFT-P (WS-I-3) THEN
             EXIT SECTION
          END-IF.
          PERFORM VARYING WS-I-4 FROM 1 BY 1
          UNTIL WS-I-4 > WS-PIPES-LEN (WS-I-3)
             MOVE WS-PIPES (WS-I-3, WS-I-4) TO WS-I-5
             ADD 1 TO WS-STACK-LEN
             MOVE WS-I-1 TO WS-D-R (WS-STACK-LEN)
             MOVE WS-I-2 TO WS-D-C (WS-STACK-LEN)
             MOVE WS-I-5 TO WS-D-I (WS-STACK-LEN)
          END-PERFORM.
          EXIT.
      *
       CHECK-RIGHT-PARA SECTION.
          IF NOT WS-RIGHT-P (WS-I-3) THEN
             EXIT SECTION
          END-IF.
          COMPUTE WS-I-4 = WS-I-3 + 1.
          ADD 1 TO WS-STACK-LEN.
          MOVE WS-I-1 TO WS-D-R (WS-STACK-LEN).
          MOVE WS-I-2 TO WS-D-C (WS-STACK-LEN).
          MOVE WS-I-4 TO WS-D-I (WS-STACK-LEN).
          EXIT.
      *
       CHECK-END SECTION.
          IF NOT WS-END (WS-I-3) THEN
             EXIT SECTION
          END-IF.
          EXIT.
      *
       BFS SECTION.
          MOVE 1 TO WS-QUEUE-HEAD.
          MOVE 1 TO WS-QUEUE-TAIL.
          MOVE WS-S-R TO WS-Q-R (WS-QUEUE-TAIL).
          MOVE WS-S-C TO WS-Q-C (WS-QUEUE-TAIL).
          MOVE 0 TO WS-Q-D (WS-QUEUE-TAIL).
          MOVE WS-P-VISIT TO WS-M (WS-S-R, WS-S-C).
          PERFORM UNTIL WS-QUEUE-HEAD > WS-QUEUE-TAIL
             MOVE WS-Q-R (WS-QUEUE-HEAD) TO WS-I-1
             MOVE WS-Q-C (WS-QUEUE-HEAD) TO WS-I-2
             MOVE WS-Q-D (WS-QUEUE-HEAD) TO WS-I-3
             ADD 1 TO WS-QUEUE-HEAD
             IF WS-I-3 > WS-PART-1-RESULT THEN
                MOVE WS-I-3 TO WS-PART-1-RESULT
             END-IF
             IF WS-I-3 >= WS-PART-2-THRESHOLD THEN
                ADD 1 TO WS-PART-2-RESULT
             END-IF
             IF WS-H-DOOR (WS-I-1 - 1, WS-I-2) AND
                NOT WS-EXPLORED (WS-I-1 - 2, WS-I-2) THEN
                   COMPUTE WS-I-4 = WS-I-1 - 2
                   MOVE WS-I-2 TO WS-I-5
                   PERFORM BFS-ENQUEUE
             END-IF
             IF WS-H-DOOR (WS-I-1 + 1, WS-I-2) AND
                NOT WS-EXPLORED (WS-I-1 + 2, WS-I-2) THEN
                   COMPUTE WS-I-4 = WS-I-1 + 2
                   MOVE WS-I-2 TO WS-I-5
                   PERFORM BFS-ENQUEUE
             END-IF
             IF WS-V-DOOR (WS-I-1, WS-I-2 - 1) AND
                NOT WS-EXPLORED (WS-I-1, WS-I-2 - 2) THEN
                   MOVE WS-I-1 TO WS-I-4
                   COMPUTE WS-I-5 = WS-I-2 - 2
                   PERFORM BFS-ENQUEUE
             END-IF
             IF WS-V-DOOR (WS-I-1, WS-I-2 + 1) AND
                NOT WS-EXPLORED (WS-I-1, WS-I-2 + 2) THEN
                   MOVE WS-I-1 TO WS-I-4
                   COMPUTE WS-I-5 = WS-I-2 + 2
                   PERFORM BFS-ENQUEUE
             END-IF
          END-PERFORM.
          EXIT.
      *
       BFS-ENQUEUE SECTION.
          MOVE WS-P-VISIT TO WS-M (WS-I-4, WS-I-5).
          ADD 1 TO WS-QUEUE-TAIL.
          MOVE WS-I-4 TO WS-Q-R (WS-QUEUE-TAIL).
          MOVE WS-I-5 TO WS-Q-C (WS-QUEUE-TAIL).
          COMPUTE WS-Q-D (WS-QUEUE-TAIL) = WS-I-3 + 1.
          EXIT.
