       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY8.
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
             03 INPUT-STR PIC X(40000).
       WORKING-STORAGE SECTION.
          01 WS-INPUT-FILE-EOF PIC X(1) VALUE "N".
          01 WS-POINTER PIC 9(5) USAGE BINARY VALUE 1.
          01 WS-IDX PIC 9(5) USAGE BINARY VALUE 1.
          01 WS-TREE.
             03 WS-LENGTH PIC 9(5) USAGE BINARY VALUE 0.
             03 WS-TREE-TABLE OCCURS 20000.
                05 WS-E PIC 9(4) USAGE BINARY VALUE 9999.
          01 WS-TMP-1 PIC 9(4) VALUE 0.
          01 WS-PART-1-RESULT PIC 9(18) USAGE BINARY VALUE 0.
          01 WS-PART-2-RESULT PIC 9(18) USAGE BINARY VALUE 0.
      *
       PROCEDURE DIVISION.
       PARSE-INPUT SECTION.
          OPEN INPUT INPUT-FILE.
          PERFORM UNTIL WS-INPUT-FILE-EOF = "Y"
             READ INPUT-FILE
                AT END
                   MOVE "Y" TO WS-INPUT-FILE-EOF
                NOT AT END
                   PERFORM UNTIL WS-POINTER > LENGTH OF INPUT-STR
                      UNSTRING INPUT-STR 
                         DELIMITED BY ALL " " 
                         INTO WS-TMP-1
                         WITH POINTER WS-POINTER
                      ADD 1 TO WS-LENGTH
                      MOVE WS-TMP-1 TO WS-E (WS-LENGTH)
                   END-PERFORM
             END-READ
          END-PERFORM.
          CLOSE INPUT-FILE.
          PERFORM PART-1.
          PERFORM PART-2.
          STOP RUN.
      *
       PART-1 SECTION.
          MOVE 1 TO WS-IDX.
          CALL "METASUM" USING WS-IDX, WS-TREE
             RETURNING WS-PART-1-RESULT.
          DISPLAY "PART 1: " WS-PART-1-RESULT.
          EXIT.
      *
       PART-2 SECTION.
          MOVE 1 TO WS-IDX.
          CALL "TREEVALUE" USING WS-IDX, WS-TREE, WS-PART-2-RESULT.
          DISPLAY "PART 2: " WS-PART-2-RESULT.
          EXIT.
