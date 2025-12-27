       IDENTIFICATION DIVISION.
       PROGRAM-ID. TREEVALUE RECURSIVE.
       AUTHOR. OK999
      *
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
          01 LS-VALUE PIC 9(18) USAGE BINARY VALUE 0.
          01 LS-HEADER.
             03 LS-CHILD PIC 9(4) USAGE BINARY.
             03 LS-META PIC 9(4) USAGE BINARY.
          01 LS-CHILD-VALUES OCCURS 1000.
             03 LS-V PIC 9(18) USAGE BINARY VALUE 0.
          01 LS-TMP PIC 9(4) USAGE BINARY.
          01 LS-IDX PIC 9(4) USAGE BINARY.
       LINKAGE SECTION.
          01 L-IDX PIC 9(5) USAGE BINARY.
          01 L-TREE.
             03 L-LENGTH PIC 9(5) USAGE BINARY.
             03 L-TREE-TABLE OCCURS 20000.
                05 L-E PIC 9(4) USAGE BINARY.
          01 L-VALUE PIC 9(18) USAGE BINARY.
      *
       PROCEDURE DIVISION USING L-IDX, L-TREE, L-VALUE.
       COMPUTE-TREE-VALUE SECTION.
          IF L-IDX > L-LENGTH THEN
             MOVE 0 TO L-VALUE
             EXIT PROGRAM
          END-IF.
          MOVE L-E (L-IDX) TO LS-CHILD.
          ADD 1 TO L-IDX.
          MOVE L-E (L-IDX) TO LS-META.
          ADD 1 TO L-IDX.
          PERFORM VARYING LS-TMP FROM 1 BY 1 UNTIL LS-TMP > LS-CHILD
             CALL "TREEVALUE" USING L-IDX, L-TREE, LS-V (LS-TMP)
          END-PERFORM.
          IF LS-CHILD EQUAL 0 THEN
             PERFORM VARYING LS-TMP FROM 1 BY 1 UNTIL LS-TMP > LS-META
                ADD L-E (L-IDX) TO LS-VALUE
                ADD 1 TO L-IDX
             END-PERFORM
             MOVE LS-VALUE TO L-VALUE
             EXIT PROGRAM
          END-IF.
          PERFORM VARYING LS-TMP FROM 1 BY 1 UNTIL LS-TMP > LS-META
             IF 0 LESS THAN L-E (L-IDX) AND
                L-E (L-IDX) LESS THAN OR EQUAL LS-CHILD THEN
                   MOVE L-E (L-IDX) TO LS-IDX
                   ADD LS-V (LS-IDX) TO LS-VALUE
             END-IF 
             ADD 1 TO L-IDX
          END-PERFORM.
          MOVE LS-VALUE TO L-VALUE.
          EXIT PROGRAM.
            
       
       