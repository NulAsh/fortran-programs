C     Compile with:  FL TESTCRT.FOR MINICRT.OBJ
      INCLUDE 'MINICRT.FI'
      PROGRAM MAIN
      INTEGER*1 RDK_INT
      CALL INITCRT()
      CALL SOUND(440)
      CALL DELAY(1000)
      CALL NOSOUND()
      I = 0
      DO 10 WHILE (I.NE.27)
      I = RDK_INT()
10    WRITE(6,*) CHAR(I), I
      END
