C RANDOM TESTING
      INCLUDE 'MINICRT.FI'
      INTEGER*2 I
      CALL INITCRT
      DO 40 I=0,108
      CALL PLAYNUM(I)
      CALL DELAY(2000)
40    WRITE(6,*)I
      CALL NOSOUND
      END
