      Program Lab2
      common /base/ minX, maxX, minY, maxY, step, K
1     REAL minX, maxX, minY, maxY, step
      REAL K=3.14159265359/180
      PRINT *, 'Enter minX, maxX, minY, maxY, step'
      READ *, minX, maxX, minY, maxY, step
      call Calculations()
      READ *, a
      END


      SUBROUTINE Calculations()
      common /base/ minX, maxX, minY, maxY, step, K
      REAL minX, maxX, minY, maxY, step
      PRINT *, 'Calcs'
      OPEN(1, FILE='assets/source.txt')
      WRITE(1,5) ' y\x'
      DO x=minX, maxX, step
        IF(x.GT.maxX) THEN
        WRITE(1,3) maxX
        EXIT
        END IF
      WRITE(1,3) x
      END DO
      WRITE(1, *) ' '
      DO y=minY, maxY, step
        WRITE(1,6) y
        DO x=minX, maxX, step
          IF(x.GT.maxX .OR. y.GT.maxY) THEN
            IF(y.LT.maxY)THEN
              WRITE(1,2) 1/TAN(ABS(maxX + y))
              EXIT
            ELSE
              WRITE(1,2) 1/TAN(ABS(x + maxY))
              EXIT
            END IF
            IF(x.GT.maxX .AND. y.GT.maxY)THEN
              WRITE(1,2) 1/TAN(ABS(maxX + maxY))
              EXIT
            END IF
          END IF
          WRITE(1,2) 1/TAN(ABS(x+y))
        END DO
        IF(y.GT.maxY) THEN
        WRITE(1,3) maxY
        END IF
        WRITE(1, *) ' '
      END DO
2     FORMAT(' |', F5.1, $)
3     FORMAT('  ', F5.1, $)
6     FORMAT('', F5.1, $)
5     FORMAT(' ', A, $)
4     FORMAT(' |', F5.1, $)
      CLOSE(1)
      END
