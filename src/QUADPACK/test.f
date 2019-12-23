       PROGRAM TEST
       REAL A,ABSERR,B,F,EPSABS,EPSREL,RESULT
       INTEGER IER,NEVAL
       EXTERNAL F
       A = 0.0E0
       B = 1.0E0
       EPSABS = 0.0E0
       EPSREL = 1.0E-3
       CALL QNG(F,A,B,EPSABS,EPSREL,RESULT,ABSERR,NEVAL,IER)
       write(6,1) result
 1     format(' result=',1pe15.7)
       STOP
       END
       REAL FUNCTION F(X)
       REAL X
       F = EXP(X)/(X*X+0.1E+01)
       RETURN
       END
