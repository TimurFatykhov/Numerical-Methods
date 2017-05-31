      PROGRAM main
         IMPLICIT NONE
         REAL*8 nodes(50000000)
         INTEGER numNodes
         REAL*8 integralSimpson
         REAL*8 integralGaus2
         REAL*8 a,b,h
         INTEGER i
         
         a= -5
         b= -1
         h= b - a
         !h= (b - a)/2
         
         CALL createEvenGrid(a,b,h,nodes(1),numNodes)
         !write(*,*)integralSimpson(nodes,numNodes)
         write(*,*)integralGaus2(nodes,numNodes)
         
         DO i = 0,17
            h = h/2
            CALL createEvenGrid(a,b,h,nodes(1),numNodes)
            !write(*,*)integralSimpson(nodes,numNodes)
            write(*,*)integralGaus2(nodes,numNodes)
         END DO
         
         !CALL createEvenGrid(a,b,h,nodes(1),numNodes)
         
         !write(*,*)integralSimpson(nodes,numNodes)
         !write(*,*)integralGaus2(nodes,numNodes)
         
         read*
                        
      END
    
      SUBROUTINE createEvenGrid(a,b,h, nodes,numNodes)
        REAL*8 nodes(*)
        REAL*8 a,b,h
        INTEGER numNodes
        INTEGER i
        i = 1
        nodes(1) = a
        ! a + i * h = nodes(i) + h
1       IF( a + i * h .LT. b) THEN
            nodes(i + 1) = a + i * h
            i = i + 1
            GOTO 1
        ELSE
            i = i + 1
        END IF
        nodes(i) = b
        numNodes = i       
      END
      
      
      REAL*8 FUNCTION fun(x)
        IMPLICIT NONE
        REAL*8 x
        
        !SIMPSON
        !fun = x**2
        !fun = x**3 + x**2
        !fun = x**4 - 3*x**3 + 2*x**2 + 1
        !fun = 2*x**5 + x**2 + 7*x
        
        !fun = x**7 + x**6
        fun = 5 + cos(7*x)*x
        
        
        
        !GAUSS
        !fun = 4*x**2 + 7*x + 3
        !fun = x**3 - 5*x + 10
        !fun = x**4 + 20*(x**3) + 3000
        !fun = -1*(x**5)+ x**4 + 20*(x**3)+50 
        
        !fun = x**7 + x**6
        !fun = cos(x)*x
      END
      
      
      
      
      REAL*8 FUNCTION integralSimpson(nodes, numNodes)
        IMPLICIT NONE
        REAL*8 nodes(*), fun, result
        INTEGER numNodes,i
        
        result = 0
        
        DO 99 i=1,numNodes - 1
            result = result + (nodes(i+1)-nodes(i))/6*
     +      ( fun(nodes(i))+4*(fun(( nodes(i+1)+nodes(i))/2))+
     +      fun(nodes(i+1))) 
99      CONTINUE
        
        integralSimpson=result
      END
      
      
      
      
      REAL*8 FUNCTION integralGaus2(nodes,numNodes)
        IMPLICIT none
        REAL*8 nodes(*)
        INTEGER numNodes
        
        REAL*8 fun
        REAL*8 res
        REAL*8 sum
        
        REAL*8 x_j(2)
        REAL*8 q(2)
        
        REAL*8 x_k_j
        REAL*8 h_k
        
        INTEGER k, j
        
        x_j(1) = -1./SQRT(3.d0)
        x_j(2) = -1 * x_j(1)
        
        
        q(1) = 1.d0
        q(2) = 1.d0
        
        res = 0
        DO 98 j = 1,2
            sum=0                  
            DO 97 k = 1,numNodes - 1
                h_k = nodes(k + 1) - nodes(k)  
                x_k_j =(nodes(k)+nodes(k+1) + x_j(j)*h_k)/2
                sum = sum + q(j) * fun(x_k_j) * h_k
97          CONTINUE
            res = res + sum
98      CONTINUE
        integralGaus2 = res/2
      END
