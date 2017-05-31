      program main
      !IMPLICIT NONE
      DIMENSION mem(20)
      INTEGER num,N,c
      num = 0
      N = 0
      
      call scan_arr(mem, num, N)
      
      call mult(mem(1),mem(num+1),mem(2*num+1),mem(2*num+N+1+1),
     + mem(2*num+2*N+1+1),N,num)
      
      c = 2*num+2*N+1+1
      
      call print_arr(mem(2*num+2*N+1+1),num,N)
      
      read*
      end
      
      
      
      
      subroutine scan_arr(mem,num,N)
      !IMPLICIT NONE
      INTEGER num,N
      DIMENSION mem(20)
      
      N = 3
      num = 4
      
      mem(1)=3
      mem(2)=1
      mem(3)=2
      mem(4)=9
      mem(5)=1
      mem(6)=3
      mem(7)=1
      mem(8)=2
      mem(9)=1
      mem(10)=3
      mem(11)=4
      mem(12)=5
      mem(13)=1
      mem(14)=2
      mem(15)=3
      
      
      end
      
      
      subroutine mult(ia,iaj,iai,ivector,iresult,N,num)
      !IMPLICIT NONE
      INTEGER num, N, j, k, i, summ, o
      DIMENSION ia(num),iaj(num),iai(N+1),ivector(N),iresult(N)
     
      j = 1
      DO 99 i=1,N
        k = iai(i+1) - iai(i)
        summ = 0
        DO 98 ii=1,k
            o = iaj(j) 
            summ = summ + ia(j)*ivector(o)
            j = j + 1
98      CONTINUE
      iresult(i) = summ
99    CONTINUE  
      end
      
      
      subroutine print_arr(iresult,num,N)
      !IMPLICIT NONE
      INTEGER num,N
      DIMENSION iresult(N)
      
      DO 97 i=1,N
        WRITE(*,*)iresult(i),' '
97    CONTINUE  
12    format(\,e11.4,a1)
      end
