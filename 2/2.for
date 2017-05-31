      program main
        real minX,maxX,stepX,minY,maxY,stepY,x,y,null,zero
        open(1,file='input8.txt')
        open(2,file='output8.txt')
        read(1,*)minX,maxX,stepX,minY,maxY,stepY
        
        null = 10.**aint(log10(abs(min(stepX,stepY)))-3)
        zero = 0
        
        nX = 0
        nY = 0
        if(stepX.NE.0.)nX = (maxX-minX)/stepX
        if(stepY.NE.0.)nY = (maxY-minY)/stepY
        
        if(minX+nX*stepX.EQ.maxX.AND.nX.NE.0)nX = nX - 1
        if(minY+nY*stepY.EQ.maxY.AND.nY.NE.0)nY = nY - 1
        
        if(nX*(maxX-minX).LT.0)then
            print*,'incorrect data'
            read*
            return
        end if
        
        write(2,10)'x\y',9,9,'|',9
10      format(\,a3,4a1)

        ! print first line of table
        y = minY
        itY = 0
        do while(maxY-y.GT.null)
        if(abs(y).GT.null)then                           !
            write(2,11)y,9,'|',9
        else
            write(2,11)zero,9,'|',9
        end if
            !!!!!!!!!!!!!!!!!!!itY = itY + 1
            y = aNextValue(minY,stepY,maxY,itY)
        end do
        if(abs(maxY).GT.null)then                        !
            write(2,11)maxY,9,'|',9
        else
            write(2,11)zero,9,'|',9
        end if
        write(2,13)' '
        
        ! print other lines of table without last
        x = minX
        itX = 0 
        do while(maxX-x.GT.null)
            if(abs(x).GT.null)then                       !
                write(2,11)x,9,'|',9
            else
                write(2,11)zero,9,'|',9             
            end if
            y = minY
            itY = 0
            do while(maxY-y.GT.null)
                call sec(x,y,null)
                !!!!!!!!!!!!!!!!!itY = itY + 1
                y = aNextValue(minY,stepY,maxY,itY)
            end do
            call sec(x,maxY,null)
            write(2,13)' '
            x = aNextValue(minX,stepX,maxX,itX)
        end do
        
        ! print last line of table
        if(abs(maxX).GT.null)then                    !
            write(2,11)maxX,9,'|',9
        else
            write(2,11)zero,9,'|',9
        end if
        y = minY
        itY = 0
        do while(maxY-y.GT.null)
            call sec(maxX,y,null)
            !!!!!!!!!!!!!!!!itY = itY + 1
            y = aNextValue(minY,stepY,maxY,itY)
        end do
        call sec(maxX,maxY,null)
        
        !read*,
        
11      format(\,E11.4,3a1)
13      format(' ',a1)
31      format(\,a1)
      end
      
      subroutine sec(x,y,null)
        real x,y,null
        pi = 3.1415927
        
        if((abs(mod(x+y,360.)-90).GT.null)
     +  .AND.(abs(mod(x+y,360.)-270).GT.null))then
            write(2,11)abs(1/cos(x*pi/180+y*pi/180)),9,'|',9
        else
            write(2,21)'ndet',9,9,'|',9
        end if
        
11      format(\,E11.4,3a1)
21      format(\,a,4a1)
      end
      
      function aNextValue(p,stepP,maxP,itP)
        real p,stepP,maxP
        logical next,isEquivalent
        i = 1
        next = .true.
        dowhile((next).AND.((p+(itP+i)*stepP).LT.maxP))
        !dowhile(true.AND.true)
            next = isEquivalent(p+itP*stepP,p+(itP+i)*stepP)
            i=i+1
        end do
        itP=itP+(i-1)
        if(next)then
            aNextValue=maxP
        else
            aNextValue=p+itP*stepP
        end if
      end
      
      logical function isEquivalent(a,b)
        character *11 buf1, buf2
        write(buf1,99)a
        write(buf2,99)b
        if(buf1.EQ.buf2)then
            isEquivalent = .true.
        else
            isEquivalent = .false.
        end if
99     format(\,E11.4)
      end
