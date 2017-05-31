      program main
        common/trian/a,b,alfa,pi,pisc
        pi=3.14159
        e = 0       !exit
        pisc = 0    !parametres is correct
      
        do while(e.EQ.0)
      
            print*,'1)Enter new triangle'
            print*,'2)Compute area of triangle'
            print*,'3)Compute minimum angle'
            print*,'4)Compute cosine minimum angle'
            print*,'5)Exit'
            read*,num
      
            if(num.EQ.1)call new_t
            if(num.EQ.2.AND.pisc.EQ.1)call area
            if(num.EQ.3.AND.pisc.EQ.1)call min_angle
            if(num.EQ.4.AND.pisc.EQ.1)call cos_of_min
            if(num.EQ.5)e = 1
            if(num.EQ.6)call max_value
      
        end do
      end
      
      
      subroutine max_value
        read*,a
        print*,a
      end
      
      

      subroutine new_t
        common/trian/a,b,alfa,pi,pisc
        read*,a,b,alfa
        pisc = 1
        
        if(a.LE.0.OR.b.LE.0)then
            print*,'uncorrect side'
            pisc = 0
        end if
      
        if(alfa.LE.0.OR.alfa.GE.180)then
            print*,'uncorrect angle'
            pisc = 0
        end if
        
        if((alfa.GT.90).AND.(a.LT.b))then
            print*,'triangle does not exist'
            pisc = 0
        end if
        
        if((alfa.LT.90).AND.(a.LT.(b*SIN(alfa*pi/180))))then
            print*,'triangle does not exist'
            pisc = 0
        end if
      end
      
      
      subroutine area
        common/trian/a,b,alfa,pi,pisc
        betta=ASIN(b*SIN((alfa*pi)/180)/a)*180/pi
        print*,0.5*a*b*SIN((180-alfa-betta)*pi/180)
        if(alfa.LT.90.AND.a.LT.b)then
            print*,0.5*a*b*SIN((betta-alfa)*pi/180)
        end if
      end
      
      
      subroutine min_angle
        common/trian/a,b,alfa,pi,pisc
        betta=ASIN(b*SIN((alfa*pi)/180)/a)*180/pi
        gamma=180-betta-alfa
        angle_min=min(alfa,betta,gamma)
        print*,angle_min
        
        if(alfa.LT.90.AND.a.LT.b)then
            betta = 180 - betta
            gamma = 180 - betta - alfa
            angle_min=min(alfa,betta,gamma)
            print*,angle_min
        end if
      end
      
      
      subroutine cos_of_min
        common/trian/a,b,alfa,pi,pisc
        betta=ASIN(b*SIN((alfa*pi)/180)/a)*180/pi
        gamma=180-betta-alfa
        angle_min=min(alfa,betta,gamma)
        print*,COS(angle_min*pi/180)
        
        if(alfa.LT.90.AND.a.LT.b)then
            betta = 180 - betta
            gamma = 180 - betta - alfa
            angle_min=min(alfa,betta,gamma)
            print*,COS(angle_min*pi/180)
        end if
      end

