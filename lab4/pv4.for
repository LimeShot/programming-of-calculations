      program IntegralCalc
        Implicit NONE
        integer len, i
        real rgrid(30), rectcringe, RectCalc, simpcringe, SimpCalc
        real gausscringe, GaussCalc
        open(1,file = 'cringeinput.txt')
        read(1,*) len
        do i = 1, len
           read(1,*) rgrid(i)
        end do
        rectcringe = RectCalc(rgrid,len)
        simpcringe = SimpCalc(rgrid,len)
        gausscringe = GaussCalc(rgrid,len)
      end

      function RectCalc(rgrid,len)
        Implicit NONE
        integer len, i
        real rgrid(30), fun, RectCalc  
        RectCalc = 0
        do i = 1, len-1
          RectCalc = RectCalc+(rgrid(i+1)-rgrid(i))*
     >fun((rgrid(i+1)-rgrid(i))/2)
        end do
      end 

      function SimpCalc(rgrid,len)
        Implicit NONE
        integer len, i
        real rgrid(30), fun, SimpCalc
        SimpCalc = 1/6*(fun(rgrid(1))*(rgrid(2)-rgrid(1))+
     >fun(rgrid(len))*(rgrid(len)-rgrid(len-1)))
        do i = 1, len-1
          SimpCalc = SimpCalc+(rgrid(i+1)-rgrid(i))*
     >fun((rgrid(i+1)-rgrid(i))/2.0)
        end do
        do i = 1, len-2
            SimpCalc = SimpCalc+(rgrid(i+2)-rgrid(i))*
     >fun((rgrid(i+1)))
          end do
      end

      function GaussCalc(rgrid,len)
        Implicit NONE
        integer len, i,k
        real rgrid(30), fun, GaussCalc, root(2), x
        root(1) = (1/sqrt(3.0))
        root(2) = (-1/sqrt(3.0))
        do i = 1, 2
            do k=1,len-1
                x =(rgrid(k+1)+rgrid(k))/2.0+root(i)*
     >(rgrid(k+1)-rgrid(k))/2.0
            GaussCalc = GaussCalc+fun(x)*(rgrid(k+1)-rgrid(k))
            end do
        end do
        GaussCalc = GaussCalc/2.0
      end

      function fun(x)
        fun = x
      end