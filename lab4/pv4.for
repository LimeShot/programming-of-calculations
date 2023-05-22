      program IntegralCalc
        Implicit NONE
        integer N, i
        real grid(100000000), rectcringe, RectCalc, simpcringe
        real gausscringe, GaussCalc, SimpCalc
        real a, b
        a = 0.0
        b = 5.0
        open(3,file ='Gauss.txt')
        do i=0,22
        N=2**i
         call GridCreate(grid, a, b, N)
         !rectcringe = RectCalc(grid,N+1)
        ! print*,'Rectangle', rectcringe
         !simpcringe = SimpCalc(grid,N+1)
        ! print*,'Simpson', simpcringe
         gausscringe = GaussCalc(grid,N+1)
        ! print*,'Gauss', gausscringe
        !write(3,228) N
        write(3,*) gausscringe
        end do
  228   FORMAT(I4,$)
        close(3)
      end

      function RectCalc(grid,len)
        Implicit NONE
        integer len, i
        real grid(100000000), fun, RectCalc  
        RectCalc = 0
        do i = 1, len-1
          RectCalc = RectCalc+(grid(i+1)-grid(i))*
     >fun((grid(i+1)+grid(i))/2.0)
        end do
      end 

      function SimpCalc(grid,len)
        Implicit NONE
        integer len, i
        real grid(100000000), fun, SimpCalc
        SimpCalc = (fun(grid(1))*(grid(2)-grid(1))+
     >fun(grid(len))*(grid(len)-grid(len-1)))
        do i = 1, len-1
          SimpCalc = SimpCalc+4.0*(grid(i+1)-grid(i))*
     >fun((grid(i+1)+grid(i))/2.0)
        end do
        do i = 1, len-2
            SimpCalc = SimpCalc+(grid(i+2)-grid(i))*
     >fun((grid(i+1)))
        end do
        SimpCalc = 1.0/6.0*SimpCalc
      end

      function GaussCalc(grid,len)
        Implicit NONE
        integer len, i,k
        real grid(100000000), fun, GaussCalc, root(2), x
        root(1) = (1.0/sqrt(3.0))
        root(2) = (-1.0/sqrt(3.0))
        do i = 1, 2
            do k=1,len-1
                x =(grid(k+1)+grid(k))/2.0+root(i)*
     >(grid(k+1)-grid(k))/2.0
            GaussCalc = GaussCalc+fun(x)*(grid(k+1)-grid(k))
            end do
        end do
        GaussCalc = GaussCalc/2.0
      end

      function fun(x)
        real x, fun
        !fun = 5.0**(x*(-0.5))*cos(8.0*x)
        fun = -x*cos(5.0*x)
        !fun =(x+2.0)*sin(8.0*x)
      end

      subroutine GridCreate(grid, a, b, N)
        Implicit NONE
        integer N, i
        real h, a, b
        real grid(100000000)
        h = (b-a)/real(N)
        print*, 'h:', h
        do i = 1, N
            grid(i+1)=a+h*real(i)
        end do
        grid(1)=a
      end 