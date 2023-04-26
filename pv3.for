      program MatMult
      common /matrix/ al,di,ia, X, V, Y, dim, maxdim
      DIMENSION X(maxdim)
      maxdim = 1000000
      call input
      call calc
      call output
      end 

      subroutine input
        common /matrix/ al,di,ia, X, V, Y, dim, maxdim
        
        open (1,file = 'dim.txt')
        read (1, *) , dim
        close(1)
        DIMENSION di(dim)

        open (2, file = 'DI.txt')
        do i = 0, dim-1
            read(2, *) di(i)
        end do
        close (2)

        open (4, file = 'ia.txt')
        do i = 0, dim
            read(4, *) ia(i)
        end do
        close (4)

        open (5, file = 'AL.txt')
        do i = 1, dim
            do k = ia(i-1), ia(i)
            read(2, *) al(k)
            end do
        end do
        close (2)