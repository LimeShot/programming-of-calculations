      program MatMult
      common /matrix/ al,di,ia, X, V, Y, dim, maxdim
      DIMENSION X(maxdim)
      maxdim = 1000000
      call input
      call calc
      call output
      end 

      subroutine input(X)
        common /matrix/ al,di,ia, V, Y, dim, maxdim
        
        open (1,file = 'dim.txt')
        read (1, *) , dim
        close(1)

        di = 1
        ia = dim+1
        al = 2*dim+2
        
        
        open (4, file = 'ia.txt')
        do i = ia, ia+dim+1
            read(4, *) X(i)
        end do
        close (4)

        V = X(ia+dim+1)-X(ia+dim)


        open (2, file = 'DI.txt')
        do i = 0, dim-1
            read(2, *) di(i)
        end do
        close (2)



        open (5, file = 'AL.txt')
        do i = 1, dim
            do k = ia(i-1), ia(i)
            read(2, *) al(k)
            end do
        end do
        close (2)