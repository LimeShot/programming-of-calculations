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
        real X(maxdim)
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
        do i = di,dim 
            read(2, *) X(i)
        end do
        close (2)



        open (5, file = 'AL.txt')
        do i = al, al+V
          read(2, *) X(i)
        end do
        close (2)