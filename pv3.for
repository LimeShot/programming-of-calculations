      program MatMult
        Implicit NONE
        common /matrix/ al, di, ia, V, Y, dim, maxdim, ans
        integer al,di,ia,V,Y,dim,maxdim,ans
        real X(1000000)
        maxdim = 1000000
        call input(X(1))
        !call calc(X(ia),X(al),X(di),X(V),X(ans))
        call calc(X(dim+1),X(2*dim+2),X(1),X(2*dim+2+Y),X(V+Y))
        print*,'wtf'
        call output(X(ans))
      end 

      subroutine input(X)
        Implicit NONE
        common /matrix/al,di,ia,V,Y,dim,maxdim,ans
        integer al,di,ia,V,Y,dim,maxdim,ans,i
        real X(1000000)
        open (1,file = 'dim.txt')
        read (1, *) dim
        close(1)

        di = 1
        ia = dim+1
        al = 2*dim+1
        
        open (78, file = 'IA.txt')
        do i = ia, ia+dim
            read(78, *) X(i)
        end do
        close (78)

        Y = X(ia+dim+1)-X(ia+dim)
        V = al+Y
        ans = V+Y

        open (2, file = 'DI.txt')
        do i = 1,dim 
            read(2, *) X(i)
        end do
        close (2)

        open (5, file = 'AL.txt')
        do i = al, al+Y
          read(5, *) X(i)
        end do
        close (5)

        open (6, file = 'Vector.txt')
        do i = Y, Y+dim-1
            read(6, *) X(i)
        end do
        close(6)
        print*,'ended 1'
      end

      subroutine calc(ia,al,di,V,ans)
        IMPLICIT NONE
        common /matrix/ Y,dim
        integer Y,dim,i,k
        real al(Y),di(dim),V(Y),ans(dim),ia(dim+1)
        print*,'started 2'
        do i = 1, dim
            ans(i)= ans(i)+di(i)*V(i)
            do k = 1, int(ia(i+1)-ia(i))
                ans(k)=ans(k)+al(int(ia(i))+k-1)*V(k)
                ans(dim-i)=ans(dim-i)+V(dim-i)*al(int(ia(i))+k-1)
            end do
        end do
      end


      subroutine output(ans)
        Implicit NONE
        common /matrix/ Y,dim
        integer Y,dim,i
        real ans(dim)
        print*,'Lol'
        do i=1,dim
          print*,ans(i)
        end do
      end

      