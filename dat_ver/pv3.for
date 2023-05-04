      program MatMult
        Implicit NONE
        common /musthave/ Y, dim
        common /matrix/al, di, ia, V, maxdim, ans
        integer al,di,ia,V,Y,dim,maxdim,ans
        real X(1000000)
        maxdim = 1000000
        call prim_input()
        call TXT2DAT('IA',dim+1)
        call cringe_input(X)
        call TXT2DAT('AL',Y)
        call TXT2DAT('DI',dim)
        call TXT2DAT('VC',dim)
        call sec_input(X(1))
       !call calc(X(ia),   X(al),    X(di),    X(V),    X(ans))
        call calc(X(dim+1),X(2*dim+2),X(1),X(2*dim+2+Y),X(V+Y))
        call check
        call output(X(ans))
      end 

      subroutine prim_input()
        Implicit NONE
        common /musthave/Y,dim
        common /matrix/al, di, ia, V, maxdim, ans
        integer al,di,ia,V,Y,dim,maxdim,ans
        open (1,file = 'dim.txt')
        read (1, *) dim
        close(1)

        di = 1
        ia = dim+1
        al = 2*dim+2
      end

      subroutine cringe_input(X)
        Implicit NONE
        common /musthave/ Y, dim
        common /matrix/al, di, ia, V, maxdim, ans
        integer al,di,ia,V,Y,dim,maxdim,ans,i
        real X(maxdim) 
        open (78, file = 'IA.dat',recl=4,access='direct',
     >  form='unformatted')
        print*,'IA'
        do i = ia, ia+dim
            read(78, rec=i-ia+1) X(i)
            print*,i,' ',X(i)
        end do
        close (78)
        Y = int(X(ia+dim))-1
        V = al+Y
        ans = V+Y
      end

      subroutine sec_input(X)
        Implicit NONE
        common /musthave/ Y, dim
        common /matrix/al, di, ia, V, maxdim, ans
        integer al,di,ia,V,Y,dim,maxdim,ans,i
        real X(1000000)

        open (2, file = 'DI.dat',recl=4,access='direct',
     >  form='unformatted')
        print*,'DI'
        do i = 1,dim 
            read(2, rec=i) X(i)
            print*,i,' ',X(i)
        end do
        close (2)

        open (5, file = 'AL.dat',recl=4,access='direct',
     >  form='unformatted')
        print*,'AL', al, Y
        do i = al, al+Y-1
          read(5, rec=i-al+1) X(i)
          print*,i,' ',X(i)
        end do
        close (5)

        open (88, file = 'VC.dat',recl=4,access='direct',
     >  form='unformatted')
        print*,'VC'
        do i = V, V+dim-1
            read(88, rec=i-v+1) X(i)
            print*,i,' ',X(i)
        end do
        close(88)

        print*,'ans',ans
      end

      subroutine calc(ia,al,di,V,ans)
        IMPLICIT NONE
        common /musthave/ Y,dim
        integer Y,dim,i,k,ind
        real al(Y),di(dim),V(Y),ans(dim),ia(dim+1)
        do i = 1, dim
            ans(i)= ans(i)+di(i)*V(i)
            print*,i, ia(i+1)-ia(i), ia(i)
            do k = 1, int(ia(i+1)-ia(i))
                ind = i-1-int(ia(i+1)-ia(i))+k
                ans(i)=ans(i)+al(int(ia(i))+k-1)*V(ind)
                ans(ind)=ans(ind)+V(i)*al(int(ia(i))+k-1)
            end do
        end do
      end


      subroutine output(ans)
        Implicit NONE
        common /musthave/ Y,dim
        integer Y,dim,i
        real ans(dim)
        open(66, file='OutputCringe.txt')
        do i=1,dim
          write(66, 8) ans(i)
        end do
   8   format(F11.4)  
      end

      subroutine check()
        Implicit NONE
        common /musthave/Y, dim
        common /matrix/al, di, ia, V, maxdim, ans
        integer al,di,ia,V,Y,dim,maxdim,ans
        if(maxdim.LT.ans+Y) then
           print *,'You have posted a lot cringe. It takes too much 
     >     memory'
           stop
        end IF
      end
      
      subroutine TXT2DAT(name,len)
        Implicit NONE
        CHARACTER name*2
        integer len, i
        real rtmp
        open(14,file=name//'.txt')
        open(41,file=name//'.dat',access='direct',recl=4,
     >  form='unformatted')
        do i=1,len
          read (14,*) rtmp
          write(41,rec=i)rtmp
        end do
        close(14)
        close(41)
      end
