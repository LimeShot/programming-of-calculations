      program MatMult
        Implicit NONE
        common /musthave/ Y, dim
        common /matrix/al, di, ia, V, maxdim, ans
        integer al,di,ia,V,Y,dim,maxdim,ans
        real X(10000)
        maxdim = 10000
        call input(X)
       !call calc(X(ia),   X(al),    X(di),    X(V),    X(ans))
        call calc(X(dim+1),X(2*dim+2),X(1),X(2*dim+2+Y),X(V+Y))
        call output(X(ans))
        call MatOutput(X(dim+1),X(2*dim+2),X(1))
      end 

      subroutine input(X)
        Implicit NONE
        common /musthave/ Y, dim
        common /matrix/al, di, ia, V, maxdim, ans
        integer al,di,ia,V,Y,dim,maxdim,ans
        real X(10000)
        call prim_input()
       ! call TXT2DAT('IA',dim+1)
        call cringe_input(X(ia))
       ! call TXT2DAT('AL',Y)
       ! call TXT2DAT('DI',dim)
       ! call TXT2DAT('VC',dim)
        !              X(di), X(al),     X(V))
        call sec_input(X(1),X(2*dim+2),X(2*dim+2+Y))
      end
      
      subroutine prim_input()
        Implicit NONE
        common /musthave/Y,dim
        common /matrix/al, di, ia, V, maxdim, ans
        integer al,di,ia,V,Y,dim,maxdim,ans
        open (1,file = 'dim.txt')
        read (1, *) dim
        close(1)

        if(4*dim+1.GT.maxdim) then
          print *,'You have posted a lot cringe. It takes too much 
     >memory'
          pause
          stop
        end IF
        di = 1
        ia = dim+1
        al = 2*dim+2
      end

      subroutine cringe_input(array)
        Implicit NONE
        common /musthave/ Y, dim
        common /matrix/al, di, ia, V, maxdim, ans
        integer al,di,ia,V,Y,dim,maxdim,ans,i
        integer array
        dimension array(dim+1) 
        open (78, file = 'IA.dat',recl=4,access='direct')
        do i = 1, dim+1
            read(78, rec=i) array(i)
            PRINT *, array(i)
        end do
        close (78)
        Y = (array(dim+1))-1
        V = al+Y
        ans = V+Y

        if(maxdim.LT.ans+dim) then
          print *,'You have posted a lot cringe. It takes too much 
     >memory'
          pause
          stop
       end IF

      end

      subroutine sec_input(di, al, V)
        Implicit NONE
        common /musthave/ Y, dim
        integer Y,dim,i
        real di(dim),al(Y),V(dim)

        open (2, file = 'DI.dat',recl=4,access='direct',
     >  form='unformatted')
        do i = 1,dim 
            read(2, rec=i) di(i)
        end do
        close (2)

        open (5, file = 'AL.dat',recl=4,access='direct',
     >  form='unformatted')
        do i = 1, Y
          read(5, rec=i) al(i)
        end do
        close (5)

        open (88, file = 'VC.dat',recl=4,access='direct',
     >  form='unformatted')
        do i = 1, dim
            read(88, rec=i) V(i)
        end do
        close(88)
      end

      subroutine calc(ia,al,di,V,ans)
        IMPLICIT NONE
        common /musthave/ Y,dim
        integer Y,dim,i,k,ind
        real al(Y),di(dim),V(Y),ans(dim)
        integer ia(dim+1)
        
        do i = 1, dim
            ans(i)= ans(i)+di(i)*V(i)
            do k = 1, (ia(i+1)-ia(i))
                ind = i-1-(ia(i+1)-ia(i))+k
                ans(i)=ans(i)+al((ia(i))+k-1)*V(ind)
                ans(ind)=ans(ind)+V(i)*al((ia(i))+k-1)
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

      subroutine MatOutput(ia,al,di)
        IMPLICIT NONE
        common /musthave/Y, dim
        INTEGER dim,i,j,k,Y,m, elem
        REAL di(dim),al(Y)
        integer ia(dim+1)
        OPEN(67, file='ma.txt')
        do i=1,dim
          do k = 1, i-(ia(i+1)-ia(i))-1 
            write(67,10) 0.0
          end do
          do j = 1, ia(i+1)-ia(i)
            WRITE(67,10) al((ia(i))+j-1)
          end do
          write(67,10)di(i)
          do m = i+1, dim
            elem = m-(ia(m+1)-ia(m))-1
            if(elem.LT.i) then 
              write(67,10)al(i-m+(ia(m+1)))        
            else 
              write(67,10) 0.0
            end if
          end do
          write(67,99)
        end do
  10    FORMAT(F11.4$)
  99    FORMAT(' ')
        close(67)
      end
