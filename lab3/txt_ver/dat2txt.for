      program MatMult
       Implicit NONE
       common /musthave/ Y, dim
       common /matrix/al, di, ia, V, maxdim, ans
       integer al,di,ia,V,Y,dim,maxdim,ans
       real X(100000000)
       maxdim = 100000000
       call input(X)
       call IDAT2TXT('IA',dim+1)
       call DAT2TXT('AL',Y)
       call DAT2TXT('DI',dim)
       call DAT2TXT('VC',dim)
      end
      
      subroutine input(X)
        Implicit NONE
        common /musthave/ Y, dim
        common /matrix/al, di, ia, V, maxdim, ans
        integer al,di,ia,V,Y,dim,maxdim,ans
        real X(maxdim)
        call prim_input()
        call cringe_input(X(ia))
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
        end do
        close (78)
        Y = (array(dim+1))-1
        V = al+Y
        ans = V+Y
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
      
      
      subroutine DAT2TXT(name,len)
       Implicit NONE
       CHARACTER name*2
       integer len, i
       real rtmp
       open(14,file=name//'.txt')
       open(41,file=name//'.dat',access='direct',recl=4,
     >  form='unformatted')
       do i=1,len
         read (41,rec=i) rtmp
         write(14,22) rtmp
       end do
       close(14)
       close(41)
  22   format(f11.4)  
      end

      subroutine IDAT2TXT(name,len)       
        Implicit NONE
        CHARACTER name*2
        integer len, i
        integer tmp
        open(14,file=name//'.txt')
        open(41,file=name//'.dat',access='direct',recl=4,
     >  form='unformatted')
        do i=1,len
          read (41,rec=i) tmp
          write(14,22) tmp
        end do
        close(14)
        close(41)
   22   format(I11)  
       end