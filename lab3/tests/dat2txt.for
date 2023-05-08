      program MatMult
       Implicit NONE
       common /musthave/ Y, dim
       common /matrix/al, di, ia, V, maxdim, ans
       integer al,di,ia,V,Y,dim,maxdim,ans
       real X(500)
       maxdim = 500
       call prim_input()
       call DAT2TXT('IA',dim+1)
       call cringe_input(X)
       call DAT2TXT('AL',Y)
       call DAT2TXT('DI',dim)
       call DAT2TXT('VC',dim)
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
          stop
        end IF
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
        do i = ia, ia+dim
            read(78, rec=i-ia+1) X(i)
        end do
        close (78)
        Y = int(X(ia+dim))-1
        V = al+Y
        ans = V+Y
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