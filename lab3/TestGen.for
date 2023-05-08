      program TestGen
        IMPLICIT NONE
        integer dim, i, flag, count,j,cur
        real num, cringe, k
        read*,dim
        count = 0
        flag = 0


        open(234,file='DI.dat', access='direct',recl=4)
        num = 1
        write(234,rec=1) num
        do i = 2, dim 

          if(MOD(count,4).GE.2)then
            if(flag.eq.0) then
              num=num+1
              flag = 1
            else
              flag = 0
            end if
            write(234,rec=i)num
          else 
            write(234,rec=i) 0
          endif
          count = count+1
        end do
        close(234)

        
        open(235,file='IA.dat', access='direct',recl=4)
        num=1
        flag = 0
        count = 1
        cringe = 0
        write(235,rec = 1)num
        do i = 1, dim
          if(MOD(count,2).eq.0)then
            cringe = cringe+1
          end if
          num = num+cringe
          count = count+1
          write(235,rec = i+1)num
        end do
        close(235)
        
        open(236,file='AL.dat', access='direct',recl=4)
        cur = 1
        count = 2
        do i = 2, dim
          num = 1
          do j = 1, count 
            if(MOD(j,2).eq.1)then
              write(236,rec = cur) num
              num = num+1
            else
              write(236,rec = cur) 0
            end if
            cur = cur+1
          end do
          if(MOD(i,2).eq.0) then 
            count = count+1
          end if
        end do
        close(236)

        open(237,file='VC.dat', access='direct',recl=4)
        k=1
        do i = 1,dim
          write(237,rec = i) k
          k=k+1
        end do
        close(237)

        open(238,file='dim.txt')
          write(238,*) dim
        close(238)


      end