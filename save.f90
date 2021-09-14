      !****************************************************
      ! save results
      !****************************************************
      subroutine save
        use machine
        use mesh 
        implicit none 

        ! variables 
        integer i
        double precision xcord, dens, dpress, dvel, dinten
        character*32 filename

        write(filename, 101) number_of_test, N, ilimiter
101	format("Test",i1.1,"_N",i4.4,"L",i1.1,".dat")

        open(501, file = filename)

201     format(f14.6,1x,f14.6,1x,f14.6,1x,f14.6,1x,f14.6)

        ! save results
        do i=2,N+1
          xcord  = (i-1)*h - h/2.d0
          dens   = rho(i).vl
          dpress = press(i).vl
          dvel   = velx(i).vl
          dinten = dpress/dens/(dgamma-1.d0)
          write(501,201) xcord,dens,dvel,dpress,dinten
        enddo

        close(501)

      end subroutine save
      !**************************************************

