      !****************************************************
      ! limiter function
      !****************************************************
      double precision function dlimiter(dminus, dzero, dplus)
        use machine
        use mesh 
        implicit none 

        ! input index of cell
        double precision, intent(in) :: dminus, dzero, dplus

        ! calculate limiter
        dlimiter = dmin1( dmax1(dzero-dminus,0.d0), dmax1(dplus-dzero,0.d0) ) + dmax1( dmin1(dzero-dminus,0.d0), dmin1(dplus-dzero,0.d0) )
          
      end function dlimiter

      !****************************************************
      ! piecewise-polynomial reconstruction subroutine
      !****************************************************
      subroutine reconstruction
        use mesh
        implicit none
        double precision, external :: dlimiter

        ! variables 
        integer i

        ! reset linear reconstruction
        rho(:).dx   = 0.d0
        press(:).dx = 0.d0 
        velx(:).dx  = 0.d0 

        ! reconstruction
        do i=2,N+1
          rho(i).dx   = dlimiter(rho(i-1).vl, rho(i).vl, rho(i+1).vl)
          press(i).dx = dlimiter(press(i-1).vl, press(i).vl, press(i+1).vl)
          velx(i).dx  = dlimiter(velx(i-1).vl, velx(i).vl, velx(i+1).vl)
        enddo

      end subroutine reconstruction
      !**************************************************

