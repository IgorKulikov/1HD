      !****************************************************
      ! load physics problem subroutine
      !****************************************************
      subroutine load
        use mesh
        implicit none

        ! variables 
        double precision xcord, dens, dvelx, dpress 
        integer i

        ! compute length of cell
        h = length/N

        ! load shock tube
        call shock_tube

        ! set shock tube
        do i=2,N+1
          xcord = (i-1)*h - h/2.d0
          
          if(xcord < x0) then
            dens   = dens_left 
            dvelx  = vel_left
            dpress = press_left
          else
            dens   = dens_right 
            dvelx  = vel_right
            dpress = press_right
          endif

          density(i) = dens
          dimplsx(i) = dens * dvelx
          denergy(i) = dpress/(dgamma - 1.d0) + dens * dvelx * dvelx/2.d0

        enddo

      end subroutine load
      !**************************************************

