      !****************************************************
      ! get time step
      !****************************************************
      subroutine get_tau
        use mesh
        use machine
        implicit none

        ! variables 
        double precision dmaxv, dsnd, dens, dpress, dvel
        integer i

        ! calculate maximal velocity
        dmaxv = 0.d0

        do i=1,N+2
       
          ! get physics variables
          dens   = rho(i).vl
          dpress = press(i).vl
          dvel   = velx(i).vl
          dsnd   = dsqrt(dgamma*dpress/dens)

          ! get maximal velocity
          if(dabs(dvel) + dsnd > dmaxv) dmaxv = dabs(dvel) + dsnd 

        enddo

        ! calculate time step
        tau = cfl * h / dmaxv

        ! reset to maximal time
        if(timer + tau >= dmaxtime) tau = dmaxtime - timer

      end subroutine get_tau
      !**************************************************
