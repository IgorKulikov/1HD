      !****************************************************
      ! recovery primitive variables
      !****************************************************
      subroutine primitive
        use machine
        use mesh
        implicit none

        ! variables 
        double precision dens, dimp, derg, dvel
        integer i

        ! recovery primitive variables
        do i=1,N+2
       
          ! get conservative variables
          dens = density(i)
          dimp = dimplsx(i)
          derg = denergy(i)
          dvel = dimp/dens

          ! set physics variables
          rho(i).vl   = dens
          velx(i).vl  = dvel
          press(i).vl = dmax1(eps,(dgamma - 1.d0) * (derg - dens*dvel*dvel/2.d0))

        enddo

      end subroutine primitive
      !**************************************************
