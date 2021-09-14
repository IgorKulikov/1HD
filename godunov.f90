      !****************************************************
      ! godunov method
      !****************************************************
      subroutine godunov
        use mesh
        implicit none

        ! variables 
        integer i
        double precision fplus_density, fplus_dimplsx, fplus_denergy
        double precision fminus_density, fminus_dimplsx, fminus_denergy

        ! Godunov method
        do i=2,N+1
          ! flux
          call riemann(fplus_density,  fplus_dimplsx,  fplus_denergy,  rho(i), rho(i+1), press(i), press(i+1), velx(i), velx(i+1))
          call riemann(fminus_density, fminus_dimplsx, fminus_denergy, rho(i-1), rho(i), press(i-1), press(i), velx(i-1), velx(i))

          ! Godunov scheme
          density(i) = density(i) - tau * (fplus_density - fminus_density)/h
          dimplsx(i) = dimplsx(i) - tau * (fplus_dimplsx - fminus_dimplsx)/h
          denergy(i) = denergy(i) - tau * (fplus_denergy - fminus_denergy)/h

        enddo

      end subroutine godunov
      !**************************************************

