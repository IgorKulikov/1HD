      !****************************************************
      ! boundary condition for hydrodynamics
      !****************************************************
      subroutine hydro_boundary
        use mesh
        implicit none 
                                                         
        ! set boundary conditions
        density(1) = density(2)
        dimplsx(1) = dimplsx(2)
        denergy(1) = denergy(2) 

        density(N+2) = density(N+1)
        dimplsx(N+2) = dimplsx(N+1)
        denergy(N+2) = denergy(N+1) 

      end subroutine hydro_boundary
      !**************************************************
