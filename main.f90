      !****************************************************
      ! One-dimensional hydrodynamics solver 1HD code
      !****************************************************
      program code1hd
        ! using modules
        use machine
        use mesh
        implicit none

        ! hydrodynamics code
        timer = 0.d0                   ! reset timer  
        call init                      ! read command line and create arrays
        call load                      ! load problem
        call hydro_boundary            ! hydrodynamics boundary conditions
        call primitive                 ! physics variables recovery

        do while(timer < dmaxtime)
          call get_tau                 ! calculate tau
          timer = timer + tau          ! increment timer
          call reconstruction          ! piecewise-polynomial reconstruction 
          call godunov                 ! godunov method
          call hydro_boundary          ! hydrodynamics boundary conditions
          call primitive               ! physics variables recovery
          print *,"Time = ",timer      ! info print of current timer
        enddo                          ! end cycle by global time 

        call save(dmaxtime)            ! save solution
        call destroy                   ! destroy array

      end program code1hd
