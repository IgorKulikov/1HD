      !****************************************************
      ! init/destroy subroutines
      !****************************************************
      subroutine init
        use mesh
        implicit none

        ! variables
        character*32 iarg_num
        character*32 iarg_mesh 
        character*32 iarg_limiter

        ! read of command line
        call get_command_argument(1,iarg_num)
        call get_command_argument(2,iarg_mesh)
        call get_command_argument(3,iarg_limiter)

        read(iarg_num,*)     number_of_test  ! read number of test
        read(iarg_mesh,*)    N               ! read size of mesh
        read(iarg_limiter,*) ilimiter        ! read limiter 

        ! allocate arrays
        allocate(density(N+2))      
        allocate(dimplsx(N+2))      
        allocate(denergy(N+2))      
        allocate(rho(N+2))          
        allocate(press(N+2))        
        allocate(velx(N+2))         

      end subroutine init
      !**************************************************
      subroutine destroy
        use mesh
        implicit none

        ! deallocate arrays
        deallocate(density)      
        deallocate(dimplsx)      
        deallocate(denergy)      
        deallocate(rho)          
        deallocate(press)        
        deallocate(velx)         

      end subroutine destroy
      !**************************************************

