      !****************************************************
      ! module for mesh description
      !****************************************************
      module mesh
        implicit none 

        !**************************************************
        ! structure for physics variables
        !**************************************************
        type physics
          double precision vl      ! hydrodynamics value
          double precision dx      ! supplement polynomial
        end type physics

        !**************************************************
        ! mesh constants & variables
        !**************************************************
        double precision, parameter :: length = 1.d0      ! length of domain
        integer N                                         ! length of mesh
        double precision h                                ! length of cell
        double precision tau                              ! time step
        double precision timer                            ! internal timer
        double precision, parameter :: cfl = 0.2          ! Courant number
        integer ilimiter                                  ! limiter 
        !**************************************************

        !**************************************************
        ! shock tube physics variables
        !**************************************************
        double precision, parameter :: dgamma = 1.4d0       ! adiabatic index
        double precision dmaxtime                           ! maximal time
        double precision x0                                 ! break
        double precision dens_left, vel_left, press_left    ! left state
        double precision dens_right, vel_right, press_right ! right state
        integer number_of_test                              ! number of test

        !**************************************************
        ! conservative and physics variables on mesh
        !**************************************************
        double precision, allocatable :: density(:)  ! conservative density            
        double precision, allocatable :: dimplsx(:)  ! conservative momentum of impulse
        double precision, allocatable :: denergy(:)  ! conservative energy             
        type(physics), allocatable :: rho(:)         ! physics density                 
        type(physics), allocatable :: press(:)       ! physics pressure                
        type(physics), allocatable :: velx(:)        ! physics velocity                
        !**************************************************

      end module mesh
