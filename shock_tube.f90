      !****************************************************
      ! set shock tube test subroutine
      !****************************************************
      subroutine shock_tube
        use mesh
        implicit none

        ! set physics variables 
        select case(number_of_test)

        case (0)                 
          dmaxtime    = 0.2d0    ! maximal time
          x0          = 0.5d0    ! break
          dens_left   = 2.d0     ! left state
          press_left  = 2.d0
          vel_left    = 0.d0
          dens_right  = 1.d0     ! right state
          press_right = 1.d0
          vel_right   = 0.d0
      
        case (1)
          dmaxtime    = 0.2d0    ! maximal time
          x0          = 0.3d0    ! break
          dens_left   = 1.d0     ! left state
          press_left  = 1.d0
          vel_left    = 0.75d0
          dens_right  = 0.125d0  ! right state
          press_right = 0.1d0
          vel_right   = 0.d0
 
        case (2)
          dmaxtime    = 0.15d0   ! maximal time
          x0          = 0.5d0    ! break
          dens_left   = 1.d0     ! left state
          press_left  = 0.4d0
          vel_left    =-2.d0
          dens_right  = 1.d0     ! right state
          press_right = 0.4d0
          vel_right   = 2.d0

        case (3)
          dmaxtime    = 0.012d0  ! maximal time
          x0          = 0.5d0    ! break
          dens_left   = 1.d0     ! left state
          press_left  = 1000.d0
          vel_left    = 0.d0
          dens_right  = 1.d0     ! right state
          press_right = 0.01d0
          vel_right   = 0.d0

        case (4)
          dmaxtime    = 0.05d0      ! maximal time
          x0          = 0.3d0       ! break
          dens_left   = 5.99924d0   ! left state
          press_left  = 460.894d0
          vel_left    = 19.5975d0
          dens_right  = 5.99242d0   ! right state
          press_right = 46.095d0
          vel_right   =-6.19633d0

        case(5)
          dmaxtime    = 0.012d0     ! maximal time
          x0          = 0.8d0       ! break
          dens_left   = 1.d0        ! left state
          press_left  = 1000.d0
          vel_left    =-19.59745d0
          dens_right  = 1.d0        ! right state
          press_right = 0.01d0
          vel_right   =-19.59745d0

        end select
 
      end subroutine shock_tube
      !**************************************************

