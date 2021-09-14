      !****************************************************
      ! riemann solver
      !****************************************************
      subroutine riemann(fdensity, fdimplsx, fdenergy, rdrhol, rdrhor, rdpressl, rdpressr, rdvelxl, rdvelxr)
        use mesh
        use machine
        implicit none

        ! output flux solution
        double precision, intent(out) :: fdensity, fdimplsx, fdenergy

        ! input hydrodynamics parameters
        type(physics), intent(in) :: rdrhol, rdrhor, rdpressl, rdpressr, rdvelxl, rdvelxr

        ! variables 
        double precision dbigP, dbigU, dbigR, dbigS, dustU, dcl, dcr, scll, sclr, sclc
        double precision drhol, drhor, dpressl, dpressr, dvelxl, dvelxr
        double precision dsnrhol, dsnrhor, dsnpressl, dsnpressr, dsnvelxl, dsnvelxr

        ! computing standart variables
        drhol   = rdrhol.vl  
        drhor   = rdrhor.vl  
        dpressl = rdpressl.vl
        dpressr = rdpressr.vl
        dvelxl  = rdvelxl.vl 
        dvelxr  = rdvelxr.vl

        ! using limiter for base characteristics
        if(ilimiter .ne. 0) then
          drhol   = rdrhol.vl   + 0.5d0 * rdrhol.dx   
          dpressl = rdpressl.vl + 0.5d0 * rdpressl.dx 
          dvelxl  = rdvelxl.vl  + 0.5d0 * rdvelxl.dx  
          drhor   = rdrhor.vl   - 0.5d0 * rdrhor.dx   
          dpressr = rdpressr.vl - 0.5d0 * rdpressr.dx 
          dvelxr  = rdvelxr.vl  - 0.5d0 * rdvelxr.dx  
        endif 

        ! left and right sound speeds
        dcl = dsqrt(dgamma*dpressl/drhol)
        dcr = dsqrt(dgamma*dpressr/drhor)

        ! computing dust speed
        dustU = (dsqrt(drhol)*dvelxl + dsqrt(drhor)*dvelxr)/(dsqrt(drhol) + dsqrt(drhor))

        ! using limiter for sound's characteristics
        if(ilimiter .ne. 0) then
          scll = (1.d0 - dabs(dcl)*tau/h) !(1.d0 - dabs(dvelxl-dcl)*tau/h)
          sclr = (1.d0 - dabs(dcr)*tau/h) !(1.d0 - dabs(dvelxr+dcr)*tau/h)
          drhol   = rdrhol.vl   + 0.5d0 * rdrhol.dx   * scll
          dpressl = rdpressl.vl + 0.5d0 * rdpressl.dx * scll
          dvelxl  = rdvelxl.vl  + 0.5d0 * rdvelxl.dx  * scll
          drhor   = rdrhor.vl   - 0.5d0 * rdrhor.dx   * sclr
          dpressr = rdpressr.vl - 0.5d0 * rdpressr.dx * sclr
          dvelxr  = rdvelxr.vl  - 0.5d0 * rdvelxr.dx  * sclr
        endif 

        ! computing P and U
        dbigP = (dpressl/(drhol*dcl)+dpressr/(drhor*dcr)+dvelxl-dvelxr)/(1.d0/(drhol*dcl)+1.d0/(drhor*dcr))
        dbigU = (drhol*dcl*dvelxl+drhor*dcr*dvelxr+dpressl-dpressr)/(drhol*dcl+drhor*dcr)

        ! using limiter for transfer characteristics
        if(ilimiter .ne. 0) then
          sclc = (1.d0 - dabs(dustU)*tau/h) 
          drhol   = rdrhol.vl   + 0.5d0 * rdrhol.dx   * sclc   
          dpressl = rdpressl.vl + 0.5d0 * rdpressl.dx * sclc
          dvelxl  = rdvelxl.vl  + 0.5d0 * rdvelxl.dx  * sclc
          drhor   = rdrhor.vl   - 0.5d0 * rdrhor.dx   * sclc
          dpressr = rdpressr.vl - 0.5d0 * rdpressr.dx * sclc
          dvelxr  = rdvelxr.vl  - 0.5d0 * rdvelxr.dx  * sclc
        endif 

        ! computing transfer term
        if(dustU > 0.d0) then
          fdensity = drhol * dustU
          fdimplsx = drhol * dvelxl * dustU + dbigP
          fdenergy = (dpressl/(dgamma-1.d0) + drhol*dvelxl*dvelxl/2.d0)*dustU + dbigP*dbigU
        else
          fdensity = drhor * dustU
          fdimplsx = drhor * dvelxr * dustU + dbigP
          fdenergy = (dpressr/(dgamma-1.d0) + drhor*dvelxr*dvelxr/2.d0)*dustU + dbigP*dbigU
        endif

      end subroutine riemann
      !**************************************************
 


