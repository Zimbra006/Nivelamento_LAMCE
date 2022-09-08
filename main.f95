PROGRAM Pendulum_zero

implicit none

    integer :: i, nsteps
    real*8  :: timetot, time, theta, omega, theta_0, omega_0
    real*8  :: grav, dt, xL, mass, tension, tensionX, tensionY, weight
    
    open (unit=1, file='Pendulum_results.txt',form='formatted')
    
    ! Begin Pendulum Parameters
    time    = 0.0  ! Beginning time
    timetot = 8.0
    xL      = 0.8
    grav    = 9.81
    dt      =  .01
    theta_0 = 5.0   ! Degrees
    omega_0 = 0.0
    nsteps  = timetot / dt
    mass    =  .3   ! Kilograms
    weight  = mass * grav   ! Modulum of weight force
    ! End Pendulum Parameters
    
    ! Begin Initial Conditions
    theta   = theta_0 * 3.1416 / 180.0  ! Converts to radians
    omega   = omega_0
    ! End Initial Conditions
    
    ! Begin loop over time
    do i = 1, nsteps
        time        = time + dt
        omega       = omega - grav * theta * dt / xL
        theta       = theta + omega * dt
        tension     = mass * (omega**2 * xL + grav) ! Tension on cable
        tensionY    = tension * COS(theta)          ! Y component of Tension
        tensionX    = tension * SIN(theta)          ! X component of Tension
        write(1,*)time, theta, omega, tension, tensionX, tensionY, weight
    enddo ! End loop over time
    
END PROGRAM Pendulum_zero