program PlanetOrbit 
    implicit none
    ! Liam Wntenack
    ! 04/20/2021
    ! This program uses Euler's method to numerically advance the differential equations for Planetary motion
    
    ! Input variables:
    character*20 :: outputFile
    real*8 :: h, totalTime
    real*8 :: Planet(4)
    ! code block variables:
    integer :: n, i, in
    real*8 :: time, r, PE, KE
    !real*8 :: fP(4)
    ! output variables:
    real*8 :: kinetic, potential, total
    ! subroutine variables
    real*8 :: fP(4)
    
    print*, 'Enter the name of your output file'
    read(*,*) outputFile
    
    open(42, file = outputFile)
    
    ! take the user input
    print*, 'Enter the initial x-position:'
    read(*,*) Planet(1)
    print*, 'Enter the initial y-position:'
    read(*,*) Planet(3)
    print*, 'Enter the initial x-velocity:'
    read(*,*) Planet(2)
    print*, 'Enter the initial y-velocity:'
    read(*,*) Planet(4)
    print*, 'Enter the Time increment:'
    read(*,*) h
    print*, 'Enter the Total Time:'
    read(*,*) totalTime
    potential = PE(Planet)
    kinetic = KE(Planet)
    total = potential + kinetic
    print*, 'The initial potential energy is:', potential
    print*, 'The initial kinetic energy is:', kinetic
    print*, 'The initial total energy is:', total
    print*, ''
    
    n = int(totalTime / h)
    
    do i = 1, n
        time = time + h
        potential = PE(Planet)
        kinetic = KE(Planet)
        total = potential + kinetic
        call fPrime(Planet, fP)
        do in = 1, 4
            Planet(in) = Planet(in) + h * fP(in)
        enddo
        write(42,*) Planet(1), Planet(3), potential, kinetic, total, time
    enddo
    
    print*, 'The final potential energy is:', potential
    print*, 'The final kinetic energy is:', kinetic
    print*, 'The final total energy is:', total
    
    close(42)

    print*, 'Done!'
    
end program PlanetOrbit

subroutine fPrime(Planet, fP)
    implicit none
    real*8 :: Planet(4), fP(4), r
    r = sqrt(Planet(1)**2 + Planet(3)**2)
    fP(1) = Planet(2)
    fP(2) = -1 * Planet(1) / r**3
    fP(3) = Planet(4)
    fP(4) = -1 * Planet(3) / r**3
end subroutine fPrime

function KE(Planet) result (kinetic)
    implicit none
    real*8 :: Planet(4), kinetic
    kinetic = 0.5 * ((Planet(2) ** 2) + (Planet(4) ** 2))
end function KE

function PE(Planet) result (potential)
    implicit none
    real*8 :: Planet(4), potential
    potential = -1 / sqrt((Planet(1) ** 2) + (Planet(3) ** 2))
end function PE
