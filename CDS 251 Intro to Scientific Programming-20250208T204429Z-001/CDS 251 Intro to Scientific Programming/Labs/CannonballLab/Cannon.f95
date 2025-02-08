program CannonBall 
    implicit none
    ! Liam Wntenack
    ! 04/07/2021
    ! This program uses a simluation of random x and y values to find the value of pi.
    
    real*8 :: pi
    ! Variables
    real*8 :: vx
    real*8 :: vy
    real*8 :: y
    real*8 :: h
    real*8 :: n
    real*8 :: g
    ! Inputs
    real*8 :: V ! Total Velocity
    real*8 :: theta ! angle of shot
    ! Output
    real*8 :: x ! Distance travelled
    real*8 :: t ! time aloft 
    
    print*, 'Plot #1'
    
    open(41, file = 'CannonBall1.txt')
    
    pi = acos(-1.0d0)
    g = 9.8 ! m/s^2
    vx = 0
    vy = 0
    x = 0
    y = 0
    t = 0
    
    ! take the user input
    print*, 'Enter the total velocity'
    read(*,*) V
    print*, 'Enter the angle of shot (theta):'
    read(*,*) theta
    print*, 'Enter the step size (h):'
    read(*,*) h
    
    vx = V * cos((pi * theta)/180)
    vy = V * sin((pi * theta)/180)
    
    do
        if (y < 0) exit
        write(41, *) x, y
        y = y + (h * vy)
        vy  = vy - (h * g)
        x = x + (h * vx)
        t = t + h
    enddo    
    
    print*, 'The Distance travelled is', x
    print*, 'The time aloft is', t
    
    close(41)
    
    print*, 'Plot #2'
    
    open(42, file = 'CannonBall2.txt')
    
    vx = 0
    vy = 0
    x = 0
    y = 0
    t = 0
    
    ! take the user input
    print*, 'Enter the total velocity'
    read(*,*) V
    print*, 'Enter the angle of shot (theta):'
    read(*,*) theta
    print*, 'Enter the step size (h):'
    read(*,*) h
    
    vx = V * cos((pi * theta)/180)
    vy = V * sin((pi * theta)/180)
    
    do
        if (y < 0) exit
        write(42, *) x, y
        y = y + (h * vy)
        vy  = vy - (h * g)
        x = x + (h * vx)
        t = t + h
    enddo    
    
    print*, 'The Distance travelled is', x
    print*, 'The time aloft is', t
    
    close(42)
    
    print*, 'Plot #3'
    
    open(43, file = 'CannonBall3.txt')
    
    vx = 0
    vy = 0
    x = 0
    y = 0
    t = 0
    
    ! take the user input
    print*, 'Enter the total velocity'
    read(*,*) V
    print*, 'Enter the angle of shot (theta):'
    read(*,*) theta
    print*, 'Enter the step size (h):'
    read(*,*) h
    
    vx = V * cos((pi * theta)/180)
    vy = V * sin((pi * theta)/180)
    
    do
        if (y < 0) exit
        write(43, *) x, y
        y = y + (h * vy)
        vy  = vy - (h * g)
        x = x + (h * vx)
        t = t + h
    enddo    
    
    print*, 'The Distance travelled is', x
    print*, 'The time aloft is', t
    
    close(43)
    
    print*, 'Plot #4'
    
    open(44, file = 'CannonBall4.txt')
    
    vx = 0
    vy = 0
    x = 0
    y = 0
    t = 0
    
    ! take the user input
    print*, 'Enter the total velocity'
    read(*,*) V
    print*, 'Enter the angle of shot (theta):'
    read(*,*) theta
    print*, 'Enter the step size (h):'
    read(*,*) h
    
    vx = V * cos((pi * theta)/180)
    vy = V * sin((pi * theta)/180)
    
    do
        if (y < 0) exit
        write(44, *) x, y
        y = y + (h * vy)
        vy  = vy - (h * g)
        x = x + (h * vx)
        t = t + h
    enddo    
    
    print*, 'The Distance travelled is', x
    print*, 'The time aloft is', t
    
    close(44)

    print*, 'Done!'
    
end program CannonBall
