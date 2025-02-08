program Brownian
    implicit none
    ! Liam Wntenack
    ! 04/07/2021
    ! Write a program that simulates Brownian Motion with a 2-D walk
    
    real*8 :: x, y ! function variables
    real*8 :: rNum
    integer :: seed 
    integer :: h ! number of steps
    integer :: i
    
    x = 0
    y = 0
    
    open(42, file = 'Brownian.txt')
    
    ! take the user input
    print*, 'Enter the Random Number Seed'
    read(*,*) seed
    call srand(seed)
    print*, 'Enter the Number of steps:'
    read(*,*) h
    
    do i = 1, h
        rNum = 1.0d0 * rand()
        if(rNum < 0.25) then
        x = x + 1
        elseif (rNum < 0.5) then
        y = y + 1
        elseif (rNum < 0.75) then
        x = x - 1
        else
        y = y - 1
        endif
        write(42, *) x, y
    enddo
    
    close(42)
    
    print*, 'Done!'
    
    
end program Brownian 
