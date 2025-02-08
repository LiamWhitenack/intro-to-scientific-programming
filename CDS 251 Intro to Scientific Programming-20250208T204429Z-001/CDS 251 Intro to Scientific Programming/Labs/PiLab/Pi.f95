program RandomPi 
    implicit none
    ! Liam Wntenack
    ! 3/24/2021
    ! This program uses a simluation of random x and y values to find the value of pi.
    
    real*8 :: x, y ! function variables
    integer :: seed 
    real*8 :: rpi ! calculated value of pi
    integer :: i, n ! number of iterations
    integer :: Count ! number of values inside of the graph
    
    call srand(seed)
    
    ! take the user input
    print*, 'Enter the number of iterations (n):'
    read(*,*) n
    print*, 'Enter the Random Number Seed'
    read(*,*) seed
    
    ! start do loop where pi is calculated
    do i = 1, n
        x = rand()
        y = rand()
        if (x ** 2 + y ** 2 .le. 1.d0) Count = Count + 1
    enddo
    
    rpi = 4.0d0 * float(Count) / float(n)
    
    print*, 'The estimated value of pi is:'
    print*, rpi
    
    
end program RandomPi
