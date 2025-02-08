program Gaussian 
    implicit none
    ! Liam Wntenack
    ! 04/07/2021
    ! Write a program that generates 13000 numbers with a Normal (Gaussian) distribution with an average of 22.0 and a standard deviation of 2.5, then generates 7000 numbers with an average of 15.5 and a standard deviation of 1.0.
    
    real*8 :: x, y ! function variables
    integer :: seed 
    real*8 :: sdv1, sdv2, mean1, mean2
    real*8 :: BoxMuller
    integer :: i, n ! number of iterations
    integer :: Count ! number of values inside of the graph
    
    sdv1 = 2.5
    sdv2 = 1.0
    mean1 = 22.0
    mean2 = 15.5
    
    open(42, file = 'Bumps.txt')
    
    ! take the user input
    print*, 'Enter the Random Number Seed'
    read(*,*) seed
    call srand(seed)
    
    do i = 1, 13000
        write(42, *) BoxMuller(sdv1, mean1)
    enddo
    
    do i = 1, 7000
        write(42, *) BoxMuller(sdv2, mean2)
    enddo
    
    close(42)
    
    print*, 'Done!'
    
    
end program Gaussian 

function BoxMuller(sdv, mean) result(rNum1)
    implicit none
    real*8 :: sdv, mean
    real*8 :: rNum1, rNum2, x1, x2, w 
    do
            x1 = 2.d0 * rand() - 1.d0
            x2 = 2.d0 * rand() - 1.d0
            w = x1**2 + x2**2
            if (w .lt. 1.d0) exit ! Valid numbers
        enddo
        w = sqrt((-2.0 * log(w))/w)
        rNum1 = x1 * w
        rNum1 = rNum1 * sdv + mean
end function BoxMuller
