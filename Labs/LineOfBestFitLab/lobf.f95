program LineOfBestFit
    implicit none
    ! Liam Wntenack
    ! 3/10/2021
    ! This program reads a file and finds the slope and y-intercept of a line of best fit

    ! Assignments
    real :: x, y ! the values from the txt file
    real :: meanx, meany ! mean of x and y
    real :: Sxx, Sxy ! Used in calculating for the results
    real :: m, b ! slope and y-intercept, final results
    integer :: i ! counter

    ! code block
    
    ! opent the file
    open(42, file = 'LineData.txt')
    
    
    do i = 1, 200 ! run for all 200 values
        ! read in the file values to x and y
        read(42, *) x, y
        
        ! determine Sxx and Syy using the mean from the last do loop
        Sxx = Sxx + (float(i - 1) / float(i)) * ((x - meanx) ** 2) 
        Sxy = Sxy + (float(i - 1) / float(i)) * (x - meanx) * (y - meany)
        
        ! find the new means using the formula for average
        meanx = meanx + ((x - meanx) / float(i))
        meany = meany + ((y - meany) / float(i))
        
    enddo
    
    ! find the slope and y-intercept using the formulas from the class notes
    m = Sxy / Sxx
    b = meany - (m * meanx)
    
    ! output
    print*, 'The slope is: '
    print*, m
    print*, 'The y-intercept is:'
    print*, b

end program LineOfBestFit
