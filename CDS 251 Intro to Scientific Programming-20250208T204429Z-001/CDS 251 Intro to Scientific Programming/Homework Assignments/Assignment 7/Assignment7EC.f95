program Assignment6
implicit none
    ! Liam Wntenack
    ! 3/10/2021
    ! This program reads a file and finds the coefficient m, b, and c.

    ! Assignments
    real :: x, y! the values from the txt file, as well as their eponential forms
    real :: meanx, meany, meanx2 ! mean of x and y, as well as the average of x squared
    real :: Sxx, Syy, Sxy, Sxx2, Sx2x2, Sx2y ! Used in calculating for the results
    real :: a, b, c ! coefficients
    integer :: i ! counter
    
    ! code block
    
    ! opent the file
    open(42, file = 'QuadData.txt')
    
    
    do i = 1, 200 ! run for all 200 values
        ! read in the file values to x and y
        read(42, *) x, y
        
        ! determine Sxx and Syy using the mean from the last do loop
        Sxx = Sxx + (float(i - 1) / float(i)) * ((x - meanx) ** 2) 
        Sxy = Sxy + (float(i - 1) / float(i)) * (x - meanx) * (y - meany)
        
        Sxx2 = Sxx2 + (float(i - 1) / float(i)) * ((x - meanx) * ((x ** 2) - meanx2))
        Sx2x2 = Sx2x2 + (float(i - 1) / float(i)) * (((x ** 2) - meanx2) ** 2)
        Sx2y = Sx2y + (float(i - 1) / float(i)) * ((y - meany) * ((x ** 2) - meanx2))
        
        ! find the new means using the formula for average
        meanx = meanx + ((x - meanx) / float(i))
        meany = meany + ((y - meany) / float(i))
        meanx2 = meanx2 + (((x ** 2) - meanx2) / float(i))
        
    enddo
    
    
    ! find the coefficients
    a = ((Sx2y * Sxx) - (Sxy * Sxx2)) / ((Sxx * Sx2x2) - (Sxx2 ** 2))
    b = ((Sxy * Sx2x2) - (Sx2y * Sxx2)) / ((Sxx * Sx2x2) - (Sxx2 ** 2))
    c = meany - (b * meanx) - (a * meanx2)
    
    ! output
    print*, a, b, c
    
    

end program Assignment6
