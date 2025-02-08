program Assignment3
    ! Liam Whitenack
    ! CDS 251
    ! Assignment 2
    ! Wednesday, February 10, 2021
    ! This program will find the average and standard deviation of a text file
    
    implicit none
    
    ! Assignments
    real, allocatable :: Numbers(:)
    integer*8 :: i, n, x
    real*4 :: mean, sum, Q, stddev
    
    mean = 0.0
    Q = 0.0
    
    ! code block
    
    ! open the file
    
    open(Unit=10, file="Numbers1.txt")
    
        ! read the first line. It contains the number of values. once it is read, it is not used to compute the new mean or stddev
        read (10,*) n
        
        ! allocate the array
        allocate(Numbers(n))
        
        ! start a do loop that works to find the sum of all of the computions
        do i = 1, n
            
            !read the values onto the array
            read (10,*) Numbers(i)
            
            ! assign a value to hold the value of the mean from the last do loop
            sum = mean
            
            ! use the equations for average and standard deviation like before
            mean = mean + ((Numbers(i)-mean)/float(i))
            Q = Q + (Numbers(i) - (sum))*(Numbers(i)-mean)
        end do
        
        ! finish the standard deviation equation outside of the do loop
        stddev = sqrt(Q/float(n))
        
    close(10)
    
    
    
    !print the values
    print*, 'mean: ', mean
    print*, 'standard deviation: ', stddev
    
    ! deallocate the array
    deallocate(Numbers)
    
    ! print done when the assignment is complete
    print*, 'Done!'

end program Assignment3
