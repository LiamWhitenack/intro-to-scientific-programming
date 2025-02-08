program DoLoopLab1
    ! Liam Whitenack
    ! CDS 251
    ! Do loop lab practice
    ! Wednesday, February 10, 2021
    ! this program will print out all even numbers from 2 to 42
    
    implicit none
    
    ! Assignments
    integer*4 :: n
    integer*4 :: i
            
    ! Start a do loop where the number ascends from 0 to 42 and stops, adding 2 for each cycle and printing out the value    
    do i = 0, 42, 2
        print*, i
    enddo
    
end program DoLoopLab1
