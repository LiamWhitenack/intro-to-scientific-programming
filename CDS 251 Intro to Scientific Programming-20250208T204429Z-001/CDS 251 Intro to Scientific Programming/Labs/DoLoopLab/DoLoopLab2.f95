program DoLoopLab2
    ! Liam Whitenack
    ! CDS 251
    ! Do loop lab practice
    ! Wednesday, February 10, 2021
    ! Write a program that asks the user for a number then counts down to 1 from that numbers then prints ‘blast-off’
    
    implicit none 
    
    ! Assignments 
    integer*4 :: n
    integer*4 :: i
    
    ! code block
    ! receive an input for a number from the user
    print*, 'Enter a number'
    read*, n
    
    ! start a descending do loop that prints its value until it reaches 0, starting at the number previously entered
    do i = n, 0, -1
        print*, i
    enddo
    
    ! print Blastoff
    print*, 'Blastoff!'
    
end program DoLoopLab2
