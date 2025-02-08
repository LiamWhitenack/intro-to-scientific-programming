program DoLoopLab3
    ! Liam Whitenack
    ! CDS 251
    ! Do loop lab practice
    ! Wednesday, February 10, 2021
    ! Write a program that asks a user to guess numbers from 1 to 100 until they enter 42. Then it prints ‘You Win!’ You may use a do while () loop here or an infinite do loop with an if (.....) exit statement
    
    implicit none 
    
    ! Assignments
    integer*4 :: n
    
    ! print a statement and recieve an input
    print*, 'Write any number between 1 and 100'
    read*, n
    
    ! use a do loop to repeat the process of using user inputs until 42 is entered
    do 
        if (n == 42) then
            EXIT
        endif
        print*, 'Write a different number between 1 and 100'
        read*, n
    enddo
    
    !finish the program
    print*, 'You Win!'
    
end program DoLoopLab3
