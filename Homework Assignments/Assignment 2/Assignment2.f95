program Assignment2
    ! Liam Whitenack
    ! CDS 251
    ! Assignment 2
    ! Wednesday, February 10, 2021
    ! this program will add 1 * 10^-7 to a variable stored as a real number with four bytes 
    
    implicit none
    
    
    ! Assignments
    real*4 :: varOne
    real*4 :: varTwo
    integer*4 :: i
        
    
    varOne = 1.0
    varTwo = 2.0
    
    ! Code Block
    ! This do loop adds to the original varOne and varTwo variable ten million times
    do i = 1, 10000000
        varOne = varOne + 0.0000001
    enddo 
    do i = 1, 10000000
        varTwo = varTwo + 0.0000001
    enddo 
    print*, varOne, varTwo
    
    

end program Assignment2
