program Assignment2EC
    ! Liam Whitenack
    ! CDS 251
    ! Assignment 2
    ! Wednesday, February 10, 2021
    ! this program attempts to solve a series problem that diverges in real life in an attempt to see what it does in FORTRAN

    implicit none
    
    ! Assignments
    integer*4 :: n
    real*4 :: Sum
    real*4 :: oldSum
    
    n = 1
    sum = 1
    
    ! Extra credit code block
    ! Infinite do loop has the code repeat itself infinitely
    ! Every time the loop runs, add 1/n to the previous sum
    ! the loop ends when the sum no longer increases, i.e. when the sum is equal to the sum the last time the loop ran
    do
        oldSum = Sum
        n = n + 1
        sum = sum + (1.0/float(n))
        if (sum == oldSum) then
            exit
        endif
    enddo
    
    print*, sum
    
end program Assignment2EC
