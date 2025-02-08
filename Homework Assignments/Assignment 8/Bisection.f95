program Bisection
    implicit none
    ! Liam Wntenack
    ! 3/17/2021
    ! This program finds the root of a function
    
    ! Assignments
    real*8 :: m, a, b, starta, startb ! midpoint and two x values
    real*8 :: f, y ! function
    real*8 ::  Tolerance ! how close the answer has to be to zero to accept the value
    integer :: counter ! counts the number of do loop iterations
    
    Tolerance = 0.00000000000001
    
    counter = 1
    
    
    ! read in the a and b value from user input
    print*, 'Enter two x-values to find the root in between them: '
    
    print*, 'write your first x-value: '
    
    read(*, *) a
    
    print*, 'write your second x-value: '
    
    read(*, *) b
    
    starta = a
    startb = b
    
    ! check to make sure that the two values produce opposite signs
    if (f(a) * f(b) .ge. 0.0) then
            print*, 'x-values do not produce opposite signs! quitting.'
            stop
    endif
    
    
    ! run the main code block
    do
        m = (a + b) / 2.0 ! Computes x midpoint
        if (abs(f(m)) .lt. Tolerance) exit ! when the answer is close enough, quit the do loop
        if (f(a) * f(m) .gt. 0.0) then ! check to see if the signs are the same. If they are, assign a the value of the midpoint
            a = m
        else ! otherwise, assign b the value of the midpoint
            b = m
        endif
        counter  = counter + 1 ! count the number of do loops
    enddo
    
    
    ! print the results
    print*, 'the root is: '
    print*, m
    print*, "Your starting x-values were:"
    print*, starta, startb
    print*, 'the f(x) is: '
    print*, f(m)
    print*, 'the number of loops required is: '
    print*, counter


end program Bisection


! write in the function being tested
function f(x) result(y)
    implicit none
    
    real*8 :: x, y
    
    y = sin(x) + 1.5 - (0.15 * x)

end function f
