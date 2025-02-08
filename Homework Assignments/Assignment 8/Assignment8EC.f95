program Assignment8EC
    implicit none
    ! Liam Wntenack
    ! 3/22/2021
    ! This program finds the root of a function using the secant method
    
    ! Assignments
    real*8 :: x(99999) ! midpoint and two x values
    real*8 :: f, y, yOld ! function
    real*8 ::  Tolerance ! how close the answer has to be to zero to accept the value
    integer*8 :: counter
    
    counter = 1
    
    Tolerance = 0.00000000000001
    
    print*, 'write your first x-value: '
    
    read(*, *) x(1)
    
    print*, 'write your second x-value: '
    
    read(*, *) x(2)
    
    do
        if(abs(f(x(counter + 1))) .lt. Tolerance) EXIT
        x(counter + 2) = x(counter + 1) - (f(x(counter + 1)) * (x(counter + 1) - x(counter)) / (f(x(counter + 1)) - f(x(counter))))
        counter = counter + 1
    
    enddo
    
    print*, "The root is:"
    print*, x(counter + 1)
    print*, "Your starting x-values were:"
    print*, x(1), x(2)
    print*, 'the f(x) is: '
    print*, f(x(counter + 1))
    print*, 'the number of loops required for convergence is: '
    print*, counter


end program Assignment8EC

! write in the function being tested
function f(x) result(y)
    implicit none
    
    real*8 :: x, y
    
    y = sin(x) + 1.5 - (0.15 * x)

end function f
