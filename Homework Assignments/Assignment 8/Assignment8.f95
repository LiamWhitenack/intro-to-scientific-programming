program Assignment8
    implicit none
    ! Liam Wntenack
    ! 3/22/2021
    ! This program finds the root of a function using newton's method
    
    ! Assignments
    real*8 :: x, startX
    real*8 :: df, f, dy, y ! function
    real*8 ::  Tolerance ! how close the answer has to be to zero to accept the value
    integer*8 :: counter
    
    counter = 1
    
    Tolerance = 0.00000000000001
    
    print*, 'write your x-value: '
    
    read(*, *) x
    
    startX = x
    
    do
        if(abs(f(x)) .lt. Tolerance) EXIT
        x = x + (f(x) / df(x))
        counter = counter + 1
    
    enddo
    
    print*, "The root is:"
    print*, x
    print*, "Your starting x-values was:"
    print*, startX
    print*, 'the f(x) is: '
    print*, f(x)
    print*, 'the number of loops required for convergence is: '
    print*, counter


end program Assignment8


! write in the function being tested
function f(x) result(y)
    implicit none
    
    real*8 :: x, y
    
    y = sin(x) + 1.5 - (0.15 * x)

end function f

function df(x) result(dy)
    implicit none
    
    real*8 :: x, dy
    
    dy = -cos(x) - (0.15)
    
end function df
