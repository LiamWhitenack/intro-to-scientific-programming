program RiemannIntegration 
    implicit none
    ! Liam Wntenack
    ! 3/24/2021
    ! This program finds the integral of a function using midpoint rule
    
    ! file:///E:/Downloads/School/George%20Mason/2021%20Spring/CDS%20251%20Intro%20to%20Scientific%20Programming/Class%20Notes/Week%209.1.pdf
    
    real*8 :: y, f, integralf, integraly ! function variables
    real*8 :: a, b, h ! input variables
    integer :: n ! the number of rectangles being used
    real*8 :: x, xLeft, xMiddle, xRight
    real*8 :: midarea, simparea, exactarea
    real*8 :: aError1, aError2, rError1, rError2
    integer :: i ! counter
    
    ! enter inputs
    print*, 'input your first limit of integration:'
    read(*, *) a 
    print*, 'input your second limit of integration:'
    read(*, *) b
    print*, 'What increment do you want?'
    read(*, *) h
    
    midarea = 0.0
    simparea = 0.0
    n = int((b - a) / h)
    
    ! start do loop where the integral of f(x) from a to b is calculated using the midpoint rule
    do i = 1, n
        x = a + float(i - 1) * h + 0.5 * h
        midarea = midarea + (h * f(x))
    enddo
    
    ! calculate Simpson's Rule Integral
    do i = 1, n
        xLeft = a + float(i - 1) * h
        xMiddle = a + float(i - 1) * h + 0.5 * h
        xRight = a + float(i) * h
        simparea = simparea + (h / 6) * (f(xLeft) + (4 * f(xMiddle)) + f(xRight))
    enddo
    
    ! calculate the exact area
    exactarea = integralf(b) - integralf(a)
    
    ! calculate the absolute error for the midpoint (aError1) and the Simpson's Rule (aError2)
    aError1 = abs(exactarea - midarea)
    aError2 = abs(exactarea - simparea)
    
    ! calculate the relative error for the midpoint (aError1) and the Simpson's Rule (aError2)
    rError1 = abs(1 - (midarea / exactarea))
    rError2 = abs(1 - (simparea / exactarea))
    
    ! enter the output
    print*, "The exact area is:", exactarea
    print*, "MIDPOINT RULE"
    print*, "The estimated area is:", midarea
    print*, "The absolute error is:", aError1
    print*, "The relative error is:", rError1
    print*, "SIMPSON'S RULE"
    print*, "The estimated area is:", simparea
    print*, "The absolute error is:", aError2
    print*, "The relative error is:", rError2
    
end program RiemannIntegration

! enter the function from the instructions
function f(x) result (y)
    implicit none
    real*8 :: x, y
    
    y = exp(-x) * ((sin(x)) ** 2)

end function f

! write the exact integral function from the notes
function integralf(x) result (integraly)
    implicit none
    real*8 :: x, integraly
    
    integraly = (exp(-x) / 10) * (cos(2 * x) - (2 * sin(2 * x)) - 5)

end function integralf
