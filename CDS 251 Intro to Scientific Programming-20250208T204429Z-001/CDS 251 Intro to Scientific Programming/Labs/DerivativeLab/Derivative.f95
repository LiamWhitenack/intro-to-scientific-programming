program CentralDifference 
    implicit none
    ! Liam Wntenack
    ! 3/24/2021
    ! This program uses the Central Difference to 
    
    real :: y, f ! function variables
    real :: a, h ! input variables
    real :: yprime, x ! derivative graph x and y values
    integer :: i ! counter
    
    open(42, file = 'DerivitaveLabPlotOutput.txt') ! open the output file
    
    ! enter inputs
    print*, 'input a:'
    read(*, *) a 
    print*, 'input h:'
    read(*, *) h
    
    ! start do loop where the derivative of f(x) and the values of x are calculated
    do i = 0, int(4.0 / h)
        x = a + float(i - 1) * h
        yprime = (f(x + h) - f(x - h)) / (2 * h)
        write(42,*) x, yprime
    enddo
    
    close(42)

    
end program CentralDifference

! enter the function from the lab instructions
function f(x) result (y)
    implicit none
    real :: x, y
    
    y = cos(x) * ((x ** 3) - (3 * (x ** 2)))

end function f
