program Assignment11 
    implicit none
    ! Liam Wntenack
    ! 04/07/2021
    ! This program uses a simluation of random x and y values to find the value of pi.
    
    real*8 :: t, tFinal, h, y, y2
    real*8 :: f, Tol
    integer*8 :: i, n
    
    Tol = 0.0001
    t = 0
    h = 0.05
    tFinal = 5
    n = int(tFinal / h)
    
    open(41, file = 'Assignment11ECOutput1.txt')
    open(42, file = 'Assignment11ECOutput2.txt')
    open(43, file = 'Assignment11ECOutput3.txt')
    open(44, file = 'Assignment11ECOutput4.txt')
    open(45, file = 'Assignment11ECOutput5.txt')
    open(46, file = 'Assignment11ECOutput6.txt')
    open(47, file = 'Assignment11ECOutput7.txt')
    
    print*, 'GRAPH 1'
    ! take the user input
    print*, 'Enter the initial y value:'
    read(*,*) y
    
    t = 0
    
    do i = 1, n
        y2 = y
        do
            y2 = y + h * f(t+h, y2)
            write(41,*) t, y
            if (abs(y2 - y) .lt. Tol) exit
        enddo
        y = y2
        t = t + h
    enddo
    
    
    close(41)
    close(42)
    close(43)
    close(44)
    close(45)
    close(46)
    close(47)

    print*, 'Done!'
    
end program Assignment11

function f(t, y) result(answer)
    implicit none
    real*8 :: t, y, answer
    answer = (y ** 2) - ((y ** 3) / 5) - t
end function
