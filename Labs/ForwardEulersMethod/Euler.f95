program ForwardEuler 
    implicit none
    ! Liam Wntenack
    ! 04/07/2021
    ! This program uses a simluation of random x and y values to find the value of pi.
    
    real*8 :: tFinal1, h1, y1 ! input variables
    real*8 :: tFinal2, h2, y2 ! input variables
    real*8 :: tFinal3, h3, y3 ! input variables
    real*8 :: t1, t2, t3
    real*8 :: f
    integer*8 :: i, n1, n2, n3
    
    open(41, file = 'EulerLabOutput1.txt')
    open(42, file = 'EulerLabOutput2.txt')
    open(43, file = 'EulerLabOutput3.txt')
    
    print*, 'GRAPH 1'
    ! take the user input
    print*, 'Enter the final time'
    read(*,*) tFinal1
    print*, 'Enter the step size (h):'
    read(*,*) h1
    print*, 'Enter the initial y value:'
    read(*,*) y1
    
    n1 = int(tFinal1 / h1)
    
    do i = 1, n1
        y1 = y1 + (h1 * f(t1, y1))
        write(41,*) t1, y1
        t1 = t1 + h1
    enddo
    
    print*, 'GRAPH 2'
    ! take the user input
    print*, 'Enter the final time'
    read(*,*) tFinal2
    print*, 'Enter the step size (h):'
    read(*,*) h2
    print*, 'Enter the initial y value:'
    read(*,*) y2
    
    n2 = int(tFinal2 / h2)
    
    do i = 1, n2
        y2 = y2 + (h2 * f(t2, y2))
        write(42,*) t2, y2
        t2 = t2 + h2
    enddo
    
    print*, 'GRAPH 3'
    ! take the user input
    print*, 'Enter the final time'
    read(*,*) tFinal3
    print*, 'Enter the step size (h):'
    read(*,*) h3
    print*, 'Enter the initial y value:'
    read(*,*) y3
    
    n3 = int(tFinal3 / h3)
    
    do i = 1, n3
        y3 = y3 + (h3 * f(t3, y3))
        write(43,*) t3, y3
        t3 = t3 + h3
    enddo
    
    close(41)
    close(42)
    close(43)

    print*, 'Done!'
    
end program ForwardEuler

function f(t, y) result(answer)
    implicit none
    real*8 :: t, y, answer
    answer = 1 - (2 * y * y) + (5 * t) - (t ** 2)

end function
