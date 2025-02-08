program Assignment11 
    implicit none
    ! Liam Wntenack
    ! 04/07/2021
    ! This program uses a simluation of random x and y values to find the value of pi.
    
    real*8 :: t, tFinal, h, y, k1, k2, k3, k4
    real*8 :: f
    integer*8 :: i, n
    
    t = 0
    h = 0.05
    tFinal = 5
    n = int(tFinal / h)
    
    open(41, file = 'Assignment11Output1.txt')
    open(42, file = 'Assignment11Output2.txt')
    open(43, file = 'Assignment11Output3.txt')
    open(44, file = 'Assignment11Output4.txt')
    open(45, file = 'Assignment11Output5.txt')
    open(46, file = 'Assignment11Output6.txt')
    open(47, file = 'Assignment11Output7.txt')
    
    print*, 'GRAPH 1'
    ! take the user input
    print*, 'Enter the initial y value:'
    read(*,*) y
    
    t = 0
    
    do i = 1, n
        k1 = f(y, t)
        k2 = f((y+((h*k2)/2)), (t+(h/2)))
        k3 = f((y+((h*k2)/2)), (t+(h/2)))
        k4 = f((y+(h*k3)), (t+h))
        y = y + (h/6) * (k1+k2+k3+k4)
        write(41,*) t, y
        t = t + h
    enddo
    
    print*, 'GRAPH 2'
    ! take the user input
    print*, 'Enter the initial y value:'
    read(*,*) y
    
    t = 0
    
    do i = 1, n
        k1 = f(y, t)
        k2 = f((y+((h*k2)/2)), (t+(h/2)))
        k3 = f((y+((h*k2)/2)), (t+(h/2)))
        k4 = f((y+(h*k3)), (t+h))
        y = y + (h/6) * (k1+k2+k3+k4)
        write(42,*) t, y
        t = t + h
    enddo
    
    print*, 'GRAPH 3'
    ! take the user input
    print*, 'Enter the initial y value:'
    read(*,*) y
    
    t = 0
    
    do i = 1, n
        k1 = f(y, t)
        k2 = f((y+((h*k2)/2)), (t+(h/2)))
        k3 = f((y+((h*k2)/2)), (t+(h/2)))
        k4 = f((y+(h*k3)), (t+h))
        y = y + (h/6) * (k1+k2+k3+k4)
        write(43,*) t, y
        t = t + h
    enddo
    
    print*, 'GRAPH 4'
    ! take the user input
    print*, 'Enter the initial y value:'
    read(*,*) y
    
    t = 0
    
    do i = 1, n
        k1 = f(y, t)
        k2 = f((y+((h*k2)/2)), (t+(h/2)))
        k3 = f((y+((h*k2)/2)), (t+(h/2)))
        k4 = f((y+(h*k3)), (t+h))
        y = y + (h/6) * (k1+k2+k3+k4)
        write(44,*) t, y
        t = t + h
    enddo
    
    print*, 'GRAPH 5'
    ! take the user input
    print*, 'Enter the initial y value:'
    read(*,*) y
    
    t = 0
    
    do i = 1, n
        k1 = f(y, t)
        k2 = f((y+((h*k2)/2)), (t+(h/2)))
        k3 = f((y+((h*k2)/2)), (t+(h/2)))
        k4 = f((y+(h*k3)), (t+h))
        y = y + (h/6) * (k1+k2+k3+k4)
        write(45,*) t, y
        t = t + h
    enddo
    
    print*, 'GRAPH 6'
    ! take the user input
    print*, 'Enter the initial y value:'
    read(*,*) y
    
    t = 0
    
    do i = 1, n
        k1 = f(y, t)
        k2 = f((y+((h*k2)/2)), (t+(h/2)))
        k3 = f((y+((h*k2)/2)), (t+(h/2)))
        k4 = f((y+(h*k3)), (t+h))
        y = y + (h/6) * (k1+k2+k3+k4)
        write(46,*) t, y
        t = t + h
    enddo
    
    print*, 'GRAPH 7'
    ! take the user input
    print*, 'Enter the initial y value:'
    read(*,*) y
    
    t = 0
    
    do i = 1, n
        k1 = f(y, t)
        k2 = f((y+((h*k2)/2)), (t+(h/2)))
        k3 = f((y+((h*k2)/2)), (t+(h/2)))
        k4 = f((y+(h*k3)), (t+h))
        y = y + (h/6) * (k1+k2+k3+k4)
        write(47,*) t, y
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
