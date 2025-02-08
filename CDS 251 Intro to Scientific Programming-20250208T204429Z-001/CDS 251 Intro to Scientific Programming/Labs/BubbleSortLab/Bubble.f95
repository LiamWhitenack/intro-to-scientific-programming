program Bubble

    implicit none

    ! Assignments
    character*50 :: inFile
    integer :: n, i, t, u, k
    real :: store1, store2
    real, allocatable :: num(:)
    
    print*, 'Write your input file: '
    
    read(*, *) inFile
    
    open(42, file = inFile)
    open(43, file = 'BubbleOut.txt')
    
    read(42, *) n
    
    allocate(num(n))
    
    do i = 1, n
    
        read (42, *) num(i)
    
    enddo
    
    
    do t = 1, n
    
        do u = 1, n - 1
        
            if (num(u) .gt. num(u + 1)) then
                store1 = num(u)
                num(u) = num(u + 1)
                num(u + 1) = store1
            endif
            
        
        enddo
    
    enddo
    
    do k = 1, n
    
        write(43,*) num(k)
    
    enddo
    
    
    
    close(43)
    close(42)

    
    deallocate(num)

end program Bubble
