program Assignment5

    implicit none

    ! Assignments
    character*50 :: inFile
    integer :: n, i, t, u, k
    real :: store1, store2
    real, allocatable :: num(:)
    integer, allocatable :: innum(:)
    logical :: Done
    
    print*, 'Write your input file: '
    
    read(*, *) inFile
    
    open(41, file = inFile)
    open(42, file = 'Output.txt')
    
    read(41, *) n
    
    allocate(num(n))
    allocate(innum(n))
    
    do i = 1, n
    
        read (41, *) num(i)
        innum(i) = i
    
    enddo
    
    
    do t = 1, n - 1
    
        do u = 1, n - t
            Done = .true.
            if (num(innum(u)) .gt. num(innum(u + 1))) then
                store1 = innum(u)
                innum(u) = innum(u + 1)
                innum(u + 1) = store1
                Done = .false.
            endif
        enddo
    
    enddo
    
    do k = 1, n
    
        write(42,*) num(innum(k))
    
    enddo
    
    
    
    close(42)
    close(41)

    
    deallocate(num)
    deallocate(innum)
    
    print *, 'Done!'

end program Assignment5
