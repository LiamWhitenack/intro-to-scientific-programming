program Assignment6
    ! Liam Wntenack
    ! 3/7/2021
    ! Tns function will use recursive functions to use quick sort on a text file.

    
    character*50 :: inFile
    integer :: n, i, lo, hi
    real, allocatable :: A(:)
    logical :: Done
    
    print*, 'Write your input file: '
    
    read(*, *) inFile
    
    open(42, file = inFile)
    open(43, file = 'HW6Out.txt')
    
    read(42, *) n
    write(43, *) n
    
    lo = 1
    hi = n
    
    allocate(A(n))
    
    do i = 1, n
    
        read (42, *) A(i)
    
    enddo
    
    call QuickSort(A, n, lo, hi)
    
    do i = 1, n
    
        write (43, *) A(i)
    
    enddo
    
    close(42)
    close(43)
    
    deallocate(A)
    
    print *, 'Done!'


end program Assignment6

function Partition(A, n, lo, hi) result(p)

    implicit none
    
    integer :: hi, lo, n, p, i
    real :: A(n), Pivot
    real :: store1
    logical :: Done
    
    Pivot = A(hi)
    p = lo
    
    do i = lo, hi - 1
    
        if (A(i) .le. Pivot) then
        
            !swap A(P) with A(i)
            store1 = A(i)
            A(i) = A(p)
            A(p) = store1
            
            p = p + 1
            
        endif
        
    enddo
    
    ! swap A(P) with A(hi)
    store1 = A(P)
    A(P) = A(hi)
    A(hi) = store1

end function

recursive subroutine QuickSort(A, n, lo, hi)

    implicit none
    
    real :: A(n)
    integer :: n, lo, hi, p, Partition
    
    if (lo < hi) then 
        p = Partition(A, n, lo, hi)
        call QuickSort(A, n, lo, p - 1)
        call QuickSort(A, n, p + 1, hi)
    end if

end subroutine QuickSort
