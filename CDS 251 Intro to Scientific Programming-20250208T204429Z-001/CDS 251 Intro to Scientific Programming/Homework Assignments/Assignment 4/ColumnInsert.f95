program Cramer

   ! This subroutine does Cramer's Rule
   implicit none
   
   ! Declare and initialize your variables first.
    integer :: n, col
    real, allocatable :: M(:,:), b(:), MatOut(:,:)
    real :: x
    
    call ColumnInsert(M, b, n, col, MatOut)
      
   ! Find the determinant of M first. print it to screen.
   ! If it is zero, set the Success logical variable and quit.

      
   ! Allocate memory for a working matrix for column substituion. Then, for each
   ! column, i, substitute column i with vector b and get that determinant. 
   ! Compute the ith solution.

      
   ! deallocate memory for the working matrix.
   
      
end program Cramer

subroutine ColumnInsert(M, b, n, col, MatOut)

    implicit none
    
   ! This subroutine takes vector b and inserts in into matrix M at column col.
   ! Don't forget to set MatOut = M before you substitute the column in.
    integer :: n ! used to count in do loop
    integer :: col ! number of rows, listed before the matrix
    real, allocatable :: M(:, :), MatOut(:,:), b(:) ! where Mx = b and MatOut is Mi

    open(Unit=44, file="Data2.txt")
    
        read (44,*) col
        
        !allocate(M(col, col))
        !allocate(MatOut(col, col))
        !allocate(b(col))
        
        do n = 1, col
        
            !read(44,*) M(n, 1 : col), b(n)
            MatOut(n, 1 : col) = M(n, 1 : col)
            MatOut(n, col) = b(n)
            
        enddo
    
    close(44)
    
    !deallocate(M)
    !deallocate(MatOut)
    !deallocate(b)
    
end subroutine ColumnInsert

function Determinant(M, n) result(Det)

   implicit none
   
   ! Assignments
   integer :: n
   real M(n, n)
   real :: Det
   
   ! Start an if function for 2x2 Matrrices
   if (n == 2) then
        ! Use the formula for a 2x2 Matrix
        Det = (M(1, 1) * M(2, 2)) - (M(1, 2) * M(2, 1))
    
    ! Start a new function for 3x3 Matrices
    else if (n == 3) then
        
        ! Use the formula for a 3x3 Matrix
        Det = (M(1, 1) * M(2, 2) * M(3, 3)) &
        + (M(1, 2) * M(2, 3) * M(3,1)) &
        + (M(1, 3) * M(2, 1) * M(3,2)) &
        - (M(1, 1) * M(2, 3) * M(3, 2)) &
        - (M(1, 2) * M(2, 1) * M(3, 3)) &
        - (M(1, 3) * M(2, 2) * M(3, 1))
    endif
        
end function Determinant
