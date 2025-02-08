program CramersRule

   ! System of equations. 2x2, 3x3
   ! The main program is written for you. Read through the comments and
   ! see how the main program works.
   ! 2 Special Notes!!!!!
   ! 1: Take note of how the logial variable 'Success' will either write
   !    the solution or 'No Solution' to the output file.
   ! 2: Take note of how inside the do loop, allocating and deallocating
   !    memory for the arrays Matrix1, b, and x are done so the amount of
   !    memory allocated changes for each system. You cannot allocate more
   !    memory for an array until currently allocated memory is deallocated.
   
   implicit none
   
   ! Declare varialble
   integer :: n, row, col, i
   real, allocatable :: Matrix1(:,:), b(:), x(:)
   real :: detA, detM, determinant
   logical :: Success
   
   ! Open the input and output files.
   open(42,file='Data2.txt')
   open(43,file='Data2Out.txt')
   
   ! Solve each system in the input files.
   do
      ! Read in size of first system.
      read(42,*) n
      if (n .eq. 0) exit  ! Quit if zero.
      
      ! Allocate memory for system, right hand side, and solution vector.
      allocate(Matrix1(n,n), b(n), x(n))
      
      ! Read in the system. Ask if you do not understand how this works!
      do row = 1, n
         read(42,*) (Matrix1(row, col), col = 1, n), b(row)
      enddo
      
      ! Use cramers rule to get solution.
      call Cramer(Matrix1, b, n, x, Success)
      
      if (Success) then
         ! Write solution to file
         do row = 1, n
            write(43,*) x(row)
         enddo
         write(43,*)
      else ! This happens when there is no unique solution.
         write(43,*) 'No Solution'
         write(43,*)
      endif
      
      ! clean up memory and go back up to top for next system.
      deallocate(Matrix1, b, x)
      
   enddo
   
   ! close files
   close(42)
   close(43)

end program CramersRule

subroutine Cramer(M, b, n, x, Success)

   ! This subroutine does Cramer's Rule
   implicit none
   
   ! Declare and initialize your variables first.
    integer :: n, i
    real :: M(n,n), b(n), MatOut(n,n), detA, detM, Determinant, x(n), Matrix1(n, n)
    real, allocatable :: xi(:, :)
    logical :: Success
    Success = .true.
    
   ! Find the determinant of M first. print it to screen.
   ! If it is zero, set the Success logical variable and quit.
   detM = Determinant(M, n)
   print*, 'all of the determinants of M are:'
   print*, detM
    if (detM == 0) then
        Success = .false.
        return
    endif
      
   ! Allocate memory for a working matrix for column substituion. Then, for each
   ! column, i, substitute column i with vector b and get that determinant. 
   ! Compute the ith solution.
   
    allocate(xi(n, n))

   
   do i = 1, n
    
    call ColumnInsert(M, b, n, i, MatOut)
    detA = Determinant(MatOut, n)
    
    x(i) = (detA / detM)
   
   
   enddo
   
   deallocate(xi)
      
   ! deallocate memory for the working matrix.
   
      
end subroutine Cramer

subroutine ColumnInsert(M, b, n, col, MatOut)

    implicit none
    
   ! This subroutine takes vector b and inserts in into matrix M at column col.
   ! Don't forget to set MatOut = M before you substitute the column in.
    integer :: n, row, col! used to count in do loop
    real :: M(n, n), MatOut(n,n), b(n) ! where Mx = b and MatOut is Mi
                
        MatOut = M
        
        do row = 1, n
        
            MatOut(row, col) = b(row)
            
        enddo
    
    
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
