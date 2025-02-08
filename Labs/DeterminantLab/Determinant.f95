program MainProgram

   implicit none
   
    ! Liam Whitenack
    ! CDS 251
    ! Assignment 2
    ! Wednesday, February 10, 2021
    ! This program will find the determinant of a matrix with two or three different rows and columns
   
   ! Assignments
   integer :: n
   real, allocatable :: Matrix1(:,:)
   ! give the Determinant a number type so that FORTRAN knows what type of number it will return
   real :: detM, Determinant
   
        ! Enter the Matrices

   ! Enter the number of rows and columns in the matrix
   n = 2
   
   ! allocate the array
   allocate(Matrix1(n,n))
   
   ! Enter the values for the array
   Matrix1(1,1:2) = (/  3.0, 11.0 /)
   Matrix1(2,1:2) = (/ 2.0, 7.0 /)
   
   
   ! enter the print statement that prints the finished value of the Determinant
   print*,'2x2 Determinant is: ', Determinant(Matrix1,n)
   
   !deallocate the Matrix
   deallocate(Matrix1)
   
   ! Enter the number of rows and columns in the matrix
   n = 3
   
   ! allocate the array
   allocate(Matrix1(n,n))
   
   ! Enter the values for the array
   Matrix1(1,1:3) = (/  1.0,  4.0,  3.0 /)
   Matrix1(2,1:3) = (/ -1.0,  2.0,  1.0 /)
   Matrix1(3,1:3) = (/  2.0,  2.0,  3.0 /)
   
   ! enter the print statement that prints the finished value of the Determinant
   print*,'3x3 Determinant is: ', Determinant(Matrix1,n)
   
   !deallocate the Matrix
   deallocate(Matrix1)
      
end program MainProgram

! Start a new function that solves for the determinant of any matrix plugged into the file
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
        
        
   ! Write this function so that it can compute the determinant of a 2x2 or
   ! 3x3 matric depending on the value of n.

      
end function Determinant
