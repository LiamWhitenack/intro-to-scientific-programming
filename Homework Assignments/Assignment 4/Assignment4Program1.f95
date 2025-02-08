program Assignment4
    implicit none
   
    ! Header
        ! Liam Whitenack
        ! CDS 251
        ! Assignment 4
        ! Wednesday, February 21, 2021
        ! This program will find the inverse of a 2x2 matrix and decode a message
        
    ! Assignments
    integer :: n, x ! for do loops
    integer :: a, b, c, d ! for storing matrix values
    integer, dimension(2, 2) :: M, IM ! matrices for storing the first matrix
    integer, dimension(2) :: EM, DM ! 2 x 1 matrices for multiplication and reading from the txt file
    integer :: Determinant ! use for calling the Determinant function
    character*32 :: DS ! DecodedString
    
    ! Code Block
    
    ! read the txt file
    open(Unit=10, file="Data3.txt")
    
        read (10,*) M(1, 1 : 2), M(2, 1 : 2)
        
        ! Assign variable names for each part of the Matrix
        
        a = M(1, 1)
        b = M(1, 2)
        c = M(2, 1)
        d = M(2, 2)
        
        ! change the spots for each spot in the matrix
        
        IM(1, 1) = d
        IM(1, 2) = (-1 * b)
        IM(2, 1) = (-1 * c)
        IM(2, 2) = a
        
        ! calculate the inverse matrix
        
        IM = IM / Determinant(M, 2)
        
        ! start a do loop that reads the rest of the file
        
        do n = 1, 16
            
            read (10,*) EM(1)
            
            read (10,*) EM(2)
            
            DM(1) = (EM(1) * IM(2, 1)) + (EM(2) * IM(2, 2))
            DM(2) = (EM(1) * IM(1, 1)) + (EM(2) * IM(1, 2))
            
            x = n * 2 - 1
            
            DS(x:x) = char(DM(2))
            
            x = x + 1
            
            DS(x:x) = char(DM(1))
        
        enddo
        
        
    ! output the solution    
    print*, DS
        
    close(10) 
    
    
end program Assignment4



function Determinant(M, n) result(Det)

   implicit none
   
   ! Assignments
   integer :: n
   integer M(n, n)
   integer :: Det
   
    Det = (M(1, 1) * M(2, 2)) - (M(1, 2) * M(2, 1))

      
end function Determinant
