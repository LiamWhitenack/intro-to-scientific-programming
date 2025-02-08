program Amino
    implicit none
    ! Liam Whitenack
    ! 4/30/2021
    ! This program reads one letter codes from a text file, counts the number of different amino acids present, and outputs it all to a .txt file
    
    ! Assignments
    character*1, allocatable :: Acids(:)
    character*3 :: newAcid(50)
    integer*8 :: i, n, a, b, acidValue(50), counter, x
    
    counter = 0
    
    b = 1
    
    ! make an array to store all of the new acid types in
    do i = 1, 50
        acidValue(i) =  0
    enddo
    
    ! make an array of bins to count the number of times an acid is repeated
    do i = 1, 50
        newAcid(i) =  "X"
    enddo
    
    ! open the file
    
    open(Unit=42, file="AAsequenceData.txt")
    
        ! read the first line. It contains the number of values. once it is read, it is not used any more
        read (42,*) n
        
        ! allocate the array
        allocate(Acids(n))
        
        ! make a do loop that reads in every line
        do i = 1, n
            
            !read the values onto the array
            read (42,*) Acids(i)
            
            
            ! read the array, storing the values into the empty slots of the array
            do a = 1, b
                ! if the value of the array is not taken, store the new value inside. If not, check to see if the value has been taken. Once this is done, there is no reason to contine with the loop.
                if(newAcid(a) == "X") then
                    newAcid(a) = Acids(i)
                    b = b + 1
                    EXIT
                ! if the value that was just read already has a spot, add one to the bin. Exit so that the new value isn't stored in a different slot.
                elseif(newAcid(a) == Acids(i)) then
                    acidValue(a) = acidValue(a) + 1
                    EXIT
                endif
            enddo
        end do
        
    close(42)
    
    ! count the number of different acids
    do i = 1, 50
        if(newAcid(i) /= "X") then
            counter = counter + 1
        endif
    enddo
    
    
    ! rename the acids to their three letter code
    do i = 1, 22
        if(newAcid(i) == "A") then
            newAcid(i) = "ala"
        elseif(newAcid(i) == "R") then
            newAcid(i) = "arg"
        elseif(newAcid(i) == "N") then
            newAcid(i) = "asn"
        elseif(newAcid(i) == "D") then
            newAcid(i) = "asp"
        elseif(newAcid(i) == "B") then
            newAcid(i) = "arx"
        elseif(newAcid(i) == "C") then
            newAcid(i) = "cys"
        elseif(newAcid(i) == "E") then
            newAcid(i) = "glu"
        elseif(newAcid(i) == "Q") then
            newAcid(i) = "gln"
        elseif(newAcid(i) == "Z") then
            newAcid(i) = "glx"
        elseif(newAcid(i) == "G") then
            newAcid(i) = "gly"
        elseif(newAcid(i) == "H") then
            newAcid(i) = "his"
        elseif(newAcid(i) == "I") then
            newAcid(i) = "ile"
        elseif(newAcid(i) == "L") then
            newAcid(i) = "leu"
        elseif(newAcid(i) == "K") then
            newAcid(i) = "lys"
        elseif(newAcid(i) == "M") then
            newAcid(i) = "met"
        elseif(newAcid(i) == "F") then
            newAcid(i) = "phe"
        elseif(newAcid(i) == "P") then
            newAcid(i) = "pro"
        elseif(newAcid(i) == "S") then
            newAcid(i) = "ser"
        elseif(newAcid(i) == "T") then
            newAcid(i) = "thr"
        elseif(newAcid(i) == "W") then
            newAcid(i) = "trp"
        elseif(newAcid(i) == "Y") then
            newAcid(i) = "tyr"
        elseif(newAcid(i) == "V") then
            newAcid(i) = "val"
        endif
    enddo
    
    
    
    deallocate(Acids)
    
    open(43, file='AcidTable.txt')
    
    ! output to make the histogram
    do i = 1, counter
        x = (float(i) - 0.5) + 1
        write(43,*)x, acidValue(i), newAcid(i)
    enddo
    
    close(43)
    
    ! print done when the assignment is complete
    print*, 'The number of different acids is', counter
    print*, 'Done!'

end program Amino
