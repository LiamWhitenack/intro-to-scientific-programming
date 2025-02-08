program Histogram 
    implicit none
    ! Liam Wntenack
    ! 04/07/2021
    ! Write a program that reads ”Bumps.txt” and creates a histogram file called ”BumpsHist.txt”

    real*8 :: BoxWidth, HistStart, HistEnd, HistRange
    real*8 :: x, Num
    integer :: nBoxes, BadCount, iHist
    integer :: i
    integer*8 :: Hist(20000)
    
    HistEnd = 30
    HistStart = 10
    nBoxes = 100
    HistRange = HistEnd - HistStart
    BoxWidth = HistRange / nBoxes
    
    ! take the user input
    open(42, file = 'Bumps.txt')
    open(43, file = 'BumpsHist.txt')
    
    do i = 1, 20000
        read(42,*) Num
        iHist = Int((Num-Histstart) * float(nBoxes) / HistRange)
        if (iHist .ge. 1 .and. iHist .le. nBoxes) then
            Hist(iHist) = Hist(iHist) + 1
        else
            BadCount = BadCount + 1
        endif
    enddo

    
    do i = 1, nBoxes
        x = (float(i) - 0.5) * BoxWidth + HistStart
        write(43,*) x, Hist(i)
    enddo
    
    print*, 'Done!'
    
    close(42)
    close(43)
    
end program Histogram
