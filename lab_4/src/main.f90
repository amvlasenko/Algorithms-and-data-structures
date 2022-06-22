program lab_4
   use Environment
   use Matrix_Process
   use Matrix_IO

   implicit none
   character(:), allocatable :: F1, F2
   integer                   :: M, N
   type(element), allocatable    :: matrix 

   F1 = "../data/input.txt"
   F2 = "output.txt"

   call Read_Matrix(F1, matrix, M, N)
   if(allocated(matrix)) then
       call printer(matrix)
      call outputMatrix(F2, matrix, N)
   end if

end program lab_4
