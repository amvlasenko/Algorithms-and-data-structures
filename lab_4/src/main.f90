program lab_2_12
   use Environment
   use Matrix_Process
   use Matrix_IO

   implicit none
   character(:), allocatable :: F1, F2
   integer                   :: M, N
   type(element), pointer    :: matrix  => Null()

   F1 = "../data/input.txt"
   F2 = "output.txt"

   matrix => Read_Matrix(F1, M, N)
   call printer(matrix)
   call outputMatrix(F2, matrix, M, N)

end program lab_2_12
