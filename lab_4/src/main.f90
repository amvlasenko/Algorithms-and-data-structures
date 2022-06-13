program lab_2_12
   use Environment
   use Matrix_Process
   use String_IO

   implicit none
   character(:), allocatable :: F1, F2
   integer                   :: i = 1, result
   type(element), pointer    :: matrix  => Null()

   F1 = "../data/input.txt"
   F2 = "output.txt"

   matrix => Read_Matrix(F1)
   
   ! haystack => Read_Source_Code(F1)
   ! needle  => Read_Source_Code(F2)
   print *, "Hi"

end program lab_2_12
