program task_8_2
   use Environment
   use Array_IO
   use Array_process

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: M = 0, N = 0
   integer, allocatable    :: B(:, :), C(:), Res(:)

   call ReadParams(input_file, M, N,B, C) 
 
   B = FormMatrix(M,N)
   Res = MultiplyMatrix(B, C)
   
   call WriteResponse(output_file, Res)

end program task_8_2
