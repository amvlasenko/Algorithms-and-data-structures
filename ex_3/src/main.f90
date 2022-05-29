program task_3_2
   use Environment
   implicit none
   character(*), parameter       :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                       :: In = 0, Out = 0, k, m, i
   character(:), allocatable     :: message
   integer, allocatable          :: Arr(:)


   open (file=input_file, newunit=In)
      read (In, *) k
   close (In)


   Arr=[(i, i=2, k)]
  
   if (k < 0) then
      message = "Ошибка, k < 0"
      open (file=output_file, newunit=Out)
       write(*, *) message
      close (Out)
   else
      m = Product(Arr)
      open (file=output_file, newunit=Out)
       write(*,*) "k!=", m
      close (Out)
   end if

     
end program task_3_2
