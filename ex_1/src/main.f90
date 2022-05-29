program task_1_8
   use Environment

   implicit none
   character(*), parameter       :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                       :: In = 0, Out = 0
   real(R_)                      :: Arr(3) = 0
   
   
   open (file=input_file, newunit=In)
      read (In, *) Arr
   close (In)
  
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, *) "Before:", Arr
   close (Out)
   
   Arr = CShift(Arr, -1)
  
   open (file=output_file, encoding=E_, newunit=Out, position="append")
      write (Out, *) "After:", Arr
   close (Out)


end program task_1_8
