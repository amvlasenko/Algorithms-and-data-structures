program task_7_12
  use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt" 
   integer                 :: In = 0, Out = 0, N = 0, i = 0, Low = 0
   real, allocatable       :: Z(:, :), SumZ(:)

   open (file=input_file, newunit=In)
      read (In, *) N
      allocate (Z(N, N))
      allocate(SumZ(N), source=.0)
      read (In, *) (Z(:,i), i = 1, N)
   close (In)
   !Массив сум(з, 2) в отдельную переменную
   SumZ = sum(Z, 2)
   Low = minloc(SumZ, 1)
   
   if (all(SumZ == SumZ(Low)) then
      Low = 0
   endif
   
   open (file=output_file, newunit=Out)
      write (Out, *) "N = ", Low
   close (Out)


end program task_7_12
