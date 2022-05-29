program task_7_50
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, i = 0, j = 0
   real(R_), allocatable   :: Z(:, :)

   open (file=input_file, newunit=In)
      read (In, *) N
      allocate (Z(N, N))
      read (In, *) Z
   close (In)

   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, '('//N//'f6.2)') Z
   close (Out)

   do concurrent (j = 1:N-1)
      Z(j:N,j) = Z(j+1:N, j)
   end do
   
   open (file=output_file, encoding=E_, newunit=Out, position='append')
      write(Out, *) "Результат:"
      write (Out, '('//N-1//'f6.2)') (Z(1:N-1, i), i = 1, N)
   close (Out)

end program task_7_50
