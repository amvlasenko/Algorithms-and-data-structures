program task_7_32v
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, M = 0
   real(R_), allocatable   :: A(:, :)

   open (file=input_file, newunit=In)
      read (In, *) N, M
      !M число столбцов, Н число строк
      allocate (A(M, N))
      read (In, *) A
   close (In)
   
   open (file=output_file, encoding=E_, newunit=Out)
      write (Out, "("//M//"f7.2)") A
      write (Out, *) ""
   close (Out)
   
   call Sort(A)
   
   open (file=output_file, encoding=E_, newunit=Out, position="append")
      write (Out, "("//M//"f7.2)") A
   close (Out)

contains
   pure subroutine Sort(A)
      real(R_)                     A(:, :)
      real(R_)                     string(M), AbsA(M)
      integer                      i, j, k

      intent(inout)                A

      do concurrent (i=1:N)
         string = A(:, i)
         AbsA = abs(string)

         do j=1, M - 1
            do k=j + 1, M
               if (AbsA(j) < AbsA(k)) then

                  AbsA([j, k]) = AbsA([k, j])
                  string([j, k]) = string([k, j])

               endif
            end do
         end do
         A(:, i) = string
      end do

   end subroutine Sort
end program task_7_32v
