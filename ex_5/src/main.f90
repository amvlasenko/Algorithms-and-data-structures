program task_5_3
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, M = 0
   real(R_)                :: Average = 0
   integer, allocatable    :: Z(:)
   logical, allocatable    :: Neg(:)

   open (file=input_file, newunit=In)
      read (In, *) N
      allocate (Z(N))
      read (In, *) Z
   close (In)

   allocate(Neg(N))
   call Negative(Z, Neg, Average, M)

   open (file=output_file, newunit=Out)
      write (Out, "(i0)") N
      write (Out, "("//N//"(i0, 1x))") Z
      
      write (Out, '(/2(a, T12, "= ", i0/))') 'Neg. items', M
      write (Out, '(/2(a, T12, "= ", f0.2/))') 'Average', Average
   close (Out)

contains
  
   pure subroutine Negative(Z, Neg, Average, M)
      integer     Z(:), M
      real(R_)    Average
      logical     Neg(:)
      intent(in)  Z
      intent(out) Neg, Average, M

      Neg = Z < 0
      M = Count(Neg)
      Average = real(Sum(Z, Neg, R_)) / M
   end subroutine Negative

end program task_5_3
