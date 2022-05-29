program task_7_17g
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, N = 0, M = 0, i = 0
   real(R_), allocatable   :: C(:, :)
   real(R_)                :: min_value = 0
   integer, allocatable    :: Indexes(:, :), Ind_min_value(:, :)
   logical, allocatable    :: Mask(:)

   open (file=input_file, newunit=In)
      read (In, *) N, M
      allocate (C(N, M))
      read (In, *) (C(i, :), i = 1, N)
   close (In)
   
   open (file=output_file, newunit=Out)
      write (Out, '('//M//'f6.2)') (C(i, :), i = 1, N)
   close (Out)

   allocate (Indexes(N*M, 2))
   allocate (Mask(N*M), source=.false.)
  
 
   call AbsoluteMinPos(C, min_value, Mask, Indexes, Ind_min_value)
   
   open (file=output_file, newunit=Out, position='append')
      write (Out, '(a, f6.2)') "Наименьшее по модулю:", min_value
      write (Out, '(2i3)') (Ind_min_value(i, :), i = 1, UBound(Ind_min_value, 1))
   close (Out)

contains

   pure subroutine AbsoluteMinPos(C, min_value, Mask, Indexes, Ind_min_value)
      real(R_), intent(in)    :: C(:, :)
      real(R_), intent(out)   :: min_value
      integer, intent(out)    :: Indexes(:, :)
      integer, allocatable, intent(out) :: Ind_min_value(:, :)
      logical, intent(out)    :: Mask(:)
      integer N_min_value, i, j

     
      Indexes(:, 1) = [((i, i = 1, N), j = 1, M)]
      Indexes(:, 2) = [((j, i = 1, N), j = 1, M)]

      min_value = minval(abs(C))

      Mask        = [abs(C) == min_value]
      N_min_value   = Count(Mask)

      allocate(Ind_min_value(N_min_value, 2))

      Ind_min_value = Reshape( Pack(Indexes, Spread(Mask, 2, 2)), [N_min_value, 2])
 
   end subroutine AbsoluteMinPos
end program task_7_17g
