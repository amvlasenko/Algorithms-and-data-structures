program task_7_2a
   use Environment

   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0, M = 0
   real(R_), allocatable   :: A(:), Negatives(:)
   logical, allocatable    :: Pos(:)


      read (In, *) M
      allocate (A(M))
      read (In, *) A
   close (In)

   open (file=output_file, newunit=Out)
      write (Out, "("//M//"f6.2)") A
   close (Out)
 
   allocate(Pos(M), source = .false.)
   allocate(Negatives(M))
   call SortNegatives(A, Pos, Negatives)
   
   open (file=output_file, newunit=Out, position='append')
      write (Out, "(/"//M//"f6.2)") A
   close (Out)


contains
   subroutine SortNegatives(A, Pos, Negatives)
      real, intent(inout) :: A(:), Negatives(:)
      logical, intent(inout)  :: Pos(:)
      real, allocatable :: New(:)
      real :: tmp
      integer  :: i, MinInd

      Pos = A > 0
      A = [pack(A, .not. Pos), pack(A, Pos)]
      
      do i = 1, COUNT( .not. Pos) - 1
         MinInd = minloc(A(i:), 1) + i-1
         if (i /= MinInd) then
            tmp       = A(i)
            A(i)      = A(MinInd)
            A(MinInd) = tmp
         end if
      end do
      
   end subroutine SortNegatives

end program task_7_2a
