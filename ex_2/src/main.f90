program task_2_10
   use Environment
   use IEEE_Arithmetic
   implicit none
   character(*), parameter       :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                       :: In = 0, Out = 0
   integer                       :: l = 0, k = 0, q
   character(:), allocatable     :: message

   open (file=input_file, newunit=In)
      read (In, *) l, k
   close (In)

   if (Checker(l, k)) then
      message = "l кратно k"
   else
      message = "l не кратно k"
   end if

   open (file=output_file, newunit=Out)
      write(*, *) message
   close (Out)
  
contains

   pure function Checker(l, k)
      integer q, l, k
      logical Checker
      intent(in) l, k

      q = l/k
        
      if (q * k == l) then
         Checker = .true.
      else
         Checker = .false.
      end if

   end function Checker



end program task_2_10
