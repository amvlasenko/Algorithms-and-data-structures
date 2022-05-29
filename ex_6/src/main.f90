program task_6_1g
   use Environment
   
   implicit none
   character(*), parameter :: input_file = "../data/input.txt", output_file = "output.txt"
   integer                 :: In = 0, Out = 0
   real(R_)                :: asin_x = 0, x = 0

   open (file=input_file, newunit=In)
      read (In, *) x
   close (In)
   
   asin_x = aSinXImp(x)

   open (file=output_file, newunit=Out)
      write (Out, '(4(a, T16, "= ", e13.6/))') 'x', x, "aSin(x)", asin_x, "Fortran aSin(x)", aSin(x), "Error", asin_x - aSin(x)
   close (Out)

contains
   ! Чистая функция в императивном стиле.
   real(R_) function aSinXImp(x) result(aSinX)
      real(R_), intent(in) :: x
      real(R_)    r, q, x_2, OldaSinX, x_s
      integer     n

      x_2   = x * x
      
      n     = -1
      r     = x
      aSinX  = r

      do
      print *, r, aSinX
         n        = n + 1
         q        = x_2 * ( ((n+0.5)**2) / ((n+1)*(n+1.5)) )
         r        = r * q
         OldaSinX  = aSinx
         aSinX     = aSinX + r
         if (OldaSinX == aSinx) exit
      end do
   end function aSinXImp
end program task_6_1g
