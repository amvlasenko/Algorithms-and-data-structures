module String_Process
   use Environment
   use String_IO

   implicit none

contains
   pure recursive integer function KMP(h, n, i) result(index)
      type(String), intent(in) :: h, n
      integer, intent(in) :: i
      logical :: isMatch

      isMatch = prefix(h, n)

      if (isMatch) then
         index = i
      else if (associated(h%next)) then
         index = KMP(h%next, n, i + 1)
      else
         index = 0
      end if
   end function

   pure recursive logical function prefix(h, n) result(isMatch)
      type(String), intent(in) :: h, n

      if (n%char == h%char .AND. (.not. associated(n%next))) then
         isMatch = .true.
      else if (n%char == h%char) then
         isMatch = prefix(h%next, n%next)
      else 
         isMatch = .false.
      end if
   end function
end module String_process
