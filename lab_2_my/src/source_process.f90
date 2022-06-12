module Source_Process
   use Environment
   use Source_IO

   implicit none

contains
   pure recursive integer function KMP(h, n, i) result(index)
      type(SourceLine), intent(in) :: h, n
      integer, intent(in) :: i
      if ((.not. associated(n%next)) .and. n%char == h%char) then
         index = i
      else if (n%char /= h%char) then
         index = KMP(h%next, n, i + 1)
      else if (n%char == h%char) then
         index = KMP(h%next, n%next, i)
      end if
      !Проверять есть ли h, нужна вторая рекурсия чтоб не потерять начало, вторая логическая смотрит все ли ок дальше по слову 
   end function
end module Source_process
