module Group_Process
   use Environment
   use Group_IO

   implicit none
   
contains
   
   pure recursive subroutine Get_SPB_Boys(Group, Boys_From_SPB, amount)
      type(student), intent(in)        :: Group
      type(student), pointer           :: Boys_From_SPB
      character(kind=CH_), parameter   :: MALE = Char(1052, CH_), REG = Char(1055, CH_)
      integer, intent(inout)           :: amount

      if (Group%Gender == MALE .AND. Group%Registration == REG) then
         allocate (Boys_From_SPB, source=Group)
         amount = amount + 1
         Boys_From_SPB%next => Null()
         if (Associated(Group%next)) &
            call Get_SPB_Boys(Group%next, Boys_From_SPB%next, amount)
         else if (Associated(Group%next)) then
            call Get_SPB_Boys(Group%next, Boys_From_SPB, amount)
         else
            Boys_From_SPB => Null()
      end if

   end subroutine

   pure recursive subroutine Sort_class_list(ClassList, N)
      type(student), pointer, intent(inout)  :: ClassList
      integer, intent(in)                    :: N

      ! Работаем только с первыми N элементами: помещаем в ИХ конец менее успешного.
      call Drop_down(ClassList, 1, N-1)
      
      ! Если необходимо, делаем то же с первыми N-1 элементами.
      if (N >= 11) &
         call Sort_class_list(ClassList, N-1)
   end subroutine Sort_class_list

   ! Помещаем c j-ой на N-ую позицию менее успешного, поочерёдно сравнивая.
   pure recursive subroutine Drop_down(ClassList, j, N)
      type(student), pointer  :: ClassList
      integer, intent(in)                  :: j, N

      ! Если требуется, то меняем местами текущего студента со следующим.
      if (Swap(ClassList)) &
         call Swap_from_current(ClassList)
      if (j < N) &
         call Drop_down(ClassList%next, j+1, N)
   end subroutine Drop_down

   ! Проверка того, стоит ли менять местами текущего учащегося со следующим.
   pure logical function Swap(Current)
      type(student), intent(in)  :: Current

      Swap = .false.
      if (Current%Year < Current%next%Year) then
         Swap = .true.
      else if (Current%Year == Current%next%Year) then
         if (Current%Surname > Current%next%Surname) then
            Swap = .true.
         else if (Current%Surname==Current%next%Surname .and. Current%Initials>Current%next%Initials) then
            Swap = .true.
         end if
      end if
   end function Swap

   ! Перестановка местами двух эелементов списка, начиная с текущего.
   pure subroutine Swap_from_current(Current)
      type(student), pointer  :: Current

      type(student), pointer  :: tmp_stud
               
      tmp_stud       => Current%next
      Current%next   => Current%next%next
      tmp_stud%next  => Current
      Current        => tmp_stud
   end subroutine Swap_from_current

end module group_process
