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
   
   !Три ссылки студ_Н, три else if

   pure recursive subroutine Look_at_youngests(Boys_From_SPB, stud1, stud2, stud3)
      type(student), pointer, intent(inout)     :: Boys_From_SPB, stud1, stud2, stud3
      
      if (Boys_From_SPB%year >= stud1%year) then
         stud3 = stud2
         stud2 = stud1
         stud1 = Boys_From_SPB
      else if (Boys_From_SPB%year >= stud2%year) then
         stud3 = stud2
         stud2 = Boys_From_SPB
      else if (Boys_From_SPB%year >= stud3%year) then
         stud3 = Boys_From_SPB
      end if

      if (associated(Boys_From_SPB%next)) then
         call Look_at_youngests(Boys_From_SPB%next, stud1, stud2, stud3)
      else
         stud1%next => Null()
         stud2%next => Null()
         stud3%next => Null()
      end if

   end subroutine

end module group_process
