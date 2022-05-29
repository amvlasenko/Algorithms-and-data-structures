module Group_Process
   use Environment
   use Group_IO

   implicit none
   
contains
   
   pure function Get_SPB_Boys(Group) result(Boys_From_SPB)
      type(student) :: Group
      type(student) :: Boys_From_SPB
      character(kind=CH_), parameter   :: MALE = Char(1052, CH_), REG = Char(1055, CH_)
      logical       :: Mask(STUD_AMOUNT)
      integer       :: i
      intent(in)    :: Group

      do concurrent (i=1:STUD_AMOUNT)
         Mask(i) = (Group%Genders(i) == MALE .AND. Group%Registrations(i) == REG)
      end do
   
      allocate (Boys_From_SPB%Surnames(count(Mask)))
      Boys_From_SPB%Surnames = Pack(Group%Surnames, Mask)
      Boys_From_SPB%Initials = Pack(Group%Initials, Mask)
      Boys_From_SPB%Years    = Pack(Group%Years, Mask)
      Boys_From_SPB%Registrations = Pack(Group%Registrations, Mask)
      Boys_From_SPB%Genders = Pack(Group%Genders, Mask)
        
   end function

   pure subroutine Get_Youngest(Boys_From_SPB, Youngest_Boys_From_SPB)
      type(student)                    :: Boys_From_SPB
      type(student)                    :: Youngest_Boys_From_SPB
      logical, allocatable             :: Boys_Mask(:)
      integer                          :: i
      intent(in) Boys_From_SPB
      intent(out) Youngest_Boys_From_SPB 


      allocate (Boys_Mask(size(Boys_From_SPB%Years)), source = .TRUE.)
      do i=1, 3
        Boys_Mask(maxloc(Boys_From_SPB%Years, Boys_Mask)) = .FALSE.
      end do
         
      Youngest_Boys_From_SPB%Surnames = Pack(Boys_From_SPB%Surnames, .NOT. Boys_Mask)
      Youngest_Boys_From_SPB%Initials = Pack(Boys_From_SPB%Initials, .NOT. Boys_Mask)
      Youngest_Boys_From_SPB%Years = Pack(Boys_From_SPB%Years, .NOT. Boys_Mask)
      Youngest_Boys_From_SPB%Registrations = Pack(Boys_From_SPB%Registrations, .NOT. Boys_Mask)
      Youngest_Boys_From_SPB%Genders = Pack(Boys_From_SPB%Genders, .NOT. Boys_Mask)

   end subroutine

end module group_process
