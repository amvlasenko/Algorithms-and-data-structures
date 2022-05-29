module Group_Process
   ! Модуль с ЧИСТЫМИ процедурами обработки данных.
   use Environment
   use Group_IO

   implicit none

contains

pure subroutine Get_Youngest(Boys_From_SPB, Youngest_Boys_From_SPB)
      type(student), intent(inout)                  :: Boys_From_SPB(:)
      type(student), allocatable, intent(out)       :: Youngest_Boys_From_SPB(:)
      logical, allocatable                          :: Boys_Mask(:)
      integer                                       :: i
      integer, parameter                            :: INDEXES(*) = [(i, i=1, STUD_AMOUNT)]

      allocate       (Boys_Mask(size(Boys_From_SPB%Year)), source = .TRUE.)

      do i = 1, 3
            Boys_Mask(maxloc(Boys_From_SPB%Year, Boys_Mask)) = .FALSE.
      end do
            
      Youngest_Boys_From_SPB  = Pack(Boys_From_SPB, .NOT. Boys_Mask) 

end subroutine

end module group_process
