module Group_IO
   use Environment

   implicit none
   integer, parameter :: SURNAME_LEN   = 15
   integer, parameter :: INITIALS_LEN  = 5

   type student
      character(SURNAME_LEN, kind=CH_)    :: Surname = ""
      character(INITIALS_LEN, kind=CH_)   :: Initials = ""
      integer(I_)                         :: Year = 0
      character(kind=CH_)                 :: Registration = ""
      character(kind=CH_)                 :: Gender = ""
      type(student), pointer              :: next => Null()
   end type student
   
contains
   function Read_class_list(Input_File) result(Group)
      type(student), pointer              :: Group
      character(*), intent(in)            :: Input_File
      integer In
      open (file=Input_File, encoding=E_, newunit=In)
         Group => Read_student(In)
      close (In)
   end function Read_class_list

   recursive function Read_student(In) result(Stud)
      type(student), pointer  :: Stud
      integer, intent(in)     :: In
      integer  IO
      character(:), allocatable  :: format
      
      allocate (Stud)
      format = '(2(a, 1x), i4, 1x, a, 1x, a)'
      read (In, format, iostat=IO) Stud%Surname, Stud%Initials, Stud%Year, &
      Stud%Registration, Stud%Gender
      call Handle_IO_status(IO, "reading line from file")
      if (IO == 0) then
          Stud%next => Read_student(In)
      else
         deallocate (Stud)
         nullify (Stud)
      end if
   end function Read_student
 
   ! Вывод списка класса.
   subroutine Output_class_list(Output_File, Group, Position, List_name)
      character(*), intent(in)   :: Output_File, Position
      character(*), optional, intent(in) :: List_name
      type(student), intent(in)  :: Group
      integer                    :: Out
      
      open (file=Output_File, encoding=E_, position=Position, newunit=Out)
      if(present(List_name)) then
         write (out, '(/a)') List_Name
      end if
         call Output_student(Out, Group)
      close (Out)
   end subroutine Output_class_list
   
   recursive subroutine Output_student(Out, Stud)
      integer, intent(in)        :: Out
      type(student), intent(in)  :: Stud
      integer  :: IO
      character(:), allocatable  :: format
   
      
      format = '(2(a, 1x), i4, 1x, a, 1x, a)'
      write (Out, format, iostat=IO) Stud%Surname, Stud%Initials, Stud%Year, &
      Stud%Registration, Stud%Gender
      
      call Handle_IO_status(IO, "writing student")
      if (Associated(Stud%next)) then
         call Output_student(Out, Stud%next)
      end if
   end subroutine Output_student

   subroutine outputResponse(Output_File, Stud, Position, List_Name)
      character(*), intent(in)   :: Output_File, Position
      character(*), optional, intent(in) :: List_name
      integer                    :: Out
      type(student), intent(in)  :: Stud
      integer  :: IO
      character(:), allocatable  :: format

      format = '(2(a, 1x), i4, 1x, a, 1x, a)'

      open (file=Output_File, encoding=E_, position=Position, newunit=Out)
         if(present(List_name)) then
            write (out, '(/a)') List_Name
         end if
         write (Out, format, iostat=IO) Stud%Surname, Stud%Initials, Stud%Year, &
         Stud%Registration, Stud%Gender
      close (Out)

      call Handle_IO_status(IO, "writing student")
   end subroutine
end module Group_IO 
