module Group_IO
   use Environment

   implicit none
   integer, parameter :: STUD_AMOUNT   = 17
   integer, parameter :: SURNAME_LEN   = 15
   integer, parameter :: INITIALS_LEN  = 5

   ! Структура данных для хранения данных о студенте.
   type student
      character(SURNAME_LEN, kind=CH_)    :: Surname              = ""
      character(INITIALS_LEN, kind=CH_)   :: Initials              = ""
      integer(I_)                         :: Year                  = 0
      character(kind=CH_)                 :: Registration          = ""
      character(kind=CH_)                 :: Gender                = ""  
   end type student
   
contains
   ! Создание неформатированного файла данных.
   subroutine Create_data_file(Input_File, Data_File)
      character(*), intent(in)   :: Input_File, data_file    
      type(student)              :: stud
      integer                    :: In, Out, IO, i, recl
      character(:), allocatable  :: format
      
      open (file=Input_File, encoding=E_, newunit=In)
      recl = (SURNAME_LEN + INITIALS_LEN)*CH_ + I_ + CH_ + CH_
      open (file=Data_File, form='unformatted', newunit=Out, access='direct', recl=recl)
         format = '(2(a, 1x), i4, 1x, a, 1x, a)'
         do i = 1, STUD_AMOUNT
            read (In, format, iostat=IO) stud

            call Handle_IO_status(IO, "reading formatted class list, line " // i)
            
            write (Out, iostat=IO, rec=i) stud
            call Handle_IO_status(IO, "creating unformatted file with class list, record " // i)
         end do
      close (In)
      close (Out)
   end subroutine Create_data_file

   ! Чтение списка класса: фамилии, инициалы, полы и оценки.
   function Read_class_list(Data_File) result(Group)
      type(student)                 Group(STUD_AMOUNT)
      character(*), intent(in)   :: Data_File

      integer In, IO, recl
      
      recl = ((SURNAME_LEN + INITIALS_LEN)*CH_ + I_ + CH_ + CH_) * STUD_AMOUNT
      open (file=Data_File, form='unformatted', newunit=In, access='direct', recl=recl)
         read (In, iostat=IO, rec=1) Group
         call Handle_IO_status(IO, "reading unformatted class list")
      close (In)
   end function Read_class_list
 
   ! Вывод списка класса.
   subroutine Output_class_list(Output_File, Group, List_name, Position)
      character(*), intent(in)   :: Output_File, Position, List_name
      type(student), intent(inout)  :: Group(:)

      integer                    :: Out, IO
      character(:), allocatable  :: format 
      
      open (file=Output_File, encoding=E_, position=Position, newunit=Out)
         write (out, '(/a)') List_name
         call Write_class_list(Out, Group, 1)
         call Handle_IO_status(IO, "writing " // List_name)
      close (Out)
   end subroutine Output_class_list

   recursive subroutine Write_class_list(Out, Group, start_index)
   type(student), intent(inout)              :: Group(:)
   integer                    :: Out, IO, start_index
   character(:), allocatable  :: format 
   intent (in)                   Out, start_index
 
   format = '(2(a, 1x), i4, 1x, a, 1x, a)'

   if (start_index <= Size(Group)) then
      write (Out, format, iostat=IO) Group(start_index)
      call Handle_IO_status(IO, "writing users list")
      call Write_class_list(Out, Group, start_index + 1)
   end if

   end subroutine
end module Group_IO 
