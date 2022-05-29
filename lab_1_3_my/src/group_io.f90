module Group_IO
   use Environment

   implicit none
   integer, parameter :: STUD_AMOUNT   = 17
   integer, parameter :: SURNAME_LEN   = 15
   integer, parameter :: INITIALS_LEN  = 5

   ! Структура данных для хранения данных о студенте.
   type student
      character(SURNAME_LEN, kind=CH_), allocatable    :: Surnames(:)
      character(INITIALS_LEN, kind=CH_), allocatable   :: Initials(:)
      integer(I_), allocatable                         :: Years(:)
      character(kind=CH_), allocatable                 :: Registrations(:)
      character(kind=CH_), allocatable                 :: Genders(:)
   end type student
   
contains
   ! Создание неформатированного файла данных.
   subroutine Create_data_file(Input_File, Data_File)
      character(*), intent(in)   :: Input_File, data_file    
      integer                    :: In, Out, IO, i
      character(SURNAME_LEN, kind=CH_)    :: Surnames(STUD_AMOUNT)
      character(INITIALS_LEN, kind=CH_)   :: Initials(STUD_AMOUNT)
      integer(I_)                         :: Years(STUD_AMOUNT)
      character(kind=CH_)                 :: Registrations(STUD_AMOUNT)
      character(kind=CH_)                 :: Genders(STUD_AMOUNT)
      character(:), allocatable           :: format
      
      open (file=Input_File, encoding=E_, newunit=In)
         format = '(2(a, 1x), i4, 1x, a, 1x, a)'
         read (In, format, iostat=IO) (Surnames(i), Initials(i), Years(i), Registrations(i), &
            Genders(i), i=1, STUD_AMOUNT)
         call Handle_IO_status(IO, "reading formatted class list, line ")
      close (In)
      
      open (file=Data_File, form='unformatted', newunit=Out, access='stream')
         write (Out, iostat=IO) Surnames, Initials, Years, Registrations, Genders
         call Handle_IO_status(IO, "creating unformatted file with class list, record ")
      close (Out)
   
   end subroutine Create_data_file

   ! Чтение списка класса: фамилии, инициалы, полы и оценки.
   function Read_class_list(Data_File) result(Group)
      type(student)              :: Group
      character(*), intent(in)   :: Data_File
      integer In, IO
     
      allocate (Group%Surnames(STUD_AMOUNT), Group%Initials(STUD_AMOUNT), Group%Years(STUD_AMOUNT), &
         Group%Registrations(STUD_AMOUNT), Group%Genders(STUD_AMOUNT))

      open (file=Data_File, form='unformatted', newunit=In, access='stream')
      !Group
         read (In, iostat=IO) Group%Surnames, Group%Initials, Group%Years, Group%Registrations, Group%Genders
         call Handle_IO_status(IO, "reading unformatted class list")
      close (In)
   end function Read_class_list
 
   ! Вывод списка класса.
   subroutine Output_class_list(Output_File, Group, List_name, Position)
      character(*), intent(in)   :: Output_File, Position, List_name
      type(student), intent(in)  :: Group
      integer                    :: Out, IO, i
      character(:), allocatable  :: format
      
      open (file=Output_File, encoding=E_, position=Position, newunit=Out)
         write (out, '(/a)') List_name
         format = '(2(a, 1x), i4, 1x, a, 1x, a)'
         write (Out, format, iostat=IO) (Group%Surnames(i), Group%Initials(i), Group%Years(i), &
            Group%Registrations(i), Group%Genders(i), i=1, size(Group%Surnames))
         call Handle_IO_status(IO, "writing " // List_name)
      close (Out)
   end subroutine Output_class_list
end module Group_IO 
