module String_IO
   use Environment

   implicit none
   
   integer, parameter                 :: max_len = 1024

   type String
      character(kind=CH_)             :: char =""
      type(String), pointer           :: Next  => Null()
   end type String

contains
   ! Чтение исходного кода. 
   function Read_Source_Code(InputFile) result (Code)
      type(String), pointer      :: Code
      character(*), intent(in)   :: InputFile
      integer                    :: In
      
      open (file=InputFile, encoding=E_, newunit=In)
         Code => Read_String(in)
      close (In)
   end function Read_Source_Code

  ! Чтение строки из файла
   function Read_String(In) result(str)
      integer, intent(in)                  :: In
      integer                              :: IO, lenght_string
      type(String), pointer                :: str
      character(MAX_LEN, CH_)              :: input_string

      read (In, "(a)", iostat=IO) input_string
      call Handle_IO_status(IO, "read character")
     
      lenght_string = len_trim(input_string)

      print *, trim(input_string)
      
      if ( lenght_string >= 1 ) then
         str => Create_String_List(trim(input_string), 1, lenght_string)
      else
         str => Null()
      end if
   end function Read_String

   ! Рекурсивное чтение исходной строки и перевод ее в модель String
   recursive function Create_String_List(input_str, index, count) result(str)
      character(*, kind=CH_), intent(in)   :: input_str
      integer(I_), intent(in)              :: index, count
      type(String), pointer                :: str
      
      allocate (str)

      str%char = input_str(index:index)
      str%next => Null()
      
      if ( index < count ) &
         str%next => Create_String_List(input_str, index+1, count)
   end function Create_String_List

   recursive subroutine Print_Str(Str)
      type(String), pointer :: str

      write(*, '(a)', advance="no") str%char
      
      if (associated(str%next)) then
         call Print_Str(str%next)
      else
         write(*, '(/a)', advance="no")
      end if
   end subroutine Print_Str

   subroutine Output_Result(output_file, R)
      character(*), intent(in)   :: output_file
      integer, intent(in)        :: R
      integer                    :: Out = 0

      open (file=output_file, encoding=E_, newunit=Out, position='rewind')
         write (Out, "(a, i4)") "Index: ", R
      close (Out)
   end subroutine Output_Result

end module String_IO
