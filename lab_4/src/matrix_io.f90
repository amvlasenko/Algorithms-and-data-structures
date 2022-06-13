module Matrix_IO
   use Environment

   implicit none
   
   integer, parameter                 :: max_len = 1024

   type element
      integer                 :: Aij = 0
      integer                 :: i   = 0
      integer                 :: j   = 0
      type(element), pointer  :: Next  => Null()
   end type element

contains
   function Read_Matrix(InputFile) result(response)
      character(*), intent(in)   :: InputFile
      type(element), pointer     :: response
      integer                    :: In, M, N

      open (file=InputFile, encoding=E_, newunit=In)
         M = getM(In, 0)
         N = getN(In)
         !response => Read_element(In, 0)
      close (In)
      print *, M
      print *, N

   end function

   recursive function getM(In, acc) result(M)
      integer, intent(in)           :: In
      integer, optional, intent(in) :: acc
      character(MAX_LEN, CH_)       :: input_element
      integer                       :: IO, M

      M = acc

      read (In, '(a)', iostat=IO) input_element
      if (IO == 0) then
         M = getM(In, acc+1)
      end if
   end function

   function getN(In) result(N)
      integer, intent(in)      :: In
      character(MAX_LEN, CH_)  :: input_element, string
      character(1, CH_)        :: space = " "
      integer                  :: IO, i, N, acc = 1

      read (In, '(a)', iostat=IO) input_element
      string = trim(input_element)

      do i = 1, len_trim(input_element)
         if (string(i:i) == space) then
          acc = acc + 1
         end if
      end do
 
      N = acc
   end function

   ! recursive function Read_element(In, acc) result(elem)
   !    integer, intent(in)           :: In
   !    integer, optional, intent(in) :: acc
   !    type(element), pointer        :: elem
   !    character(MAX_LEN, CH_)       :: input_element
   !    integer                       :: IO, M, N, countSymbols = 0


   !    read (In, '(a)', iostat=IO) input_element
    

   !    !Находим количество элементов в строке
   !    if (countSymbols == 0) then
   !       countSymbols = parsString(trim(input_element), len_trim(input_element))
   !    end if

   !    !Находим количество строк
   !    call Handle_IO_status(IO, "read character")
   !    if (IO == 0) then
   !       elem => Read_element(In, acc+1)
   !    else
   !       Print *, "M: ", M
   !       Print *, "N: ", N
   !    end if
       
     

   ! end function

   ! function parsString(string, len) result(countSymbols)
   !    integer, intent(in)              :: len
   !    ! character(len, CH_), intent(in)  :: string
   !    ! character(1, CH_)                :: space = " "
   !    integer  :: i, countSymbols, acc = 1
         
   !    do i = 1, len
   !      if (string(i:i) == space) then
   !       acc = acc + 1
   !      end if
   !    end do

   !    countSymbols = acc

   ! end function
!   ! Чтение строки из файла
!    function Read_element(In) result(str)
!       integer, intent(in)                  :: In
!       integer                              :: IO, lenght_element
!       type(element), pointer                :: str
!       character(MAX_LEN, CH_)              :: input_element

!       read (In, "(a)", iostat=IO) input_element
!       call Handle_IO_status(IO, "read character")
     
!       lenght_element = len_trim(input_element)

!       print *, trim(input_element)
      
!       if ( lenght_element >= 1 ) then
!          str => Create_element_List(trim(input_element), 1, lenght_element)
!       else
!          str => Null()
!       end if
!    end function Read_element

!    ! Рекурсивное чтение исходной строки и перевод ее в модель element
!    recursive function Create_element_List(input_str, index, count) result(str)
!       character(*, kind=CH_), intent(in)   :: input_str
!       integer(I_), intent(in)              :: index, count
!       type(element), pointer                :: str
      
!       allocate (str)

!       str%char = input_str(index:index)
!       str%next => Null()
      
!       if ( index < count ) &
!          str%next => Create_element_List(input_str, index+1, count)
!    end function Create_element_List

!    recursive subroutine Print_Str(Str)
!       type(element), pointer :: str

!       write(*, '(a)', advance="no") str%char
      
!       if (associated(str%next)) then
!          call Print_Str(str%next)
!       else
!          write(*, '(/a)', advance="no")
!       end if
!    end subroutine Print_Str

!    subroutine Output_Result(output_file, R)
!       character(*), intent(in)   :: output_file
!       integer, intent(in)        :: R
!       integer                    :: Out = 0

!       open (file=output_file, encoding=E_, newunit=Out, position='rewind')
!          write (Out, "(a, i4)") "Index: ", R
!       close (Out)
!    end subroutine Output_Result

end module Matrix_IO
