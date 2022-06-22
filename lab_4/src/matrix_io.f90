module Matrix_IO
   use Environment

   implicit none
   
   integer, parameter                 :: max_len = 1024

   type element
      integer                 :: Aij
      integer                 :: i
      integer                 :: j
      type(element), allocatable  :: next
   end type element

contains
   subroutine Read_Matrix(InputFile, elem, M, N)
      character(*), intent(in)   :: InputFile
      integer, intent(inout)     :: M, N
      type(element), allocatable     :: elem
      integer                    :: In

      open (file=InputFile, encoding=E_, newunit=In)
         N = getN(In)
         M = getM(In)
      close (In)

      allocate(elem)
      
      open (file=InputFile, encoding=E_, newunit=In)
         call getMatrix(In, elem, N, 1)
      close (In)
   end subroutine

   function getM(In) result(M)
      integer, intent(in)           :: In
      character(MAX_LEN, CH_)       :: input_element
      integer                       :: IO, M, acc = 1

      do while (IO == 0)
         read (In, '(a)', iostat=IO) input_element
         if (IO == 0) then
            acc = acc + 1
         else
            M = acc
         end if
      end do
      
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

   recursive subroutine getMatrix(In, elem, N, M)
      integer, intent(in)                  :: In, N, M
      type(element), allocatable               :: elem
      integer                              :: IO
      integer                              :: input_string(N)

      read (In, '('//N//'i2.2)', iostat=IO) input_string
      call Handle_IO_status(IO, "read character")
      if (IO == 0) then
         call parseString(In, elem, input_string, N, M, 1)
      else
         deallocate(elem)
      end if

   end subroutine getMatrix
 
   recursive subroutine parseString(In, elem, input_string, N, M, i) 
      type(element), allocatable  :: elem
      integer, intent(in)     :: In
      integer, intent(in)     :: input_string(:), M, N, i
      
      if (input_string(i) /= 0) then
         elem%Aij = input_string(i)   
         elem%i = M
         elem%j = i
         if (i < N) then
            allocate(elem%next)
            call parseString(In, elem%next, input_string, N, M, i+1)
         end if
      else if (i < N) then
         call parseString(In, elem, input_string, N, M, i+1)
      else if (i == N) then
         call getMatrix(In, elem, N, M+1)
      end if
      
   end subroutine

   recursive subroutine printer(matrix)
      type(element), allocatable :: matrix

      write(*, *) "Aij: ", matrix%Aij, "i: ", matrix%i, "j: ", matrix%j
      
      if (allocated(matrix%next)) then
         call printer(matrix%next)
      else
         write(*, '(/a)', advance="no")
      end if
   end subroutine

   subroutine outputMatrix(output_file, matrix, N)
      type(element), allocatable     :: matrix
      character(*), intent(in)   :: output_file
      integer, intent(in)        :: N
      integer                    :: Out = 0

      open (file=Output_File, encoding=E_, position="rewind", newunit=Out)
         call outputString(Out, matrix, 1, N)
      close (Out)

   end subroutine

   recursive subroutine outputString(Out, matrix, M, N)
      integer, intent(in)        :: Out
      type(element), allocatable     :: matrix
      integer, intent(in)        :: M, N

      call outputElement(Out, matrix, M, N, 1)
   
   end subroutine

   recursive subroutine outputElement(Out, matrix, M, N, i)
      integer, intent(in)        :: Out
      type(element), allocatable     :: matrix
      integer, intent(in)        :: M, N, i

      if (i == matrix%j .and. matrix%i == M) then
         write(Out, '(i3.1, 1x)', advance="no") matrix%Aij
         if (i < N .and. allocated(matrix%next)) then
            call outputElement(Out, matrix%next, M, N, i+1)
         else if (i < N) then
            call outputElement(Out, matrix, M, N, i+1)
         end if
      else
         write(Out, '(i3.1, 1x)', advance="no") 0
         if (i < N) then
            call outputElement(Out, matrix, M, N, i+1)
         end if
      end if

      if (i == N .and. allocated(matrix%next)) then
         write(Out, *)
         call outputString(Out, matrix, M+1, N)
      else if (i == N .and. matrix%i == M+1) then
         write(Out, *)
         call outputElement(Out, matrix, M+1, N, 1)
      end if

   end subroutine

end module Matrix_IO
