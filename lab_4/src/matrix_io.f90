module Matrix_IO
   use Environment

   implicit none
   
   integer, parameter                 :: max_len = 1024

   type element
      integer                 :: Aij
      integer                 :: i
      integer                 :: j
      type(element), pointer  :: next  => Null()
   end type element

contains
   function Read_Matrix(InputFile, M, N) result(elem)
      character(*), intent(in)   :: InputFile
      integer, intent(inout)     :: M, N
      type(element), pointer     :: elem
      integer                    :: In

      open (file=InputFile, encoding=E_, newunit=In)
         M = getM(In, 0)
         N = getN(In)
      close (In)

      open (file=InputFile, encoding=E_, newunit=In)
         elem => getMatrix(In, 7, 1)
      close (In)


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

   recursive function getMatrix(In, N, M) result(elem)
      integer, intent(in)                  :: In, N, M
      type(element), pointer               :: elem
      integer                              :: IO
      integer                              :: input_string(N)

      read (In, '('//N-1//'(i1, 1x), i1)', iostat=IO) input_string
      call Handle_IO_status(IO, "read character")

      if (IO == 0) then
         elem => parseString(In, input_string, N, M, 1)
      else
         elem => Null()
      end if

   end function getMatrix
 
   recursive function parseString(In, input_string, N, M, i) result(elem)
      type(element), pointer  :: elem
      integer, intent(in)     :: In
      integer, intent(in)     :: input_string(:), M, N, i

      allocate (elem)
      if (input_string(i) /= 0) then
         elem%Aij = input_string(i)   
         elem%i = M
         elem%j = i
         if (i < N) then
            elem%next => parseString(In, input_string, N, M, i+1)
         end if
      else if (i < N) then
         elem => parseString(In, input_string, N, M, i+1)
      else if (i == N) then
         elem => getMatrix(In, N, M+1)
      end if
   end function

   recursive subroutine printer(matrix)
      type(element), pointer :: matrix

      write(*, *) "Aij: ", matrix%Aij, "i: ", matrix%i, "j: ", matrix%j
      
      if (associated(matrix%next)) then
         call printer(matrix%next)
      else
         write(*, '(/a)', advance="no")
      end if
   end subroutine

   subroutine outputMatrix(output_file, matrix, M, N)
      type(element), pointer     :: matrix
      character(*), intent(in)   :: output_file
      integer, intent(in)        :: M, N
      integer                    :: Out = 0

      open (file=Output_File, encoding=E_, position="rewind", newunit=Out)
         call outputString(Out, matrix, 1, N)
      close (Out)

   end subroutine

   recursive subroutine outputString(Out, matrix, M, N)
      integer, intent(in)        :: Out
      type(element), pointer     :: matrix
      integer, intent(in)        :: M, N
      integer                    :: IO, i

      call outputElement(Out, matrix, M, N, 1)
   
   end subroutine

   recursive subroutine outputElement(Out, matrix, M, N, i)
      integer, intent(in)        :: Out
      type(element), pointer     :: matrix
      integer, intent(in)        :: M, N, i
      integer                    :: IO
     

      if (i == matrix%j .and. matrix%i == M) then
         write(Out, '(i1, 1x)', advance="no") matrix%Aij
         if (i < N .and. associated(matrix%next)) then
            call outputElement(Out, matrix%next, M, N, i+1)
         else if (i < N) then
            call outputElement(Out, matrix, M, N, i+1)
         end if
      else
         write(Out, '(i1, 1x)', advance="no") 0
         if (i < N) then
            call outputElement(Out, matrix, M, N, i+1)
         end if
      end if

      if (i == N .and. associated(matrix%next)) then
         write(Out, *)
         call outputString(Out, matrix, M+1, N)
      else if (i == N .and. matrix%i == M+1) then
         write(Out, *)
         call outputElement(Out, matrix, M+1, N, 1)
      end if

   end subroutine

end module Matrix_IO
