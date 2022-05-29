module Array_IO
   use Environment

   implicit none
contains
   subroutine ReadParams(input_file, M, N, B, C)
      character(*), intent(in) :: input_file
      integer, intent(inout) :: M, N
      integer, allocatable, intent(inout) :: C(:), B(:,:)
      integer :: In = 0
      
      open (file=input_file, newunit=In)
         read (In, *) M, N
         allocate (B(M,N))
         allocate (C(N))
         read (In, *) C
      close (In)

   end subroutine ReadParams

   subroutine WriteResponse(output_file, Res)
      character(*), intent(in) :: output_file
      integer, intent(in) :: Res(:)
      integer :: Out = 0
      
      open (file=output_file, encoding=E_, newunit=Out)
         write (Out, '(i5)') Res

      close (Out)

   end subroutine WriteResponse
end module Array_IO
