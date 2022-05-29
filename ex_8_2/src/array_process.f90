module Array_process
   use Environment

   implicit none
contains
   pure function FormMatrix(M, N) result(B)
      integer, intent(in)  :: M, N
      integer, allocatable :: B(:,:)
      integer              :: i, j
     
      allocate (B(M,N))

      do j = 1, N
         do i = 1, M
            B(i,j) = i * (j + 5)
         end do
      end do

   end function FormMatrix

   pure function MultiplyMatrix(B, C) result(Res)
      integer, intent(in)  :: B(:,:), C(:)
      integer, allocatable :: Res(:)
      integer              :: j, M, N
      !ubound по двум измерениям 
      M = Ubound(B, 1)
      N = Ubound(B, 2)
      allocate (Res(M))

      do concurrent (j = 1:N)
         Res = Res +  B(:,j) * C(j)
      end do

   end function MultiplyMatrix
end module Array_process
