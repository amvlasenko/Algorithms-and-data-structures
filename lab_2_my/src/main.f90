program lab_2_12
   use Environment
   use Source_Process
   use Source_IO

   implicit none
   character(:), allocatable :: F1, F2, F3
   integer                   :: i = 1, result
   type(SourceLine), pointer :: haystack  => Null()  
   type(SourceLine), pointer :: needle   => Null()

   F1 = "../data/haystack.txt"
   F2 = "../data/needle.txt"
   F3 = "output.txt"
   
   haystack => Read_Source_Code(F1)
   needle  => Read_Source_Code(F2)

   result = KMP(haystack, needle, i)
   call Output_Result(F3, result)

end program lab_2_12
