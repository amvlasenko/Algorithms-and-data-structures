program lab_1_4
   use Environment
   use Group_Process
   use Group_IO

   implicit none
   character(:), allocatable :: input_file, output_file, data_file
   character(kind=CH_), parameter   :: MALE = Char(1052, CH_), REG = Char(1055, CH_)
   type(student)              :: Group(STUD_AMOUNT)
   type(student), allocatable :: Boys_From_SPB(:)
   type(student), allocatable :: Youngest_Boys_From_SPB(:)

   input_file  = "../data/class.txt"
   output_file = "output.txt"
   data_file   = "class.dat"
   
   call Create_data_file(input_file, data_file)
   
   Group = Read_class_list(data_file)

   call Output_class_list(output_file, Group, "Исходный список:", "rewind")

   Boys_From_SPB  = Pack(Group, Group%Gender == MALE .AND. Group%Registration == REG) 
   
   call Get_Youngest(Boys_From_SPB, Youngest_Boys_From_SPB)

   call Output_class_list(output_file, Youngest_Boys_From_SPB, "Отсортированный список:", "append")

end program lab_1_4
