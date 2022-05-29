program lab_1_3
   use Environment
   use Group_Process
   use Group_IO

   implicit none
   character(:), allocatable :: input_file, output_file, data_file
   type(student)              :: Group
   type(student)              :: Boys_From_SPB
   type(student)              :: Youngest_Boys_From_SPB

   input_file  = "../data/class.txt"
   output_file = "output.txt"
   data_file   = "class.dat"
   
   call Create_data_file(input_file, data_file)
   
   Group = Read_class_list(data_file)

   call Output_class_list(output_file, Group, "Исходный список:", "rewind")

   Boys_From_SPB = Get_SPB_Boys(Group)

   call Get_Youngest(Boys_From_SPB, Youngest_Boys_From_SPB)

   call Output_class_list(output_file, Youngest_Boys_From_SPB, "Отсортированный список:", "append")

end program lab_1_3
