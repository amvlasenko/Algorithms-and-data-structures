program lab_1_5
   use Environment
   use Group_Process
   use Group_IO

   implicit none
   character(:), allocatable           :: input_file, output_file
   type(student), pointer              :: Group, Boys_From_SPB
   integer                             :: amount = 0

   input_file  = "../data/class.txt"
   output_file = "output.txt"

   Group => Read_class_list(input_file)

   if (Associated(Group)) then
      call Output_class_list(output_file, Group, "Исходный список:", "rewind", 0)
      call Get_SPB_Boys(Group, Boys_From_SPB, amount)
      if (Associated(Boys_From_SPB)) then 
         call Output_class_list(output_file, Boys_From_SPB, "Молодые список:", "append", 0)
         call Sort_class_list(Boys_From_SPB, amount)
         call Output_class_list(output_file, Boys_From_SPB, "Самые молодые:", "append", 3)
      end if
     
   
   end if

end program lab_1_5
