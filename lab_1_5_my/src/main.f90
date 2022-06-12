program lab_1_5
   use Environment
   use Group_Process
   use Group_IO

   implicit none
   character(:), allocatable           :: input_file, output_file
   type(student), pointer              :: Group, Boys_From_SPB, stud1, stud2, stud3
   integer                             :: amount = 0


   input_file  = "../data/class.txt"
   output_file = "output.txt"

   Group => Read_class_list(input_file)

   allocate(stud1, stud2, stud3)
      
   if (Associated(Group)) then
      call Output_class_list(output_file, Group, "rewind", "Исходный список:")
      call Get_SPB_Boys(Group, Boys_From_SPB, amount)
      if (Associated(Boys_From_SPB)) then 
         call Output_class_list(output_file, Boys_From_SPB, "append", "Молодые список:")
         call Look_at_youngests(Boys_From_SPB, stud1, stud2, stud3)
         if(Associated(stud1)) then
            call Output_class_list(output_file, stud1, "append", "Самые молодые:")
         end if
         if(Associated(stud2)) then
            call Output_class_list(output_file, stud2, "append")
         end if
         if(Associated(stud3)) then
         call Output_class_list(output_file, stud3, "append")
         end if
      end if
   end if

end program lab_1_5
