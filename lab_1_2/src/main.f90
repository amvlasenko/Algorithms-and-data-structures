program lab_1_2
   use Environment

   implicit none
   integer, parameter               :: STUD_AMOUNT = 17, SURNAME_LEN = 15, INITIALS_LEN = 5
   character(kind=CH_), parameter   :: MALE = Char(1052, CH_), REG = Char(1055, CH_)
   character(:), allocatable  :: input_file, output_file
   
   character(kind=CH_)                    :: Surnames(SURNAME_LEN, STUD_AMOUNT)  = "", &
                                             Initials(INITIALS_LEN, STUD_AMOUNT) = "", &
                                             Genders(STUD_AMOUNT)                   = ""
   character(kind=CH_)                    :: Registrations(STUD_AMOUNT) = ""
   integer                                :: Years(STUD_AMOUNT), Boys_Amount
   character(kind=CH_), allocatable       :: Boys_Surnames(:,:), Boys_Initials(:,:), Res_Boys_Surnames(:,:), Res_Boys_Initials(:,:)
   integer, allocatable                   :: Boys_Years(:), Res_Boys_Years(:)
   character(kind=CH_), allocatable       :: Boys_Registrations(:)

   input_file = "../data/class.txt"
   output_file = "output.txt"


   call Read_class_list(input_file, Surnames, Initials, Years, Registrations, Genders)
   call Output_class_list(output_file, Surnames, Initials, Years, Registrations, Genders, &
      "Исходный список:", "rewind")
 
   call Get_Boys_From_SPB(Surnames, Initials, Years, Registrations, Genders, &
   Boys_Surnames, Boys_Initials, Boys_Years, Boys_Registrations, Boys_Amount)
   call Get_Youngest(Boys_Surnames, Boys_Initials, Boys_Years, Boys_Amount, &
   Res_Boys_Surnames, Res_Boys_Initials, Res_Boys_Years)

   call Output_Response(Output_File, Res_Boys_Surnames, Res_Boys_Initials, Res_Boys_Years)


contains

   subroutine Read_Class_List(input_file, Surnames, Initials, Years, Registrations, Genders)
      character(*)         Input_File
      character(kind=CH_)  Surnames(:, :), Initials(:, :), Genders(:), Registrations(:)
      integer              Years(:)
      intent (in)          Input_File
      intent (out)         Surnames, Initials, Years, Registrations, Genders
      integer                    :: In, IO, i
      character(:), allocatable  :: format

      open (file=Input_File, encoding=E_, newunit=In)
         format = '(' // SURNAME_LEN // 'a1, 1x, ' // INITIALS_LEN // 'a1, 1x, i4, 1x, a, 1x, a)'
         read (In, format, iostat=IO) (Surnames(:,i), Initials(:,i), Years(i), Registrations(i), Genders(i), &
         i = 1, STUD_AMOUNT)
         call Handle_IO_status(IO, "reading class list")
      close (In)
      
   end subroutine

   subroutine Output_class_list(Output_File, Surnames, Initials, Years, Registrations, Genders, List_name, Position)
      character(*)         Output_File, Position, List_name
      character(kind=CH_)  Surnames(:, :), Initials(:, :), Genders(:), Registrations(:)
      integer              Years(:)
      intent (in)          Output_File, Surnames, Initials, Years, Registrations, Genders, List_name, Position

      integer                    :: Out, i, IO
      character(:), allocatable  :: format
   
      open (file=output_file, encoding=E_, position=position, newunit=Out)
         write (out, '(/a)') List_name
         format = '(' // SURNAME_LEN // 'a1, 1x, ' // INITIALS_LEN // 'a1, 1x, i4, 1x, a, 1x, a)'
         write (Out, format, iostat=IO) &
            (Surnames(:,i), Initials(:,i), Years(i), Registrations(i), Genders(i), i = 1, Size(Genders))
         call Handle_IO_status(IO, "writing " // List_name)
      close (Out)
   end subroutine

   subroutine Output_Response(Output_File, Res_Boys_Surnames, Res_Boys_Initials, Res_Boys_Years)
      character(*)         Output_File
      character(kind=CH_)  Res_Boys_Surnames(:, :), Res_Boys_Initials(:, :)
      integer              Res_Boys_Years(:)
      intent (in)          Output_File, Res_Boys_Surnames, Res_Boys_Initials, Res_Boys_Years

      integer                    :: Out, i, IO
      character(:), allocatable  :: format
   
      open (file=output_file, encoding=E_, position="append", newunit=Out)
         write (out, '(/a)') "Молодые Петербуржцы:"
         format = '(' // SURNAME_LEN // 'a1, 1x, ' // INITIALS_LEN // 'a1, 1x, i4, 1x, a, 1x, a)'
         write (Out, format, iostat=IO) &
            (Res_Boys_Surnames(:,i), Res_Boys_Initials(:,i), Res_Boys_Years(i), "П", "М", i = 1, 3)
         call Handle_IO_status(IO, "writing " // "Молодые Петербуржцы:")
      close (Out)
   end subroutine

   pure subroutine Get_Boys_From_SPB(Surnames, Initials, Years, Registrations, Genders, &
      Boys_Surnames, Boys_Initials, Boys_Years, Boys_Registrations, Boys_Amount)
      character(kind=CH_)  Surnames(:, :), Initials(:, :), Registrations(:), Genders(:)
      character(kind=CH_)  Boys_Surnames(:, :), Boys_Initials(:,:), Boys_Registrations(:)
      integer              Boys_Years(:), Boys_Amount
      integer              Years(:)
      intent(in)           Surnames, Initials, Years, Registrations, Genders
      intent(out)          Boys_Surnames, Boys_Initials, Boys_Years, Boys_Registrations, Boys_Amount
      allocatable          Boys_Surnames, Boys_Initials, Boys_Years, Boys_Registrations

      logical                       Is_A_Boy(STUD_AMOUNT), From_SPb(STUD_AMOUNT)
      integer, allocatable       :: Boys_Pos(:)
      integer                    :: i
      integer, parameter         :: INDEXES(*) = [(i, i = 1, STUD_AMOUNT)]

      Is_A_Boy       = Genders == MALE 
      From_SPb       = Registrations == REG
      Boys_Amount    = Count(Is_A_Boy .AND. From_SPb)
      
      Boys_Pos   = Pack(INDEXES, (Is_A_Boy .AND. From_SPb))

      allocate (Boys_Surnames(SURNAME_LEN, Boys_Amount), Boys_Initials(INITIALS_LEN, Boys_Amount), &
      Boys_Years(Boys_Amount), Boys_Registrations(Boys_Amount))
      
      do concurrent (i = 1:Boys_Amount)
         Boys_Surnames(:,i)  = Surnames(:,Boys_Pos(i))
         Boys_Initials(:,i)  = Initials(:,Boys_Pos(i))
         Boys_Years(i) = Years(Boys_Pos(i))
      end do

   end subroutine 

   pure subroutine Get_Youngest(Boys_Surnames, Boys_Initials, Boys_Years, Boys_Amount, &
      Res_Boys_Surnames, Res_Boys_Initials, Res_Boys_Years)
      character(kind=CH_)  Boys_Surnames(:, :), Boys_Initials(:,:)
      integer              Boys_Years(:), Unique_Pos(3), Boys_Amount, i
      character(kind=CH_), allocatable :: Res_Boys_Surnames(:,:), Res_Boys_Initials(:,:)
      integer, allocatable             :: Res_Boys_Years(:)
      logical, allocatable             :: Boys_Mask(:)
      integer, parameter               :: INDEXES(*) = [(i, i=1, STUD_AMOUNT)]

      intent(inout) Boys_Surnames, Boys_Initials, Boys_Years, Boys_Amount
      intent(out)  Res_Boys_Surnames, Res_Boys_Initials, Res_Boys_Years


      allocate       (Boys_Mask(Boys_Amount), source = .TRUE.)
      allocate       (Res_Boys_Surnames(SURNAME_LEN,3), Res_Boys_Initials(INITIALS_LEN,3), Res_Boys_Years(3))

      do i = 1, 3
         Boys_Mask(maxloc(Boys_Years, Boys_Mask)) = .FALSE.
      end do
      Unique_Pos = Pack(INDEXES, .not. Boys_Mask)

      Res_Boys_Surnames = Boys_Surnames(:,Unique_Pos)
      Res_Boys_Initials = Boys_Initials(:,Unique_Pos)
      Res_Boys_Years = Boys_Years(Unique_Pos)

   end subroutine

end program lab_1_2
