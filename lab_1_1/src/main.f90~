program reference_lab_1_1
   use Environment

   implicit none
   integer, parameter               :: STUD_AMOUNT = 17, GOAL = 3, SURNAME_LEN = 15, INITIALS_LEN = 5
   character(kind=CH_), parameter   :: MALE = Char(1052, CH_), REG = Char(1055, CH_)

   character(:), allocatable  :: input_file, output_file, format

   character(SURNAME_LEN, kind=CH_)       :: Surnames(STUD_AMOUNT) = "", tmpSurname = ""
   character(INITIALS_LEN, kind=CH_)      :: Initials(STUD_AMOUNT) = "", tmpInitials = ""
   character(kind=CH_)                    :: Registrations(STUD_AMOUNT) = ""
   character(kind=CH_)                    :: Genders(STUD_AMOUNT) = ""
   integer                                :: Years(STUD_AMOUNT), tmpYear = 0

   logical, allocatable                   :: Is_A_Boy(:), From_SPb(:)
   integer                                :: Boys_Amount = 0
   integer, allocatable                   :: Boys_Pos(:)
   character(SURNAME_LEN, kind=CH_), allocatable    :: Boys_Surnames(:)
   character(INITIALS_LEN, kind=CH_), allocatable   :: Boys_Initials(:)
   integer, allocatable                             :: Boys_Years(:)
   character(kind=CH_), allocatable                 :: Boys_Registrations(:)

   integer :: In, Out, IO, i, j
   integer, parameter                              :: INDEXES(*) = [(i, i = 1, STUD_AMOUNT)]
   logical :: Swap

   input_file = "../data/class.txt"
   output_file = "output.txt"

   open (file=input_file, encoding=E_, newunit=In)
      format = '(2(a, 1x), i4, 1x, a, 1x, a)'
      read (In, format, iostat=IO) (Surnames(i), Initials(i), Years(i), Registrations(i), Genders(i), i = 1, STUD_AMOUNT)
   close (In)

   Out = OUTPUT_UNIT
   open (Out, encoding=E_)
   select case(io)
      case(0)
      case(IOSTAT_END)
         write (Out, '(a)') "End of file has been reached while reading class list."
      case(1:)
         write (Out, '(a)') "Error while reading class list: ", io
      case default
         write (Out, '(a)') "Undetermined error has been reached while reading class list: ", io
   end select


   open (file=output_file, encoding=E_, newunit=Out)
      write (out, '(a)') "Исходный список:"
      write (Out, format, iostat=IO) (Surnames(i), Initials(i), Years(i), Registrations(i), Genders(i), i = 1, STUD_AMOUNT)
   close (Out)

   Out = OUTPUT_UNIT
   open (Out, encoding=E_)
   select case(io)
      case(0)
      case(IOSTAT_END)
         write (Out, '(a)') "End of file has been reached while writing class list."
      case(1:)
         write (Out, '(a)') "Error while writing class list: ", io
      case default
         write (Out, '(a)') "Undetermined error has been reached while writing class list: ", io
   end select

 
   Is_A_Boy       = Genders == MALE 
   From_SPb       = Registrations == REG
   Boys_Amount    = Count(Is_A_Boy .AND. From_SPb)

   Boys_Pos   = Pack(INDEXES, (Is_A_Boy .AND. From_SPb))
   allocate (Boys_Surnames(Boys_Amount), Boys_Initials(Boys_Amount), Boys_Years(Boys_Amount), Boys_Registrations(Boys_Amount))
   do concurrent (i = 1:Boys_Amount)
      Boys_Surnames(i)  = Surnames(Boys_Pos(i))
      Boys_Initials(i)  = Initials(Boys_Pos(i))
      Boys_Years(i) = Years(Boys_Pos(i))
      Boys_Registrations(i) = Registrations(Boys_Pos(i))
   end do

   !Можно сделать через maxLoc записывая false в маску, макслок по маске, сначала маска вся Т
   do i = Boys_Amount, Boys_Amount - 4, -1
     
      do j = 1, i-1
         Swap = .false.
         if (Boys_Years(j) < Boys_Years(j+1)) then
            Swap = .true.
         else if (Boys_Years(j) == Boys_Years(j+1)) then
            if (Boys_Surnames(j) > Boys_Surnames(j+1)) then
               Swap = .true.
            else if (Boys_Surnames(j)==Boys_Surnames(j+1) .and. Boys_Initials(j)>Boys_Initials(j+1)) then
               Swap = .true.
            end if
         end if

         if (Swap) then
            tmpSurname           = Boys_Surnames(j+1)
            Boys_Surnames(j+1)   = Boys_Surnames(j)
            Boys_Surnames(j)     = tmpSurname

            tmpInitials          = Boys_Initials(j+1)
            Boys_Initials(j+1)   = Boys_Initials(j)
            Boys_Initials(j)     = tmpInitials

            tmpYear              = Boys_Years(j+1)
            Boys_Years(j+1)      = Boys_Years(j)
            Boys_Years(j)        = tmpYear
            
         end if
      end do
   end do


   open (file=output_file, encoding=E_, position='append', newunit=Out)
      write (out, '(/a)') "Самые молодые петербуржцы:"
      write (Out, format, iostat=IO) &
         (Boys_Surnames(i), Boys_Initials(i), Boys_Years(i), "П", "М", i = 1, 3)
   close (Out)

   Out = OUTPUT_UNIT
   open (Out, encoding=E_)
   select case(io)
      case(0)
      case(IOSTAT_END)
         write (Out, '(a)') "End of file has been reached while writing sorted boys list."
      case(1:)
         write (Out, '(a)') "Error while writing sorted boys list: ", io
      case default
         write (Out, '(a)') "Undetermined error has been reached while writing sorted boys list: ", io
   end select

   Out = OUTPUT_UNIT
   open (Out, encoding=E_)
   select case(io)
      case(0)
      case(IOSTAT_END)
         write (Out, '(a)') "End of file has been reached while writing sorted girls list."
      case(1:)
         write (Out, '(a)') "Error while writing sorted girls list: ", io
      case default
         write (Out, '(a)') "Undetermined error has been reached while writing sorted girls list: ", io
   end select

end program reference_lab_1_1
