
!> This contains the 'class' person. The person can be recorded and retrieved
!! using the recret style of serializing. recret provides 3 methods: setFile,
!! record, and retrieve. setFile is called to open a file and assign a unique
!! file ID number. record and retrieve are interfaces for similar subroutines
!! that each handle a different input type, eg integer, real(8), etc. Notice 
!! that the sequence of writes for the record_person method is the same for 
!! retrieve person. 

module person_mod
  
  use recret ! make sure you include the module with the use statement

  implicit none

  !> This is the definition of the 'class' attributes
  type person
     character(len=10) :: first_name
     character(len=10) :: last_name
     integer :: age
  end type person

  contains 

    !< this acts like a member function, eg self is the person, but the 
    !! paramter sequence is modeled after that used in recret, so self
    !! is not the first member
  subroutine record_person(fileID, self, inToAscii)

    implicit none

    integer :: fileID
    logical,optional :: inToAscii
    logical  :: toAscii=.FALSE.
    type(person) :: self

    ! this next bit just repeats the option used in recret, by default
    ! the values are stored in binary, if inToAscii is present, then the 
    ! file is store in Ascii
    if (present(inToAscii)) then 
       toASCII = .TRUE.
    endif
    ! this record method is an interface to many that follow the same 
    ! proceduce for different types
    call record(fileID, self%first_name, toAscii)
    call record(fileID, self%last_name, toAscii)
    call record(fileID, self%age, toAscii)

  end subroutine record_person

  subroutine retrieve_person(fileID, self, inFromAscii)

    implicit none

    integer :: fileID
    logical,optional :: inFromAscii
    logical  :: fromAscii=.FALSE.
    type(person) :: self

    if (present(inFromAscii)) then 
       fromASCII = .TRUE.
    endif
    ! retrieve is also an interface for the same method created for different
    ! inputs, the second parameter
    call retrieve(fileID, self%first_name, fromAscii)
    call retrieve(fileID, self%last_name, fromAscii)
    call retrieve(fileID, self%age, fromAscii)

  end subroutine retrieve_person

  subroutine print_person(self)

    implicit none
    
    type(person) self

    print *, self%first_name, " ", self%last_name, " - ", self%age
  end subroutine print_person

end module person_mod


!> This demonstrates the use of recret and how it is extend with custom types.
!! Here two people are created, one is recorded in binary format to a file
!! and then retrieved to the other person. The first step in a record and 
!! retrieve is to call setFile, which opens the file and assigns a unique
!! file ID. 

program person_main

  use person_mod
  use recret

  implicit none

  ! person is defined below, as well as record and retrieve routines
  type(person) :: me
  type(person) :: you
  integer :: fileID
  character(len=80) :: filename = 'person_record.bin'

  me%first_name = "Joe"
  me%last_name = "Smith"
  me%age = 27
  you%first_name = "Jane"
  you%last_name = "Doe"
  you%age = 34
  
  call setFile('person_record.bin', fileID) ! opens file and sets fileID
  call record_person(fileID, me)
  close(fileID)
 
  print *,"Before retrieve: "
  call print_person(me)
  call print_person(you) 
 

  call setFile(filename, fileID) 
  call retrieve_person(fileID, you)
  close(fileID)
  
  print *,"After retrieve: "
  call print_person(me)
  call print_person(you)

end program person_main
