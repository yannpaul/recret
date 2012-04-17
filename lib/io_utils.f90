

module io_utils
  private
  public :: get_fid

  !< module variable that store the last file id used. It is initialized to 10
  integer :: last_fid = 10 

contains


  !<  gets the next unused fid, and secures it
  !!  \param fid file id, in and out 
  subroutine get_fid(fid)

    integer :: fid

    do fid=last_fid+1,1000
       if (.not.check_fid(fid)) exit
    end do
    last_fid=fid
    return

  end subroutine get_fid

  !<  a utility to check if a file id has been secured by the program
  !!  \param fid the file ID to be tested

  function check_fid(fid) result (passed)

    integer :: fid
    logical :: passed

    inquire(unit=fid,opened=passed)
    return

  end function check_fid

end module io_utils
