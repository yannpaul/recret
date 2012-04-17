
module recret

  implicit none

  private
  public :: record, retrieve, setFile

  character(len=*),parameter :: real8fmt="(F16.8)"

  integer :: recretDEBUG = 0

  interface record
     module procedure &
          recreal8,&!recreal8pt,&
          recint,&!recintpt,&
          recreal8vecpt,&
          recreal8matpt,&
          recreal8tenpt,&
          recreal8_4dpt,&
          recreal8_5dpt,&
          recchar,&
          reccharvecpt
  end interface

  interface retrieve
     module procedure &
          retreal8,&!retreal8pt,&
          retint,&!retintpt,&
          retreal8vecpt,&
          retreal8matpt,&
          retreal8tenpt,&
          retreal8_4dpt,&
          retreal8_5dpt,&
          retchar,&
          retcharvecpt
  end interface

  integer, parameter :: errorFid = 5

contains

  subroutine setFile(filename, fileID, inwithAscii)
    
    use io_utils

    implicit none

    logical,optional :: inwithAscii
    logical :: withAscii = .FALSE.
    character(len=*) :: filename
    integer :: fileID

    if(present(inwithAscii)) withAscii = inwithAscii
    call get_fid(fileID)
    if (withAscii) then
       open(UNIT=fileID,FILE=filename,FORM='FORMATTED',ACCESS='SEQUENTIAL')
    else
       open(UNIT=fileID,FILE=filename,FORM='UNFORMATTED')
    endif
  end subroutine setFile

  subroutine recretWriteError(fid, errStat)
    
    implicit none

    integer :: fid
    integer :: errStat 

    write(errorFid, fmt='(A,I3,A,I3))') "Failed to write to file (id = ", &
         fid, ") with error ", errStat

  end subroutine recretWriteError

  subroutine recretReadError(fid, errStat)
    
    implicit none

    integer :: fid
    integer :: errStat 

    write(errorFid, fmt='(A,I3,A,I3))') "Failed to read from file (id = ", &
         fid, ") with error ", errStat

  end subroutine recretReadError

  subroutine recreal8(fileID,real8val,inFormated)

    implicit none

    logical,optional :: inFormated
    logical :: formatted=.FALSE.
    integer :: fileID,errStat
    real(8) :: real8val
    if(present(inFormated)) formatted=inFormated    

    if(formatted) then
       write(fileID,*,iostat=errStat) real8val
    else
       write(fileID,iostat=errStat) real8val
    endif
    if (errStat .ne. 0) then 
       call recretWriteError(fileID, errStat)
    end if
  end subroutine recreal8

  subroutine retreal8(fileID,real8val,inFormated)
    
    implicit none

    logical,optional :: inFormated
    logical :: formatted=.FALSE.
    integer :: fileID,errStat
    real(8) :: real8val

    if(present(inFormated)) formatted=inFormated    
    if (formatted) then
       read(fileID,*,iostat=errStat) real8val
    else
       read(fileID,iostat=errStat) real8val
    endif
    if (errStat .ne. 0) then
       call recretReadError(fileID, errStat)
    end if
    
  end subroutine retreal8

  subroutine recint(fileid,intval,inFormated)

    implicit none

    logical,optional :: inFormated
    logical :: formatted=.FALSE.
    integer :: fileID,errStat,intval
    character(len=80) :: filename

    if(present(inFormated)) formatted=inFormated

    if (recretDEBUG.ne.0) print*,"Writing intval: ",intval
    if (formatted) then
       write(fileID,*,iostat=errStat) intval
    else
       write(fileID,iostat=errStat) intval
    endif
    if (errStat .ne. 0) then
       call recretWriteError(fileID, errStat)
    end if

  end subroutine recint

  subroutine retint(fileid,intval,inFormated)

    implicit none

    logical,optional :: inFormated
    logical :: formatted=.FALSE.
    integer :: fileID,errStat,intval
    character(len=80) :: filename
    
    if(present(inFormated)) formatted=inFormated

    if(formatted) then
       read(fileID,*,iostat=errStat) intval
    else
       read(fileID,iostat=errStat) intval
    endif
    if (errStat .ne. 0) then
       call recretReadError(fileID, errStat)
    endif

  end subroutine retint

  subroutine recreal8vecpt(fileID,real8val,inFormated)

    implicit none

    logical,optional :: inFormated
    logical :: formatted=.FALSE.
    integer :: fileID,errStat
    real(8),dimension(:),pointer :: real8val

    if (recretDEBUG.eq.1) then
       print *,"real8val"
       print '(3F16.8)',real8val
    end if

    if(present(inFormated)) formatted=inFormated
    
    if(formatted) then
       write(fileID,*,iostat=errStat) size(real8val,1)
    else
       write(fileID,iostat=errStat) size(real8val,1)
    endif
    if (errStat .ne. 0) then
       call recretWriteError(fileID, errStat)
    end if
    if(formatted) then
       write(fileID,*,iostat=errStat) real8val
    else
       write(fileID,iostat=errStat) real8val
    endif
    if (errStat .ne. 0) then 
       call recretWriteError(fileID, errStat)
    endif
    
  end subroutine recreal8vecpt

  subroutine retreal8vecpt(fileID,real8val,inFormated)
    
    implicit none

    logical,optional :: inFormated
    logical :: formatted=.FALSE.
    integer :: fileID,errStat,veclen
    real(8),dimension(:),pointer :: real8val
    character(len=80) :: filename

    if (present(inFormated)) formatted=inFormated

    veclen = size(real8val)
    if(associated(real8val)) deallocate(real8val)
    nullify(real8val)

    if(formatted) then
       read(fileID,*,iostat=errStat) veclen
    else
       read(fileID,iostat=errStat) veclen
    endif
    if (errStat .ne. 0) then
       call recretReadError(fileID, errStat)
    endif
    
    allocate(real8val(veclen))

    if(formatted) then
       read(fileID,*,iostat=errStat) real8val
    else
       read(fileID,iostat=errStat) real8val
    endif
    if (errStat .ne. 0) then
       call recretReadError(fileID, errStat)
    endif

    if (recretDEBUG.eq.1) then
       print *,"real8val"
       print '(3F16.8)',real8val
 !      read(*,*)
    end if

  end subroutine retreal8vecpt

  subroutine recreal8matpt(fileID,real8val,informated)

    implicit none

    logical,optional :: inFormated
    logical :: formatted=.FALSE.
    integer :: fileID,errStat
    real(8),dimension(:,:),pointer :: real8val
 
    if (present(inFormated)) formatted = inFormated
   
    if(recretDEBUG .eq. 1) print *,"Entering recreal8mat"
    if(recretDEBUG .eq. 1) print *,&
         "Storing vector that should be of length: ", &
         size(real8val,1),size(real8val,2)
    if (formatted) then
       write(fileID,*,iostat=errStat) size(real8val,1),size(real8val,2)
    else
       write(fileID,iostat=errStat) size(real8val,1),size(real8val,2)
    endif
    if (errStat .ne. 0) then 
       call recretWriteError(fileID, errStat)
    endif
    if (formatted) then
       write(fileID,*,iostat=errStat) real8val
    else
       write(fileID,iostat=errStat) real8val
    endif
    if (errStat .ne. 0) then
       call recretWriteError(fileID, errStat)
    endif
    if(recretDEBUG .eq. 1) print *,"Leaving recreal8mat"
  end subroutine recreal8matpt

  subroutine retreal8matpt(fileID,real8val,informated)
    
    implicit none

    logical,optional :: inFormated
    logical :: formatted=.FALSE.
    integer :: fileID,errStat
    integer,dimension(2) :: veclen
    real(8),dimension(:,:),pointer :: real8val

    if (present(informated)) formatted = informated

    if(recretDEBUG .eq. 1) print *,"Entering retreal8mat"
    if(associated(real8val)) deallocate(real8val)
    nullify(real8val)
    if (formatted) then
       read(fileID,*,iostat=errStat) veclen
    else
       read(fileID,iostat=errStat) veclen
    end if
    if(recretDEBUG .eq. 1) print *,&
         "Making vector that should be of length: ",veclen
    if (errStat .ne. 0) then 
       call recretReadError(fileID, errStat)
    endif
    allocate(real8val(veclen(1),veclen(2)))

    if (formatted) then
       read(fileID,*,iostat=errStat) real8val
    else
       read(fileID,iostat=errStat) real8val
    end if
    if (errStat .ne. 0) then
       call recretReadError(fileID, errStat)
    endif
    if(recretDEBUG .eq. 1) print *,"Leaving retreal8mat"

  end subroutine retreal8matpt

  subroutine recreal8tenpt(fileID,real8val,informated)

    implicit none

    logical,optional :: inFormated
    logical :: formatted=.FALSE.
    integer :: fileID,errStat
    real(8),dimension(:,:,:),pointer :: real8val

    if (present(informated)) formatted = informated

    if (formatted) then
       write(fileID,*,iostat=errStat) size(real8val,1),size(real8val,2),&
            size(real8val,3)
    else
       write(fileID,iostat=errStat) size(real8val,1),size(real8val,2),&
            size(real8val,3)
    end if
    if (errStat .ne. 0) then
       call recretWriteError(fileID, errStat)
    endif

    if (formatted) then
       write(fileID,*,iostat=errStat) real8val
    else
       write(fileID,iostat=errStat) real8val
    endif
    if (errStat .ne. 0) then
       call recretWriteError(fileID, errStat)
    endif
    
  end subroutine recreal8tenpt
  
  subroutine retreal8tenpt(fileID,real8val,informated)
    
    implicit none

    logical,optional :: inFormated
    logical :: formatted=.FALSE.
    integer :: fileID,errStat
    integer,dimension(3) :: veclen
    real(8),dimension(:,:,:),pointer :: real8val

    if (present(informated)) formatted = informated

    if(associated(real8val)) deallocate(real8val)
    nullify(real8val)

    if (formatted) then
       read(fileID,*,iostat=errStat) veclen
    else
       read(fileID,iostat=errStat) veclen
    end if
    if (errStat .ne. 0) then
       call recretReadError(fileID, errStat)
    endif
    
    allocate(real8val(veclen(1),veclen(2),veclen(3)))

    if (formatted) then
       read(fileID,*,iostat=errStat) real8val
    else
       read(fileID,iostat=errStat) real8val
    endif
    if (errStat .ne. 0) then
       call recretReadError(fileID, errStat)
    endif
    
  end subroutine retreal8tenpt
  
  subroutine recreal8_4dpt(fileID,real8val,informated)

    implicit none

    logical,optional :: inFormated
    logical :: formatted=.FALSE.
    integer :: fileID,errStat
    real(8),dimension(:,:,:,:),pointer :: real8val

    if (present(informated)) formatted = informated

    if (formatted) then
       write(fileID,*,iostat=errStat) size(real8val,1),size(real8val,2),&
            size(real8val,3),size(real8val,4)
    else
       write(fileID,iostat=errStat) size(real8val,1),size(real8val,2),&
            size(real8val,3),size(real8val,4)
    endif
    if (errStat .ne. 0) then
       call recretWriteError(fileID, errStat)
    endif

    if (formatted) then
       write(fileID,*,iostat=errStat) real8val       
    else
       write(fileID,iostat=errStat) real8val
    endif
    if (errStat .ne. 0) then 
       call recretWriteError(fileID, errStat)
    endif

  end subroutine recreal8_4dpt
  
  subroutine retreal8_4dpt(fileID,real8val,informated)
    
    implicit none

    logical,optional :: inFormated
    logical :: formatted=.FALSE.
    integer :: fileID,errStat
    integer,dimension(4) :: veclen
    real(8),dimension(:,:,:,:),pointer :: real8val

    if (present(informated)) formatted = informated

    if(associated(real8val)) deallocate(real8val)
    nullify(real8val)
    
    if (formatted) then
       read(fileID,*,iostat=errStat) veclen
    else
       read(fileID,iostat=errStat) veclen
    endif
    if (errStat .ne. 0) then
       call recretReadError(fileID, errStat)
    endif
    
    allocate(real8val(veclen(1),veclen(2),veclen(3),veclen(4)))

    if (formatted) then
       read(fileID,*,iostat=errStat) real8val
    else
       read(fileID,iostat=errStat) real8val
    endif
    if (errStat .ne. 0) then
       call recretReadError(fileID, errStat)
    endif
    
  end subroutine retreal8_4dpt
  
  subroutine recreal8_5dpt(fileID,real8val,informated)

    implicit none

    logical,optional :: inFormated
    logical :: formatted=.FALSE.
    integer :: fileID,errStat
    real(8),dimension(:,:,:,:,:),pointer :: real8val

    if (present(informated)) formatted = informated
    
    if (formatted) then
       write(fileID,*,iostat=errStat) size(real8val,1),size(real8val,2),&
            size(real8val,3),size(real8val,4),size(real8val,5)
    else
       write(fileID,iostat=errStat) size(real8val,1),size(real8val,2),&
            size(real8val,3),size(real8val,4),size(real8val,5)
    endif
    if (errStat .ne. 0) then
       call recretWriteError(fileID, errStat)
    endif

    if (formatted) then
       write(fileID,*,iostat=errStat) real8val
    else
       write(fileID,iostat=errStat) real8val
    endif
    if (errStat .ne. 0) then
       call recretWriteError(fileID, errStat)
    endif
    
  end subroutine recreal8_5dpt
  
  subroutine retreal8_5dpt(fileID,real8val,informated)
    
    implicit none

    logical,optional :: inFormated
    logical :: formatted=.FALSE.
    integer :: fileID,errStat
    integer,dimension(5) :: veclen
    real(8),dimension(:,:,:,:,:),pointer :: real8val

    if (present(informated)) formatted = informated

    if(associated(real8val)) deallocate(real8val)
    nullify(real8val)

    if (formatted) then
       read(fileID,*,iostat=errStat) veclen
    else
       read(fileID,iostat=errStat) veclen
    endif
    if (errStat .ne. 0) then
       call recretReadError(fileID, errStat)
    endif
    
    allocate(real8val(veclen(1),veclen(2),veclen(3),veclen(4),veclen(5)))

    if (formatted) then
       read(fileID,*,iostat=errStat) real8val
    else
       read(fileID,iostat=errStat) real8val
    endif
    if (errStat .ne. 0) then
       call recretReadError(fileID, errStat)
    endif
    
  end subroutine retreal8_5dpt
  
  subroutine recchar(fileID,charval,informated)

    implicit none
    
    logical,optional :: inFormated
    logical :: formatted=.FALSE.
    integer :: fileID,errstat
    character(len=*) :: charval

    if (present(informated)) formatted = informated

    if (formatted) then
       write(fileID,*,iostat=errStat) charval
    else
       write(fileID,iostat=errStat) charval
    endif
    if (errStat .ne. 0) then
       call recretWriteError(fileID, errStat)
    endif

  end subroutine recchar

  subroutine retchar(fileID,charval,informated)

    implicit none
    
    logical,optional :: inFormated
    logical :: formatted=.FALSE.
    integer :: fileID,errStat
    character(len=*) :: charval

    if (present(informated)) formatted = informated

    if (formatted) then
       read(fileID,*,iostat=errStat) charval
    else
       read(fileID,iostat=errStat) charval
    endif
    if(recretDEBUG.gt.0) print *,"Charval = ",charval
    if (errStat .ne. 0) then
       call recretReadError(fileID, errStat)
    endif

  end subroutine retchar

  subroutine reccharvecpt(fileID,charval,informated)

    implicit none
    
    logical,optional :: inFormated
    logical :: formatted=.FALSE.
    integer :: fileID,errstat
    character(len=*),dimension(:),pointer :: charval

    if(present(informated)) formatted = informated

    if(formatted) then
       write(fileID,*,iostat=errStat) size(charval)
    else
       write(fileID,iostat=errStat) size(charval)
    endif
    if (errStat .ne. 0) then
       call recretWriteError(fileID, errStat)
    endif

    if (formatted) then
       write(fileID,*,iostat=errStat) charval
    else
       write(fileID,iostat=errStat) charval
    endif
    if (errStat .ne. 0) then
       call recretWriteError(fileID, errStat)
    endif

  end subroutine reccharvecpt

  subroutine retcharvecpt(fileID,charval,informated)

    implicit none
    
    logical,optional :: inFormated
    logical :: formatted=.FALSE.
    integer :: fileID,errStat
    character(len=*),dimension(:),pointer :: charval
    integer :: veclen,charlen

    if (present(informated)) formatted = informated

    if(recretDEBUG .eq. 1) print *,"Entering retcharvecpt"

    if (formatted) then
       read(fileID,*,iostat=errStat) veclen
    else
       read(fileID,iostat=errStat) veclen
    endif
    if (errStat .ne. 0) then
       call recretReadError(fileID, errStat)
    endif

    if (formatted) then
       read(fileID,*,iostat=errStat) charlen
    else
       read(fileID,iostat=errStat) charlen
    endif
    if (errStat .ne. 0) then 
       call recretReadError(fileID, errStat)
    endif
    call getcharvecpt(fileID,charval,veclen,charlen)

    if(recretDEBUG .eq. 1) print *,"Leaving retcharvecpt"
  end subroutine retcharvecpt

  
  !< A helper routine for the retcharvecpt method
  subroutine getcharvecpt(fileID,charval,veclen,charlen,informated)

    implicit none

    logical,optional :: inFormated
    logical :: formatted=.FALSE.
    integer :: fileID,veclen,charlen,errStat
    character(len=charlen),dimension(:),pointer :: charval
    character(len=charlen),dimension(veclen),target :: charvalTarget

    if (present(informated)) formatted = informated

    if(associated(charval)) deallocate(charval)
    nullify(charval)

    if (formatted) then
       read(fileID,*,iostat=errStat) charvalTarget
    else
       read(fileID,iostat=errStat) charvalTarget
    endif
    if(recretDEBUG.gt.0) print *,"CharvalTarget =",charvalTarget(1),";"
    if(recretDEBUG.gt.0) print *,"CharvalTarget len =",&
         len(charvalTarget(1)),"; Should be:",charlen
    if (errStat .ne. 0) then
       call recretWriteError(fileID, errStat)
    endif
    if(associated(charval)) print *,"Should not be associated"
    charval => charvalTarget
    if(associated(charval,charvalTarget)) print *,"they're associated"
    allocate(charval(veclen))
    charval = charvalTarget
    if(associated(charval,charvalTarget)) print *,"they're associated"
    if(associated(charval)) print *,"but it's associated"
    if(recretDEBUG.gt.0) print *,"Charval =",charval(1),";"
    if(recretDEBUG.gt.0) print *,"Charval len =",len(charval(1)),&
         "; Should be:",charlen
    
  end subroutine getcharvecpt


end module recret
