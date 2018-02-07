module spur_ioerrorhandler
  public
!--- Sanity status
  integer,parameter         :: IO_UNKNOWN           = -9
  integer,parameter         :: IO_NOTEXIST          = -2
  integer,parameter         :: IO_ENDOFFILE         = -1
  integer,parameter         :: IO_NOERR             =  0
  integer,parameter         :: IO_CLOSING           =  1
  integer,parameter         :: IO_READING           =  2
  integer,parameter         :: IO_WRITING           =  3
  integer,parameter         :: IO_DEFINING          =  4
!
  integer,parameter,private :: N_SANITY             =  8
  integer,parameter,private :: N_OPENS              =  4
!
  integer,parameter         :: IO_OPENS(N_OPENS)    = [IO_WRITING,IO_DEFINING,&
                                                     & IO_READING,IO_ENDOFFILE]
  integer,parameter         :: IO_SANITYS(N_SANITY) = [IO_UNKNOWN,IO_ENDOFFILE,&
                                                     & IO_NOTEXIST,IO_WRITING,&
                                                     & IO_DEFINING,IO_READING,&
                                                     & IO_CLOSING,IO_NOERR]
!--- Error status
  integer,parameter,private :: N_ERROR              = 12
  integer,parameter         :: IO_FMTERR            = 11
  integer,parameter         :: IO_NOTREADABLE       = 12
  integer,parameter         :: IO_NOTWRITABLE       = 13
  integer,parameter         :: IO_EMPTYPATH         = 14
  integer,parameter         :: IO_OPENERR           = 15
  integer,parameter         :: IO_PERMERR           = 16
  integer,parameter         :: IO_READERR           = 17
  integer,parameter         :: IO_WRITERR           = 18
  integer,parameter         :: IO_MKDIRERR          = 19
  integer,parameter         :: IO_DEVERR            = 20
  integer,parameter         :: IO_DEFERR            = 21
  integer,parameter         :: IO_ILEGALOPT         = 22
  integer,parameter         :: IO_DISCONNECT        = 23
  integer,parameter         :: IO_ABNORMAL          = 99
  integer,parameter         :: IO_ERRORS(N_ERROR)   = [IO_FMTERR,IO_NOTREADABLE,&
                                                     & IO_NOTWRITABLE,IO_EMPTYPATH,&
                                                     & IO_OPENERR,IO_PERMERR,&
                                                     & IO_READERR,IO_WRITERR,&
                                                     & IO_MKDIRERR,IO_DEVERR,&
                                                     & IO_DEFERR,IO_ABNORMAL]
!
  logical,parameter         :: terminates_default   = .TRUE.
!
  character(:),allocatable,private :: ROUTINE
!
contains
  subroutine RoutineNameIs(RName)
  character(*),intent(in) :: RName
    if(.not.allocated(ROUTINE)) allocate(character(len_trim(RName))::ROUTINE)
    ROUTINE = trim(RName)
  end subroutine RoutineNameIs
!
  pure logical function CheckSanity(ierr) result(res)
  integer,intent(in) :: ierr
    res = ANY(ierr==IO_SANITYS)
  end function CheckSanity
!
  pure logical function CheckError(ierr) result(res)
  integer,intent(in) :: ierr
    res = ALL(ierr/=IO_SANITYS)
  end function CheckError
!
  pure logical function CheckOpen(ierr) result(res)
  integer,intent(in) :: ierr
    res = ANY(ierr==IO_OPENS)
  end function CheckOpen
!
  subroutine IO_echo_errmsg(ierr,filename)
  use,intrinsic :: ISO_FORTRAN_ENV, only : STDERR => ERROR_UNIT
  integer,intent(in)               :: ierr
  character(*),intent(in),optional :: filename
  character(:),allocatable         :: space
  integer                          :: io
    if(CheckSanity(ierr)) RETURN
    write(STDERR,'(a)',iostat=io) IO_return_errmsg(ierr)
    if(present(filename))then
      if(filename=='')RETURN
      allocate(character(len_trim(ROUTINE)+4)::space) ; space(:) = ''
      write(STDERR,'(a)',iostat=io) space//filename
      deallocate(space)
    endif
  end subroutine IO_echo_errmsg
!
  pure function IO_return_errmsg(ierr) result(res)
  integer,intent(in)           :: ierr
  character(:),allocatable     :: r,res
    allocate(character(0)::res,r)
    if(allocated(ROUTINE)) r = ROUTINE//' ::'
    select case(ierr)
    case(IO_FMTERR)      ; res = r//' FILE FORMAT ERROR'
    case(IO_NOTEXIST)    ; res = r//' NO SUCH FILE'
    case(IO_NOTREADABLE) ; res = r//' PERMISSION DENIED (READING)'
    case(IO_NOTWRITABLE) ; res = r//' PERMISSION DINED (WRITING)'
    case(IO_EMPTYPATH)   ; res = r//' PATHNAME IS EMPTY'
    case(IO_OPENERR)     ; res = r//' ERROR OCCURED OPEN FILE'
    case(IO_PERMERR)     ; res = r//' PERMISSION DINED'
    case(IO_READERR)     ; res = r//' ERROR OCCURED WHILE READING'
    case(IO_WRITERR)     ; res = r//' ERROR OCCURED WHILE WRITING'
    case(IO_MKDIRERR)    ; res = r//' ERROR OCCURED WHILE MKDIR'
    case(IO_DEVERR)      ; res = r//' TOO MANY OPEN FILES)'
    case(IO_DEFERR)      ; res = r//' ERROR OCCURED WHILE DEFINING)'
    case default         ; res = r//' UNKNOWN ERROR.'
    end select
    deallocate(r)
  end function IO_return_errmsg
end module spur_ioerrorhandler
