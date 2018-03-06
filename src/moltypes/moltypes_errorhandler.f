module moltypes_errorhandler
  public
  integer,parameter :: MOLTYPES_NOERR       =  0
!
  integer,parameter :: IO_FMTERR            =  1
  integer,parameter :: IO_NOTEXIST          =  2
  integer,parameter :: IO_NOTREADABLE       =  3
  integer,parameter :: IO_NCFMTERR          =  4
  integer,parameter :: IO_NATOMERR          =  5
  integer,parameter :: IO_NOTWRITABLE       =  6
  integer,parameter :: IO_CANNOTOPEN        =  7
  integer,parameter :: IO_READERR           =  8
  integer,parameter :: IO_WRITERR           =  9
  integer,parameter :: IO_EMPTYATOM         = 10
!
  integer,parameter :: RMSK_EMPTY           =-51
  integer,parameter :: RMSK_INVALID_NATM    = 51
  integer,parameter :: RMSK_INVALID_NVAR    = 52
  integer,parameter :: RMSK_UNDIFINED_PARSE = 53
  integer,parameter :: RMSK_ILLEGAL_PARSE   = 54
!
  integer,parameter :: PRCS_EMPTY_TRAJ      = 91
!
  logical,parameter :: terminates_default   = .TRUE.
!
  character(:),allocatable,private :: ROUTINE
contains
  subroutine RoutineNameIs(RName)
  character(*),intent(in) :: RName
    if(.not.allocated(ROUTINE)) allocate(character(len_trim(RName))::ROUTINE)
    ROUTINE = trim(RName)
  end subroutine RoutineNameIs
!
    subroutine moltypes_echo_errmsg(ierr,filename)
    use,intrinsic :: ISO_FORTRAN_ENV, only : STDERR => ERROR_UNIT
    integer,intent(in)               :: ierr
    character(*),intent(in),optional :: filename
    character(:),allocatable         :: space
    integer                          :: io
      if(ierr==IO_NOERR) RETURN
      write(STDERR,'(a)',iostat=io) moltypes_return_errmsg(ierr)
      if(present(filename))then
        if(filename/='')then
          allocate(character(len_r+4)::space) ; space(:) = ''
          write(STDERR,'(a)',iostat=io) space//filename
          deallocate(space)
        endif
      endif
    end subroutine moltypes_echo_errmsg
!
    pure function moltypes_return_errmsg(ierr) result(res)
    integer,intent(in)           :: ierr
    character(:),allocatable     :: res,r
      allocate(character(0)::r)
      if(allocated(ROUTINE)) r = ROUTINE//' ::'
      allocate(character(0)::res)
      select case(ierr)
      case(IO_FMTERR)            ; res = r//' ASCII FILE FORMAT ERROR'
      case(IO_NOTEXIST)          ; res = r//' NO SUCH FILE'
      case(IO_NOTREADABLE)       ; res = r//' PERMISSION DENIED'
      case(IO_NCFMTERR)          ; res = r//' NetCDF FILE FORMAT ERROR'
      case(IO_NATOMERR)          ; res = r//' ERROR IN NUMBER OF ATOM'
      case(IO_NOTWRITABLE)       ; res = r//' ERROR IN WRITING PERMISSION DINED'
      case(IO_CANNOTOPEN)        ; res = r//' ERROR CANNOT OPEN'
      case(IO_READERR)           ; res = r//' ERROR OCCURED WHILE READING'
      case(IO_WRITERR)           ; res = r//' ERROR OCCURED WHILE WRITING'
      case(IO_EMPTYATOM)         ; res = r//' NO ATOM DATA IN'
      case(RMSK_EMPTY)           ; res = r//' THIS MASK ISNOT SETUP YET'
      case(RMSK_INVALID_NATM)    ; res = r//' NATOM IN MASK PARSER IS INVALID'
      case(RMSK_INVALID_NVAR)    ; res = r//' NVAR IN MASK PARSER IS INVALID'
      case(RMSK_UNDIFINED_PARSE) ; res = r//' MASK UNDIFINED'
      case(RMSK_ILLEGAL_PARSE)   ; res = r//' MASK IS INVALID'
      case default         ; res = r//' UNKNOWN ERROR.'
      end select
    end function moltypes_return_errmsg
!
    logical function CheckAbort(test,ierr,filename) result(res)
    logical,intent(in)               :: test
    integer,intent(in)               :: ierr
    character,intent(in),optional    :: filename
      res = test ; if(.not.res) RETURN
      if(present(filename))then
        call moltypes_echo_errmsg(ierr,filename)
      else
        call moltypes_echo_errmsg(ierr)
      endif
      call exit(ierr)
    end function CheckAbort
end module moltypes_errorhandler
