module spur_ncio
use netcdf
use spur_pathname
use spur_vector
  implicit none
!  include 'mpif.h'
  private
  public :: ncio
  public :: assignment(=)
!
  integer,parameter         :: IO_WRITING    =  3
  integer,parameter         :: IO_DEFINING   =  2
  integer,parameter         :: IO_READING    =  1
  integer,parameter         :: IO_CLOSING    =  0
!
  integer,parameter         :: LENGTH_NULL   = -1
  integer,parameter         :: NF90_XTYPE_ERROR = - (abs(NF90_BYTE)+abs(NF90_CHAR)+abs(NF90_SHORT)+&
                                                &    abs(NF90_INT)+abs(NF90_FLOAT)+abs(NF90_DOUBLE))
!
  type,extends(pathname) :: ncio
    private
    integer                :: ncid=0,stat=IO_CLOSING,io=NF90_NOERR,vid=0
!    logical                :: mpi = .FALSE.
!    integer                :: mpi_comm=MPI_COMM_WORLD,mpi_rank=0,mpi_info=MPI_INFO_NULL
    type(vector_integer)   :: length,xtype
    type(vector_character) :: dms,var
  contains
    include "spur_ncio_putvariable.h"
    include "spur_ncio_getvariable.h"
    procedure         :: fetch                  => NcFetch
    procedure         :: append                 => NcAppend
    procedure         :: generate               => NcGenerate
    procedure         :: add_dimension          => NcDefineDimension
    procedure         :: add_variable           => NcDefineVariable
    procedure         :: put_attribute          => NcPutAttribute
    procedure         :: get_attribute          => NcGetAttribute
    procedure         :: dim_length             => NcLength
    procedure         :: isreading              => NcIsReading
    procedure         :: isnotreading           => NcIsNotReading
    procedure         :: iswriting              => NcIsWriting
    procedure         :: isnotwriting           => NcIsNotWriting
    procedure         :: isdefining             => NcIsDefining
    procedure         :: isnotdefining          => NcIsNotDefining
    procedure         :: isopen                 => NcIsOpen
    procedure         :: isclose                => NcIsClose
    procedure         :: ioerr                  => NcIoERR
    procedure         :: devn                   => NcDevN
    procedure         :: quit                   => NcQuit
    procedure         :: StopsAtAbnormal        => NcStopsAtAbnormal
!   procedure         :: mpistart               => NcMpiStart
!   procedure         :: mpiquit                => NcMpiQuit
    final             :: NcDestractor
  end type ncio
!
  interface assignment(=)
    module procedure NcCopy,NcCopyArray
  end interface assignment(=)
contains
  include "spur_ncio_putvariable.f"
  include "spur_ncio_getvariable.f"
!
! subroutine NcMpiStart(this)
! class(ncio),intent(inout) :: this
! integer                   :: ierr
!   call MPI_Init(ierr)
!    this%mpi = ierr==0
!   if(.not.this%mpi) RETURN
!   call MPI_COMM_RANK(this%mpi_comm,this%mpi_rank,ierr)
! end subroutine  NcMpiStart
!
! subroutine NcMpiQuit(this)
! class(ncio),intent(inout) :: this
! integer :: ierr
!   if(.not.this%mpi)RETURN
!   call MPI_FINALIZE(ierr)
!   this%mpi_rank = 0 ; this%mpi = .FALSE.
! end subroutine NcMpiQuit
!
  subroutine NcFetch(this,path)
  class(ncio),intent(inout)         :: this
  character(*),intent(in),optional  :: path
  character(NF90_MAX_NAME)          :: xname
  integer                           :: ndim,nvar
  integer                           :: i,xtype,length,mode
  integer                           :: rank,proc,ierr
    if(present(path)) this = path
    if(this%isnotExist()) RETURN  ; if(this%isnotReadable()) RETURN
    if(this%isReading())RETURN
    call this%quit()
    mode = nf90_nowrite
    this%io = nf90_open(this%Is(),mode=mode,ncid=this%ncid)
    call NcStopsAtAbnormal(this) ; if(this%ioErr()) RETURN
!
    this%stat = IO_READING
    this%io   = nf90_inquire(this%ncid,nDimensions=ndim,nVariables=nvar)
print*,'fetch'
    call NcStopsAtAbnormal(this) ; if(this%ioErr()) RETURN
    call this%dms%clear() ; call this%length%clear()
    do i=1,ndim
      this%io = nf90_inquire_dimension(this%ncid,dimid=i,name=xname,len=length)
      call NcStopsAtAbnormal(this) ; if(this%ioErr()) RETURN
      call this%dms%push(trim(xname)) ; call this%length%push(length)
    enddo
    call this%var%clear() ; call this%xtype%clear()
    do i=1,nvar
      this%io = nf90_inquire_variable(this%ncid,varid=i,name=xname,xtype=xtype)
      call NcStopsAtAbnormal(this) ; if(this%ioErr()) RETURN
      call this%var%push(trim(xname)) ; call this%xtype%push(xtype)
    enddo
  end subroutine NcFetch
!
  subroutine NcAppend(this,path)
  class(ncio),intent(inout)         :: this
  character(*),intent(in),optional  :: path
  character(NF90_MAX_NAME)          :: xname
  integer                           :: ndim,nvar
  integer                           :: i,xtype,length,mode
  integer                           :: rank,proc,ierr
    if(present(path)) this = path ; if(this%isWriting())RETURN
    call this%quit()

    if(this%isExist())then
      mode = nf90_write
      this%io = nf90_open(this%Is(),mode=mode,ncid=this%ncid)
print*,'append'
return
    else
      mode = IOR(nf90_clobber,nf90_64bit_offset)
      this%io = nf90_create(this%Is(),cmode=mode,ncid=this%ncid)
print*,'append cr'
      call NcEndDefine(this)
    endif
    call NcStopsAtAbnormal(this) ; if(this%ioErr()) RETURN
!
    this%stat = IO_WRITING
    this%io   = nf90_inquire(this%ncid,nDimensions=ndim,nVariables=nvar)
    call NcStopsAtAbnormal(this) ; if(this%ioErr()) RETURN
    call this%dms%clear() ; call this%length%clear()
    do i=1,ndim
      this%io = nf90_inquire_dimension(this%ncid,dimid=i,name=xname,len=length)
      call NcStopsAtAbnormal(this) ; if(this%ioErr()) RETURN
      call this%dms%push(trim(xname)) ; call this%length%push(length)
    enddo
    call this%var%clear() ; call this%xtype%clear()
    do i=1,nvar
      this%io = nf90_inquire_variable(this%ncid,varid=i,name=xname,xtype=xtype)
      call NcStopsAtAbnormal(this) ; if(this%ioErr()) RETURN
      call this%var%push(trim(xname)) ; call this%xtype%push(xtype)
    enddo
  end subroutine NcAppend
!
  subroutine NcGenerate(this,path)
  class(ncio),intent(inout)        :: this
  character(*),intent(in),optional :: path
  integer                          :: system,mode
    if(present(path)) this = path ; if(this%is()=="")RETURN
!
    if(this%dirname()/='')then
      this%io = system('mkdir -p '//this%dirname()) ; if(this%io/=0) RETURN
    endif
!
    mode = IOR(nf90_clobber,nf90_64bit_offset)
    this%io = nf90_create(this%Is(),cmode=mode,ncid=this%ncid)
print*,'generate'
    call NcStopsAtAbnormal(this) ; if(this%ioErr()) RETURN
    this%stat = IO_DEFINING
  end subroutine NcGenerate
!
  subroutine NcDefineDimension(this,str)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: str
  type(vector_character)           :: words,hands
  integer                          :: i,length,did
    if(this%isnotDefining()) call NcReDefine(this)
    call NcStopsAtAbnormal(this) ; if(this%ioErr()) RETURN
!
    call words%split(str,delimiter=[' ',','])
!
    do i=1,words%size()
      this%io = 0
      length  = nf90_unlimited
      call hands%clear()
      call hands%split(words%at(i),delimiter=['='])
      if(hands%size()>2) CYCLE
      if(hands%size()==2)then
        length = hands%toint(2); if(length<=0)CYCLE
      endif
      this%io = nf90_def_dim(this%ncid,hands%at(1),length,did)
      call NcStopsAtAbnormal(this) ; if(this%ioErr()) RETURN
      call this%dms%push(hands%at(1))
      call this%length%push(length)
    enddo
  end subroutine NcDefineDimension
!
  subroutine NcDefineVariable(this,str)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: str
  type(vector_character)           :: words,hands
  type(vector_integer)             :: did
  integer                          :: i,irem,xtype,vid,tmp
    if(this%isnotDefining()) call NcReDefine(this)
    call NcStopsAtAbnormal(this) ; if(this%ioErr()) RETURN
!
    call hands%split(str,delimiter=[':']) ; if(hands%size()/=2) RETURN
    xtype = nf90_type(hands%at(1)) ; if(xtype==NF90_XTYPE_ERROR)RETURN
    irem = 1
    do
      call parseBracket(hands%at(2),words,irem) ; if(words%size()<=1) RETURN
      do i=2,words%size()
        tmp = this%dms%find(words%at(i)) ; if(tmp==0) RETURN ; call did%push(tmp)
      enddo
      this%io = nf90_def_var(this%ncid,name=words%at(1),xtype=xtype,dimids=did%lookup(),varid=vid)
      call NcStopsAtAbnormal(this) ; if(this%ioErr()) RETURN
      call did%clear() ; call this%var%push(words%at(1)) ; call this%xtype%push(xtype)
    enddo
  end subroutine NcDefineVariable
!
  subroutine NcPutAttribute(this,str)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: str
  type(vector_character)           :: words,hands
  integer                          :: vid
    call hands%split(str,delimiter=[':'])
    if(hands%size()==1)then
      vid = NF90_GLOBAL
      call words%split(hands%at(1),delimiter=['='])
    elseif(hands%size()==2)then
      vid = this%var%find(hands%at(1)) ; if(vid==0) RETURN
      call words%split(hands%at(2),delimiter=['='])
    else
      RETURN
    endif
    if(words%size()/=2)   RETURN
    if(this%isClose())       call this%fetch()
    if(this%isnotDefining()) call NcReDefine(this)
    if(this%isnotDefining()) RETURN
    this%io = nf90_put_att(this%ncid,vid,words%at(1),words%at(2))
  end subroutine NcPutAttribute
!
  function NcGetAttribute(this,str) result (res)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: str
  character(:),allocatable         :: res
  type(vector_character)           :: words,hands
  integer                          :: vid,length
    call hands%split(str,delimiter=[':'])
    if(hands%size()==1)then
      vid = NF90_GLOBAL
      call words%split(hands%at(1))
    elseif(hands%size()==2)then
      vid = this%var%find(hands%at(1)) ; if(vid==0) RETURN
      call words%split(hands%at(2))
    else
      allocate(character(0)::res) ; RETURN
    endif
    if(this%isClose())    call this%fetch()
    if(this%isDefining()) call NcEndDefine(this)
    if(this%isnotReading()) RETURN
    this%io = nf90_inquire_attribute(this%ncid,vid,words%at(1),len=length)
    allocate(character(length)::res)
    this%io = nf90_get_att(this%ncid,vid,words%at(1),res)
    if(this%terminates_at_abnormal)call NcStopsAtAbnormal(this)
  end function NcGetAttribute
!
  subroutine parseBracket(str,words,remain)
  character(*),intent(in)              :: str
  type(vector_character),intent(inout) :: words
  integer,intent(inout)                :: remain
  integer                              :: i,j
    call words%clear() ; j = remain
    do i=j,len(str)-1
      select case(str(i:i))
      case('(','[','{')
        if(i==0)RETURN
        remain = index(str(i:),ket(str(i:i))) + i
        if(remain==i)RETURN
        call words%push(str(j:i-1))
        call words%split(str(i+1:remain-2),delimiter=[' ',','])
        RETURN
      end select
    enddo
  contains
    pure function ket(bra)
    character(1),intent(in) :: bra
    character(1)            :: ket
      select case(bra)
      case('(') ; ket = ')'
      case('[') ; ket = ']'
      case('{') ; ket = '}'
      end select
    end function ket
  end subroutine parseBracket
!
  pure integer function NcLength(this,var) result(res)
  class(ncio),intent(in)           :: this
  character(*),intent(in)          :: var
  integer                          :: did
    did = this%dms%find(var)
    if(did==0)then
      res = LENGTH_NULL
    else
      res = this%length%at(did)
    endif
  end function NcLength
!
  pure integer function NcDevN(this) result(res)
  class(ncio),intent(in)       :: this
    res = this%ncid
  end function NcDevN
!
  pure logical function NcIsReading(this) result(res)
  class(ncio),intent(in)    :: this
    res = this%stat==IO_READING
  end function NcIsReading
!
  pure logical function NcIsNotReading(this) result(res)
  class(ncio),intent(in)    :: this
    res = this%stat/=IO_READING
  end function NcIsNotReading
!
  pure logical function NcIsWriting(this) result(res)
  class(ncio),intent(in)    :: this
    res = this%stat==IO_WRITING
  end function NcIsWriting
!
  pure logical function NcIsnotWriting(this) result(res)
  class(ncio),intent(in)    :: this
    res = this%stat/=IO_WRITING
  end function NcIsnotWriting
!
  pure logical function NcIsDefining(this) result(res)
  class(ncio),intent(in)    :: this
    res = this%stat==IO_DEFINING
  end function NcIsDefining
!
  pure logical function NcIsNotDefining(this) result(res)
  class(ncio),intent(in)    :: this
    res = this%stat/=IO_DEFINING
  end function NcIsNotDefining
!
  pure logical function NcIsOpen(this) result(res)
  class(ncio),intent(in)    :: this
    res = this%stat==IO_READING&
     &.or.this%stat==IO_DEFINING&
     &.or.this%stat==IO_WRITING
  end function NcIsOpen
!
  pure logical function NcIsClose(this) result(res)
  class(ncio),intent(in)    :: this
    res = .not.this%isOpen()
  end function NcIsClose
!
  pure logical function NcIoERR(this) result(res)
  class(ncio),intent(in)    :: this
    res = this%io/=nf90_NoErr
  end function NcIoERR
!
  subroutine NcStopsAtAbnormal(this)
  use,intrinsic :: ISO_FORTRAN_ENV, only : STDERR => ERROR_UNIT
  class(ncio),intent(inout) :: this
  integer                   :: rank,ierr
    if(this%io==nf90_NoErr) RETURN
!    if(this%terminates_at_abnormal)then
      write(STDERR,'(a)') trim(nf90_strerror(this%io))
      call NcDestractor(this) ; call exit(this%io)
!    endif
  end subroutine NcStopsAtAbnormal
!
  subroutine NcEndDefine(this)
  class(ncio),intent(inout) :: this
  integer                   :: io
    if(this%isWriting()) RETURN
    this%io = nf90_enddef(this%ncid)
    this%stat = IO_WRITING
  end subroutine NcEndDefine
!
  subroutine NcReDefine(this)
  class(ncio),intent(inout)        :: this
  integer                   :: io
    if(this%isDefining())RETURN
    this%io = nf90_redef(this%ncid)
    this%stat = IO_DEFINING
  end subroutine NcReDefine
!
  subroutine NcQuit(this)
  class(ncio),intent(inout) :: this
  integer                   :: io
    if(this%isClose()) RETURN
    if(this%isDefining()) call NcEndDefine(this)
    io = nf90_close(this%ncid)
    this%stat = IO_CLOSING
  end subroutine NcQuit
!
  pure integer function nf90_type(xtype) result(res)
  use spur_string, only : small
  character(*),intent(in)          :: xtype
    select case(small(xtype))
    case('byte','b') ;             res = NF90_BYTE
    case('character','char','c') ; res = NF90_CHAR
    case('short','s') ;            res = NF90_SHORT
    case('integer','int', 'i')   ; res = NF90_INT
    case('float',  'real','f')   ; res = NF90_FLOAT
    case('double', 'long','d')   ; res = NF90_DOUBLE
    case default                 ; res = NF90_XTYPE_ERROR
    end select
  end function nf90_type
!
  pure subroutine NcDestractor(this)
  type(ncio),intent(inout)  :: this
    call this%clear()
    call this%dms%clear()
    call this%var%clear()
    call this%length%clear()
    call this%xtype%clear()
    this%ncid=0 ; this%stat=IO_CLOSING ; this%io=NF90_NOERR ; this%vid=0
  end subroutine NcDestractor
!
  subroutine NcCopy(LHS,RHS)
    type(ncio),intent(inout) :: LHS
    type(ncio),intent(in)    :: RHS
    call NcDestractor(LHS)
    call LHS%path_assign(RHS)
    LHS%ncid = RHS%ncid ; LHS%stat = RHS%stat
    LHS%io   = RHS%io   ; LHS%vid  = RHS%vid
!   LHS%mpi  = RHS%mpi
!   LHS%mpi_comm = RHS%mpi_comm
!   LHS%mpi_rank = RHS%mpi_rank
!   LHS%mpi_info = LHS%mpi_info
    LHS%length   = RHS%length
    LHS%xtype    = RHS%xtype
    LHS%dms = RHS%dms
    LHS%var = RHS%var
  end subroutine NcCopy
!
  subroutine NcCopyArray(LHS,RHS)
    type(ncio),intent(inout) :: LHS(:)
    type(ncio),intent(in)    :: RHS(:)
    integer                  :: i
    do i=1,minval([size(LHS),size(RHS)],1)
      call NcCopy(LHS(i),RHS(i))
    enddo
  end subroutine NcCopyArray
end module spur_ncio
