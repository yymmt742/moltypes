module spur_ncio
use netcdf
use spur_vector_int4
use spur_vector_chr
use spur_pathname
use spur_ioerrorhandler
  implicit none
!  include 'mpif.h'
  private
  public :: ncio
  public :: assignment(=)
!
  integer,parameter         :: LENGTH_NULL   = -1
  integer,parameter         :: NF90_XTYPE_ERROR = - (abs(NF90_BYTE)+abs(NF90_CHAR)+abs(NF90_SHORT)+&
                                                &    abs(NF90_INT)+abs(NF90_FLOAT)+abs(NF90_DOUBLE))
!
  type,extends(pathname) :: ncio
    private
    integer                :: ncid=0,stat=IO_CLOSING,io=NF90_NOERR
    type(vector_int4)      :: length,xtype
    type(vector_chr)       :: dms,var
  contains
    include "spur_ncio_putvariable.h"
    include "spur_ncio_getvariable.h"
    procedure         :: generate               => NcGenerate
    procedure         :: Loadheader             => NcLoadheader
    procedure         :: add_dimension          => NcAddDimension
    procedure         :: add_variable           => NcAddVariable
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
    procedure         :: issanity               => NcIsSanity
    procedure         :: iserr                  => NcIsERR
    procedure         :: quit                   => NcQuit
    procedure         :: clear                  => NcClear
    final             :: NcDestractor
  end type ncio
!
  interface assignment(=)
    module procedure NcCopy
  end interface assignment(=)
contains
  include "spur_ncio_putvariable.f"
  include "spur_ncio_getvariable.f"
!
  subroutine NcGenerate(this)
  class(ncio),intent(inout)        :: this
  integer                          :: is,system,mode
    call RoutineNameIs('NCIO_GENERATE')
    if(CheckAbort(this,this%is()=='',IO_EMPTYPATH))RETURN
    if(CheckOpen(this%stat)) call this%quit()
!
    if(this%dirname()/='')then
      is = system('mkdir -p '//this%dirname())
      if(CheckAbort(this,is/=0,IO_MKDIRERR,this%is()))RETURN
    endif
!
    mode = IOR(nf90_clobber,nf90_64bit_offset)
    this%io = nf90_create(this%Is(),cmode=mode,ncid=this%ncid)
    if(CheckAbort(this,ierr=IO_OPENERR,filename=this%is()))RETURN
    this%stat = IO_DEFINING
  end subroutine NcGenerate
!
  logical function NcReadOpen(this) result(res)
  class(ncio),intent(inout) :: this
  integer                   :: mode
    res = this%isnotReading() ; if(this%isReading())RETURN
    call this%quit()
    mode = nf90_nowrite
    this%io = nf90_open(this%Is(),mode=mode,ncid=this%ncid)
    res = CheckAbort(this,ierr=IO_OPENERR,filename=this%is())
    if(res)RETURN ; this%stat = IO_READING
  end function NcReadOpen
!
  logical function NcWriteOpen(this) result(res)
  class(ncio),intent(inout) :: this
  integer                   :: mode
    res = this%isnotWriting() ; if(this%isWriting())RETURN
    call this%quit()
    mode = nf90_write
    this%io = nf90_open(this%Is(),mode=mode,ncid=this%ncid)
    res = CheckAbort(this,ierr=IO_OPENERR,filename=this%is())
    if(res)RETURN ; this%stat = IO_WRITING
  end function NcWriteOpen
!
  logical function NcEndDefine(this) result(res)
  class(ncio),intent(inout) :: this
    res = this%isDefining() ; if(.not.res)RETURN
    this%io = nf90_enddef(this%ncid)
    res = CheckAbort(this,ierr=IO_OPENERR,filename=this%is())
    if(res)RETURN ; this%stat = IO_WRITING
  end function NcEndDefine
!
  logical function NcReDefine(this) result(res)
  class(ncio),intent(inout) :: this
    res = this%isnotDefining() ; if(this%isDefining())RETURN
    res = CheckAbort(this,NcWriteOpen(this),ierr=IO_OPENERR,filename=this%is())
    if(res)RETURN
    this%io = nf90_redef(this%ncid)
    res = CheckAbort(this,ierr=IO_OPENERR,filename=this%is())
    if(res)RETURN ; this%stat = IO_DEFINING
  end function NcReDefine
!
  subroutine NcAddDimension(this,str)
  use spur_string_tonum
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: str
  type(vector_chr)                 :: words,hands
  integer                          :: i,length,did
    call RoutineNameIs('NCIO_ADD_DIMENSION')
    if(CheckAbort(this,NcReDefine(this),ierr=IO_OPENERR,filename=this%is())) RETURN
    call words%split(str,delimiter=' ,')
!
    do i=1,words%size()
      this%io = 0
      length  = nf90_unlimited
      call hands%clear()
      call hands%split(words%at(i),delimiter='=')
      if(hands%size()>2) CYCLE
      if(hands%size()==2)then
        length = tonum_int4(hands%at(2),0); if(length<=0)CYCLE
      endif
      this%io = nf90_def_dim(this%ncid,hands%at(1),length,did)
      if(CheckAbort(this,ierr=IO_DEFERR,filename=this%is())) RETURN
      call this%dms%push(hands%at(1))
      call this%length%push(length)
    enddo
    if(CheckAbort(this,NcEndDefine(this),ierr=IO_OPENERR,filename=this%is())) RETURN
  end subroutine NcAddDimension
!
  subroutine NcAddVariable(this,str)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: str
  type(vector_chr)                 :: words,hands
  type(vector_int4)                :: did
  integer                          :: i,irem,xtype,vid,tmp
    call RoutineNameIs('NCIO_ADD_VARIABLE')
    if(CheckAbort(this,NcReDefine(this),ierr=IO_OPENERR,filename=this%is())) RETURN
!
    call hands%split(str,delimiter=':') ; if(hands%size()/=2) RETURN
    xtype = nf90_type(hands%at(1))
    if(CheckAbort(this,xtype==NF90_XTYPE_ERROR,ierr=IO_DEFERR,filename=this%is())) RETURN
    irem = 1
    do
      call parseBracket(hands%at(2),words,irem) ; if(words%size()<2) RETURN
      do i=2,words%size()
        tmp = this%dms%find(words%at(i)) ; if(tmp==0) RETURN ; call did%push(tmp)
      enddo
      this%io = nf90_def_var(this%ncid,name=words%at(1),xtype=xtype,dimids=did%lookup(),varid=vid)
      if(CheckAbort(this,ierr=IO_DEFERR,filename=this%is())) RETURN
      call did%clear() ; call this%var%push(words%at(1)) ; call this%xtype%push(xtype)
    enddo
    if(CheckAbort(this,NcEndDefine(this),ierr=IO_OPENERR,filename=this%is())) RETURN
  end subroutine NcAddVariable
!
  subroutine NcPutAttribute(this,str)
  class(ncio),intent(inout)        :: this
  character(*),intent(in)          :: str
  type(vector_chr)                 :: words,hands
  integer                          :: vid
    call hands%split(str,delimiter=':')
    if(hands%size()==1)then
      vid = NF90_GLOBAL
      call words%split(hands%at(1),delimiter='=')
    elseif(hands%size()<=2)then
      vid = this%var%find(hands%at(1)) ; if(vid==0) RETURN
      call words%split(hands%at(2),delimiter='=')
    endif
    if(words%size()<2) RETURN
    if(CheckAbort(this,NcReDefine(this),ierr=IO_OPENERR,filename=this%is())) RETURN
    this%io = nf90_put_att(this%ncid,vid,words%at(1),words%at(2))
    if(CheckAbort(this,ierr=IO_DEFERR,filename=this%is())) RETURN
    if(CheckAbort(this,NcEndDefine(this),ierr=IO_OPENERR,filename=this%is())) RETURN
  end subroutine NcPutAttribute
!
  subroutine NcLoadheader(this)
  class(ncio),intent(inout)         :: this
  character(NF90_MAX_NAME)          :: xname
  integer                           :: ndim,nvar
  integer                           :: i,j,io,xtype,length
    call RoutineNameIs('NCIO_LOAD_HEADER')
    if(CheckAbort(this,NcReadOpen(this),ierr=IO_OPENERR,filename=this%is())) RETURN
!
    this%io = nf90_inquire(this%ncid,nDimensions=ndim,nVariables=nvar)
    if(CheckAbort(this,ierr=IO_READERR,filename=this%is())) RETURN
!
    call this%dms%clear() ; call this%length%clear()
    call this%var%clear() ; call this%xtype%clear()
    do i=1,ndim
      this%io = nf90_inquire_dimension(this%ncid,dimid=i,name=xname,len=length)
      if(CheckAbort(this,ierr=IO_READERR,filename=this%is())) RETURN
      call this%dms%push(trim(xname)) ; call this%length%push(length)
    enddo
    do i=1,nvar
      this%io = nf90_inquire_variable(this%ncid,varid=i,name=xname,xtype=xtype)
      if(CheckAbort(this,ierr=IO_READERR,filename=this%is())) RETURN
      call this%var%push(trim(xname)) ; call this%xtype%push(xtype)
    enddo
    call this%quit() ; if(CheckAbort(this,ierr=IO_OPENERR,filename=this%is())) RETURN
  end subroutine NcLoadheader
!
  function NcGetAttribute(this,str) result (res)
  class(ncio),intent(inout) :: this
  character(*),intent(in)   :: str
  character(:),allocatable  :: res
  character(NF90_MAX_NAME)  :: xname
  type(vector_chr)          :: words,hands
  integer                   :: vid,length
    allocate(character(0)::res)
    call hands%split(str,delimiter=':')
    if(hands%size()==1)then
      vid = NF90_GLOBAL ; call words%split(hands%at(1))
    elseif(hands%size()>=2)then
      vid = this%var%find(hands%at(1)) ; if(vid==0) RETURN
      call words%split(hands%at(2))
    else ; RETURN
    endif
    if(CheckAbort(this,NcReadOpen(this),ierr=IO_OPENERR,filename=this%is())) RETURN
    this%io = nf90_get_att(this%ncid,vid,words%at(1),xname)
    if(CheckAbort(this,ierr=IO_READERR,filename=this%is()))RETURN
    res = xname
  end function NcGetAttribute
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
  pure subroutine parseBracket(str,words,remain)
  character(*),intent(in)              :: str
  type(vector_chr),intent(inout)       :: words
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
        call words%split(str(i+1:remain-2),delimiter=' ,')
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
  subroutine NcQuit(this)
  class(ncio),intent(inout) :: this
    this%stat = IO_CLOSING
    if(nf90_inquire(this%ncid)/=NF90_NOERR) RETURN
    if(CheckAbort(this,NcEndDefine(this),ierr=IO_OPENERR,filename=this%is()))RETURN
    this%io = nf90_close(this%ncid)
  end subroutine NcQuit
!
  pure integer function nf90_type(xtype) result(res)
  use spur_string_neaten
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
    res = CheckOpen(this%stat)
  end function NcIsOpen
!
  pure logical function NcIsClose(this) result(res)
  class(ncio),intent(in)    :: this
    res = .not.this%isOpen()
  end function NcIsClose
!
  pure logical function NcIsSanity(this) result(res)
  class(ncio),intent(in)    :: this
    res = CheckSanity(this%stat).and.this%io==NF90_NOERR
  end function NcIsSanity
!
  pure logical function NcIsERR(this) result(res)
  class(ncio),intent(in)    :: this
    res = CheckError(this%stat).or.this%io/=NF90_NOERR
  end function NcIsERR
!
  logical function CheckAbort(this,test,ierr,filename) result(res)
  use,intrinsic :: ISO_FORTRAN_ENV, only : STDERR => ERROR_UNIT
  type(ncio),intent(inout)    :: this
  logical,intent(in),optional :: test
  integer,intent(in),optional :: ierr
  character(*),intent(in),optional :: filename
    if(present(test))then
      res = test
    else
      res = this%iserr()
    endif
    if(.not.res) RETURN
    if(present(ierr)) this%stat = ierr
    if(this%terminates_at_abnormal)then
      write(STDERR,'(a)') trim(nf90_strerror(this%io))
      if(present(filename))then
        call IO_echo_errmsg(this%stat,filename)
      else
        call IO_echo_errmsg(this%stat)
      endif
      call NcDestractor(this) ; call exit(ierr)
    endif
    if(present(ierr)) this%stat = ierr
  end function CheckAbort
!
  pure elemental subroutine NcClear(this)
  class(ncio),intent(inout)  :: this
    call NcDestractor(this)
  end subroutine
!
  pure subroutine NcDestractor(this)
  type(ncio),intent(inout)  :: this
    call this%PathNameClear()
    call this%dms%clear()
    call this%var%clear()
    call this%length%clear()
    call this%xtype%clear()
    this%ncid=0 ; this%stat=IO_CLOSING ; this%io=NF90_NOERR
  end subroutine NcDestractor
!
  pure elemental subroutine NcCopy(LHS,RHS)
  type(ncio),intent(inout) :: LHS
  type(ncio),intent(in)    :: RHS
    call NcDestractor(LHS)
    call LHS%fetch(RHS)
    LHS%ncid = RHS%ncid ; LHS%stat = RHS%stat
    LHS%io   = RHS%io
    LHS%length = RHS%length
    LHS%xtype  = RHS%xtype
    LHS%dms    = RHS%dms
    LHS%var    = RHS%var
  end subroutine NcCopy
end module spur_ncio
