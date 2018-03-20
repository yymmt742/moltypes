module spur_optparse2
use spur_vector_chr
  implicit none
  private
  public :: optparse2
!  public :: assignment(=)
!
  character(10),parameter :: RoutineName = "Optparser:"
  character(14),parameter :: help_def    = 'no help entry.'
  character(4),parameter  :: Metavar_def = 'FILE'
  integer,save            :: Maxlen      = 0
!
  type OptNode
    type(vector_chr)     :: s, arg, def
    logical                    :: isExist = .FALSE.
    integer                    :: Narg = 0
  contains
    final     :: OptNodeDestractor
  end type OptNode
!
  type optparse2
    private
    character(:),allocatable   :: desc
    type(vector_chr)           :: CommandArg,FixedArg
    type(vector_chr)           :: helpargs,help,Metavar
    character(:),allocatable   :: ScriptName,VersionNumber
    type(optnode),allocatable  :: opt(:)
    type(vector_chr)           :: error,warn
    integer                    :: Nopt=0, stack=0
    logical                    :: hasarg=.FALSE.
    logical                    :: helper=.TRUE.
  contains
!   procedure         :: add_description => DescAdd
!   procedure         :: add_option      => OptAdd
!   procedure         :: call_usage      => OptPrintUsage
!   procedure         :: parser          => Optparser
!   procedure         :: option          => OptOption
!   procedure         :: noptarg         => OptNOptarg
!   procedure         :: narg            => OptNarg
!   procedure,private :: OptOptargListD
!   procedure,private :: OptOptargIdxD
!   generic           :: optargd         => OptOptargListD,OptOptargIdxD
!   procedure,private :: OptOptargListF
!   procedure,private :: OptOptargIdxF
!   generic           :: optargf         => OptOptargListF,OptOptargIdxF
!   procedure,private :: OptOptargListI
!   procedure,private :: OptOptargIdxI
!   generic           :: optargi         => OptOptargListI,OptOptargIdxI
!   procedure,private :: OptOptargList
!   procedure,private :: OptOptargIdx
!   generic           :: optargs         => OptOptargList,OptOptargIdx
!   procedure,private :: OptArgList
!   procedure,private :: OptArgIdx
!   generic           :: args            => OptArgList,OptArgIdx
!   final     :: OptDestractor
  end type optparse2
!
! interface assignment(=)
!   module procedure ErrAssign
! end interface assignment(=)
!
contains
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!                    Getopt ROUTINEs                       
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! subroutine OptInit(this)
! class(Optparse),intent(inout)   :: this
! character(256)                  :: Arg
! type(vector_character)          :: optlist
! integer                         :: i,j,is,Narg,slash
!   Narg = COMMAND_ARGUMENT_COUNT()
!   call this%CommandArg%clear()
!
!   call GET_COMMAND_ARGUMENT(0,Arg,status=is)
!   if(.not.allocated(this%ScriptName))allocate(character(0)::this%ScriptName)
!   if(.not.allocated(this%VersionNumber))allocate(character(0)::this%VersionNumber)
!   slash = index(Arg,"/",back=.TRUE.)
!   this%ScriptName = trim(Arg(slash+1:)) ; this%hasArg = .TRUE.
!   do i=1,Narg
!     call GET_COMMAND_ARGUMENT(i,Arg,status=is)
!     call this%CommandArg%split(trim(Arg),["="])
!   enddo
!   maxlen = maxval([6,maxlen,this%CommandArg%maxlength()],1) ; RETURN
! end subroutine OptInit
!
! subroutine Optparser(this,help,version)
! use,intrinsic :: ISO_FORTRAN_ENV, only : ERROR_UNIT
! use spur_string
! class(Optparse),intent(inout)    :: this
! logical,intent(in),optional      :: help
! character(*),intent(in),optional :: version
! type(vector_integer)             :: head,tail,optidx
! integer                          :: i,j,k,NARG
! logical                          :: isExist
!   if(.not.this%hasArg)call OptInit(this)
!
!   this%helper = .FALSE.
!   if(present(help))this%helper=help
!   if(this%helper.and.any(["-h    ","--help"]==this%CommandArg)) call OptPrintUsage(this)
!   if(present(version))this%VersionNumber=version
!   if(present(version).and.any(["-V       ","--version"]==this%CommandArg)) call OptPrintVersion(this)
!
!   do j=1,this%CommandArg%size()
!     do i=1,this%Nopt
!       isExist = any(this%opt(i)%s==this%CommandArg%at(j))
!       if(this%opt(i)%isExist.and.isExist)then
!         call this%warn%push('dupulicate option was found '//this%CommandArg%at(j))
!       endif
!       if(isExist)then
!         this%opt(i)%isExist = isExist ; call head%push(j) ; call optidx%push(i) ; EXIT
!       endif
!     enddo
!     if(this%CommandArg%at(j,1,1)=="-".and..not.isExist)then
!       call this%warn%push('Unknown option was found '//this%CommandArg%at(j))
!     endif
!   enddo
!
!   call tail%reserve(head%size())
!   do i=2,head%size()
!     call tail%push(head%at(i)-1)
!   enddo
!   call tail%push(this%CommandArg%size())
!
!   if(head%empty().and..not.this%CommandArg%empty()) call this%FixedArg%push(this%CommandArg%lookup())
!
!   do i= 1,head%size()
!     k = optidx%at(i)
!     NARG = 0
!     do j=head%at(i)+1,tail%at(i)
!       NARG = NARG + 1
!       if(NARG<=this%opt(k)%NARG)then
!         call this%opt(k)%arg%push(this%CommandArg%at(j))
!       else
!         call this%FixedArg%push(this%CommandArg%at(j))
!       endif
!     enddo
!     if(this%opt(k)%Narg/=this%opt(k)%arg%size().and.this%opt(k)%Narg>=0)then
!       call this%error%push('argument size is not mutch in '//join(this%opt(k)%s%lookup(),"|"))
!     endif
!   enddo
!
!   do i = 1,this%warn%size()
!     write(ERROR_UNIT,'(A)') RoutineName//": WARNING) "//this%warn%at(i)
!   enddo
!   do i = 1,this%error%size()
!     write(ERROR_UNIT,'(A)') RoutineName//": ERROR) "//this%error%at(i)
!   enddo
!   if(this%error%size()>0)call exit(1)
!   RETURN
! end subroutine Optparser
!
! subroutine DescAdd(this,string)
! class(Optparse),intent(inout)    :: this
! character(*),intent(in)          :: String
!   if(.not.allocated(this%desc)) allocate(character(0)::this%desc)
!   this%desc = string
!   RETURN
! end subroutine DescAdd
!
! subroutine OptAdd(this,String,alias,Narg,def,help,metavar)
! class(Optparse),intent(inout)    :: this
! character(*),intent(in)          :: String
! character(*),intent(in),optional :: alias(:),def(:),help,metavar
! integer,intent(in),optional      :: Narg
! type(vector_character)           :: tmp
! integer                          :: i
!   this%Nopt = this%Nopt + 1
!   call OptPush(String)
!   if(present(alias))then
!     do i=lbound(alias,1),ubound(alias,1)
!       call OptPush(alias(i))
!     enddo
!   endif
!
!   if(present(Narg)) this%opt(this%Nopt)%Narg = Narg
!   if(present(def))then
!     call this%opt(this%Nopt)%def%push(def)
!     maxlen = maxval([maxlen,this%opt(this%Nopt)%def%maxlength()],1)
!   endif
!   if(present(help))then    ; call this%help%push(help)
!   else                     ; call this%help%push(help_def)
!   endif
!   if(present(metavar))then ; call this%Metavar%push(metavar)
!   else                     ; call this%Metavar%push(metavar_def)
!   endif
!   call tmp%push("[ "//string)
!   do i=1,this%opt(this%Nopt)%Narg
!     call tmp%push(this%Metavar%at(this%Nopt))
!   enddo
!   call tmp%push("]")
!   call this%helpargs%push(tmp%join())
! contains
!   subroutine OptPush(string)
!   character(*),intent(in)        :: String
!   integer                        :: i
!   if(any([(any(this%opt(i)%s==String),i=1,this%Nopt-1)]))then
!     call this%warn%push("dupulicate option was defined "//String)
!   endif
!   call OptExtention(this)
!   call this%opt(this%Nopt)%s%push(string)
!   RETURN
!   end subroutine OptPush
! end subroutine OptAdd
!
! pure subroutine OptExtention(this)
! class(Optparse),intent(inout):: this
! integer                      :: OldStack
! type(optnode),allocatable    :: TmpOpt(:)
!   if(this%Stack>=this%nopt)RETURN
!   if(this%Stack<=0)this%Stack = 1
!   OldStack = this%Stack
!   do while(this%Stack<this%Nopt)
!     this%Stack = this%Stack * 2
!   enddo
!   allocate(TmpOpt(this%Stack))
!   if(allocated(this%opt))call optassign(this%Opt(1:OldStack),TmpOpt(1:OldStack))
!   call move_alloc(from=TmpOpt,to=this%opt)
! contains
!   elemental subroutine optassign(from,to)
!   type(optnode),intent(in)  :: from
!   type(optnode),intent(out) :: to
!     to%s = from%s ; to%arg = from%arg ; to%def = from%def
!     to%isExist = from%isExist ; to%Narg = from%Narg
!   end subroutine optassign
! end subroutine OptExtention
!
! subroutine OptPrintUsage(this)
! use,intrinsic :: ISO_FORTRAN_ENV, only : OUTPUT_UNIT
! class(Optparse),intent(in)  :: this
! type(vector_character)      :: optlist, helpargs
! character(50)               :: cfmt
! integer                     :: headspace
! integer                     :: i,j,k
!   headspace = 17
!   if(allocated(this%desc)) write(OUTPUT_UNIT,'(A,/)')this%ScriptName//" :: "//this%desc
!   write(OUTPUT_UNIT,'(A)',ADVANCE='NO')'Usage: '//this%ScriptName//" "
!   call helpargs%TextWrap(this%helpargs%Join(delimiter="@"),60,delimiter=["@"])
!   if(helpargs%size()>0)then
!     write(OUTPUT_UNIT,'(A)')helpargs%at(1)
!   else
!     write(OUTPUT_UNIT,'(A)')''
!   endif
!   write(cfmt,'(A,I0,A)')'(',len('Usage: '//this%ScriptName)+1,'X,A)'
!   do i=2,helpargs%size()
!     write(OUTPUT_UNIT,cfmt)helpargs%at(i)
!   enddo
!
!   write(OUTPUT_UNIT,'(a)')''
!
!   do i=1,this%Nopt
!     do j = 1,this%opt(i)%s%size()
!       call optlist%push(this%opt(i)%s%at(j))
!       do k=1,this%opt(i)%narg
!         call optlist%push("<"//this%Metavar%at(i)//">")
!       enddo
!       if(j==this%opt(i)%s%size())EXIT
!       call optlist%push("|")
!     enddo
!     write(OUTPUT_UNIT,'(8X,A)',ADVANCE='NO') optlist%join(delimiter=" ")
!     if(len_trim(optlist%join(delimiter=" "))>=headspace)then
!       write(OUTPUT_UNIT,'(/,25X,A)',ADVANCE='NO')":"
!     else
!       write(cfmt,'(A,I0,A)')'(',headspace-len_trim(optlist%join(delimiter=' ')),'X,A)'
!       write(OUTPUT_UNIT,cfmt,ADVANCE='NO')":"
!     endif
!     write(OUTPUT_UNIT,'(X,A)')this%help%at(i)
!     call optlist%erace()
!   enddo
!   call exit(1)
! end subroutine OptPrintUsage
!
! subroutine OptPrintVersion(this)
! use,intrinsic :: ISO_FORTRAN_ENV, only : OUTPUT_UNIT
! class(Optparse),intent(in)  :: this
!   write(OUTPUT_UNIT,'(A)')this%ScriptName//': '//this%VersionNumber
!   call exit(0)
! end subroutine OptPrintVersion
!
! elemental function OptOption(this,String) result(res)
! class(Optparse),intent(in)    :: this
! logical                       :: res
! character(*),intent(in)       :: String
! integer                       :: at
!   res = .FALSE. ; if(.not.this%hasArg)RETURN
!   do at=1,this%Nopt
!     if(any(string==this%opt(at)%s))then
!       res = this%opt(at)%isExist ; RETURN
!     endif
!   enddo
! end function OptOption
!
! pure function OptNOptarg(this,String) result(res)
! class(Optparse),intent(in)         :: this
! character(*),intent(in)            :: String
! integer                            :: res
! integer                            :: at
!   do at=1,this%Nopt
!     if(any(string==this%opt(at)%s))then
!       res = this%opt(at)%arg%size() ; RETURN
!     endif
!   enddo
!   res=0 ; RETURN
! end function OptNOptarg
!
! elemental function OptOptargIdxD(this,String,idx) result(res)
! class(Optparse),intent(in)         :: this
! character(*),intent(in)            :: String
! integer,intent(in)                 :: idx
! double precision                   :: res
! integer                            :: at
!   do at=1,this%Nopt
!     if(any(string==this%opt(at)%s))then
!       if(idx>this%opt(at)%Narg.or.idx<0)EXIT
!       if(this%opt(at)%arg%size()==0)then
!         res = arg2dble(this%opt(at)%def%at(idx)) ; RETURN
!       else
!         res = arg2dble(this%opt(at)%arg%at(idx)) ; RETURN
!       endif
!     endif
!   enddo
!   res=0.d0 ; RETURN
! end function OptOptargIdxD
!
! pure function OptOptargListD(this,String) result(res)
! class(Optparse),intent(in)         :: this
! character(*),intent(in)            :: String
! double precision,allocatable       :: res(:)
! integer                            :: at
!   do at=1,this%Nopt
!     if(any(string==this%opt(at)%s))then
!       if(this%opt(at)%Narg==0)EXIT
!       allocate(res(this%opt(at)%Narg))
!       if(this%opt(at)%arg%size()==0)then
!         res = arg2dble(this%opt(at)%def%lookup()) ; RETURN
!       else
!         res = arg2dble(this%opt(at)%arg%lookup()) ; RETURN
!       endif
!     endif
!   enddo
!   allocate(res(1)) ; res=0.d0 ; RETURN
! end function OptOptargListD
!
! elemental function OptOptargIdxF(this,String,idx) result(res)
! class(Optparse),intent(in)         :: this
! character(*),intent(in)            :: String
! integer,intent(in)                 :: idx
! real                               :: res
! integer                            :: at
!   do at=1,this%Nopt
!     if(any(string==this%opt(at)%s))then
!       if(idx>this%opt(at)%Narg.or.idx<0)EXIT
!       if(this%opt(at)%arg%size()==0)then
!         res = arg2real(this%opt(at)%def%at(idx)) ; RETURN
!       else
!         res = arg2real(this%opt(at)%arg%at(idx)) ; RETURN
!       endif
!     endif
!   enddo
!   res=0.0 ; RETURN
! end function OptOptargIdxF
!
! pure function OptOptargListF(this,String) result(res)
! class(Optparse),intent(in)         :: this
! character(*),intent(in)            :: String
! real,allocatable                   :: res(:)
! integer                            :: at
!   do at=1,this%Nopt
!     if(any(string==this%opt(at)%s))then
!       if(this%opt(at)%Narg==0)EXIT
!       allocate(res(this%opt(at)%Narg))
!       if(this%opt(at)%arg%size()==0)then
!         res = arg2real(this%opt(at)%def%lookup()) ; RETURN
!       else
!         res = arg2real(this%opt(at)%arg%lookup()) ; RETURN
!       endif
!     endif
!   enddo
!   allocate(res(1)) ; res=0.0 ; RETURN
! end function OptOptargListF
!
! elemental function OptOptargIdxI(this,String,idx) result(res)
! class(Optparse),intent(in)         :: this
! character(*),intent(in)            :: String
! integer,intent(in)                 :: idx
! integer                            :: res
! integer                            :: at
!   do at=1,this%Nopt
!     if(any(string==this%opt(at)%s))then
!       if(idx>this%opt(at)%Narg.or.idx<0)EXIT
!       if(this%opt(at)%arg%size()==0)then
!         res = arg2int(this%opt(at)%def%at(idx)) ; RETURN
!       else
!         res = arg2int(this%opt(at)%arg%at(idx)) ; RETURN
!       endif
!     endif
!   enddo
!   res=0 ; RETURN
! end function OptOptargIdxI
!
! pure function OptOptargListI(this,String) result(res)
! class(Optparse),intent(in)         :: this
! character(*),intent(in)            :: String
! integer,allocatable                :: res(:)
! integer                            :: at
!   do at=1,this%Nopt
!     if(any(string==this%opt(at)%s))then
!       if(this%opt(at)%Narg==0)EXIT
!       allocate(res(this%opt(at)%Narg))
!       if(this%opt(at)%arg%size()==0)then
!         res = arg2int(this%opt(at)%def%lookup()) ; RETURN
!       else
!         res = arg2int(this%opt(at)%arg%lookup()) ; RETURN
!       endif
!     endif
!   enddo
!   allocate(res(1)) ; res=0 ; RETURN
! end function OptOptargListI
!
! elemental function arg2dble(c) result(res)
! character(*),intent(in)   :: c
! double precision          :: res
! integer                   :: is
!   read(c,*,iostat=is)res ; if(is/=0) res = 0.d0 ; RETURN
! end function arg2dble
!
! elemental function arg2real(c) result(res)
! character(*),intent(in)   :: c
! real                      :: res
! integer                   :: is
!   read(c,*,iostat=is)res ; if(is/=0) res = 0.0 ; RETURN
! end function arg2real
!
! elemental function arg2int(c) result(res)
! character(*),intent(in)   :: c
! integer                   :: is,res
!   read(c,*,iostat=is)res ; if(is/=0) res = 0 ; RETURN
! end function arg2int
!
! elemental function OptOptargIdx(this,String,idx) result(res)
! class(Optparse),intent(in)         :: this
! character(*),intent(in)            :: String
! integer,intent(in)                 :: idx
! character(maxlen)                  :: res
! integer                            :: at
!   do at=1,this%Nopt
!     if(any(string==this%opt(at)%s))then
!       if(idx>this%opt(at)%Narg.or.idx<0)EXIT
!       if(this%opt(at)%arg%size()==0)then
!         res = this%opt(at)%def%at(idx) ; RETURN
!       else
!         res = this%opt(at)%arg%at(idx) ; RETURN
!       endif
!     endif
!   enddo
!   res='' ; RETURN
! end function OptOptargIdx
!
! pure function OptOptargList(this,String) result(res)
! class(Optparse),intent(in)         :: this
! character(*),intent(in)            :: String
! character(maxlen),allocatable      :: res(:)
! integer                            :: at
!   do at=1,this%Nopt
!     if(any(string==this%opt(at)%s))then
!       if(this%opt(at)%Narg==0)EXIT
!       allocate(res(this%opt(at)%Narg))
!       if(this%opt(at)%arg%size()==0)then
!         res = this%opt(at)%def%lookup() ; RETURN
!       else
!         res = this%opt(at)%arg%lookup() ; RETURN
!       endif
!     endif
!   enddo
!   allocate(res(1)) ; res='' ; RETURN
! end function OptOptargList
!
! pure integer function OptNArg(this) result(res)
! class(Optparse),intent(in)         :: this
!   res = this%FixedArg%size() ; RETURN
! end function OptNarg
!
! pure function OptArgList(this) result(res)
! class(Optparse),intent(in)         :: this
! character(maxlen)                  :: res(this%FixedArg%size())
!   res = this%FixedArg%lookup() ; RETURN
! end function OptArgList
!
! elemental function OptArgIdx(this,idx) result(res)
! class(Optparse),intent(in)         :: this
! integer,intent(in)                 :: idx
! character(maxlen)                  :: res
!   res = this%FixedArg%at(idx) ; RETURN
! end function OptArgIdx
!
  elemental subroutine OptNodeDestractor(this)
  type(optnode),intent(inout) :: this
    call this%s%clear() ; call this%arg%clear()
    this%isExist = .FALSE. ; this%Narg = 0
  end subroutine OptNodeDestractor
 
  pure subroutine OptDestractor(this)
  type(optparse2),intent(inout) :: this
    call this%CommandArg%clear()
    call this%helpargs%clear() ; call this%help%clear() ; call this%metavar%clear()
    call this%Error%clear() ; call this%warn%clear()
    if(allocated(this%opt))then
      call OptNodeDestractor(this%opt(:))
      deallocate(this%opt)
    endif
    if(allocated(this%desc))deallocate(this%desc)
    if(allocated(this%ScriptName))deallocate(this%ScriptName)
    if(allocated(this%VersionNumber))deallocate(this%VersionNumber)
    this%Nopt = 0   ; this%stack = 0
    this%hasArg = .FALSE.
    RETURN
  end subroutine OptDestractor
!
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!                  ErroroHandle ROUTINEs                   
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
! pure subroutine ErrAssign(this,routine)
! type(ErrorHandle),intent(inout)  :: this
! character(*),intent(in)          :: routine
!   this%tnum = 0  ; this%enum = 0
!   if(.not.allocated(this%routine))allocate(character(len_trim(routine)+1)::this%routine)
!   this%routine = trim(routine)//":"
!   call this%message%clear() ; RETURN
! end subroutine ErrAssign
!
! pure subroutine ErrDestractor(this)
! type(ErrorHandle),intent(inout)  :: this
!   this%tnum = -1  ; this%enum = 0 ; this%base = 0
!   if(allocated(this%routine)) deallocate(this%routine)
!   call this%message%clear() ; RETURN
! end subroutine ErrDestractor
!
! pure subroutine ErrInit(this,routine,base)
! class(ErrorHandle),intent(inout) :: this
! character(*),intent(in)          :: routine
! integer,intent(in),optional      :: base
!   this%tnum = 0  ; this%enum = 0
!   if(present(base))this%base = maxval([base,0],1)
!   if(.not.allocated(this%routine))allocate(character(len_trim(routine)+1)::this%routine)
!   this%routine = trim(routine)//":"
!   call this%message%clear() ; RETURN
! end subroutine ErrInit
!
! pure subroutine ErrWarn(this,auth,message)
! class(ErrorHandle),intent(inout) :: this
! logical,intent(in)               :: auth
! character(*),intent(in)          :: message
!   if(this%tnum<0) this = "" ; this%tnum = this%tnum + 1
!   if(.not.auth)RETURN
!   call this%message%push("WARNING) "//message)
!   RETURN
! end subroutine ErrWarn
!
! pure subroutine ErrCheck(this,auth,message)
! class(ErrorHandle),intent(inout) :: this
! logical,intent(in)               :: auth
! character(*),intent(in)          :: message
!   if(this%tnum<0) this = "" ; this%tnum = this%tnum + 1
!   if(.not.auth)RETURN
!   call this%message%push("ERROR) "//trim(this%routine)//" "//message)
!   this%enum = this%enum + 2**this%tnum
!   RETURN
! end subroutine ErrCheck
!
! logical function ErrOccurred(this) result(res)
! class(ErrorHandle)             :: this
!   res = this%enum/=0 ; RETURN
! end function ErrOccurred
!
! integer function ErrPunch(this) result(res)
! use,intrinsic :: ISO_FORTRAN_ENV, only : ERROR_UNIT
! class(ErrorHandle)             :: this
!   if(this%message%size()>0)write(ERROR_UNIT,'(A)') this%message%lookup()
!   res = this%enum + this%base
!   this%tnum = 0  ; this%enum = 0
!   call this%message%clear()
!   RETURN
! end function ErrPunch
!
! subroutine ErrPunchAbort(this)
! use,intrinsic :: ISO_FORTRAN_ENV, only : ERROR_UNIT
! class(ErrorHandle)            :: this
! logical                       :: abort
! integer                       :: exitstatus
!   if(this%message%size()>0)write(ERROR_UNIT,'(A)') this%message%lookup()
!   abort = this%enum/=0 ; exitstatus = this%enum+this%base
!   this%tnum = 0  ; this%enum = 0 ; this%base = 0
!   call this%message%clear()
!   if(abort) call exit(exitstatus) ; RETURN
! end subroutine ErrPunchAbort
end module spur_optparse2
