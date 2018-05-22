module spur_optparse
use spur_vector_chr
use spur_stdio
implicit none
  private
  public :: optparse
!
  integer(4),parameter   :: OFFSET_32 = 16777619
  integer(4),parameter   :: PRIME_32  = 16777619
!
  integer,parameter       :: arglen       = 256
  integer,parameter       :: width_def    = 80
  integer,parameter       :: n_def        = 0
  integer,parameter       :: mxl_def      = 0
  character(14),parameter :: help_def     = 'no help entry.'
  character(5),parameter  :: metavar_def  = '<var>'
  character(6),parameter  :: opt_help     = '--help'
  character(17),parameter :: desc_help    = 'display this help and exit'
  character(9),parameter  :: opt_vern     = '--version'
  character(35),parameter :: desc_vern    = 'output version information and exit'
  logical,parameter       :: use_help_def = .FALSE.
  logical,parameter       :: use_vern_def = .FALSE.
!
  type optnode
    integer                    :: narg
    type(vector_chr)           :: arg,def
    character(:),allocatable   :: opt
    logical                    :: isexist = .false.
  contains
    final     :: OptNodeDestractor
  end type OptNode
!
  type optparse
    private
    type(optnode),allocatable  :: node(:)
    character(:),allocatable   :: desc,srpt,vern
    type(vector_chr)           :: arg,optlist,help
    integer                    :: stack = n_def
    integer                    :: mxl   = mxl_def
    logical                    :: unset = .TRUE.
    type(stdio)                :: dat
    logical,public             :: use_help=use_help_def
    logical,public             :: use_vern=use_vern_def
  contains
    procedure         :: add_description => OptAddDesc
    procedure         :: add_version     => OptAddVern
    procedure         :: add_option      => optadd
    procedure         :: call_usage      => OptPrintUsage
    procedure         :: call_version    => OptPrintVersion
    procedure         :: parser          => optparser
    procedure         :: option          => OptOption
    procedure         :: narg            => OptNarg
    procedure         :: noptarg         => OptNOptarg
    procedure,private :: OptOptargListD
    procedure,private :: OptOptargIdxD
    generic           :: optargd         => OptOptargListD,OptOptargIdxD
    procedure,private :: OptOptargListF
    procedure,private :: OptOptargIdxF
    generic           :: optargf         => OptOptargListF,OptOptargIdxF
    procedure,private :: OptOptargListI
    procedure,private :: OptOptargIdxI
    generic           :: optargi         => OptOptargListI,OptOptargIdxI
    procedure,private :: OptOptargListS
    procedure,private :: OptOptargIdxS
    generic           :: optargs         => OptOptargListS,OptOptargIdxS
    procedure,private :: OptArgList
    procedure,private :: OptArgIdx
    generic           :: args            => OptArgList,OptArgIdx
    final             :: optdestractor
  end type Optparse
!
contains
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!                    Getopt ROUTINEs                       
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  subroutine Optparser(this)
  class(Optparse),intent(inout) :: this
  integer                       :: i,j,pnt,sft,is
  type(vector_chr)              :: carg
  character(arglen)             :: arg
    call this%arg%clear()
!
    call GET_COMMAND_ARGUMENT(0,arg,status=is)
    if(is/=0) RETURN
    if(.not.allocated(this%srpt)) allocate(character(0)::this%srpt)
    this%srpt = trim(arg(index(arg,"/",back=.TRUE.)+1:))
    do i=1,COMMAND_ARGUMENT_COUNT()
      call GET_COMMAND_ARGUMENT(i,arg,status=is) ; if(is/=0) RETURN
      call carg%split(trim(arg),"=")
    enddo
!
    if(this%use_help) call this%add_option(opt_help,help=desc_help)
    if(this%use_vern) call this%add_option(opt_vern,help=desc_vern)
!
    sft = 0
    do j=1,carg%size()
      pnt = HMhash32(this,trim(carg%at(j)))
      if(.not.allocated(this%node(pnt)%opt)) CYCLE
      this%node(pnt)%isexist = .TRUE.
      sft = j
      do i=j+1,minval([j+this%node(pnt)%narg,carg%size()],1)
        call this%node(pnt)%arg%push(carg%at(i))
        sft = i
      enddo
    enddo
    do i=sft+1,carg%size()
      call this%arg%push(carg%at(i))
      this%mxl = maxval([this%mxl,len_trim(carg%at(i))],1)
    enddo
    if(this%use_help.and.this%option(opt_help)) call this%call_usage()
    if(this%use_vern.and.this%option(opt_vern)) call this%call_version()
    this%unset = .FALSE.
  end subroutine Optparser
!
  pure subroutine OptAddDesc(this,str)
  class(optparse),intent(inout)    :: this
  character(*),intent(in)          :: str
    if(.not.allocated(this%desc)) allocate(character(0)::this%desc)
    this%desc = str
  end subroutine OptAddDesc
!
  pure subroutine OptAddVern(this,str)
  class(optparse),intent(inout)    :: this
  character(*),intent(in)          :: str
    if(.not.allocated(this%vern)) allocate(character(0)::this%vern)
    this%vern = str
  end subroutine OptAddVern
!
  subroutine optadd(this,str,narg,def,help,metavar)
  class(optparse),intent(inout)    :: this
  character(*),intent(in)          :: str
  character(*),intent(in),optional :: def(:),help,metavar(:)
  integer,intent(in),optional      :: narg
  type(vector_chr)                 :: tmp
  integer                          :: i,pnt
    if(.not.allocated(this%node))then
      this%stack = 0 ; call optextention(this)
    endif
    pnt = HMhash32(this,trim(str))
!
    if(.not.allocated(this%node(pnt)%opt))then
      call tmp%push('[')
!
      call tmp%push(trim(str))
!
      if(present(metavar))then
        do i=lbound(metavar,1),ubound(metavar,1)
          call tmp%push('<'//trim(metavar(i))//'>')
        enddo
      endif
!
      if(present(narg))then
        do i=tmp%size()+1,narg
          call tmp%push(metavar_def)
        enddo
      endif
      call tmp%push(']')
!
      call this%optlist%push(tmp%join(delimiter=' '))
      if(present(help))then
        call this%help%push(help)
      else
        call this%help%push(help_def)
      endif
!
      call optextention(this)
      pnt = HMhash32(this,trim(str))
      allocate(character(0) :: this%node(pnt)%opt)
      this%node(pnt)%opt  = trim(str)
    endif
!
    if(present(narg))    this%node(pnt)%narg = narg
!
    if(present(def))then
      call this%node(pnt)%def%clear()
      call this%node(pnt)%def%push(def)
      this%mxl = maxval([this%mxl,this%node(pnt)%def%maxlen()],1)
    endif
  contains
    subroutine OptExtention(this)
    class(optparse),intent(inout) :: this
    integer                       :: oldstack,i,pnt
    type(optnode),allocatable     :: tmpnode(:)
      if(this%stack>this%optlist%size()) return
      this%stack = maxval([this%stack,1],1)
      oldstack = this%stack
      do while(this%stack<=this%optlist%size())
        this%stack = this%stack * 2
      enddo
      call move_alloc(from=this%node,to=tmpnode)
      allocate(this%node(this%stack))
      do i=1,this%stack
        call nodeinit(this%node(i))
      enddo
!
      if(allocated(tmpnode))then   ! ---- rehash
        do i=1,size(tmpnode)
          if(.not.allocated(tmpnode(i)%opt)) CYCLE
          pnt = HMhash32(this,tmpnode(i)%opt)
          call optassign(tmpnode(i),this%node(pnt))
        enddo
      endif
    end subroutine OptExtention
!
    subroutine optassign(from,to)
    type(optnode),intent(in)    :: from
    type(optnode),intent(inout) :: to
      to%narg = from%narg
      call to%arg%push(from%arg%lookup())
      call to%def%push(from%def%lookup())
      if(.not.allocated(to%opt)) allocate(character(0)::to%opt)
      if(allocated(from%opt))  to%opt = from%opt
      to%isexist = from%isexist
    end subroutine optassign
!
    elemental subroutine nodeinit(this)
    type(optnode),intent(inout) :: this
      call OptNodeDestractor(this)
    end subroutine nodeinit
  end subroutine OptAdd
!
  subroutine OptPrintUsage(this,stat)
  class(optparse)             :: this
  integer,intent(in),optional :: stat
  type(vector_chr)            :: list
  character(:),allocatable    :: indent
  integer                     :: i,j,k
    if(this%unset) call this%parser()
    if(allocated(this%desc)) call this%dat%puts(this%srpt//" :: "//this%desc)
    allocate(character(0)::indent)
    indent = 'Usage: '//this%srpt//' '
!
    call list%push(this%optlist%lookup())
    call list%textwrap(maxval([width_def-len(indent),50],1))
    call this%dat%puts(indent//list%at(1))
    indent(:) = ''
    do i=2,list%size()
      call this%dat%puts(indent//list%at(i))
    enddo
    call this%dat%break()
!
    deallocate(indent) ; allocate(character(7+this%optlist%maxlen())::indent)
    do i=1,this%optlist%size()
      indent(:) = '       '//this%optlist%at(i)
      call this%dat%puts(indent//" : "//trim(this%help%at(i)))
    enddo
    if(present(stat)) call exit(stat)
  end subroutine OptPrintUsage
!
  subroutine OptPrintVersion(this,stat)
  class(optparse),intent(inout) :: this
  integer,intent(in),optional   :: stat
    call this%dat%puts(this%srpt//' : '//this%vern)
    if(present(stat)) call exit(stat)
  end subroutine OptPrintVersion
!
  elemental function optoption(this,str) result(res)
  class(optparse),intent(in)    :: this
  character(*),intent(in)       :: str
  logical                       :: res
  integer                       :: pnt
    res = .FALSE. ; if(this%unset) RETURN
    pnt = HMhash32(this,trim(str))
    res = this%node(pnt)%isexist
  end function OptOption
!
  pure elemental function OptNOptarg(this,key) result(res)
  class(optparse),intent(in)         :: this
  character(*),intent(in)            :: key
  integer                            :: res
  integer                            :: pnt
    res = 0
    if(this%unset) RETURN
    pnt = HMhash32(this,trim(key))
    if(this%node(pnt)%arg%size()>0) res = this%node(pnt)%narg
  end function OptNOptarg
!
  elemental function OptOptargIdxD(this,key,idx) result(res)
  class(optparse),intent(in) :: this
  character(*),intent(in)    :: key
  integer,intent(in)         :: idx
  double precision           :: res
  integer                    :: pnt
    if(this%unset)then
      res = 0.d0 ; RETURN
    endif
    pnt = HMhash32(this,trim(key))
    if(this%node(pnt)%arg%size()>0)then
      res = this%node(pnt)%arg%tonum(idx,0.d0)
    else
      res = this%node(pnt)%def%tonum(idx,0.d0)
    endif
  end function OptOptargIdxD
!
  pure function OptOptargListD(this,key) result(res)
  class(optparse),intent(in)    :: this
  character(*),intent(in)       :: key
  double precision,allocatable  :: res(:)
  integer                       :: i,pnt
    if(this%unset)then
      allocate(res(0)) ; res = 0.d0 ; RETURN
    endif
    pnt = HMhash32(this,trim(key))
    allocate(res(this%node(pnt)%narg))
    if(this%node(pnt)%arg%size()>0)then
      res = [(this%node(pnt)%arg%tonum(i,0.d0),i=1,this%node(pnt)%narg)]
    else
      res = [(this%node(pnt)%def%tonum(i,0.d0),i=1,this%node(pnt)%narg)]
    endif
  end function OptOptargListD
!
  elemental function OptOptargIdxF(this,key,idx) result(res)
  class(optparse),intent(in) :: this
  character(*),intent(in)    :: key
  integer,intent(in)         :: idx
  real                       :: res
  integer                    :: pnt
    if(this%unset)then
      res = 0.0 ; RETURN
    endif
    pnt = HMhash32(this,trim(key))
    if(this%node(pnt)%arg%size()>0)then
      res = this%node(pnt)%arg%tonum(idx,0.0)
    else
      res = this%node(pnt)%def%tonum(idx,0.0)
    endif
  end function OptOptargIdxF
!
  pure function OptOptargListF(this,key) result(res)
  class(optparse),intent(in)    :: this
  character(*),intent(in)       :: key
  real,allocatable              :: res(:)
  integer                       :: i,pnt
    if(this%unset)then
      allocate(res(0)) ; res = 0.0 ; RETURN
    endif
    pnt = HMhash32(this,trim(key))
    allocate(res(this%node(pnt)%narg))
    if(this%node(pnt)%arg%size()>0)then
      res = [(this%node(pnt)%arg%tonum(i,0.0),i=1,this%node(pnt)%narg)]
    else
      res = [(this%node(pnt)%def%tonum(i,0.0),i=1,this%node(pnt)%narg)]
    endif
  end function OptOptargListF
!
  elemental function OptOptargIdxI(this,key,idx) result(res)
  class(optparse),intent(in) :: this
  character(*),intent(in)    :: key
  integer,intent(in)         :: idx
  integer                    :: res
  integer                    :: pnt
    if(this%unset)then
      res = 0 ; RETURN
    endif
    pnt = HMhash32(this,trim(key))
    if(this%node(pnt)%arg%size()>0)then
      res = this%node(pnt)%arg%tonum(idx,0)
    else
      res = this%node(pnt)%def%tonum(idx,0)
    endif
  end function OptOptargIdxI
!
  pure function OptOptargListI(this,key) result(res)
  class(optparse),intent(in)    :: this
  character(*),intent(in)       :: key
  integer,allocatable           :: res(:)
  integer                       :: i,pnt
    if(this%unset)then
      allocate(res(0)) ; res = 0 ; RETURN
    endif
    pnt = HMhash32(this,trim(key))
    allocate(res(this%node(pnt)%narg))
    if(this%node(pnt)%arg%size()>0)then
      res = [(this%node(pnt)%arg%tonum(i,0),i=1,this%node(pnt)%narg)]
    else
      res = [(this%node(pnt)%def%tonum(i,0),i=1,this%node(pnt)%narg)]
    endif
  end function OptOptargListI
!
  pure function OptOptargIdxS(this,key,idx) result(res)
  class(optparse),intent(in) :: this
  character(*),intent(in)    :: key
  integer,intent(in)         :: idx
  character(:),allocatable   :: res
  integer                    :: pnt
    allocate(character(0)::res)
    if(this%unset) RETURN
    pnt = HMhash32(this,trim(key))
    if(this%node(pnt)%arg%size()>0)then
      res = this%node(pnt)%arg%at(idx)
    else
      res = this%node(pnt)%def%at(idx)
    endif
  end function OptOptargIdxS
!
  pure function OptOptargListS(this,key) result(res)
  class(optparse),intent(in)      :: this
  character(*),intent(in)         :: key
  character(this%mxl),allocatable :: res(:)
  integer                         :: i,pnt
    if(this%unset)then
      allocate(res(0)) ; res = '' ; RETURN
    endif
    pnt = HMhash32(this,trim(key))
    allocate(res(this%node(pnt)%narg))
    if(this%node(pnt)%arg%size()>0)then
      res = this%node(pnt)%arg%lookup()
    else
      res = this%node(pnt)%def%lookup()
    endif
  end function OptOptargListS
!
  pure elemental integer function OptNArg(this) result(res)
  class(optparse),intent(in) :: this
    res = this%arg%size()
  end function OptNarg
!
  pure function OptArgList(this) result(res)
  class(optparse),intent(in)         :: this
  character(this%mxl)                :: res(this%arg%size())
    res = this%arg%lookup()
  end function OptArgList
!
  pure elemental function OptArgIdx(this,idx) result(res)
  class(optparse),intent(in)         :: this
  integer,intent(in)                 :: idx
  character(this%mxl)                :: res
    res = this%arg%at(idx)
  end function OptArgIdx
!
  pure integer(4) function HMhash32(this,key) result(res)
  class(optparse),intent(in)  :: this
  character(*),intent(in)     :: key
  integer                     :: i
    res = OFFSET_32
    do i=1,len(key)
      res = ieor(res,ichar(key(i:i)))
      res = PRIME_32*res
    enddo
    do i=1,this%stack
      res = modulo(abs(res),this%stack)+1
      if(this%node(res)%opt==key) RETURN
      if(.not.allocated(this%node(res)%opt))   RETURN
    enddo
  end function HMhash32
!
  elemental subroutine OptNodeDestractor(this)
  type(optnode),intent(inout) :: this
    this%narg = 0
    call this%arg%clear() ; call this%def%clear()
    if(allocated(this%opt))  deallocate(this%opt)
    this%isexist = .FALSE.
  end subroutine OptNodeDestractor
!
  pure subroutine OptDestractor(this)
  type(optparse),intent(inout) :: this
    call this%arg%clear()
    call this%optlist%clear()
    call this%help%clear()
    if(allocated(this%node)) deallocate(this%node)
    if(allocated(this%desc)) deallocate(this%desc)
    if(allocated(this%srpt)) deallocate(this%srpt)
    if(allocated(this%vern)) deallocate(this%vern)
    this%stack = n_def
    this%mxl   = mxl_def
    this%unset = .TRUE.
  end subroutine OptDestractor
end module spur_optparse
