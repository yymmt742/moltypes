module moltypes_readmask
  use moltypes_errorhandler
  use spur_vector_int4
  use spur_vector_chr
  use spur_vector_real
  use spur_string
  use spur_string_neaten, only : small
  implicit none
  private
  public :: readmask
!
  integer,parameter       :: NATM_NULL            = -1
!
  character(5),parameter :: KWD_INDEX   = 'index'
  character(5),parameter :: KWD_ALL(1)  = ['all  ']
  character(5),parameter :: KWD_NONE(1) = ['none ']
  character(5),parameter :: KWD_NOT(2)  = ['not  ','!    ']
  character(5),parameter :: KWD_AND(3)  = ['and  ','&    ','*    ']
  character(5),parameter :: KWD_OR(3)   = ['or   ','|    ','+    ']
  character(5),parameter :: KWD_NAND(1) = ['nand ']
  character(5),parameter :: KWD_NOR(1)  = ['nor  ']
  character(2),parameter :: KWD_TO(2)   = ["to","~ "]
  character(5),parameter :: KWD_OPERATOR(12) = [KWD_ALL,KWD_NONE,KWD_NOT,KWD_AND,KWD_OR,KWD_NAND,KWD_NOR]
!
  type readmask
  private
    integer                             :: natm = NATM_NULL,stat = RMSK_EMPTY
    integer                             :: cnid=0,inid=0,rnid=0
    type(vector_chr)                    :: ckey,ikey,rkey
    type(vector_chr),allocatable        :: cvar(:),cunq(:)
    type(vector_int4),allocatable       :: ivar(:),iunq(:)
    type(vector_real),allocatable       :: rvar(:)
    logical,public                      :: terminates_at_abnormal = terminates_default
  contains
    procedure         :: init            => RmskInit
    procedure,private :: RmskDefKwd_chr
    procedure,private :: RmskDefKwd_int
    procedure,private :: RmskDefKwd_real
    generic           :: def_keyword     => RmskDefKwd_chr,RmskDefKwd_int,RmskDefKwd_real
    procedure         :: showchr         => RmskShowChr
    procedure         :: showuniqchr     => RmskShowUniqChr
    procedure         :: nuniqchr        => RmskNUniqChr
    procedure         :: showint         => RmskShowInt
    procedure         :: nuniqint        => RmskNUniqInt
    procedure         :: showuniqint     => RmskShowUniqInt
    procedure         :: showreal        => RmskShowReal
    procedure,private :: RmskParse
    procedure,private :: RmskParse1d
    generic           :: parse           => RmskParse, RmskParse1d
    procedure         :: inq_key         => RmskInqkey
    procedure         :: isempty         => RmskIsEmpty
    procedure         :: iserr           => RmskIsErr
    procedure         :: clear           => RmskClear
    final             :: RmskDestractor
  end type readmask
contains
  subroutine RmskInit(this,natm)
  class(ReadMask),intent(inout) :: this
  integer,intent(in)            :: natm
  integer                       :: i
    call RoutineNameIs('READMASK_INIT')
    if(CheckRmsk(this,natm<=0,RMSK_INVALID_NATM))RETURN
    call RmskDestractor(this)
    this%natm = natm
    call this%ikey%push(KWD_INDEX) ; this%inid=1
    allocate(this%ivar(this%inid),this%iunq(this%inid))
    this%ivar(this%inid) = [(i,i=1,natm)] ;  this%iunq(this%inid) = this%ivar(1)
    call this%iunq(this%inid)%uniq() ; call this%iunq(this%inid)%shrinktofit()
    this%stat = MOLTYPES_NOERR
  end subroutine RmskInit
!
  subroutine RmskDefKwd_chr(this,word,var)
  class(ReadMask),intent(inout)      :: this
  character(*),intent(in)            :: word
  character(*),intent(in)            :: var(:)
  integer                            :: kid,old
  type(vector_chr),allocatable       :: swp(:)
    call RoutineNameIs('READMASK_DEFKWD')
    if(CheckRmsk(this,size(var)/=this%natm,RMSK_INVALID_NVAR))RETURN
    if(this%ckey%find(small(word))==0) call this%ckey%push(small(word)) ; kid = this%ckey%find(small(word))
    if(kid>this%cnid)then
      old = this%cnid ; this%cnid = maxval([old,1],1)
      do while(kid>this%cnid)
        this%cnid = this%cnid * 2
      enddo
      allocate(swp(this%cnid)) ; if(old>0) swp(1:old) = this%cvar(1:old)
      call move_alloc(from=swp,to=this%cvar)
      allocate(swp(this%cnid)) ; if(old>0) swp(1:old) = this%cunq(1:old)
      call move_alloc(from=swp,to=this%cunq)
    endif
    this%cvar(kid) = var ;  this%cunq(kid) = var
    call this%cunq(kid)%uniq() ; call this%cunq(kid)%shrinktofit()
  end subroutine RmskDefKwd_chr
!
  subroutine RmskDefKwd_int(this,word,var)
  class(ReadMask),intent(inout)      :: this
  character(*),intent(in)            :: word
  integer,intent(in)                 :: var(:)
  integer                            :: kid,old
  type(vector_int4),allocatable      :: swp(:)
    call RoutineNameIs('READMASK_DEFKWD')
    if(CheckRmsk(this,size(var)/=this%natm,RMSK_INVALID_NVAR))RETURN
    if(this%ikey%find(small(word))==0) call this%ikey%push(small(word)) ; kid = this%ikey%find(small(word))
    if(kid>this%inid)then
      old = this%inid ; this%inid = maxval([old,1],1)
      do while(kid>this%inid)
        this%inid = this%inid * 2
      enddo
      allocate(swp(this%inid)) ; if(old>0) swp(1:old) = this%ivar(1:old)
      call move_alloc(from=swp,to=this%ivar)
      allocate(swp(this%inid)) ; if(old>0) swp(1:old) = this%iunq(1:old)
      call move_alloc(from=swp,to=this%iunq)
    endif
    this%ivar(kid) = var ;  this%iunq(kid) = var
    call this%iunq(kid)%uniq() ; call this%iunq(kid)%shrinktofit()
  end subroutine RmskDefKwd_int
!
  subroutine RmskDefKwd_real(this,word,var)
  class(ReadMask),intent(inout)      :: this
  character(*),intent(in)            :: word
  real,intent(in)                    :: var(:)
  integer                            :: kid,old
  type(vector_real),allocatable      :: swp(:)
    call RoutineNameIs('READMASK_DEFKWD')
    if(CheckRmsk(this,size(var)/=this%natm,RMSK_INVALID_NVAR))RETURN
    if(this%rkey%find(small(word))==0) call this%rkey%push(small(word)) ; kid = this%rkey%find(small(word))
    if(kid>this%rnid)then
      old = this%rnid ; this%rnid = maxval([old,1],1)
      do while(kid>this%rnid)
        this%rnid = this%rnid * 2
      enddo
      allocate(swp(this%rnid)) ; if(old>0) swp(1:old) = this%rvar(1:old)
      call move_alloc(from=swp,to=this%rvar)
    endif
    this%rvar(kid) = var
  end subroutine RmskDefKwd_real
!
  pure function RmskShowInt(this,word) result(res)
  class(ReadMask),intent(in)    :: this
  character(*),intent(in)       :: word
  integer,allocatable           :: res(:)
  integer                       :: kid
    allocate(res(maxval([this%natm,0],1)))
    kid = this%ikey%find(small(word))
    if(kid==0)then ; res = 0
    else           ; res = this%ivar(kid)%lookup()
    endif
  end function RmskShowInt
!
  pure integer function RmskNUniqInt(this,word) result(res)
  class(ReadMask),intent(in)    :: this
  character(*),intent(in)       :: word
  integer                       :: kid
    kid = this%ikey%find(small(word))
    if(kid==0)then ; res = 0
    else ; res = this%iunq(kid)%size()
    endif
  end function RmskNUniqInt
!
  pure function RmskShowUniqInt(this,word) result(res)
  class(ReadMask),intent(in)    :: this
  character(*),intent(in)       :: word
  integer,allocatable           :: res(:)
  integer                       :: kid
    kid = this%ikey%find(small(word))
    allocate(res(this%iunq(kid)%size()))
    if(kid==0)then ; res = 0
    else ; res = this%iunq(kid)%lookup()
    endif
  end function RmskShowUniqInt
!
  pure function RmskShowReal(this,word) result(res)
  class(ReadMask),intent(in) :: this
  character(*),intent(in)    :: word
  real,allocatable           :: res(:)
  integer                    :: kid
    allocate(res(maxval([this%natm,0],1)))
    kid = this%rkey%find(small(word))
    if(kid==0) RETURN
    res = this%rvar(kid)%lookup()
  end function RmskShowReal
!
  pure function RmskShowChr(this,word) result(res)
  class(ReadMask),intent(in)                   :: this
  character(*),intent(in)                      :: word
  character(RmskChrlen(this,word)),allocatable :: res(:)
  integer                                      :: kid
    allocate(res(maxval([this%natm,0],1)))
    kid = this%ckey%find(small(word))
    if(kid==0) RETURN
    res = this%cvar(kid)%lookup()
  end function RmskShowChr
!
  pure integer function RmskNUniqChr(this,word) result(res)
  class(ReadMask),intent(in)    :: this
  character(*),intent(in)       :: word
  integer                       :: kid
    kid = this%ckey%find(small(word))
    if(kid==0)then ; res = 0
    else ; res = this%cunq(kid)%size()
    endif
  end function RmskNUniqChr
!
  pure function RmskShowUniqChr(this,word) result(res)
  class(ReadMask),intent(in)                   :: this
  character(*),intent(in)                      :: word
  character(RmskChrlen(this,word)),allocatable :: res(:)
  integer                                      :: kid
    kid = this%ckey%find(small(word))
    if(kid>0)then
      allocate(res(this%cunq(kid)%size()))
      res = this%cunq(kid)%lookup()
    else
      allocate(res(0))
    endif
  end function RmskShowUniqChr
!
  pure integer function RmskChrlen(this,word) result(res)
  class(ReadMask),intent(in) :: this
  character(*),intent(in)    :: word
  integer                    :: kid
    kid = this%ckey%find(small(word))
    if(kid==0)then ; res = 0
    else           ; res = this%cvar(kid)%maxlen()
    endif
  end function RmskChrlen
!
  pure function RmskParse1d(this,str) result(res)
  class(ReadMask),intent(in)     :: this
  character(*),intent(in)        :: str(:)
  logical                        :: res(maxval([this%natm,0],1))
  integer                        :: i
    res = .FALSE.
    do i=lbound(str,1),ubound(str,1)
      res = IOR(res,RmskParse(this,Str(i)))
    enddo
  end function RmskParse1d
!
  pure function RmskParse(this,Str) result(res)
  class(ReadMask),intent(in)     :: this
  character(*),intent(in)        :: str
  logical                        :: res(maxval([this%natm,0],1))
  logical                        :: tmp(maxval([this%natm,0],1))
  type(vector_chr)               :: words,kwd
  type(vector_int4)              :: head,tail
  logical                        :: A,O,N,D,stat
  integer                        :: i,j,kid
    res = .TRUE. ; if(this%isEmpty().or.this%isErr()) RETURN
    call words%split(str,delimiter=" ,",pickup='!&|*~')
    if(words%size()==0) RETURN
!
    call head%push(1) ; call kwd%push(words%small(1))
    do i=2,words%size()
      if(ANY_KWD(this,words%small(i)))then
        call head%push(i) ; call tail%push(i-1) ; call kwd%push(words%small(i))
      endif
    enddo
    call tail%push(words%size())
!
    stat = .FALSE.
    A = .FALSE. ; O = .FALSE. ; N = .FALSE. ; D = .FALSE.
!
    do i=1,head%size()
!Keywords that does not requir argument
      if(any(kwd%at(i)==KWD_OPERATOR))then
        stat = head%at(i)/=tail%at(i) ; if(stat) EXIT
        if(    any(kwd%at(i)==KWD_ALL ))then
          Tmp = .TRUE.
        elseif(any(kwd%at(i)==KWD_NONE))then
          Tmp = .FALSE.
        elseif(any(kwd%at(i)==KWD_NOT ))then
          N = not(N) ; CYCLE
        elseif(any(kwd%at(i)==[KWD_AND,KWD_OR,KWD_NAND,KWD_NOR]))then
          stat = i==1.or.i==kwd%size().or.A.or.O ; if(stat) EXIT
          A = ANY(kwd%at(i) == [KWD_AND,KWD_NAND])
          O = ANY(kwd%at(i) == [KWD_OR, KWD_NOR ])
          if(ANY(kwd%at(i) ==  [KWD_NAND,KWD_NOR])) N = not(N)
          CYCLE
        endif
!Keywords that require arguments
      else
        stat = head%at(i)==tail%at(i) ; if(stat) EXIT
        if(any(kwd%at(i)==this%ckey%lookup()))then
          kid = this%ckey%find(kwd%at(i))
          call mask_chr(this,kid,words%at(head%at(i)+1,tail%at(i)),tmp)
        elseif(any(kwd%at(i)==this%ikey%lookup()))then
          kid = this%ikey%find(kwd%at(i))
          if(kid>0) call mask_int(this,kid,words%at(head%at(i)+1,tail%at(i)),tmp)
        elseif(any(kwd%at(i)==this%rkey%lookup()))then
          kid = this%rkey%find(kwd%at(i))
          if(kid>0) call mask_real(this,kid,words%at(head%at(i)+1,tail%at(i)),tmp)
        else
          stat = .TRUE. ; EXIT
        endif
      endif
!
      if(N)then ; tmp = .not.tmp ; N = .FALSE. ; endif
!
      if(A)then     ; res = IAND(res,Tmp) ; A = .FALSE. ; CYCLE
      elseif(O)then ; res = IOR(res,Tmp)  ; O = .FALSE. ; CYCLE 
      endif
      stat = D ; if(stat) EXIT
      res = tmp ; D = .TRUE.
    enddo
    if(stat) res = .FALSE.
  end function RmskParse
!
  pure logical function ANY_KWD(this,word) result(res)
  class(ReadMask),intent(in)         :: this
  character(*),intent(in)            :: word
    res = any(word==KWD_OPERATOR)
    if(this%cnid>0) res = any(word==this%ckey%lookup()).or.res
    if(this%inid>0) res = any(word==this%ikey%lookup()).or.res
    if(this%rnid>0) res = any(word==this%rkey%lookup()).or.res
  end function ANY_KWD
!
  pure subroutine mask_chr(this,kid,words,tmp)
  class(ReadMask),intent(in)        :: this
  integer,intent(in)                :: kid
  character(*),intent(in)           :: words(:)
  logical,intent(out)               :: tmp(:)
  type(vector_int4)                 :: list
  integer                           :: i,j,N,Nold
  logical                           :: to
    tmp = .FALSE. ; to = .FALSE.
    do i=1,size(words)
      if(any(small(words(i))==KWD_TO))then
        if(to) CYCLE ; Nold = N
        to = .TRUE. ; if(i<size(words))CYCLE ; N = this%cunq(kid)%size()
      else
        N = this%cunq(kid)%find(words(i)) ; if(N==0) RETURN
      endif
      if(to.and.Nold>N) RETURN ; if(.not.to)Nold = N
      to=.FALSE. ; call list%push([(j,j=Nold,N)])
    enddo
!
    do i=1,list%size()
      tmp = IOR(tmp,this%cunq(kid)%at(list%at(i))==this%cvar(kid)%lookup())
    enddo
   end subroutine mask_chr
!
  pure subroutine mask_int(this,kid,words,tmp)
  class(ReadMask),intent(in)        :: this
  integer,intent(in)                :: kid
  character(*),intent(in)           :: words(:)
  logical,intent(out)               :: tmp(:)
  type(vector_int4)                 :: list
  integer                           :: i,j,N,Nold
  logical                           :: to
    tmp = .FALSE. ; to = .FALSE.
    to = .FALSE. ; Nold = 1 ; N = 1
    do i=1,size(words)
      if(any(small(words(i))==KWD_TO))then
        if(to) CYCLE ; Nold = N
        to = .TRUE. ; if(i<size(words))CYCLE ; N = this%iunq(kid)%size()
      else
        N = tonum(words(i),0)
        if(N>this%natm)then   ; N = this%natm
        elseif(N==0)then      ; N = 1
        elseif(N<0)then       ; N = modulo(N,maxval([this%natm,1],1)) + 1
        endif
      endif
      if(to.and.Nold>N) RETURN ; if(.not.to) Nold = N
      to=.FALSE. ; call list%push([(j,j=Nold,N)])
    enddo
!
    do i=1,list%size()
      tmp = IOR(tmp,this%iunq(kid)%at(list%at(i))==this%ivar(kid)%lookup())
    enddo
   end subroutine mask_int
!
  pure subroutine mask_real(this,kid,words,tmp)
  class(ReadMask),intent(in)        :: this
  integer,intent(in)                :: kid
  character(*),intent(in)           :: words(:)
  logical,intent(out)               :: tmp(:)
  type(vector_int4)                 :: list
  integer                           :: i,j
  real                              :: Rmax,Rmin
  logical                           :: to,rn
    tmp = .FALSE. ; to = .FALSE. ; rn = .FALSE.
    to = .FALSE. ; Rmin = -HUGE(0.0) ; Rmax = HUGE(0.0)
    do i=1,size(words)
      if(any(small(words(i))==KWD_TO))then
        if(to) CYCLE
        to = .TRUE. ; if(i<size(words))CYCLE
        Rmax = HUGE(0.0) ; rn = .TRUE.
      else
        if(to)then
          Rmax = tonum(words(i),0.0) ; rn = .TRUE.
        else
          Rmin = tonum(words(i),0.0)
        endif
      endif
      if(.not.rn) CYCLE ; if(.not.to) Rmax = Rmin
      to=.FALSE. ; rn = .FALSE.
      tmp = IOR(tmp,Irange(this%rvar(kid)%lookup(),Rmax,Rmin))
    enddo
  contains
    pure function Irange(var,Rmax,Rmin) result(res)
    real,intent(in)           :: var(:),Rmax,Rmin
    logical                   :: res(size(var))
      res = IAND(Rmin<=var,var<=Rmax)
    end function Irange
  end subroutine mask_real
!
  pure logical function RmskInqkey(this,word,xtype) result(res)
  class(ReadMask),intent(in)    :: this
  character(*),intent(in)       :: word,xtype
    select case(small(xtype(1:1)))
    case('i')
      res = this%ikey%find(small(word))>0
    case('c')
      res = this%ckey%find(small(word))>0
    case('r')
      res = this%rkey%find(small(word))>0
    case default
      res = .false.
    end select
  end function RmskInqkey
!
  pure logical function RMSKIsEmpty(this) result (res)
  class(ReadMask),intent(in)  :: this
    res = this%stat == RMSK_EMPTY
  end function RMSKIsEmpty
!
  pure logical function RMSKIsERR(this) result (res)
  class(ReadMask),intent(in)  :: this
    res = not(this%stat == RMSK_EMPTY .or.this%stat == MOLTYPES_NOERR)
  end function RMSKIsERR
!
  logical function CheckRmsk(this,test,ierr) result(res)
  class(ReadMask),intent(inout)    :: this
  logical,intent(in)               :: test
  integer,intent(in)               :: ierr
    res = test ; if(.not.res) RETURN
    if(this%terminates_at_abnormal)then
      call moltypes_echo_errmsg(ierr)
      call RmskDestractor(this) ; call exit(ierr)
    else
      this%stat = ierr
    endif
  end function CheckRmsk
!
  pure subroutine RMSKClear(this)
  class(ReadMask),intent(inout)  :: this
    this%natm = NATM_NULL ; this%natm = MOLTYPES_NOERR
    this%cnid=0 ; this%inid=0 ; this%rnid=0
    call this%ckey%clear() ; call this%ikey%clear()
    call this%rkey%clear()
    if(allocated(this%cvar))deallocate(this%cvar)
    if(allocated(this%cunq))deallocate(this%cunq)
    if(allocated(this%ivar))deallocate(this%ivar)
    if(allocated(this%iunq))deallocate(this%iunq)
    if(allocated(this%rvar))deallocate(this%rvar)
  end subroutine RMSKClear
!
  pure subroutine RmskDestractor(this)
  type(ReadMask),intent(inout)  :: this
    call this%clear()
  end subroutine RmskDestractor
end module moltypes_readmask
