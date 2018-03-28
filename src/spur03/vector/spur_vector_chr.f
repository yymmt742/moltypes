module spur_vector_chr
  use spur_stackmanager
  implicit none
  private
  public :: vector_chr
  public :: assignment(=)
!
  integer(4),parameter       :: mxl_def = 0
  integer(4),parameter       :: tot_def = 0
  integer(4),parameter       :: NAN = HUGE(tot_def)
!
  type,extends(stackmanager) :: vector_chr
    private
    integer(4),allocatable        :: p(:,:)
    character(:),allocatable      :: c
    integer(4)                    :: mxl  = mxl_def, tot = tot_def, tail = tot_def
  contains
    procedure,private   :: CPush
    procedure,private   :: C1Push
    generic             :: push        => Cpush,C1push
    procedure           :: pop         => CPop
    procedure           :: len         => CLen
    procedure           :: maxlen      => CMaxlen
    procedure           :: memory      => Cmemory
    procedure           :: total       => Ctotal
    procedure,private   :: CAt_idx
    procedure,private   :: CAt_range
    generic             :: at          => CAt_idx, CAt_range
    procedure           :: lookup      => CLookUp
    procedure           :: small       => CSmall
    procedure           :: large       => CLarge
    procedure,private   :: CTonum_int4
    procedure,private   :: CTonum_real
    procedure,private   :: CTonum_dble
    generic             :: tonum       => CTonum_int4,CTonum_real,CTonum_dble
    procedure           :: sort        => Csort
    procedure           :: uniq        => Cuniq
    procedure           :: join        => CJoin
    procedure           :: textwrap    => CTextwrap
    procedure           :: split       => CSplit
    procedure           :: find        => CFind
    procedure           :: ShrinkToFit => CShrinkToFit
    procedure           :: clear       => CClear
    final               :: CDestractor
  end type vector_chr
!
  interface assignment(=)
    module procedure CAssign, C1Assign,&
                   & VecAssign
  end interface assignment(=)
contains
  pure subroutine CExpand(this)
  class(vector_chr),intent(inout) :: this
  integer(4),allocatable          :: ptmp(:,:)
  character(:),allocatable        :: ctmp
  integer(4)                      :: tmp
    tmp = this%memory()
    if(tmp<this%tail)then
      if(tmp==0) tmp = 1
      do while(this%tail>tmp)
        tmp = tmp * 2
      enddo
      allocate(character(tmp) :: ctmp)
      if(allocated(this%c)) ctmp(:len(this%c)) = this%c
      call move_alloc(from=ctmp,to=this%c)
    endif
!
    tmp = size(this%p,2)
    if(.not.allocated(this%p)) tmp = 0
    if(this%capacity()==0.or.tmp>=this%size()) RETURN
!
    allocate(ptmp(2,this%capacity()))
    if(allocated(this%p)) ptmp(:,:size(this%p,2)) = this%p
    call move_alloc(from=ptmp,to=this%p)
  end subroutine CExpand
!
  pure subroutine CShrinkToFit(this)
  class(vector_chr),intent(inout) :: this
  integer(4),allocatable          :: ptmp(:,:)
  character(:),allocatable        :: ctmp
  integer(4)                      :: i,tmp(2)
    if(this%size()==0) RETURN
    allocate(character(0) :: ctmp)
    if(allocated(this%c)) ctmp = this%join()
    call move_alloc(from=ctmp,to=this%c)
    call stack_shrinktofit(this)
    if(allocated(this%p))then
      allocate(ptmp(2,this%size()))
      tmp = 0
      do i=1,this%size()
        tmp(1) = tmp(2) + 1 ; tmp(2) = tmp(2) + this%len(i)
        ptmp(:,i) = tmp
      enddo
      call move_alloc(from=ptmp,to=this%p)
    endif
  end subroutine CShrinkToFit
!---------------------------------------------------------!
!       Routines for Assign varue.                        !
!---------------------------------------------------------!
  pure subroutine CAssign(LHS,RHS)
  use spur_string
  class(vector_chr),intent(inout) :: LHS
  character(*),intent(in)         :: RHS
    call CDestractor(LHS) ; call LHS%push(RHS)
  end subroutine CAssign
!
  pure subroutine C1Assign(LHS,RHS)
  use spur_string
  class(vector_chr),intent(inout) :: LHS
  character(*),intent(in)         :: RHS(:)
    call CDestractor(LHS) ; call LHS%push(RHS)
  end subroutine C1Assign
!
  pure subroutine VecAssign(LHS,RHS)
  class(vector_chr),intent(inout) :: LHS
  class(vector_chr),intent(in)    :: RHS
    call CDestractor(LHS)
    LHS%mxl  = RHS%mxl
    LHS%tot  = RHS%tot
    LHS%tail = RHS%tail
    call stack_reserve(LHS,RHS%size())
    if(allocated(RHS%p))then
      allocate(LHS%p(size(RHS%p,1),size(RHS%p,2)))
      LHS%p = RHS%p
    endif
    if(allocated(RHS%c))then
      allocate(character(len(RHS%c))::LHS%c)
      LHS%c = RHS%c
    endif
  end subroutine VecAssign
!
  pure subroutine CPush(this,var)
  class(vector_chr),intent(inout) :: this
  character(*),intent(in)         :: var
    this%tail = this%tail + len_trim(var)
    this%tot  = this%tot  + len_trim(var)
    this%mxl  = maxval([this%mxl,len_trim(var)],1)
    call stack_push(this,1) ; call CExpand(this)
    this%p(1,this%size()) = this%tail - len_trim(var) + 1
    this%p(2,this%size()) = this%tail
    if(len_trim(var)==0) RETURN
    this%c(this%p(1,this%size()):this%p(2,this%size())) = trim(var)
  end subroutine CPush
!
  pure subroutine C1Push(this,var)
  class(vector_chr),intent(inout) :: this
  character(*),intent(in)         :: var(:)
  integer(4)                      :: i
    do i=1,size(var)
      call cpush(this,var(i))
    enddo
  end subroutine C1Push
!
  function CPop(this) result (res)
  class(vector_chr),intent(inout) :: this
  character(this%mxl)             :: res
  integer                         :: i
    if(this%size()==0) RETURN
    res = this%at(this%size())
    this%tot = this%tot - this%len(this%size())
    if(this%len(this%size())==this%mxl) this%mxl = maxval([(this%len(i),i=1,this%size()-1)],1)
    call stack_pop(this,1)
  end function CPop
!
  pure elemental function CTotal(this) result(res)
  class(vector_chr),intent(in) :: this
  integer(4)                   :: res
    res = this%tot
  end function CTotal
!
  pure elemental function CMemory(this) result(res)
  class(vector_chr),intent(in) :: this
  integer(4)                   :: res
    if(allocated(this%c))then
      res = len(this%c)
    else
      res = 0
    endif
  end function CMemory
!
  pure elemental function CMaxlen(this) result(res)
  class(vector_chr),intent(in) :: this
  integer(4)                   :: res
    res = this%mxl
  end function CMaxlen
!
  pure elemental function Clen(this,idx) result(res)
  class(vector_chr),intent(in) :: this
  integer(4),intent(in)        :: idx
  integer(4)                   :: res
  integer                      :: i
    if(idx<1.or.idx>this%size())then
      res = 0
    else
      res = this%p(2,idx) - this%p(1,idx) + 1
    endif
  end function Clen
!
  pure elemental function CAt_idx(this,idx) result(res)
  class(vector_chr),intent(in) :: this
  integer(4),intent(in)        :: idx
  character(this%len(idx))     :: res
    if(idx<1.or.idx>this%size())then
      res = '' ; RETURN
    endif
    res = this%c(this%p(1,idx):this%p(2,idx))
  end function CAt_idx
!
  pure function CAt_range(this,lb,ub) result(res)
  class(vector_chr),intent(in) :: this
  integer(4),intent(in)        :: lb,ub
  character(this%mxl)          :: res(maxval([0,ub-lb+1],1))
  integer                      :: i,j
    j = 0
    do i=lb,ub
      if(i<1.or.i>this%size()) CYCLE
      j = j + 1
      res(j) = this%c(this%p(1,i):this%p(2,i))
    enddo
  end function CAt_range
!
  pure function CLookUp(this) result(res)
  class(vector_chr),intent(in) :: this
  character(this%mxl)          :: res(this%size())
  integer                      :: i
    do i=1,this%size()
      res(i) = this%c(this%p(1,i):this%p(2,i))
    enddo
  end function CLookUp
!
  pure elemental function CSmall(this,idx) result(res)
  use spur_string_neaten
  class(vector_chr),intent(in) :: this
  integer(4),intent(in)        :: idx
  character(this%len(idx))     :: res
    if(idx<1.or.idx>this%size()) RETURN
    res = small(this%c(this%p(1,idx):this%p(2,idx)))
  end function CSmall
!
  pure elemental function CLarge(this,idx) result(res)
  use spur_string_neaten
  class(vector_chr),intent(in) :: this
  integer(4),intent(in)        :: idx
  character(this%len(idx))     :: res
    if(idx<1.or.idx>this%size()) RETURN
    res = large(this%c(this%p(1,idx):this%p(2,idx)))
  end function CLarge
!
  pure elemental integer(4) function CTonum_int4(this,idx,dumm) result(res)
  use spur_string_tonum
  class(vector_chr),intent(in) :: this
  integer(4),intent(in)        :: idx
  integer(4),intent(in)        :: dumm
    if(idx<1.or.idx>this%size()) RETURN
    res = tonum_int4(this%c(this%p(1,idx):this%p(2,idx)),dumm)
  end function CTonum_int4
!
  pure elemental integer(4) function CTonum_real(this,idx,dumm) result(res)
  use spur_string_tonum
  class(vector_chr),intent(in) :: this
  integer(4),intent(in)        :: idx
  real,intent(in)              :: dumm
    if(idx<1.or.idx>this%size()) RETURN
    res = tonum_real(this%c(this%p(1,idx):this%p(2,idx)),dumm)
  end function CTonum_real
!
  pure elemental integer(4) function CTonum_dble(this,idx,dumm) result(res)
  use spur_string_tonum
  class(vector_chr),intent(in) :: this
  integer(4),intent(in)        :: idx
  double precision,intent(in)  :: dumm
    if(idx<1.or.idx>this%size()) RETURN
    res = tonum_dble(this%c(this%p(1,idx):this%p(2,idx)),dumm)
  end function CTonum_dble
!
  pure function CJoin(this,lb,ub,inc,delimiter) result(res)
  use spur_itertools
  class(vector_chr),intent(in)     :: this
  integer,intent(in),optional      :: lb,ub,inc
  character(*),intent(in),optional :: delimiter
  character(:),allocatable         :: res,ldel
  integer                          :: llb,lub,linc,nword
  integer                          :: head,tail,jlen,dlen
  integer                          :: i
    if(this%size()<1)then
      allocate(character(0)::res) ; RETURN
    endif
!
    allocate(character(0)::ldel)
    if(present(delimiter)) ldel = delimiter
    dlen = len(ldel)
!
    if(present(lb))then        ; llb  = lb
    else                       ; llb  = 1           ; endif
    if(present(ub))then        ; lub  = ub
    else                       ; lub  = this%size() ; endif
    if(present(inc))then       ; linc = inc
    else                       ; linc = 1           ; endif
!
    call IterScope(this%size(),llb,lub,linc,nword)
    jlen = sum([(this%len(i),i=llb,lub,linc)]) + dlen*(nword-1)
    allocate(character(jlen)::res)
!
    head = 1 ; tail = this%len(llb)
    res(head:tail) = this%c(this%p(1,llb):this%p(2,llb))
    do i=llb+linc,lub,linc
      head = tail + 1 ; tail = tail + dlen + this%len(i)
      res(head:tail) = ldel//this%c(this%p(1,i):this%p(2,i))
    enddo
    deallocate(ldel)
  end function CJoin
!
  pure subroutine CSplit(this,var,delimiter,pickup)
  class(vector_chr),intent(inout)       :: this
  character(*),intent(in)               :: var
  character(*),intent(in),optional      :: delimiter
  character(*),intent(in),optional      :: pickup
  character(1),allocatable              :: deli(:),pick(:)
  integer                               :: LenVar, i, j
  integer                               :: Head, Tail
  logical                               :: p
    if(present(delimiter))then
      if(len(delimiter)>0) allocate(deli(len(delimiter)))
      do i=1,len(delimiter)
        deli(i) = delimiter(i:i)
      enddo
    endif
    if(.not.allocated(deli))then
      allocate(deli(1)) ; deli = [" "]
    endif
!
    if(present(pickup))then
      if(len(pickup)>0) allocate(pick(len(pickup)))
      do i=1,len(pickup)
        pick(i) = pickup(i:i)
      enddo
    endif
    p = allocated(pick)
!
    head = 0 ; tail = 0
    do i=1,len(var)
      if(any(var(i:i)==deli))then
        if(head/=0) call this%push(adjustl(var(head:tail)))
        head = 0 ; CYCLE
      endif
      if(p)then
        if(any(var(i:i)==pick))then
          if(head/=0) call this%push(var(head:tail))
          call this%push(var(i:i))
          head = 0 ; CYCLE
        endif
      endif
      if(head == 0) head = i
      tail = i
    enddo
    if(head/=0) call this%push(adjustl(var(head:tail)))
  end subroutine CSplit
!
  pure integer function CFind(this,string,back,lb,ub) result(res)
  class(vector_chr),intent(in)       :: this
  character(*),intent(in)            :: string
  logical,intent(in),optional        :: back
  integer,intent(in),optional        :: lb,ub
  integer                            :: llb,lub,inc,tmp,i
    llb = 1 ; lub = this%size() ; inc = 1
    if(present(lb)) llb = maxval([lb,llb],1)
    if(present(ub)) lub = minval([ub,lub],1)
!
    if(present(back))then
      if(back)then ; tmp = llb ; llb = lub ; lub = tmp ; inc = -inc ; endif
    endif
!
    res = 0
    do i = llb,lub,inc
      if(string==this%at(i))then
        res = i ; RETURN
      endif
    enddo
  end function CFind
!
  pure subroutine CUniq(this,reverse)
  class(vector_chr),intent(inout) :: this
  logical,intent(in),optional     :: reverse
  logical                         :: isNew(this%size())
  integer                         :: i,j
    if(this%size()==0)RETURN
    if(present(reverse))then
      if(reverse)then
        call qs_down(this,1,this%size())
      else
        call qs_up(this,1,this%size())
      endif
    else
      call qs_up(this,1,this%size())
    endif
!
    isNew(1:1) = .TRUE.
    do i=2,this%size()
      isNew(i) = this%at(i-1)/=this%at(i)
    enddo
!
    j = 0 ; this%tot = 0
    do i=1,this%size()
      if(isNew(i))then
        j = j + 1
        this%tot = this%tot + this%len(i)
        this%p(:,j) = this%p(:,i)
      endif
    enddo
    this%p(:,j+1:) = 0
    call stack_reshape(this,count(isNew))
  end subroutine CUniq
!
  pure subroutine CSort(this,reverse)
  class(vector_chr),intent(inout) :: this
  logical,intent(in),optional     :: reverse
    if(this%size()==0)RETURN
    if(present(reverse))then
      if(reverse)then
        call qs_down(this,1,this%size())
        RETURN
      endif
    endif
    call qs_up(this,1,this%size())
  end subroutine CSort
!
  pure recursive subroutine qs_up(this,first,last)
  class(vector_chr),intent(inout) :: this
  integer,intent(in)              :: first,last
  character(this%mxl)             :: x
  integer                         :: t(2)
  integer                         :: i,j
    x=this%at(int((first+last)/2))
    i=first
    j=last
    do
      do while (this%at(i)<x)
        i=i+1
      enddo
      do while (this%at(j)>x)
        j=j-1
      enddo
      if(i>=j)exit
      t=this%p(:,i) ; this%p(:,i)=this%p(:,j) ; this%p(:,j)=t
      i=i+1
      j=j-1
    enddo
    if(first<i-1) call qs_up(this,first,i-1)
    if(j+1<last)  call qs_up(this,j+1,last)
  end subroutine qs_up
!
  pure recursive subroutine qs_down(this,first,last)
  class(vector_chr),intent(inout) :: this
  integer,intent(in)              :: first,last
  character(this%mxl)             :: x
  integer                         :: t(2)
  integer                         :: i,j
    x=this%at(int((first+last)/2))
    i=first
    j=last
    do
      do while (this%at(i)>x)
        i=i+1
      enddo
      do while (this%at(j)<x)
        j=j-1
      enddo
      if(i>=j)exit
      t=this%p(:,i) ; this%p(:,i)=this%p(:,j) ; this%p(:,j)=t
      i=i+1
      j=j-1
    enddo
    if(first<i-1) call qs_down(this,first,i-1)
    if(j+1<last)  call qs_down(this,j+1,last)
  end subroutine qs_down
!
  pure subroutine CTextWrap(this,text,width,delimiter)
  use spur_vector_int4
  class(vector_chr),intent(inout)       :: this
  character(*),intent(in)               :: text
  integer,intent(in)                    :: width
  character(*),intent(in),optional      :: delimiter
  character(width)                      :: tmp
  type(vector_chr)                      :: words
  type(vector_int4)                     :: head,tail
  integer,allocatable                   :: minima(:),lengths(:),breaks(:)
  integer,allocatable                   :: slack(:,:)
  integer                               :: width2
  integer                               :: i, j, x
    if(present(delimiter))then
      call CSplit(words,text,delimiter)
    else
      call CSplit(words,text)
    endif
    if(words%size()<1) RETURN
!
    allocate(lengths(words%size()))
    allocate(slack(words%size(),words%size()))
    allocate(minima(words%size()+1),breaks(words%size()))
!
    width2 = width + 1
    minima = NAN ; minima(1) = 0  ; breaks = 1
!
    lengths = words%len([(i,i=1,words%size())])
!
    slack = 0
    do j = 1,words%size()
      slack(j,j) = width2 - lengths(j)
      do i = j+1,words%size()
        slack(j,i) = slack(j,i-1) - lengths(i) - 1
      enddo
    enddo
!
    do j = 1,words%size()
      do i = j,1,-1
        if(slack(i,j)<0) EXIT
        x = minima(i) + slack(i,j)**2
        if(minima(j+1)>x)then
          minima(j+1) = x
          breaks(j) = i
        endif
      enddo
    enddo
!
    j = words%size() + 1
    do while(j>1)
      i = breaks(j-1)
      call head%push(i) ; call tail%push(j-1)
      j = i
    enddo
!
    do i = head%size(),1,-1
      tmp = trim(words%join(lb=head%at(i),ub=tail%at(i),delimiter=' '))
      call this%push(tmp)
    enddo
    deallocate(lengths,slack,minima,breaks)
    call words%clear()
    call head%clear() ; call tail%clear()
  end subroutine CTextwrap
!
  pure elemental subroutine CClear(this)
  class(vector_chr),intent(inout) :: this
    call stack_clear(this)
    if(allocated(this%p)) deallocate(this%p)
    if(allocated(this%c)) deallocate(this%c)
    this%mxl  = mxl_def ; this%tot = tot_def
    this%tail = tot_def
  end subroutine CClear
!
  pure subroutine CDestractor(this)
  type(vector_chr),intent(inout) :: this
    call this%clear()
  end subroutine CDestractor
end module spur_vector_chr
