module spur_hashtable
  use spur_stackmanager
  implicit none
  private
  public :: hashtable
!
  integer(4),parameter   :: mxl_def = 0
  integer(4),parameter   :: tot_def = 0
!
  integer(4),parameter   :: FNV_OFFSET_32 = 166136261
  integer(4),parameter   :: FNV_PRIME_32  = 16777619
!
  type,extends(stackmanager) :: hashtable
    private
    integer(4),allocatable        :: p(:,:)
    character(:),allocatable      :: c
    integer(4)                    :: mxl = mxl_def, tot = tot_def
    integer(4)                    :: mxk = mxl_def, tok = tot_def
  contains
    procedure,private   :: CPush
    generic             :: push        => Cpush
    procedure           :: len         => CLen
    procedure           :: maxlen      => CMaxlen
    procedure           :: memory      => Cmemory
    procedure           :: total       => Ctotal
    procedure           :: at          => CAt
    procedure           :: lookup      => CLookup
    procedure           :: key_lookup  => CkeyLookup
    procedure           :: clear       => CClear
    final               :: CDestractor
  end type hashtable
!
contains
  pure subroutine CExpand(this)
  class(hashtable),intent(inout) :: this
  integer(4),allocatable         :: ptmp(:,:)
  character(:),allocatable       :: ctmp
  integer(4)                     :: i,hash,tmp
    tmp = len(this%c)
    if(tmp<this%tot+this%tok)then
      if(tmp<1) tmp = 1
      do while(this%tot+this%tok>tmp)
        tmp = tmp * 2
      enddo
      allocate(character(tmp) :: ctmp)
      if(allocated(this%c)) ctmp(:) = this%c
      call move_alloc(from=ctmp,to=this%c)
    endif
!
    if(this%capacity()==0.or.size(this%p,2)>=this%size()) RETURN
!
    call move_alloc(from=this%p,to=ptmp)
    allocate(this%p(3,this%capacity())) ; this%p = -1
!
    if(allocated(ptmp))then   ! ---- rehash
      do i=1,size(ptmp,2)
        if(ptmp(1,i)<0) CYCLE
        tmp = HMhash32(this,this%c(ptmp(1,i):ptmp(2,i)))
        this%p(:,tmp) = ptmp(:,i)
      enddo
    endif
  end subroutine CExpand
!
  pure subroutine CPush(this,key,var)
  class(hashtable),intent(inout) :: this
  character(*),intent(in)        :: key,var
  integer(4)                     :: tmp,hash
    if(len_trim(key)==0) RETURN
    tmp = this%tot + this%tok + 1
    this%tok = this%tok + len_trim(key)
    this%mxk = maxval([this%mxk,len_trim(key)],1)
    this%tot = this%tot + len_trim(var)
    this%mxl = maxval([this%mxl,len_trim(var)],1)
    call stack_push(this,1) ; call CExpand(this)
    hash = HMhash32(this,key)
    this%p(1,hash) = tmp
    this%p(2,hash) = this%p(1,hash) + len_trim(key) - 1
    this%p(3,hash) = this%p(2,hash) + len_trim(var)
    this%c(this%p(1,hash):this%p(2,hash))   = trim(key)
    if(len_trim(var)==0) RETURN
    this%c(this%p(2,hash)+1:this%p(3,hash)) = trim(var)
  end subroutine CPush
!
  pure elemental function CTotal(this) result(res)
  class(hashtable),intent(in) :: this
  integer(4)                  :: res
    res = this%tot + this%tok
  end function CTotal
!
  pure elemental function CMemory(this) result(res)
  class(hashtable),intent(in) :: this
  integer(4)                  :: res
    res = len(this%c)
  end function CMemory
!
  pure elemental function CMaxlen(this) result(res)
  class(hashtable),intent(in) :: this
  integer(4)                  :: res
    res = this%mxl
  end function CMaxlen
!
  pure function Clen(this,key) result(res)
  class(hashtable),intent(in) :: this
  character(*),intent(in)     :: key
  integer(4)                  :: res,hash
    res = 0
    if(len_trim(key)<=0) RETURN
    hash = HMhash32(this,key)
    if(hash<=0) RETURN
    res = this%p(2,hash) - this%p(1,hash) + 1
  end function Clen
!
  pure function CAt(this,key) result(res)
  class(hashtable),intent(in) :: this
  character(*),intent(in)     :: key
  character(:),allocatable    :: res
  integer(4)                  :: hash
    allocate(character(0) :: res)
    if(len_trim(key)==0) RETURN
    hash = HMhash32(this,key)
    if(hash<1) RETURN
    res = this%c(this%p(2,hash)+1:this%p(3,hash))
  end function CAt
!
  pure function CLookUp(this) result(res)
  class(hashtable),intent(in) :: this
  character(this%mxl)         :: res(this%size())
  integer                     :: i,j
    j = 0
    do i=1,size(this%p,2)
      if(this%p(1,i)<0) CYCLE
      j = j + 1
      res(j) = this%c(this%p(2,i)+1:this%p(3,i))
    enddo
  end function CLookUp
!
  pure function CKeyLookUp(this) result(res)
  class(hashtable),intent(in) :: this
  character(this%mxk)         :: res(this%size())
  integer                     :: i,j
    j = 0
    do i=1,size(this%p,2)
      if(this%p(1,i)<0) CYCLE
      j = j + 1
      res(j) = this%c(this%p(1,i):this%p(2,i))
    enddo
  end function CKeyLookUp
!
  pure integer(4) function HMhash32(this,key) result (res)
  class(hashtable),intent(in) :: this
  character(*),intent(in)     :: key
  integer                     :: i
    res = FNV_OFFSET_32
    do i=1,len_trim(key)
      res = ieor(res,ichar(key(i:i)))
      res = FNV_PRIME_32*res
    enddo
    res = mod(abs(res),this%capacity())+1
    do i=1,this%capacity()
      if(this%p(1,res)<0) RETURN
      if(this%c(this%p(1,res):this%p(2,res))==key) RETURN
      res = mod(res,this%capacity()) + 1
    enddo
    res = 0
  end function HMhash32
!
  pure elemental subroutine CClear(this)
  class(hashtable),intent(inout) :: this
    call stack_clear(this)
    if(allocated(this%p)) deallocate(this%p)
    if(allocated(this%c)) deallocate(this%c)
    this%mxl = mxl_def ; this%tot = tot_def
    this%mxk = mxl_def ; this%tok = tot_def
  end subroutine CClear
!
  pure subroutine CDestractor(this)
  type(hashtable),intent(inout) :: this
    call this%clear()
  end subroutine CDestractor
end module spur_hashtable
