module spur_pathname
use,intrinsic :: ISO_FORTRAN_ENV, only : INT8
use spur_vector_chr
use spur_ioerrorhandler
  implicit none
  private
  public :: pathname
  public :: assignment(=)
!
  integer,parameter         :: MAXBYTE  = 1028
!
  type pathname
    private
    character(:),allocatable  :: path,ext,base,caption
    type(vector_chr)          :: dir
    integer                   :: perm = IO_UNKNOWN
    logical                   :: binary = .FALSE.
    logical,public            :: terminates_at_abnormal = terminates_default
  contains
    procedure,private :: PathnameFmtWrite
    generic           :: WRITE(formatted)   => PathnameFmtWrite
    procedure         :: check              => FileCheck
    procedure         :: is                 => PathIs
    procedure         :: extension          => PathExtension
    procedure         :: dirnumber          => PathDirnumber
    procedure         :: dirname            => PathDirname
    procedure         :: basename           => PathBasename
    procedure         :: filename           => PathFilename
    procedure         :: rename_extension   => PathRenemeExtension
    procedure         :: put_caption        => PathnamePutCaption
    procedure         :: permission         => PathPermission
    procedure         :: isexist            => PathIsExist
    procedure         :: isnotexist         => PathIsnotExist
    procedure         :: isreadable         => PathIsReadable
    procedure         :: isnotreadable      => PathIsnotReadable
    procedure         :: iswritable         => PathIsWritable
    procedure         :: isnotwritable      => PathIsnotWritable
    procedure         :: isexecutable       => PathIsExecutable
    procedure         :: isbinary           => PathIsBinary
    procedure,private :: PathFetchPath
    procedure,private :: PathFetch
    generic           :: fetch              => PathFetch,PathFetchPath
    procedure         :: PathNameClear
    final             :: PathDestractor
  end type pathname
!
  interface assignment(=)
    module procedure FileAssign,FileCopy
  end interface assignment(=)
!
contains
  subroutine PathnameFmtWrite(this,unit,iotype,v_list,iostat,iomsg)
  class(pathname),intent(in)   :: this
  integer,intent(in)           :: unit
  character(*),intent(in)      :: iotype
  integer,intent(in)           :: v_list(:)
  integer,intent(out)          :: iostat
  character(*),intent(inout)   :: iomsg
  integer                      :: i,is
    iostat = 0
    if(this%path=='')then
      write(unit,'(a)',iostat=is) 'PATHNAME  : There is no Entry.'
    else
      if(allocated(this%caption))write(unit,'(a)',iostat=is) 'PATHNAME  : ',this%caption
      if(iotype(1:2)=='DT')then
        do i=3,len_trim(iotype)
          select case(iotype(i:i))
          case('P','p') ; write(unit,'(2a,/)',iostat=is)' path      : ',this%path
          case('E','e') ; write(unit,'(2a,/)',iostat=is)' extension : ',this%ext
          case('B','b') ; write(unit,'(2a,/)',iostat=is)' basename  : ',this%base
          case('D','d') ; write(unit,'(2a,/)',iostat=is)' directory : ',this%dir%join(delimiter='')
          case('S','s') ; call WritePermission()
          case default  ; CYCLE
          end select
          iostat = iostat + abs(is)
          flush(unit)
        enddo
      elseif(iotype=='LISTDIRECTED')then
        write(unit,'(2a,/)',iostat=is)' path      : ',this%path       ; iostat = iostat + abs(is)
      elseif(iotype=='NAMELIST')then
        write(unit,'(2a,/)',iostat=is)' path      : ',this%path       ; iostat = iostat + abs(is)
        write(unit,'(2a,/)',iostat=is)' extension : ',this%ext        ; iostat = iostat + abs(is)
        write(unit,'(2a,/)',iostat=is)' basename  : ',this%base       ; iostat = iostat + abs(is)
        write(unit,'(2a,/)',iostat=is)' directory : ',this%dir%join(delimiter='') ; iostat = iostat + abs(is)
        call WritePermission()
      endif
    endif
    if(iostat==0) RETURN
    iomsg='pathname_write error'
  contains
    subroutine WritePermission()
    integer :: tmp
      select case(this%perm)
      case(-1)     ;  write(unit,'(a)',iostat=tmp)' status    : Not Exist   '
      case( 0)     ;  write(unit,'(a)',iostat=tmp)' status    : Exist [---] '
      case( 1)     ;  write(unit,'(a)',iostat=tmp)' status    : Exist [--x] '
      case( 2)     ;  write(unit,'(a)',iostat=tmp)' status    : Exist [-w-] '
      case( 3)     ;  write(unit,'(a)',iostat=tmp)' status    : Exist [-wx] '
      case( 4)     ;  write(unit,'(a)',iostat=tmp)' status    : Exist [r--] '
      case( 5)     ;  write(unit,'(a)',iostat=tmp)' status    : Exist [r-x] '
      case( 6)     ;  write(unit,'(a)',iostat=tmp)' status    : Exist [rw-] '
      case( 7)     ;  write(unit,'(a)',iostat=tmp)' status    : Exist [rwx] '
      case default ;  write(unit,'(a)',iostat=tmp)' status    : UNKOWN      '
      end select
      is = abs(tmp)
      write(unit,'(/)',iostat=tmp)
      is = is + abs(tmp)
    end subroutine WritePermission
  end subroutine PathnameFmtWrite
!
  subroutine FileCheck(this)
  class(pathname),intent(inout) :: this
  integer                       :: rwx(0:3), ACCESS
    if(this%path=='') RETURN
    rwx(0) = ACCESS(this%path," ") ; if(rwx(0)/=0)rwx(0) = -1
    rwx(1) = ACCESS(this%path,"r") ; if(rwx(1)/=0)rwx(1) = -1
    rwx(2) = ACCESS(this%path,"w") ; if(rwx(2)/=0)rwx(2) = -1
    rwx(3) = ACCESS(this%path,"x") ; if(rwx(3)/=0)rwx(3) = -1
    this%perm = rwx(0) + 4 * (1 + rwx(1)) + 2 * (1 + rwx(2)) + rwx(3) + 1
    if(this%path==''.or.this%perm<4)RETURN
    this%binary = CheckBinary(this)
  contains
    logical function CheckBinary(this) result(res)
    class(Pathname),intent(inout)        :: this
    integer(INT8)                        :: val(MAXBYTE)
    integer                              :: i,dev,st
      res = .FALSE.
      dev = GetDevN() ; if(dev<0)RETURN
      open(dev, FILE=this%path, STATUS="OLD",ACCESS="STREAM",&
          &ACTION="READ",FORM="UNFORMATTED",IOSTAT=st)
      if(st/=0) RETURN
!
      do i = 1,MAXBYTE
        read(dev,END=100,ERR=110,IOSTAT=st) val(i)
      enddo
100   continue
      res = any(val(1:i-1)==0)
      if(.not.res)res = count(val(1:i-1)<=8) * 10 > i * 2
110   continue
      close(dev,IOSTAT=st)
    end function CheckBinary
!
    integer function GetDevN() result(res)
    logical :: op
      do res=11,100
        INQUIRE(res,OPENED=op)
        if(op)CYCLE ; RETURN
      enddo
      res=-1
    end function GetDevN
  end subroutine FileCheck
!
  pure subroutine PathnamePutCaption(this,string)
  class(Pathname),intent(inout)  :: this
  character(*),intent(in)        :: string
    if(.not.allocated(this%caption)) allocate(character(len(string))::this%caption)
    this%caption = string
  end subroutine PathnamePutCaption
!
  pure subroutine GetExtension(this)
  use spur_string_neaten
  class(Pathname),intent(inout)  :: this
  integer                        :: lb,ub
    if(allocated(this%ext))  deallocate(this%ext)  ; allocate(character(0)::this%ext)
    if(allocated(this%base)) deallocate(this%base) ; allocate(character(0)::this%base)
    call this%dir%clear()
!
    lb = index(this%path,'/',.TRUE.) + 1
    ub = index(this%path(lb:),'.',.TRUE.)-1
    if(ub==-1) ub=len_trim(this%path(lb:))
    this%base = this%path(lb:lb+ub-1)
    ub = len_trim(this%path)
    lb = index(this%path,'.',.TRUE.) + 1
    if(lb<=ub) this%ext = small(this%path(lb:ub))
    lb = 1
    do
      ub = index(this%path(lb:),'/') ; if(ub==0)EXIT
      ub = ub + lb - 1
      call this%dir%push(this%path(lb:ub))
      lb = ub + 1
    enddo
  end subroutine GetExtension
!
  pure function PathIs(this) result(res)
  class(pathname),intent(in)    :: this
  character(:),allocatable      :: res
    allocate(character(0)::res) ; if(allocated(this%path)) res = this%path
  end function PathIs
!
  pure integer function PathDirnumber(this) result(res)
  class(pathname),intent(in)    :: this
    res = this%dir%size()
  end function PathDirnumber
!
  pure function PathDirname(this,idx) result(res)
  class(pathname),intent(in)    :: this
  integer,intent(in),optional   :: idx
  character(:),allocatable      :: res
    allocate(character(0)::res)
    if(.not.present(idx))then
      res = this%dir%join(delimiter='') ; RETURN
    endif
    if(abs(idx)>this%dir%size()) RETURN
    if(idx==0)then
      res = PathFilename(this)
    else
      res = this%dir%at(idx)
    endif
  end function PathDirname
!
  pure function PathExtension(this) result(res)
  class(pathname),intent(in)    :: this
  character(:),allocatable      :: res
    allocate(character(0)::res) ; if(allocated(this%ext)) res = this%ext
  end function PathExtension
!
  pure function PathBasename(this) result(res)
  class(pathname),intent(in)    :: this
  character(:),allocatable      :: res
    allocate(character(0)::res) ; if(allocated(this%ext)) res = this%base
  end function PathBasename
!
  pure function PathFilename(this) result(res)
  class(pathname),intent(in)    :: this
  character(:),allocatable      :: res
  integer                       :: lb,ub
    allocate(character(0)::res)
    if(.not.allocated(this%path))RETURN
    lb = index(this%path,'/',.TRUE.)
    ub = len_trim(this%path)
    res = this%path(lb+1:ub)
  end function PathFilename
!
  pure subroutine PathRenemeExtension(this,ext)
  class(pathname),intent(inout)  :: this
  character(*),intent(in)        :: ext
    if(.not.allocated(this%ext)) RETURN ; this%ext = ext
  end subroutine PathRenemeExtension
!
  pure logical function PathIsExist(this) result(res)
  class(pathname),intent(in) :: this
    res = this%perm >= 0
  end function PathIsExist
!
  pure logical function PathIsnotExist(this) result(res)
  class(pathname),intent(in) :: this
    res = this%perm < 0
  end function PathIsnotExist
!
  pure logical function PathIsReadable(this) result(res)
  class(pathname),intent(in) :: this
    res = this%perm>=4
  end function PathIsReadable
!
  pure logical function PathIsnotReadable(this) result(res)
  class(pathname),intent(in) :: this
    res = this%perm<4
  end function PathIsnotReadable
!
  pure logical function PathIsnotWritable(this) result(res)
  class(pathname),intent(in) :: this
    res = this%perm>0.and.this%perm<2.or.this%perm==4.or.this%perm==5
  end function PathIsnotWritable
!
  pure logical function PathIsWritable(this) result(res)
  class(pathname),intent(in) :: this
    res = this%perm<=0.or.this%perm==2.or.this%perm==3.or.this%perm==6.or.this%perm==7
  end function PathIsWritable
!
  pure logical function PathIsExecutable(this) result(res)
  class(pathname),intent(in) :: this
    res = this%perm==1.or.this%perm==3.or.this%perm==5.or.this%perm==7
  end function PathIsExecutable
!
  pure logical function PathIsnotExecutable(this) result(res)
  class(pathname),intent(in) :: this
    res = this%perm<1.or.this%perm==2.or.this%perm==4.or.this%perm==6
  end function PathIsnotExecutable
!
  pure logical function PathIsBinary(this) result(res)
  class(Pathname),intent(in) :: this
    res = this%binary
  end function PathIsBinary
!
  pure integer function PathPermission(this) result(res)
  class(pathname),intent(in) :: this
    res = this%perm
  end function PathPermission
!
  subroutine PathFetch(LHS,RHS)
  class(pathname),intent(inout)  :: LHS
  character(*),intent(in)        :: RHS
    call PathDestractor(LHS)
    allocate(character(len_trim(RHS))::LHS%path)
    LHS%path = trim(RHS)
    call GetExtension(LHS)
    call FileCheck(LHS)
  end subroutine PathFetch
!
  pure subroutine PathFetchPath(LHS,RHS)
  class(pathname),intent(inout) :: LHS
  class(pathname),intent(in)    :: RHS
    call PathDestractor(LHS)
    if(allocated(RHS%path)) LHS%path = RHS%path
    if(allocated(RHS%ext))  LHS%ext  = RHS%ext
    if(allocated(RHS%base)) LHS%base = RHS%base
    LHS%dir  = RHS%dir
  end subroutine PathFetchPath
!
  elemental subroutine FileAssign(LHS,RHS)
  class(pathname),intent(inout)  :: LHS
  character(*),intent(in)        :: RHS
    call PathDestractor(LHS)
    allocate(character(len_trim(RHS))::LHS%path)
    LHS%path = trim(RHS)
    call GetExtension(LHS)
  end subroutine FileAssign
!
  elemental subroutine FileCopy(LHS,RHS)
  type(pathname),intent(inout) :: LHS
  type(pathname),intent(in)    :: RHS
    call PathDestractor(LHS)
    if(allocated(RHS%path))    LHS%path    = RHS%path
    if(allocated(RHS%ext))     LHS%ext     = RHS%ext
    if(allocated(RHS%base))    LHS%base    = RHS%base
    if(allocated(RHS%caption)) LHS%caption = RHS%caption
    LHS%dir    = RHS%dir
    LHS%perm   = RHS%perm ; LHS%binary = RHS%binary
    LHS%terminates_at_abnormal = RHS%terminates_at_abnormal
  end subroutine FileCopy
!
  pure elemental subroutine PathNameClear(this)
  class(pathname),intent(inout)  :: this
    if(allocated(this%path))    deallocate(this%path)
    if(allocated(this%ext))     deallocate(this%ext)
    if(allocated(this%base))    deallocate(this%base)
    if(allocated(this%caption)) deallocate(this%caption)
    call this%dir%clear()
    this%terminates_at_abnormal = terminates_default
    this%perm   = IO_UNKNOWN
    this%binary = .FALSE.
  end subroutine PathNameClear
!
  pure subroutine PathDestractor(this)
  type(Pathname),intent(inout)  :: this
    call PathNameClear(this)
  end subroutine PathDestractor
end module spur_pathname
