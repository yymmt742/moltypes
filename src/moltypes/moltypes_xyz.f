module moltypes_xyz
  use moltypes_errorhandler
  use spur_vector_chr
  use spur_pathname
  use spur_stdio
  implicit none
  private
  public :: XYZFMT
!
  integer,parameter       :: atom_def         = -1
  integer,parameter       :: frame_def        =  0
  integer,parameter       :: spatial_def      =  3
  integer,parameter       :: node_def         =  0
!
  type XYZFMT
    private
    integer,public                      :: natoms  = atom_def, nnode  = node_def
    integer,public                      :: nframes = frame_def
    logical,public                      :: terminates_at_abnormal = terminates_default
    integer                             :: stat  = MOLTYPES_NOERR, stack  = node_def
    type(pathname),allocatable          :: node(:)
    type(vector_chr)                    :: atm
  contains
    procedure         :: fetch        => XyzFetch
    procedure         :: load         => XyzLoad
    procedure         :: atoms        => XyzAtoms
    procedure         :: isErr        => XyzIsErr
    procedure         :: clear        => XyzClear
    final             :: XyzDestractor
  end type XYZFMT
!
contains
  subroutine XyzFetch(this,path)
  class(XYZFMT),intent(inout)  :: this
  character(*),intent(in)      :: path
  type(stdio)                  :: baff
  type(vector_chr)             :: words
  integer                      :: i,atom,using
    call RoutineNameIs('XYZFMT_FETCH')
    baff%terminates_at_abnormal = this%terminates_at_abnormal
    this%nnode  = this%nnode + 1 ; using = this%nnode
!
    call stackExtention(this) ; call this%node(using)%fetch(path)
    call CheckXyz(this,this%node(using)%isnotExist(),IO_NOTEXIST)
    call CheckXyz(this,this%node(using)%isnotReadable(),IO_NOTREADABLE)
!
    call baff%fetch(path)
    call baff%load() ; call CheckXyz(this,baff%iserr(),IO_FMTERR)
    call words%split(baff%gets()) ; atom = words%Tonum(1,0)
    call this%node(using)%put_caption(baff%gets())
!
    if(this%natoms==atom_def)then
      call this%atm%clear()
      do i=1,atom
        call words%clear() ; call words%split(baff%gets())
        call this%atm%push(words%at(1))
      enddo
100   CONTINUE
      this%natoms = this%atm%size()
    endif
!
    call CheckXyz(this,this%natoms/=atom.or.atom<=0,IO_NATOMERR)
    this%nframes = this%nnode
    if(.not.this%isErr()) RETURN
101 this%nnode = this%nnode - 1
  end subroutine XyzFetch
!
  subroutine StackExtention(this)
  class(XYZFMT),intent(inout)  :: this
  integer                      :: old
  type(pathname),allocatable   :: Swp(:)
    if(this%stack>=this%nnode.and.this%stack>0)return
    if(this%stack<=0)this%stack = 1 ; old = this%stack
    do while(this%stack<this%nnode)
      this%stack = this%stack * 2
    enddo
    allocate(swp(this%stack))
    if(allocated(this%node)) swp(1:old) = this%node(1:old)
    call move_alloc(from=swp,to=this%node)
  end subroutine StackExtention
!
  subroutine XYZLoad(this,xyz,lb,ub,inc,mask)
  use spur_itertools, only : IterScope
  use spur_shapeshifter
  use spur_string
  class(XYZFMT),intent(inout)             :: this
  real,intent(inout),allocatable,optional :: xyz(:,:,:)
  integer,intent(in),optional             :: lb,ub,inc
  logical,intent(in),optional             :: mask(:)
  logical,allocatable                     :: lmask(:)
  type(stdio)                             :: baff
  type(vector_chr)                        :: words
  integer                                 :: is,nmask,frame
  integer                                 :: llb,lub,linc
  integer                                 :: i,j
    call RoutineNameIs('XYZFMT_LOAD')
    if(this%isErr().or.this%nnode<=0)RETURN
    llb  = 1          ; if(present(lb))  llb  = lb
    lub  = this%nnode ; if(present(ub))  lub  = ub
    linc = 1          ; if(present(inc)) linc = inc
!
    call IterScope(this%nnode,llb,lub,linc,frame)
!
    allocate(lmask(this%natoms)) ; lmask = .TRUE.
    if(present(mask)) lmask = CompleteMask(mask,this%natoms)
    nmask = count(lmask)
!
    if(present(xyz))then
      if(.not.allocated(xyz).or.LT_shape(shape(xyz),[spatial_def,nmask,frame]))then
        if(allocated(xyz)) deallocate(xyz)
        allocate(xyz(spatial_def,nmask,frame))
      endif
    endif
!
    do j=1,this%nnode
      call baff%fetch(this%node(j)%is())
      call baff%load()
      call baff%goforward(2)
      do i=1,this%natoms
        call words%clear() ; call words%split(baff%gets())
        xyz(:,i,j) = tonum([words%at(2),words%at(3),words%at(4)],[0.0,0.0,0.0])
      enddo
      call baff%clear() ; call baff%quit()
    enddo
  end subroutine XyzLoad
!
  pure function XyzAtoms(this) result(res)
  class(XYZFMT),intent(in)                 :: this
  character(this%atm%maxlen()),allocatable :: res(:)
    allocate(res(this%natoms)) ; res = this%atm%lookup()
  end function XyzAtoms
!
  pure logical function XyzIsERR(this) result (res)
  class(XYZFMT),intent(in)  :: this
    res = this%stat /= MOLTYPES_NOERR
  end function XyzIsERR
!
  subroutine CheckXyz(this,test,ierr)
  class(XYZFMT),intent(inout)  :: this
  logical,intent(in)           :: test
  integer,intent(in)           :: ierr
    if(.not.test) RETURN
    if(this%terminates_at_abnormal)then
      call moltypes_echo_errmsg(ierr)
      call XyzDestractor(this) ; call exit(ierr)
    else
      this%stat = ierr
    endif
  end subroutine CheckXyz
!
  pure subroutine XyzClear(this)
  class(XYZFMT),intent(inout)  :: this
    this%natoms = atom_def ; this%nnode  = node_def
    this%nframes = frame_def
    this%stat  = MOLTYPES_NOERR ; this%stack  = node_def
    if(allocated(this%node)) deallocate(this%node)
    call this%atm%clear()
  end subroutine XyzClear
!
  pure subroutine XyzDestractor(this)
  type(XYZFMT),intent(inout)  :: this
    call this%clear()
  end subroutine XyzDestractor
end module moltypes_xyz
