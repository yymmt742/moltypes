module moltypes_amberrst7
  use moltypes_errorhandler
  use spur_pathname
  use spur_stdio
  use spur_ncio
  implicit none
  private
  public :: AmberRst7
!
  integer,parameter       :: atom_def         = -1
  integer,parameter       :: frame_def        =  0
  integer,parameter       :: node_def         =  0
!
  integer,parameter       :: spatial_def      =  3
  integer,parameter       :: cell_spatial_def =  3
  integer,parameter       :: label_def        =  5
  integer,parameter       :: cell_angular_def =  3
!
  type AmberRst7
    private
    integer,public                      :: natoms = atom_def, nframes = frame_def
    integer,public                      :: nnode  = node_def, stack  = node_def
    integer                             :: stat  = MOLTYPES_NOERR
    type(pathname),allocatable          :: node(:)
    logical,public                      :: terminates_at_abnormal = .FALSE.
  contains
    procedure         :: fetch        => AmbRstFetch
    procedure         :: load         => AmbRstLoad
    procedure         :: isErr        => AmbRstIsErr
    procedure         :: clear        => AmbRstClear
    final             :: AmbRstDestractor
  end type AmberRst7
contains
  subroutine AmbRstFetch(this,path)
  class(AmberRst7),intent(inout) :: this
  character(*),intent(in)        :: path
  integer                        :: using
    call RoutineNameIs('AMBERRST7_FETCH')
    this%nnode = this%nnode + 1 ; using = this%nnode
    call stackExtention(this)   ; call this%node(using)%fetch(path)
    call CheckAmbRst(this,this%node(using)%isnotExist(),IO_NOTEXIST,using)
    call CheckAmbRst(this,this%node(using)%isnotReadable(),IO_NOTREADABLE,using)
    if(this%node(using)%isBinary())then
      call AmbRstFetchNetcdf(this,path)
    else
      call AmbRstFetchAscii(this,path)
    endif
    if(this%isErr()) this%nnode = this%nnode - 1
    this%nframes = this%nnode
  contains
    subroutine AmbRstFetchNetcdf(this,path)
    class(AmberRst7),intent(inout)    :: this
    character(*),intent(in)           :: path
    type(ncio)                        :: baff
    integer                           :: spatial,atom
    integer                           :: cell_spatial,label,cell_angular
    logical                           :: invalid
      call baff%fetch(path) ; call baff%loadheader()
      spatial      = baff%dim_length('spatial')
      atom         = baff%dim_length('atom')  ; cell_spatial = baff%dim_length('cell_spatial')
      label        = baff%dim_length('label') ; cell_angular = baff%dim_length('cell_angular')
      call this%node(using)%put_caption(baff%get_attribute("title"))
      invalid = ANY([baff%iserr(),spatial/=spatial_def,cell_spatial/=cell_spatial_def,&
          &          cell_angular/=cell_angular_def,label/=label_def,atom<=0])
      if(this%natoms==atom_def) this%natoms = atom
      invalid = invalid.or.this%natoms/=atom
      call CheckAmbRst(this,invalid,IO_NCFMTERR,using)
    end subroutine AmbRstFetchNetcdf
!
    subroutine AmbRstFetchAscii(this,path)
    use spur_vector_chr
    class(AmberRst7),intent(inout)    :: this
    character(*),intent(in)           :: path
    type(stdio)                       :: baff
    type(vector_chr)                  :: words
    integer                           :: atom
      baff%terminates_at_abnormal = this%terminates_at_abnormal
      call baff%fetch(path)
      call baff%load(maxn=2)
      call this%node(using)%put_caption(baff%gets())
      call words%split(baff%gets()) ; atom = words%tonum(1,0)
      if(this%natoms==atom_def) this%natoms = atom
      call CheckAmbRst(this,baff%iserr().or.this%natoms/=atom.or.atom<=0,IO_FMTERR,using)
    end subroutine AmbRstFetchAscii
  end subroutine AmbRstFetch
!
  subroutine StackExtention(this)
  class(AmberRst7),intent(inout)    :: this
  integer                           :: old
  type(pathname),allocatable        :: Swp(:)
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
  subroutine AmbRstLoad(this,xyz,vel,box,ang,time,lb,ub,inc,mask)
  use spur_itertools, only : IterScope
  use spur_shapeshifter
  class(AmberRst7),intent(inout)          :: this
  real,intent(inout),allocatable,optional :: xyz(:,:,:),vel(:,:,:)
  real,intent(inout),allocatable,optional :: box(:,:),ang(:,:),time(:)
  integer,intent(in),optional             :: lb,ub,inc
  logical,intent(in),optional             :: mask(:)
  logical,allocatable                     :: lmask(:)
  integer                                 :: nframes,nmask
  integer                                 :: llb,lub,linc
  integer                                 :: i,using
    call RoutineNameIs('AMBERRST7_LOAD')
    if(this%isErr().or.this%nnode<=0)RETURN
    llb  = 1          ; if(present(lb))  llb  = lb
    lub  = this%nnode ; if(present(ub))  lub  = ub
    linc = 1          ; if(present(inc)) linc = inc
!
    call IterScope(this%nnode,llb,lub,linc,nframes)
!
    allocate(lmask(this%natoms)) ; lmask = .TRUE.
    if(present(mask)) lmask = CompleteMask(mask,this%natoms)
    nmask = count(lmask)
!
    if(present(xyz))then
      if(LT_shape(shape(xyz),[spatial_def,nmask,nframes]))then
        if(allocated(xyz)) deallocate(xyz) ; allocate(xyz(spatial_def,nmask,nframes))
      endif
    endif
    if(present(vel))then
      if(LT_shape(shape(vel),[spatial_def,nmask,nframes]))then
        if(allocated(vel)) deallocate(vel) ; allocate(vel(spatial_def,nmask,nframes))
      endif
    endif
    if(present(box))then
      if(LT_shape(shape(box),[cell_spatial_def,nframes]))then
        if(allocated(box)) deallocate(box) ; allocate(box(cell_spatial_def,nframes))
      endif
    endif
    if(present(ang))then
      if(LT_shape(shape(ang),[cell_angular_def,nframes]))then
        if(allocated(ang)) deallocate(ang) ; allocate(ang(cell_angular_def,nframes))
      endif
    endif
    if(present(time))then
      if(size(time)<=nframes)then
        if(allocated(time)) deallocate(time) ; allocate(time(nframes))
      endif
    endif
!
    i = 0
    do using=llb,lub,linc
      i = i + 1
      if(this%node(using)%isBinary())then
        call AmbRstLoadNetcdf(this,lmask,i)
      else
        call AmbRstLoadAscii(this,lmask,i)
      endif
    enddo
  contains
    subroutine AmbRstLoadAscii(this,mask,frame)
    use spur_vector_chr
    class(AmberRst7),intent(inout)    :: this
    logical,intent(in)                :: mask(this%natoms)
    integer,intent(in)                :: frame
    real                              :: tmp(6*(this%natoms+1))
    type(stdio)                       :: baff
    type(vector_chr)                  :: words
    character(80)                     :: line
    integer                           :: i,j,k,is
      call baff%fetch(this%node(using)%is())
      call baff%connect()
      read(baff%devn(),'(a)',err=100,iostat=is) line
      read(baff%devn(),'(a)',err=100,iostat=is) line
      call words%split(line)
      if(present(time)) time(frame) = words%tonum(2,0.0)
      read(baff%devn(),'(6F12.7)',err=100,iostat=is) tmp
!
      if(present(xyz))then
        j = 0
        do i=1,this%natoms
          if(mask(i))then
            j = j + 1 ; k = 3 * i
            xyz(:,j,frame) = tmp(k-2:k)
          endif
        enddo
      endif
      if(is<0)then
        if(present(vel)) vel(:,:,frame) = 0.0
        if(present(box)) box(:,frame) = tmp(3*this%natoms+1:3*this%natoms+3)
        if(present(ang)) ang(:,frame) = tmp(3*this%natoms+4:3*this%natoms+6)
      elseif(is==0)then
        j = 0
        if(present(vel))then
          do i=1,this%natoms
            if(mask(i))then
              j = j + 1 ; k = 3 * (i + this%natoms)
              vel(:,j,frame) = tmp(k-2:k)
            endif
          enddo
        endif
        if(present(box)) box(:,frame) = tmp(6*this%natoms+1:6*this%natoms+3)
        if(present(ang)) ang(:,frame) = tmp(6*this%natoms+4:6*this%natoms+6)
      endif
      RETURN
100   continue
      call CheckAmbRst(this,.TRUE.,IO_FMTERR,using)
    end subroutine AmbRstLoadAscii
!
    subroutine AmbRstLoadNetcdf(this,mask,frame)
    class(AmberRst7),intent(inout)    :: this
    logical,intent(in)                :: mask(this%natoms)
    integer,intent(in)                :: frame
    type(ncio)                        :: baff
    real                              :: tmp(3,this%natoms)
    integer                           :: i,j,k,l,m
      call baff%fetch(this%node(using)%is())
      call baff%loadheader()
!      call baff%get('time',time(frame:frame),from=[1])
      time = 0.d0
      if(present(xyz))then
        call baff%get('coordinates',tmp,from=[1,1])
        j = 0
        do i=1,this%natoms
          if(mask(i))then
            j = j + 1 ; xyz(:,j,frame)=tmp(:,i)
          endif
        enddo
      endif
      if(present(vel))then
        tmp = 0.0
        call baff%get('velocities',tmp,from=[1,1])
        j = 0
        do i=1,this%natoms
          if(mask(i))then
            j = j + 1 ; vel(:,j,frame)=tmp(:,i)
          endif
        enddo
      endif
      if(present(box)) call baff%get('cell_lengths',box(:,frame),from=[1])
      if(present(ang)) call baff%get('cell_angles', ang(:,frame),from=[1])
      call CheckAmbRst(this,baff%isErr(),IO_FMTERR,using)
    end subroutine AmbRstLoadNetcdf
  end subroutine AmbRstLoad
!
  pure integer function AmbRstNnodes(this) result(res)
  class(AmberRst7),intent(in)  :: this
    res = this%nnode
  end function AmbRstNnodes
!
  pure logical function AmbRstIsERR(this) result (res)
  class(AmberRst7),intent(in)  :: this
    res = this%stat /= MOLTYPES_NOERR
  end function AmbRstIsERR
!
  subroutine CheckAmbRst(this,test,ierr,using)
  class(AmberRst7),intent(inout) :: this
  logical,intent(in)             :: test
  integer,intent(in)             :: ierr,using
    if(.not.test) RETURN
    if(this%terminates_at_abnormal)then
      if(using>0)then
        call moltypes_echo_errmsg(ierr,this%node(using)%is())
      else
        call moltypes_echo_errmsg(ierr)
      endif
      call AmbRstDestractor(this) ; call exit(ierr)
    else
      this%stat = ierr
    endif
  end subroutine CheckAmbRst
!
  subroutine AmbRstClear(this)
  class(AmberRst7),intent(inout)  :: this
    this%natoms = atom_def ; this%nframes = frame_def
    this%nnode  = node_def ; this%stack  = node_def
    this%stat  = MOLTYPES_NOERR
    if(allocated(this%node)) deallocate(this%node)
  end subroutine AmbRstClear
!
  subroutine AmbRstDestractor(this)
  type(AmberRst7),intent(inout)  :: this
    call this%clear()
  end subroutine AmbRstDestractor
end module moltypes_amberrst7
