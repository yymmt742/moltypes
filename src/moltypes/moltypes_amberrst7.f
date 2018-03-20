module moltypes_amberrst7
  use moltypes_errorhandler
  use spur_vector
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
    integer                             :: natom = atom_def, nnode  = node_def
    integer                             :: nmask = atom_def, nframe = frame_def
    integer                             :: stat  = MOLTYPES_NOERR, stack  = node_def
    type(pathname),allocatable          :: node(:)
    real,allocatable,public             :: xyz(:,:,:)
    real,allocatable,public             :: vel(:,:,:)
    real,allocatable,public             :: time(:)
    double precision,allocatable,public :: box(:,:),ang(:,:)
    logical,public                      :: terminates_at_abnormal = .FALSE.
  contains
    procedure,private :: AmbRstFmtWrite
    generic           :: WRITE(formatted)   => AmbRstFmtWrite
    procedure         :: fetch        => AmbRstFetch
    procedure         :: load         => AmbRstLoad
    procedure         :: nframes      => AmbRstNframes
    procedure         :: natoms       => AmbRstNatoms
    procedure         :: nnodes       => AmbRstNnodes
    procedure         :: isErr        => AmbRstIsErr
    procedure         :: clear        => AmbRstClear
    final             :: AmbRstDestractor
  end type AmberRst7
contains
  subroutine AmbRstFmtWrite(this,unit,iotype,v_list,iostat,iomsg)
  use spur_string
  class(AmberRst7),intent(in)  :: this
  integer,intent(in)           :: unit
  character(*),intent(in)      :: iotype
  integer,intent(in)           :: v_list(:)
  integer,intent(out)          :: iostat
  character(*),intent(inout)   :: iomsg
  character(:),allocatable     :: space
  integer                      :: i,dig
    iostat = 0
    if(this%nnode<=0)then
      write(unit,'(a)',iostat=iostat,iomsg=iomsg) 'HERE IS EMPTY CONTAINER' ; RETURN
    endif
    dig = digit(this%nnode)
    write(unit,'(a,i0,a,i0,a,/)',iostat=iostat,iomsg=iomsg) 'HERE CONTAINS ',this%natom,' ATOMS / ',this%nnode,' FILES'
    allocate(character(dig+3)::space) ; space(:) = ''
    do i=1,this%nnode
      if(iostat/=0) RETURN
      write(unit,'(2a,/)',iostat=iostat,iomsg=iomsg) '['//padding(i,dig)//'] ',this%node(i)%is()
    enddo
  end subroutine AmbRstFmtWrite
!
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
    this%nmask = this%natom
    if(this%isErr()) this%nnode = this%nnode - 1
    this%nframe = this%nnode
  contains
    subroutine AmbRstFetchNetcdf(this,path)
    class(AmberRst7),intent(inout)    :: this
    character(*),intent(in)           :: path
    type(ncio)                        :: baff
    integer                           :: frame,spatial,atom
    integer                           :: cell_spatial,label,cell_angular
    logical                           :: invalid
      call baff%fetch(path)
      frame        = baff%dim_length('frame') ; spatial      = baff%dim_length('spatial')
      atom         = baff%dim_length('atom')  ; cell_spatial = baff%dim_length('cell_spatial')
      label        = baff%dim_length('label') ; cell_angular = baff%dim_length('cell_angular')
      call this%node(using)%put_caption(baff%get_attribute("title"))
      invalid = ANY([baff%iserr(),spatial/=spatial_def,cell_spatial/=cell_spatial_def,&
          &          cell_angular/=cell_angular_def,label/=label_def,atom<=0,frame<0])
      if(this%natom==atom_def) this%natom = atom
      invalid = invalid.or.this%natom/=atom
      call CheckAmbRst(this,invalid,IO_NCFMTERR,using)
    end subroutine AmbRstFetchNetcdf
!
    subroutine AmbRstFetchAscii(this,path)
    class(AmberRst7),intent(inout)    :: this
    character(*),intent(in)           :: path
    type(stdio)                       :: baff
    type(vector_character)            :: words
    integer                           :: atom
      baff%terminates_at_abnormal = this%terminates_at_abnormal
      call baff%fetch(path)
      call baff%load(maxline=2)
      call this%node(using)%put_caption(baff%gets())
      call words%split(baff%gets()) ; atom = words%ToInt(1)
      if(this%natom==atom_def) this%natom = atom
      call CheckAmbRst(this,baff%iserr().or.this%natom/=atom.or.atom<=0,IO_FMTERR,using)
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
  subroutine AmbRstLoad(this,lb,ub,inc,mask)
  use spur_itertools, only : IterScope
  class(AmberRst7),intent(inout)  :: this
  integer,intent(in),optional     :: lb,ub,inc
  logical,intent(in),optional     :: mask(:)
  logical,allocatable             :: lmask(:)
  integer                         :: llb,lub,linc,using
    call RoutineNameIs('AMBERRST7_LOAD')
    if(this%isErr().or.this%nnode<=0)RETURN
    llb  = 1          ; if(present(lb))  llb  = lb
    lub  = this%nnode ; if(present(ub))  lub  = ub
    linc = 1          ; if(present(inc)) linc = inc
!
    call IterScope(this%nnode,llb,lub,linc,this%nframe)
!
    allocate(lmask(this%natom)) ; lmask = .TRUE.
    if(present(mask)) lmask = CompleteMask(this,mask)
    this%nmask = count(lmask)
!
    if(LT_shape(shape(this%xyz),[spatial_def,this%nmask,this%nframe]))then
      if(allocated(this%xyz)) deallocate(this%xyz)
      allocate(this%xyz(spatial_def,this%nmask,this%nframe))
      if(allocated(this%vel)) deallocate(this%vel)
      allocate(this%vel(spatial_def,this%nmask,this%nframe))
    endif
    if(LT_shape(shape(this%time),[this%nframe]))then
      if(allocated(this%time)) deallocate(this%time) ; allocate(this%time(this%nframe))
      if(allocated(this%box))  deallocate(this%box)  ; allocate(this%box(cell_spatial_def,this%nframe))
      if(allocated(this%ang))  deallocate(this%ang)  ; allocate(this%ang(cell_angular_def,this%nframe))
    endif
!
    do using=llb,lub,linc
      if(this%node(using)%isBinary())then
        call AmbRstLoadNetcdf(this,lmask)
      else
        call AmbRstLoadAscii(this,lmask)
      endif
    enddo
  contains
    subroutine AmbRstLoadAscii(this,mask)
    class(AmberRst7),intent(inout)    :: this
    logical,intent(in)                :: mask(this%natom)
    real                              :: tmp(6*(this%natom+1))
    type(stdio)                       :: baff
    type(vector_character)            :: words
    character(80)                     :: line
    integer                           :: i,j,k,is
      call baff%fetch(this%node(using)%is())
      call baff%connect()
      read(baff%devn(),'(a)',err=100,iostat=is) line
      read(baff%devn(),'(a)',err=100,iostat=is) line
      call words%split(line) ; this%time(using) = words%ToReal(2)
      read(baff%devn(),'(6F12.7)',err=100,iostat=is) tmp
!
      j = 0
      do i=1,this%natom
        if(mask(i))then
          j = j + 1 ; k = 3 * i
          this%xyz(:,j,using) = tmp(k-2:k)
        endif
      enddo
      if(is<0)then
        this%vel(:,:,using) = 0.0
        this%box(:,using) = tmp(3*this%natom+1:3*this%natom+3)
        this%ang(:,using) = tmp(3*this%natom+4:3*this%natom+6)
      elseif(is==0)then
        j = 0
        do i=1,this%natom
          if(mask(i))then
            j = j + 1 ; k = 3 * (i + this%natom)
            this%vel(:,j,using) = tmp(k-2:k)
          endif
        enddo
        this%box(:,using) = tmp(6*this%natom+1:6*this%natom+3)
        this%ang(:,using) = tmp(6*this%natom+4:6*this%natom+6)
      endif
      RETURN
100   continue
      call CheckAmbRst(this,.TRUE.,IO_FMTERR,using)
    end subroutine AmbRstLoadAscii
!
    subroutine AmbRstLoadNetcdf(this,mask)
    class(AmberRst7),intent(inout)    :: this
    logical,intent(in)                :: mask(this%natom)
    type(ncio)                        :: baff
    real                              :: tmp(3,this%natom,1)
    integer                           :: frame
    integer                           :: i,j,k,l,m
      call baff%fetch(this%node(using)%is())
      frame = baff%dim_length('frame')
      call baff%get('time',this%time(using:using),from=[frame])
      call baff%get('coordinates',tmp(:,:,:),from=[1,1,frame])
      j = 0
      do i=1,this%natom
        if(mask(i))then
          j = j + 1 ; this%xyz(:,j,using)=tmp(:,i,1)
        endif
      enddo
      tmp = 0.0
      call baff%get('velocities',tmp(:,:,:),from=[1,1,frame])
      j = 0
      do i=1,this%natom
        if(mask(i))then
          j = j + 1 ; this%vel(:,j,using)=tmp(:,i,1)
        endif
      enddo
      call baff%get('cell_lengths',this%box(:,using:using),from=[1,frame])
      call baff%get('cell_angles', this%ang(:,using:using),from=[1,frame])
      call CheckAmbRst(this,baff%isErr(),IO_FMTERR,using)
    end subroutine AmbRstLoadNetcdf
!
    pure function CompleteMask(this,mask) result(res)
    class(AmberRst7),intent(in) :: this
    logical,intent(in)          :: mask(:)
    logical                     :: res(this%natom)
    integer                     :: l,u,s
      l = lbound(mask,1) ; u = ubound(mask,1) ; s = u - l + 1
      if(s>this%natom)then
        res(:) = mask(:l+this%natom-1)
      else
        res(:s) = mask ; if(s<this%natom) res(s+1:) = .FALSE.
      endif
    end function CompleteMask
!
    pure logical function LT_shape(L,R) result(res)
    integer,intent(in) :: L(:),R(:)
    integer            :: i,N
      N = minval([size(L),size(R)],1)
      do i=1,N-1
        res = L(i) /= R(i) ; if(res) RETURN
      enddo
      res = L(N) < R(N)
    end function LT_shape
  end subroutine AmbRstLoad
!
  pure integer function AmbRstNatoms(this) result(res)
  class(AmberRst7),intent(in)  :: this
    res = this%nmask
  end function AmbRstNatoms
!
  pure integer function AmbRstNframes(this) result(res)
  class(AmberRst7),intent(in)  :: this
    res = this%nframe
  end function AmbRstNframes
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
    this%natom = atom_def ; this%nnode  = node_def
    this%nmask = atom_def ; this%nframe = frame_def
    this%stat  = MOLTYPES_NOERR ; this%stack  = node_def
    if(allocated(this%node)) deallocate(this%node)
    if(allocated(this%xyz))  deallocate(this%xyz)
    if(allocated(this%vel))  deallocate(this%vel)
    if(allocated(this%time)) deallocate(this%time)
    if(allocated(this%box))  deallocate(this%box)
    if(allocated(this%ang))  deallocate(this%ang)
  end subroutine AmbRstClear
!
  subroutine AmbRstDestractor(this)
  type(AmberRst7),intent(inout)  :: this
    call this%clear()
  end subroutine AmbRstDestractor
end module moltypes_amberrst7
