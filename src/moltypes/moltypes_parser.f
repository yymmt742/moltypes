module moltypes_parser
  use moltypes
  implicit none
  private
  public :: molparser
!
  integer,parameter :: spatial = 3
!
  type,extends(moltype) :: molparser
    private
  contains
    procedure,private :: center_idx
    procedure,private :: center_range
    procedure,private :: center_all
    generic           :: center_of_coordinates => center_idx, center_range, center_all
    procedure,private :: centering_all
    procedure,private :: centering_mask
    generic           :: centering_coordinates => centering_all, centering_mask
    procedure         :: center_of_residue
    procedure         :: unwrap_coordinates
  end type molparser
contains
  pure function center_idx(this,idx,mask) result(res)
  class(molparser),intent(in)      :: this
  integer,intent(in)               :: idx
  character(*),intent(in),optional :: mask
  logical,allocatable              :: slct(:,:)
  real                             :: res(spatial),ncnt
    res = 0.0 ; if(this%out_of_range(idx)) RETURN
    if(.not.allocated(this%xyz)) RETURN
    if(present(mask))then
      allocate(slct(spatial,this%natoms())) ; slct = this%atommask(mask,spatial)
      ncnt = real(count(slct)/3)    ; if(ncnt<0.5)RETURN
      res = sum(this%xyz(:,:,idx),dim=2,mask=slct) / ncnt
    else
      res = sum(this%xyz(:,:,idx),dim=2) / real(this%natoms())
    endif
  end function center_idx
!
  pure function center_range(this,lb,ub,mask) result(res)
  class(molparser),intent(in)      :: this
  integer,intent(in)               :: lb,ub
  character(*),intent(in),optional :: mask
  real                             :: res(spatial,maxval([0,ub-lb+1],1))
  logical,allocatable              :: slct(:,:)
  real                             :: ncnt
  integer                          :: i
    res = 0.0 ; if(this%out_of_range([lb,ub])) RETURN
    if(.not.allocated(this%xyz)) RETURN
    if(present(mask))then
      allocate(slct(spatial,this%natoms())) ; slct = this%atommask(mask,spatial)
      ncnt = real(count(slct)/3)    ; if(ncnt<0.5)RETURN
      ncnt = 1.0 / ncnt
      do i=lb,ub
        res(:,i) = sum(this%xyz(:,:,i),dim=2,mask=slct) * ncnt
      enddo
    else
      res = sum(this%xyz(:,:,lb:ub),dim=2) / real(this%natoms())
    endif
  end function center_range
!
  pure function center_all(this,mask) result(res)
  class(molparser),intent(in)      :: this
  character(*),intent(in),optional :: mask
  real                             :: res(spatial,this%nframes())
  logical,allocatable              :: slct(:,:)
  real                             :: ncnt
  integer                          :: i
    res = 0.0 ; if(this%out_of_range()) RETURN
    if(.not.allocated(this%xyz)) RETURN
    if(present(mask))then
      allocate(slct(spatial,this%natoms())) ; slct = this%atommask(mask,spatial)
      ncnt = real(count(slct)/3)    ; if(ncnt<0.5)RETURN
      ncnt = 1.0 / ncnt
      do i=1,this%nframes()
        res(:,i) = sum(this%xyz(:,:,i),dim=2,mask=slct) * ncnt
      enddo
    else
      res = sum(this%xyz(:,:,1:this%nframes()),dim=2) / real(this%natoms())
    endif
  end function center_all
!
  pure elemental subroutine centering_all(this)
  class(molparser),intent(inout)   :: this
  real                             :: com(spatial)
  integer                          :: i,j
    if(this%out_of_range()) RETURN
    if(.not.allocated(this%xyz)) RETURN
    do j=1,this%nframes()
      com = center_idx(this,j)
      do i=1,this%natoms()
        this%xyz(:,i,j) = this%xyz(:,i,j) - com
      enddo
    enddo
  end subroutine centering_all
!
  pure elemental subroutine centering_mask(this,mask)
  class(molparser),intent(inout)   :: this
  character(*),intent(in)          :: mask
  real                             :: com(spatial,this%nframes())
  integer                          :: i,j
    if(this%out_of_range()) RETURN
    if(.not.allocated(this%xyz)) RETURN
    com = center_all(this,mask)
    do j=1,this%nframes()
      do i=1,this%natoms()
        this%xyz(:,i,j) = this%xyz(:,i,j) - com(:,j)
      enddo
    enddo
  end subroutine centering_mask
!
  pure function center_of_residue(this,mask) result(res)
  use spur_vector_real
  use spur_vector_int4
  class(molparser),intent(in)      :: this
  character(*),intent(in),optional :: mask
  real,allocatable                 :: res(:,:,:)
  integer                          :: natm,nres,ntrj
  integer                          :: i,j,k,nmax,nmsk
  type(vector_int4)                :: resid,resuq,reseq
  type(vector_real)                :: num
  logical,allocatable              :: slct(:)
    natm = this%natoms()
    nres = this%nresidues()
    ntrj = this%nframes()
    if(any([natm,nres,ntrj]<1))then
      allocate(res(spatial,nres,ntrj))
      res = 0.0 ; RETURN
    endif
!
    allocate(slct(natm))
!
    if(present(mask))then
      slct = this%atommask(mask)
    else
      slct = .TRUE.
    endif
!
    resid = this%inq('resid',1)
    reseq = resid%lookup()
    resuq = pack(resid%lookup(),slct) ; call resuq%uniq()
!
    resid%at = 0
!
    k = 0
    do i=1,resuq%size()
      k = k + 1
      do j=1,reseq%size()
        if(resuq%at(i)==reseq%at(j).and.slct(j)) resid%at(j) = k
      enddo
    enddo
    call reseq%clear() ; call resuq%clear()
!
    nmax  = maxval(resid%lookup(),1)
    allocate(res(spatial,nmax,ntrj))
    res = 0.0 ; if(nmax<1) RETURN
!
    do i=1,nmax
      j  = count(resid%lookup()==i.and.slct)
      if(j>0) call num%push(1.0/real(j))
    enddo
!
    do j=1,ntrj
      do i=1,natm
        if(slct(i)) res(:,resid%at(i),j) = res(:,resid%at(i),j) + this%xyz(:,i,j)
      enddo
      do i=1,nmax
        res(:,i,j) = res(:,i,j) * num%at(i)
      enddo
    enddo
    deallocate(slct)
  end function center_of_residue
!
  pure subroutine unwrap_coordinates(this)
  class(molparser),intent(inout) :: this
  real                           :: offset(spatial,this%natoms())
  real                           :: prev(spatial,this%natoms())
  real                           :: width(spatial),test(spatial)
  integer                        :: i,j,k
    if(this%out_of_range()) RETURN
    if(.not.allocated(this%xyz).or..not.allocated(this%box)) RETURN
!
    offset = 0.0
    do k=2,this%nframes()
      prev  = this%xyz(:,:,k-1)
      width = this%box(:,k) * 0.5
      do j=1,this%natoms()
        this%xyz(:,j,k) = this%xyz(:,j,k) + offset(:,j) * this%box(:,k)
        test(:) = this%xyz(:,j,k) - prev(:,j)
        do i=1,spatial
          if(test(i)>width(i))then
            offset(i,j) = offset(i,j) - 1.d0
            this%xyz(i,j,k)  = this%xyz(i,j,k) - this%box(i,k)
          elseif(test(i)<-width(i))then
            offset(i,j) = offset(i,j) + 1.d0
            this%xyz(i,j,k) = this%xyz(i,j,k) + this%box(i,k)
          endif
        enddo
      enddo
    enddo
  end subroutine unwrap_coordinates
end module moltypes_parser
