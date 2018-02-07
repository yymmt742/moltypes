module moltypes_process
  use moltypes_errorhandler
  use moltypes
  implicit none
  private
  public :: centering_coordinates,unwrap_coordinates,center_of_residue
contains
  subroutine centering_coordinates(this,mask)
  class(moltype),intent(inout)     :: this
  character(*),intent(in),optional :: mask
  integer                          :: j,natm,nframe
  real,allocatable                 :: xyz(:,:,:)
  logical,allocatable              :: selection(:)
  real                             :: revn
    natm  = this%natoms() ; nframe = this%nframes()
    if(natm<=0.or.nframe<=0) RETURN
!
    allocate(xyz(3,natm,nframe),selection(natm))
    xyz = this%xyz()
    if(present(mask))then
      selection = this%Getmask(mask)
    else
      selection = .TRUE.
    endif
!
    revn = 1.d0 / real(count(selection))
    do concurrent (j=1:nframe)
      block 
        integer :: i
        real    :: com(3)
        com = 0.d0
        do concurrent (i=1:natm)
          if(selection(i)) com = com + xyz(:,i,j)
        enddo
        com = com * revn
        do concurrent (i=1:natm)
          xyz(:,i,j) = xyz(:,i,j) -  com
        enddo
      end block 
    enddo
!
    call this%putxyz(xyz)
    deallocate(xyz,selection)
  end subroutine centering_coordinates
!
  pure function center_of_residue(this,mask) result(res)
  class(moltype),intent(in)        :: this
  character(*),intent(in),optional :: mask
  real,allocatable                 :: res(:,:,:)
  integer,allocatable              :: resid(:)
  integer                          :: i,j,natm,nres,nframe
  logical,allocatable              :: selection(:)
  real                             :: com(3),revn
    natm  = this%natoms() ; nframe = this%nframes() ; nres  = this%nresidues()
    allocate(res(3,nres,nframe))
    if(natm<=0.or.nframe<=0) RETURN
!
    allocate(resid(nres)) ; resid = this%inq('resid',1)
    call comcalc(natm,nres,maxval(resid,1),nframe,this%xyz(),resid,res)
  contains
    pure subroutine comcalc(natm,nres,nmax,nframe,xyz,resid,res)
    integer,intent(in)           :: natm,nres,nmax,nframe
    real,intent(in)              :: xyz(3,natm,nframe)
    integer,intent(in)           :: resid(natm)
    real,intent(out)             :: res(3,nres,nframe)
    real                         :: num(nmax)
    logical                      :: mask(nmax)
    integer                      :: k
      num = 0.0
      do k=1,NATM
        num(resid(k)) = num(resid(k)) + 1.0
      enddo
      mask = num<1.0E-10
      do k=1,nmax
        if(mask(k))cycle ; num(k) = 1.0 / num(k)
      enddo
!
      do concurrent (k=1:nframe) ; block
        integer :: i,j,k
        real    :: com(3,nmax)
        com = 0.0
        do i=1,NATM
          com(:,resid(i)) = com(:,resid(i)) + xyz(:,i,k)
        enddo
        j = 0
        do i=1,nmax
          if(mask(i)) CYCLE
          j = j + 1 ; res(:,j,k) = com(:,i) * num(i)
        enddo
      end block ; enddo
    end subroutine comcalc
  end function center_of_residue
!
  subroutine unwrap_coordinates(this)
  class(moltype),intent(inout)     :: this
  integer                          :: natm,nframe
  real,allocatable                 :: xyz(:,:,:),box(:,:)
  real,allocatable                 :: offset(:,:),prev(:,:)
  real                             :: width(3),test(3)
  integer                          :: i,j,k
    natm  = this%natoms() ; nframe = this%nframes()
    if(natm<=0.or.nframe<=0) RETURN
!
    allocate(xyz(3,natm,nframe),box(3,nframe),offset(3,natm),prev(3,natm))
    xyz = this%xyz() ; box = this%box()
!
    offset = 0.d0
    do k=2,nframe
      prev(:,:) = xyz(:,:,k-1)
      width = box(:,k) * 0.5
      do j=1,natm
        xyz(:,j,k) = xyz(:,j,k) + offset(:,j) * box(:,k)
        test(:) = xyz(:,j,k) - prev(:,j)
        do i=1,3
          if(test(i)>width(i))then
            offset(i,j) = offset(i,j) - 1.d0
            xyz(i,j,k)  = xyz(i,j,k) - box(i,k)
          elseif(test(i)<-width(i))then
            offset(i,j) = offset(i,j) + 1.d0
            xyz(i,j,k)  = xyz(i,j,k) + box(i,k)
          endif
        enddo
      enddo
    enddo
    call this%putxyz(xyz)
  end subroutine unwrap_coordinates
end module moltypes_process
