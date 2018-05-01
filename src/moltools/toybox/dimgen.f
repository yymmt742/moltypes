program dimgen
use spur_vector_chr
use spur_vector_dble
use spur_optparse
use spur_stdio
implicit none
double precision,parameter :: Pi = 3.1415926535897932d0
double precision,parameter :: AngRad = Pi/180.d0
double precision,parameter :: RadAng = 180.d0/Pi

type(optparse)               :: arg
type(stdio)                  :: header,footer
type(stdio)                  :: inp,dat
type(vector_chr)             :: words
type(vector_dble)            :: sftx,sfty,sftz
type(vector_dble)            :: angx,angy,angz
integer                      :: natm,datm
character(4),allocatable     :: anm(:)
double precision,allocatable :: atz(:),crd(:,:),cpy(:,:)
logical,allocatable          :: hvy(:)
double precision             :: sft(3),nnt,coff
integer                      :: i,j,k,l,m,n
character(12)                :: fabc
character(12)                :: fxyz
!
double precision             :: RotMatrix(3,3)

  call arg%add_option("-m",narg=1,metavar='MaskString',help='Load mask string, like as vmd.')
  call arg%add_option("-h",narg=1,metavar='file',help='header file.')
  call arg%add_option("-f",narg=1,metavar='file',help='footer file.')
  call arg%add_option("-i",narg=1,metavar='file',help='input file.')
  call arg%add_option("-n",narg=1,metavar='value',def=['3.5'],help='neigbor value. default 3.5.')
  call arg%add_option("-c",narg=1,metavar='value',def=['999.0'],help='cutoff value. default 999.0')
  call arg%add_option("-o",narg=1,metavar='string',help='output file.')
  call arg%parser()
  if(arg%narg()==0) call arg%call_usage()
!
  call loadcrd()
!
  if(arg%option('-h'))then
    call header%fetch(arg%optargs('-h',1)) ; call header%load()
  endif
  if(arg%option('-f'))then
    call footer%fetch(arg%optargs('-f',1)) ; call footer%load()
  endif
!
  call inp%fetch(arg%optargs('-i',1)) ; call inp%load()
  call words%clear() ; call words%split(inp%gets())
  sftx = [(words%tonum(i,0.d0),i=1,words%size())]
  call words%clear() ; call words%split(inp%gets())
  sfty = [(words%tonum(i,0.d0),i=1,words%size())]
  call words%clear() ; call words%split(inp%gets())
  sftz = [(words%tonum(i,0.d0),i=1,words%size())]
  call words%clear() ; call words%split(inp%gets())
  angx = [(words%tonum(i,0.d0),i=1,words%size())]
  call words%clear() ; call words%split(inp%gets())
  angy = [(words%tonum(i,0.d0),i=1,words%size())]
  call words%clear() ; call words%split(inp%gets())
  angz = [(words%tonum(i,0.d0),i=1,words%size())]
  call inp%quit()
!
  nnt  = arg%optargd('-n',1)**2
  coff = arg%optargd('-c',1)**2
!
  call dat%fetch(trim(arg%optargs('-o',1))//'.xyz')
  call dat%generate()
!
  do n=1,angz%size()
  do m=1,angy%size()
  do l=1,angx%size()
    call Rotation([angx%at(l),angy%at(m),angz%at(n)])
    write(fabc,'(3(a1,i3.3))')'a',int(angx%at(l)+90.d0),'b',int(angy%at(m)+90.d0),'c',int(angz%at(n)+90.d0)
!
    do k=1,sftx%size() ; if(sftx%at(k)<1.0E-10.and.angz%at(n)<0.d0) CYCLE
    do j=1,sfty%size() ; if(sfty%at(j)<1.0E-10.and.angz%at(n)<0.d0) CYCLE
    do i=1,sftz%size()
      sft = [sftx%at(k),sfty%at(j),sftz%at(i)]
      if(dot_product(sft,sft)>coff)CYCLE
      if(checkAttach(sft))CYCLE
      write(fxyz,'(3(a1,i3.3))')'x',int(sft(1)*10),'y',int(sft(2)*10),'z',int(sft(3)*10)
      call writegamin()
    enddo ; enddo ; enddo
  enddo ; enddo ; enddo
!
  call dat%quit()
contains
  subroutine loadcrd()
  use moltypes_perser
  use moltypes_process
  type(molperser)      :: trj
  integer              :: i
    call trj%fetch(arg%args())
    call trj%load()
    natm = trj%natoms()
    if(natm<=0.or.trj%nframes()<=0)STOP "this trajectory is empty!"
    if(allocated(crd))deallocate(crd) ; allocate(crd(3,natm))
    if(allocated(cpy))deallocate(cpy) ; allocate(cpy(3,natm))
    if(allocated(atz))deallocate(atz) ; allocate(atz(natm))
    if(allocated(anm))deallocate(anm) ; allocate(anm(natm))
    if(allocated(hvy))deallocate(hvy) ; allocate(hvy(natm))
    datm = 2*natm
    anm  = trj%inq('name','xx  ')
    do i=1,natm
      select case(anm(i)(:1))
      case('C')
        atz(i) =  6.d0 ; hvy(i) = .TRUE.
      case('N')
        atz(i) =  7.d0 ; hvy(i) = .TRUE.
      case('O')
        atz(i) =  8.d0 ; hvy(i) = .TRUE.
      case('H')
        atz(i) =  1.d0 ; hvy(i) = .FALSE.
      case default
        atz(i) = -1.d0 ; hvy(i) = .FALSE.
      end select
    enddo
    call trj%centering_coordinates()
    crd  = trj%xyz(:,:,1)
    cpy  = crd
  end subroutine loadcrd
!
  subroutine writegamin()
  type(stdio)                 :: fio
  integer                     :: i
    call fio%fetch(trim(arg%optargs('-o',1))//'_'//fxyz//fabc//'.inp')
    call fio%generate()
    write(fio%devn(),'(a)') header%lookup()
    call fio%puts(' $DATA')
    call fio%puts('')
    call fio%puts('C1')
    write(dat%devn(),'(i0,/,a)') datm,fxyz//fabc
    do i=1,natm
      write(fio%devn(),'(x,a,f4.1,3f16.9)') anm(i),atz(i),crd(:,i)
      write(dat%devn(),'(a,3f16.9)') anm(i),crd(:,i)
    enddo
    do i=1,natm
      write(fio%devn(),'(x,a,f4.1,3f16.9)') anm(i),atz(i),cpy(:,i)+sft
      write(dat%devn(),'(a,3f16.9)') anm(i),cpy(:,i)+sft
    enddo
    call fio%puts(' $END')
    write(fio%devn(),'(a)') footer%lookup()
    call fio%quit()
  end subroutine writegamin

  logical function checkAttach(sft) result(res)
  double precision,intent(in) :: sft(3)
  double precision            :: v(3)
  integer                     :: i,j
    do j=1,natm
      if(.not.hvy(j))CYCLE
      do i=1,natm
        if(.not.hvy(i))CYCLE
        v = crd(:,j) - cpy(:,i) - sft
        res = dot_product(v,v) < nnt
        if(res) RETURN
      enddo
    enddo
  end function checkAttach
!
  subroutine Rotation(RotAng)
  double precision,intent(in) :: RotAng(3)
  double precision            :: Rot1(3,3),Rot2(3,3),Rot3(3,3)
  double precision            :: RotN(3),tmpn(3),cosht,rcosht,sinsht
  integer                     :: i
    rotn   = [0.d0,0.d0,1.d0]
    cosht  = cos(RotAng(1)*AngRad)
    rcosht = 1.d0 - cosht
    sinsht = sin(RotAng(1)*AngRad)
!
    Rot1(1,1) = RotN(1)**2*Rcosht+cosht
    Rot1(2,1) = RotN(1)*RotN(2)*rcosht-RotN(3)*sinsht
    Rot1(3,1) = RotN(3)*RotN(1)*rcosht+RotN(2)*sinsht
    Rot1(1,2) = RotN(1)*RotN(2)*rcosht+RotN(3)*sinsht
    Rot1(2,2) = RotN(2)**2*rcosht+cosht
    Rot1(3,2) = RotN(2)*RotN(3)*rcosht-RotN(1)*sinsht
    Rot1(1,3) = RotN(3)*RotN(1)*rcosht-RotN(2)*sinsht
    Rot1(2,3) = RotN(2)*RotN(3)*rcosht+RotN(1)*sinsht
    Rot1(3,3) = RotN(3)**2*rcosht+cosht
!
    rotn   = [sinsht,cosht,0.d0]
    tmpn   = [cosht,-sinsht,0.d0]
    cosht  = cos(RotAng(2)*AngRad)
    rcosht = 1.d0 - cosht
    sinsht = sin(RotAng(2)*AngRad)
!
    Rot2(1,1) = RotN(1)**2*Rcosht+cosht
    Rot2(2,1) = RotN(1)*RotN(2)*rcosht-RotN(3)*sinsht
    Rot2(3,1) = RotN(3)*RotN(1)*rcosht+RotN(2)*sinsht
    Rot2(1,2) = RotN(1)*RotN(2)*rcosht+RotN(3)*sinsht
    Rot2(2,2) = RotN(2)**2*rcosht+cosht
    Rot2(3,2) = RotN(2)*RotN(3)*rcosht-RotN(1)*sinsht
    Rot2(1,3) = RotN(3)*RotN(1)*rcosht-RotN(2)*sinsht
    Rot2(2,3) = RotN(2)*RotN(3)*rcosht+RotN(1)*sinsht
    Rot2(3,3) = RotN(3)**2*rcosht+cosht

    rotn(1) = dot_product(Rot2(:,1),tmpn)
    rotn(2) = dot_product(Rot2(:,2),tmpn)
    rotn(3) = dot_product(Rot2(:,3),tmpn)
    cosht   = cos(RotAng(3)*AngRad)
    rcosht  = 1.d0 - cosht
    sinsht  = sin(RotAng(3)*AngRad)
!
    Rot3(1,1) = RotN(1)**2*Rcosht+cosht
    Rot3(2,1) = RotN(1)*RotN(2)*rcosht-RotN(3)*sinsht
    Rot3(3,1) = RotN(3)*RotN(1)*rcosht+RotN(2)*sinsht
    Rot3(1,2) = RotN(1)*RotN(2)*rcosht+RotN(3)*sinsht
    Rot3(2,2) = RotN(2)**2*rcosht+cosht
    Rot3(3,2) = RotN(2)*RotN(3)*rcosht-RotN(1)*sinsht
    Rot3(1,3) = RotN(3)*RotN(1)*rcosht-RotN(2)*sinsht
    Rot3(2,3) = RotN(2)*RotN(3)*rcosht+RotN(1)*sinsht
    Rot3(3,3) = RotN(3)**2*rcosht+cosht
    RotMatrix = matmul(Rot1,Rot2)
    RotMatrix = matmul(RotMatrix,Rot3)
!
    do i=1,natm
      cpy(1,i) = dot_product(RotMatrix(:,1),crd(:,i))
      cpy(2,i) = dot_product(RotMatrix(:,2),crd(:,i))
      cpy(3,i) = dot_product(RotMatrix(:,3),crd(:,i))
    enddo
    RETURN
  end subroutine Rotation
end program dimgen
