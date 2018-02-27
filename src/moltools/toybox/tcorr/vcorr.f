program vcorr
use,intrinsic :: ISO_FORTRAN_ENV, only : OUTPUT_UNIT
use spur_optparse
use spur_io
use moltypes
implicit none
type(trajectory)     :: vec
type(fileio)         :: fio
!
  call LoadVector()
  call Vcorr_3d(vec%xyz(:,:,:),vec%n_atoms(),vec%n_frames())
!
contains
  subroutine LoadVector()
  type(optparse)       :: arg
  type(trajectory)     :: trj,to
  integer              :: i
    call arg%add_option("-m",narg=2,metavar='MaskString',&
                       &help='Load mask string <from> <to>, like as vmd.')
    call arg%add_option("-o",narg=1,metavar='outputfile',&
                       &help='output file.')
    call arg%parser()
    if(arg%narg()==0) call arg%call_usage()
    fio = arg%optargs("-o",1)
!
    call trj%load(arg%args())
    if(trj%n_atoms()<=0.or.trj%n_frames()<=0)STOP "this trajectory is empty!"
!
    to = trj
    call trj%pack(arg%optargs("-m",1))
    call to%pack(arg%optargs("-m",2))
    if(trj%n_atoms()/=to%n_atoms()) STOP "number of atoms must be same!"
    if(trj%n_atoms()==0) STOP "number of atoms is zero!"
    vec = to-trj
    if(vec%n_atoms()<=0.or.vec%n_frames()<=0)STOP "this trajectory is empty!"
  end subroutine LoadVector
!
  subroutine Vcorr_3d(t,natm,ntrj)
  use,intrinsic :: ISO_FORTRAN_ENV, only : OUTPUT_UNIT
  double precision,intent(in)   :: t(3,natm,ntrj)
  integer,intent(in)            :: natm,ntrj
  double precision              :: Vcorr(ntrj), Norm
  integer                       :: i,j,k,l
    Vcorr = 0.d0
    do j = 1,ntrj
      do i = j,ntrj
        l = i - j + 1
        do k = 1,natm
          Vcorr(l) = Vcorr(l) + dot_product(t(:,k,i),t(:,k,j))
        enddo
      enddo
    enddo
!
    Norm = dble(ntrj) / Vcorr(1)
    do i = 1,ntrj
      Vcorr(i) = Norm * Vcorr(i) / dble(ntrj - i + 1)
    enddo
!
    call fio%generate()
    write(fio%devn(),'(A,3X,A)')'# STEP','AUTOCORR'
    do i = 1,ntrj
      write(fio%devn(),'(i8,f16.9)')i,Vcorr(i)
    enddo
  end subroutine Vcorr_3d
end program Vcorr
