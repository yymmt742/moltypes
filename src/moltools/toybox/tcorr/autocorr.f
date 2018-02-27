program Autocorr
use,intrinsic :: ISO_FORTRAN_ENV, only : OUTPUT_UNIT
use spur_optparse
use moltypes
use spur_io
implicit none
type(optparse)       :: arg
type(trajectory)     :: trj
type(fileio)         :: fio
integer              :: i
  call arg%add_option("-m",narg=1,metavar='MaskString',help='Load mask string, like as vmd.')
  call arg%add_option("-o",narg=1,metavar='outputfile',help='output file.')
  call arg%parser()
  if(arg%narg()==0) call arg%call_usage()
!
  call trj%load(arg%args(),mask=arg%optargs('-m',1))
  if(trj%n_atoms()<=0.or.trj%n_frames()<=0)STOP "this trajectory is empty!"
  fio = arg%optargs('-o',1)
!
  call trj%center_coordinates()
  call Autocorr_3d()
  stop
contains
  subroutine Autocorr_3d()
  double precision   :: autocorr(trj%n_frames(),4)
  double precision   :: r_three
  integer            :: i
    autocorr(:,1) = Autocorr_1d(trj%xyz(1,:,:),trj%n_atoms(),trj%n_frames())
    autocorr(:,2) = Autocorr_1d(trj%xyz(2,:,:),trj%n_atoms(),trj%n_frames())
    autocorr(:,3) = Autocorr_1d(trj%xyz(3,:,:),trj%n_atoms(),trj%n_frames())
    r_three = 1.d0 /3.d0
    autocorr(:,4) = (Autocorr(:,1)+Autocorr(:,2)+Autocorr(:,3)) * r_three

    call fio%generate()
    write(fio%devn(),'(A,9X,A,15X,A,15X,A,12X,A)')'# STEP','X','Y','Z','AVERAGE'
    do i = 1,trj%n_frames()
      write(fio%devn(),'(i8,4f16.9)')i,autocorr(i,1:4)
    enddo
  end subroutine Autocorr_3d
!
  pure function Autocorr_1d(t,natm,ntrj) result(res)
  double precision,intent(in)   :: t(natm,ntrj)
  integer,intent(in)            :: natm,ntrj
  double precision              :: res(ntrj),Norm
  integer                       :: i,j,k
    res = 0.d0
    do j = 1,ntrj
      do i = j,ntrj
        k = i - j + 1
        res(k) = res(k) + sum(t(:,i)*t(:,j))
      enddo
    enddo
    Norm = dble(ntrj) / res(1)
    do i = 1,ntrj
      res(i) = Norm * res(i) / dble(ntrj - i + 1)
    enddo
  end function Autocorr_1d
end program Autocorr
