program mcut
use,intrinsic :: ISO_FORTRAN_ENV, only : STDOUT => OUTPUT_UNIT
implicit none
  call compute_mcut()
contains
  subroutine compute_mcut()
  use spur_itertools
  use moltypes
  use moltypes_export
  use moltypes_process
  use spur_optparse
  type(moltype)                    :: mt
  type(optparse)                   :: op
  logical                          :: ow
  integer                          :: lb,ub,inc,num
  integer                          :: i,j,progres
    call op%add_option("-l",narg=1,metavar='int',def=['1'], help='mask string.')
    call op%add_option("-u",narg=1,metavar='int',def=['-1'], help='mask string.')
    call op%add_option("-i",narg=1,metavar='int',def=['1'], help='mask string.')
    call op%add_option("-m",narg=1,metavar='string',def=['all'], help='mask string.')
    call op%add_option("-o",narg=1,metavar='string', help='output path.')
    call op%add_option("-O", help='over writing.')
    call op%add_option("--center",narg=1,metavar='string', help='centering atoms.')
    call op%add_option("--dry-run",metavar='string',def=['all'], help='dry run.')
    call op%parser()
    if(op%narg()==0) call op%call_usage()
!
    lb  = op%optargi('-l',1)
    ub  = op%optargi('-u',1)
    inc = op%optargi('-i',1)
    ow  = op%option("-O")
!
    mt%terminates_at_abnormal = .TRUE.
    call mt%fetch(op%args())
!
    call IterScope(mt%nframes(),lb,ub,inc,num)
    if(op%option("-m")) call mt%atomselect(op%optargs('-m',1))
!
    if(op%option("--dry-run"))then
      write(STDOUT,'(a,i0,a)') 'Processing trajectry ',mt%nframes(),' frames'
      write(STDOUT,'(i0,a,i0,a)') mt%natoms(),' atoms, output trajectry for ',num,' frames'
      RETURN
    endif
!
    call PrintProgresBar(num)
    j = 0 ; progres = 0
    do i=lb,ub,inc
      call mt%load(lb=i,ub=i)
      if(op%option("--center")) call centering_coordinates(mt,op%optargs('--center',1))
      call ExportAs(mt,op%optargs('-o',1),overwrite=ow)
      ow = .FALSE.
      j = j + 1
      call PrintCounter(num,j,progres)
    enddo
    call PrintCounter(num,num,progres)
  end subroutine compute_mcut
!
  subroutine PrintProgresBar(ntrj)
  integer,intent(in)    :: ntrj
    write(STDOUT,'(a,i0,a)') 'Processing trajectry for ',ntrj,' frames'
    write(STDOUT,'(a)') '|----------------------------------------|'
    write(STDOUT,'(a)',advance='no') '|' ; flush(STDOUT)
  end subroutine PrintProgresBar
!
  subroutine PrintCounter(ntrj,counter,progres)
  integer,intent(in)    :: ntrj,counter
  integer,intent(out)   :: progres
  integer               :: i,tmp
    tmp = 40*counter/ntrj
    do i=progres+1,tmp
      write(STDOUT,'(a)',advance='no') '>'
    enddo
    if(progres<tmp.and.tmp==40)write(STDOUT,'(a)') '|'
    progres = maxval([tmp,progres],1) ; flush(STDOUT)
  end subroutine PrintCounter
end program mcut
