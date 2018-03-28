program mcut
use,intrinsic :: ISO_FORTRAN_ENV, only : STDOUT => OUTPUT_UNIT
implicit none
  call compute_mcut()
contains
  subroutine compute_mcut()
  use spur_itertools
  use moltypes_perser
  use spur_optparse
  type(molperser)                  :: mt
  type(optparse)                   :: op
  logical                          :: ow
  integer                          :: lb,ub,inc,num
  integer                          :: i,j,progres
    call op%add_option("-l",narg=1,metavar='int',def=['1'],  help='lower bound of frame index')
    call op%add_option("-u",narg=1,metavar='int',def=['-1'], help='upper bound of frame index')
    call op%add_option("-i",narg=1,metavar='int',def=['1'],  help='increment of frame index')
    call op%add_option("-m",narg=1,metavar='mask string',def=['all'], help='mask string.')
    call op%add_option("-o",narg=1,metavar='path name', help='output path.')
    call op%add_option("-O", help='over writing.')
    call op%add_option("--center",narg=1,metavar='mask string', help='centering atoms.')
    call op%add_option("--wrap",metavar='mask string', help='unwrap coordinates.')
    call op%add_option("--unwrap",metavar='mask string', help='unwrap coordinates.')
    call op%add_option("--dry-run",metavar='string',def=['all'], help='dry run mode.')
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
    if(mt%nfetchframes()<1) RETURN
!
    if(op%option("-m")) call mt%atomselect(op%optargs('-m',1))
!
    call IterScope(mt%nfetchframes(),lb,ub,inc,num)
    if(op%option("--dry-run").or.op%option('-o')) call PrintInfomation(mt,op,lb,ub,inc,num)
!
    if(op%option("--dry-run")) RETURN
!
    if(op%option('-o')) call PrintProgresBar(num)
    j = 0 ; progres = 0
    do i=lb,ub,inc
      call mt%load(lb=i,ub=i)
      if(op%option("--center")) call mt%centering_coordinates(op%optargs('--center',1))
      if(op%option("--unwrap")) call mt%unwrap_coordinates()
      call mt%export(op%optargs('-o',1),overwrite=ow)
      ow = .FALSE.
      j = j + 1
      if(op%option('-o')) call PrintCounter(num,j,progres)
    enddo
    if(op%option('-o')) call PrintCounter(num,num,progres)
  end subroutine compute_mcut
!
  subroutine PrintInfomation(mt,op,lb,ub,inc,num)
  use spur_string
  use moltypes_perser
  use spur_optparse
  use spur_vector_chr
  type(molperser),intent(in)  :: mt
  type(optparse),intent(in)   :: op
  integer,intent(in)          :: lb,ub,inc,num
  type(vector_chr)            :: args
  integer                     :: i
    write(STDOUT,'(a,i0,a,i0,a)') 'Fetched trajectry ',mt%nfetchframes(),' frames / ',mt%nfetchatoms(),' atoms'
    call args%textwrap(join(op%args(),' '),78)
    !call args%textwrap(join(op%args(),' '),78,delimiter=' ')
    write(STDOUT,'(a)') 'Here is Input Files'
    do i = 1,args%size()
      write(STDOUT,'(a)') "| "//args%at(i)
    enddo
    write(STDOUT,'(3(a,i0))') 'Lower bound ',lb,' / Upper bound ',ub,' / Increment ',inc
    write(STDOUT,'(a,i0,a)') 'output trajectry for ',num,' frames'
  end subroutine PrintInfomation
!
  subroutine PrintProgresBar(ntrj)
  integer,intent(in)    :: ntrj
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
