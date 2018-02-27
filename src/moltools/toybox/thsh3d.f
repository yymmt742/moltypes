program thsh3d
use spur_optparse
use spur_io
implicit none
type(optparse) :: arg
type(fileio)   :: f,s
real           :: x(4)
integer        :: i,is
  call arg%add_option('-t',alias=["--second"],narg=1,metavar='value',def=['0.0'],&
                     &help='threshold default 0.0')
  call arg%parser()
  do i=1,arg%narg()
    call f%fetch(arg%args(i))
    do
      read(f%devn(),*,iostat=is)x
      if(is<0)EXIT ; if(is<0.or.x(4)<arg%optargd('-t',1)) CYCLE
      write(s%devn(),'(4f9.3)') x
    enddo
  enddo
end program thsh3d
