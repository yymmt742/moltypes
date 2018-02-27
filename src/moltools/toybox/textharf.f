program textharf
use spur_optparse
use spur_io
implicit none
type(optparse) :: arg
type(fileio)   :: f,s
character(256) :: line
integer        :: i,j,is
  call arg%add_option('-n',alias=["--second"],narg=1,metavar='value',def=['1'],&
                     &help='threshold default 0.0')
  call arg%parser()
  do i=1,arg%narg()
    call f%fetch(arg%args(i))
    j = 0
    do
      j = j + 1
      read(f%devn(),'(a)',iostat=is)line
      if(is<0)EXIT ; if(is<0.or.mod(j,arg%optargi('-n',1))/=0) CYCLE
      write(s%devn(),'(a)') trim(line)
    enddo
  enddo
end program textharf
