program addh
use moltypes
use spur_optparse
use spur_vector
use spur_io
implicit none
double precision,parameter :: HBOND = 1.09d0
double precision,parameter :: HHARF = HBOND * 0.5d0
double precision,parameter :: HSQ3H = HBOND * 0.86602540378d0
double precision,parameter :: HCSP3 = HBOND * 0.57714519003d0
double precision,parameter :: HSSP3 = HBOND * 0.81664155516d0
double precision,parameter :: HNSP3 = HBOND * 0.33380685923d0
double precision,parameter :: HPSP3 = HBOND * 0.94264149109d0
double precision,parameter :: HVSP3 = HBOND * 0.81635147794d0
type(optparse)             :: arg
type(fileio)               :: m,f,s,o
type(trajectory)           :: trj
type(vector_character)     :: aname,command
type(vector_double)        :: xyz,hydrogen
character(:),allocatable   :: output
integer                    :: nhyd
integer                    :: i,j,tmp,is
  call arg%add_option('-f',narg=1,metavar='file')
  call arg%add_option('-o',narg=1,metavar='file',def=['addh.xyz'])
  call arg%parser()
  if(arg%narg()==0) call arg%call_usage()
  nhyd = 0
!
  allocate(character(0) :: output)
  output = arg%optargs('-o',1)
!
  do i=1,arg%narg()
    call f%fetch(arg%optargs('-f',1))
    call trj%load(arg%args(i))
    aname = trj%atomnames()
    do
      write(s%devn(),'(a)',advance='no') '* >> '
      call command%clear() ; call command%split(f%gets())
      if(f%isEOF())EXIT
!
      call s%puts('>>'//command%join())
      select case(command%at(1))
      case('sp3','sp2','sp1')
        call add_hydrogen()
      case('o','output')
      case('q','quit')
        EXIT
      end select
    enddo
    call f%quit()
  enddo
!
  call o%generate(output)
  do j=1,trj%n_frames()
    write(o%devn(),'(i0,/)'),aname%size()+nhyd
    do i = 1,aname%size()
      write(o%devn(),'(a4,3f9.3)'),aname%at(i),trj%xyz(:,i,j)
      write(s%devn(),'(a4,3f9.3)'),aname%at(i),trj%xyz(:,i,j)
    enddo
    do i = 1,nhyd
      tmp = 3 * (nhyd * (j-1) + i)
      write(o%devn(),'(a,i3.3,3f9.3)'),'H',tmp/3,hydrogen%at(tmp-2),hydrogen%at(tmp-1),hydrogen%at(tmp)
      write(s%devn(),'(a,i3.3,3f9.3)'),'H',tmp/3,hydrogen%at(tmp-2),hydrogen%at(tmp-1),hydrogen%at(tmp)
    enddo
  enddo
  call o%quit()
contains
  subroutine add_hydrogen()
  type(vector_integer)     :: idx
  integer                  :: i,j,tmp(command%size()-1)
  double precision         :: v(3),h(3)
    tmp = 0
    do
      call idx%clear()
      do i=2,command%size()
        tmp(i-1) = aname%find(command%at(i),lb=tmp(i-1)+1)
        call idx%push(tmp(i-1))
      enddo
      if(any(tmp<=0))EXIT
      select case(command%at(1))
      case('sp3')
        select case(command%size())
        case(3)
          call add_sp3_3hydrogen(idx%at(1),idx%at(2))
        case(4)
          call add_sp3_2hydrogen(idx%at(1),idx%at(2),idx%at(3))
        case(5:)
          call add_sp3_1hydrogen(idx%at(1),idx%at(2),idx%at(3),idx%at(4))
        end select
      case('sp2')
        select case(command%size())
        case(3)
          call add_sp2_2hydrogen(idx%at(1),idx%at(2))
        case(4)
          call add_sp2_1hydrogen(idx%at(1),idx%at(2),idx%at(3))
        case(5:)
        end select
      case('sp1')
        select case(command%size())
        case(3)
          call add_sp1_1hydrogen(idx%at(1),idx%at(2))
        end select
      end select
    enddo
  end subroutine add_hydrogen
!
  subroutine add_sp1_1hydrogen(i1,i2)
  integer                  :: i1,i2
  integer                  :: i
  double precision         :: v(3),h(3,trj%n_frames()),nv
    do i=1,trj%n_frames()
      v      = trj%xyz(:,i1,i) - trj%xyz(:,i2,i)
      nv     = sqrt(dot_product(v,v)) ; if(nv<1.0e-5)RETURN
      h(:,i) = trj%xyz(:,i1,i) + HBOND * v / nv
    enddo
    call hydrogen%push(pack(h,h<HUGE(0.d0)))
    nhyd = nhyd + 1
  end subroutine add_sp1_1hydrogen
!
  subroutine add_sp2_1hydrogen(i1,i2,i3)
  integer                  :: i1,i2,i3
  integer                  :: i
  double precision         :: v(3),h(3,trj%n_frames()),nv
    do i=1,trj%n_frames()
      v      = trj%xyz(:,i1,i) - 0.5d0 * (trj%xyz(:,i2,i) + trj%xyz(:,i3,i))
      nv     = sqrt(dot_product(v,v)) ; if(nv<1.0e-5)RETURN
      h(:,i) = trj%xyz(:,i1,i) + HBOND * v / nv
    enddo
    call hydrogen%push(pack(h,h<HUGE(0.d0)))
    nhyd = nhyd + 1
  end subroutine add_sp2_1hydrogen
!
  subroutine add_sp2_2hydrogen(i1,i2)
  integer                  :: i1,i2
  integer                  :: i
  double precision         :: v(3),h(6,trj%n_frames())
  double precision         :: av(3),nv
    do i=1,trj%n_frames()
      v  = trj%xyz(:,i1,i) - trj%xyz(:,i2,i)
      nv = sqrt(dot_product(v,v)) ;  if(nv<1.0e-5)RETURN
      av = anyv(v) ; if(dot_product(av,av)<1.0e-5)RETURN
      v  = HHARF * v / nv
      av = HSQ3H * av
      h(1:3,i) = trj%xyz(:,i1,i) + v + av
      h(4:6,i) = trj%xyz(:,i1,i) + v - av
    enddo
    call hydrogen%push(pack(h,h<HUGE(0.d0)))
    nhyd = nhyd + 2
  end subroutine add_sp2_2hydrogen
!
  subroutine add_sp3_1hydrogen(i1,i2,i3,i4)
  integer                  :: i1,i2,i3,i4
  integer                  :: i
  double precision         :: v(3),h(3,trj%n_frames()),nv
    do i=1,trj%n_frames()
      v      = trj%xyz(:,i1,i) - (trj%xyz(:,i2,i) + trj%xyz(:,i3,i) + trj%xyz(:,i4,i)) / 3.d0
      nv     = sqrt(dot_product(v,v)) ; if(nv<1.0e-5)RETURN
      h(:,i) = trj%xyz(:,i1,i) + HBOND * v / nv
    enddo
    call hydrogen%push(pack(h,h<HUGE(0.d0)))
    nhyd = nhyd + 1
  end subroutine add_sp3_1hydrogen
!
  subroutine add_sp3_2hydrogen(i1,i2,i3)
  integer                  :: i1,i2,i3
  integer                  :: i
  double precision         :: v(3),h(6,trj%n_frames())
  double precision         :: av(3),nv
    do i=1,trj%n_frames()
      v  = trj%xyz(:,i1,i) - 0.5d0 * (trj%xyz(:,i2,i)+trj%xyz(:,i3,i))
      nv = sqrt(dot_product(v,v)) ; if(nv<1.0e-5)RETURN
      av = cross(trj%xyz(:,i2,i)-trj%xyz(:,i1,i),trj%xyz(:,i3,i)-trj%xyz(:,i1,i))
      if(dot_product(av,av)<1.0e-5)RETURN
      v  = HCSP3 * v / nv
      av = HSSP3 * av
      h(1:3,i) = trj%xyz(:,i1,i) + v + av
      h(4:6,i) = trj%xyz(:,i1,i) + v - av
    enddo
    call hydrogen%push(pack(h,h<HUGE(0.d0)))
    nhyd = nhyd + 2
  end subroutine add_sp3_2hydrogen
!
  subroutine add_sp3_3hydrogen(i1,i2)
  integer                  :: i1,i2
  integer                  :: i
  double precision         :: v(3),h(9,trj%n_frames())
  double precision         :: av(3),vv(3),nv
    do i=1,trj%n_frames()
      v  = trj%xyz(:,i1,i) - trj%xyz(:,i2,i)
      nv = sqrt(dot_product(v,v)) ; if(nv<1.0e-5)RETURN
      av = anyv(v) ; if(dot_product(av,av)<1.0e-5)RETURN
      vv = cross(v,av)
      v  = HNSP3 * v / nv
      av = HPSP3 * av
      vv = HVSP3 * vv
      h(1:3,i) = trj%xyz(:,i1,i) + v + av
      h(4:6,i) = trj%xyz(:,i1,i) + v - 0.5d0 * sqrt(3.d0) * av + vv
      h(7:9,i) = trj%xyz(:,i1,i) + v - 0.5d0 * sqrt(3.d0) * av - vv
    enddo
    call hydrogen%push(pack(h,h<HUGE(0.d0)))
    nhyd = nhyd + 3
  end subroutine add_sp3_3hydrogen
!
  pure function anyv(v) result(res)
  double precision,intent(in) :: v(3)
  double precision            :: res(3)
  double precision            :: n(3)
    n = v * v
    if(n(1)<n(2).and.n(1)<n(3))then
      res = [0.d0,v(3),-v(2)]/sqrt(n(3)+n(2))
    elseif(n(2)<n(1).and.n(2)<n(3))then
      res = [-v(3),0.d0,v(1)]/sqrt(n(3)+n(1))
    elseif(n(3)<n(1).and.n(3)<n(2))then
      res = [v(2),-v(1),0.d0]/sqrt(n(1)+n(2))
    else
      res = 0.d0
    endif
  end function anyv
!
  pure function cross(v1,v2) result(res)
  double precision,intent(in) :: v1(3),v2(3)
  double precision            :: nv,res(3)
    res(1) = v1(2)*v2(3)-v1(3)*v2(2)
    res(2) = v1(3)*v2(1)-v1(1)*v2(3)
    res(3) = v1(1)*v2(2)-v1(2)*v2(1)
    nv     = sqrt(dot_product(res,res))
    res    = res / nv
  end function cross
end program addh
