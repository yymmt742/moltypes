program spur03_test
implicit none
! call test_string()
! call test_sort()
  call test_vector_chr()
!  call test_vector()
! call test_hash()
contains
  subroutine test_string()
  use spur_string
    print*, 'neaten test :: ',neaten('  a bB '),neaten('  a bB ','l'),neaten('  a bB ','u')
    print*, 'digit test :: ',digit(12345),digit(-54321),digit(''),digit('  abc'),digit('  ab c    ')
    print*, 'tostr test :: ',tostr(12345),tostr(-12345)
    print*, 'tonum test :: ',tonum('12345',0),tonum('   012345',0),tonum('12,3,45',0.0),tonum('-12345',0.d0)
    print*, 'tonum test :: ',tonum('aaa',0.0),tonum('-12345E+1',0.d0)
    print*, 'padding test :: ',padding(-1,4)
    print*, 'padding test :: ',padding(1,4,'0')
    print*, 'padding test :: ',padding(1,4,'ab')
    print*, 'padding test :: ',padding(1,8,'ab')
    print*, 'padding test :: ',padding(-1,0)
    print*, 'padding test :: ',padding('aa',8,'*')
  end subroutine test_string
!
  subroutine test_sort()
  use spur_sort
  character(1)     :: a(5)
  integer          :: b(5)
  real             :: c(5)
  double precision :: d(5)
    a=['u','a','r','1','d']
    print*,a
    call sort(a)
    print*,a
    call sort(a,.true.)
    print*,a
    b=[21,332,4,553,-3]
    print*,b
    call sort(b)
    print*,b
    call sort(b,.true.)
    print*,b
    c=[2.1,332.0,.4,55.3,-3.0]
    print*,c
    call sort(c)
    print*,c
    call sort(c,.true.)
    print*,c
    d=[2.d1,332.d0,.4d0,55.d3,-3.d0]
    print*,d
    call sort(d)
    print*,d
    call sort(d,.true.)
    print*,d
  end subroutine test_sort
!
  subroutine test_array()
  use spur_array_int4
  type(array_int4) :: s
  integer          :: t(-5:5)
  integer          :: i
    write(*,*) s%lookup()
    write(*,*) s%max_size()
    write(*,*) s%head,s%tail,s%size(),s%capacity(),s%empty()
    call s%reserve(-5,5)
    write(*,*) s%head,s%tail,s%size(),s%capacity(),s%empty()
    do i=s%head,s%tail
      s%at(i) = i
    enddo
    write(*,*) s%lookup()
    call s%clear()
    t = [1,2,3,4,5,6,7,8,9,10,11]
    call s%push(t)
    write(*,*) s%lookup()
    call s%push([1,2,3])
    write(*,*) s%lookup()
    call s%push([.TRUE.,.FALSE.,.TRUE.])
    write(*,*) s%lookup()
    call s%push(["30","31","32","**","-3"])
    write(*,*) s%lookup()
    do i=1,10
      write(*,*) s%pop()
    enddo
    write(*,*) s%head,s%tail,s%size(),s%capacity(),s%empty()
    call s%shrinktofit()
    write(*,*) s%head,s%tail,s%size(),s%capacity(),s%empty()
  end subroutine test_array
!
  subroutine test_vector()
  use spur_vector_int4
  type(vector_int4) :: s
  integer          :: i
    write(*,*) s%lookup()
    write(*,*) s%max_size()
    write(*,*) s%size(),s%capacity(),s%empty()
    call s%reserve(5)
    write(*,*) s%size(),s%capacity(),s%empty()
    do i=1,s%size()
      s%at(i) = i
    enddo
    write(*,*) s%lookup()
    call s%clear()
    write(*,*) s%size(),s%capacity(),s%empty()
    call s%push([1,2,3,4,5,6,7,8,9,10,11])
    write(*,*) s%lookup()
    call s%push([1,2,3])
    write(*,*) s%lookup()
    call s%push([.TRUE.,.FALSE.,.TRUE.])
    write(*,*) s%lookup()
    call s%push(["30","31","32","**","-3"])
    write(*,*) s%lookup()
    do i=1,10
      write(*,*) s%pop()
    enddo
    write(*,*) s%size(),s%capacity(),s%empty()
    call s%shrinktofit()
    write(*,*) s%size(),s%capacity(),s%empty()
    call s%clear()
    write(*,*) s%size(),s%capacity(),s%empty()
    call s%push([1,2,3,4,5,6,7,8,9,10,11])
  end subroutine test_vector
!
  subroutine test_vector_chr()
  use spur_vector_chr
  type(vector_chr) :: s
  integer          :: i
!   write(*,*) s%max_size()
!   write(*,*) s%size(),s%capacity(),s%empty()
!   write(*,*) s%total(),s%memory(),s%maxlen()
!   write(*,*) s%lookup()
!   call s%push('aaa')
!   call s%push('b')
!   call s%push('')
!   call s%push('  eee')
!   call s%push(['aaa','bbb','vvv'])
!   write(*,*) s%total(),s%memory(),s%maxlen()
!   write(*,*) s%join(',')
!   do i=1,3
!     print*,'pop :: ',s%pop()
!   enddo
!   call s%push(['123','456','789'])
!   write(*,*) s%lookup()
!   write(*,*) s%join(',')
!   call s%sort(.true.)
!   write(*,*) 'lookup',s%lookup()
!   write(*,*) 'join',s%join(',')
!   call s%sort()
!   call s%push(['123','456','789'])
!   call s%push(['123','456','789'])
!   call s%push(['123','456','789'])
!   call s%split('iroha niho heto,chilinuruo',' ,')
!   call s%uniq()
!   write(*,*) 'call uniq'
!   write(*,*) 'lookup',s%lookup()
!   write(*,*) 'join',s%join(',')
!   write(*,*) s%size(),s%total(),s%memory(),s%maxlen()
!   call s%shrinktofit()
!   write(*,*) 'call stf'
!   write(*,*) s%size(),s%total(),s%memory(),s%maxlen()
!   write(*,*) 'lookup',s%lookup()
!   write(*,*) 'join',s%join(',')
!   call s%clear()
!   call s%split('iroha niho heto,chi',' ,')
!   write(*,*) 'lookup',s%lookup()
!   call s%split('heto=cho','=')
!   write(*,*) 'lookup',s%lookup()
!   write(*,*) s%size(),s%capacity(),s%total(),s%memory(),s%maxlen()
!   call s%clear()
!   write(*,*) s%size(),s%capacity(),s%total(),s%memory(),s%maxlen()
!   call s%split('iroha niho heto,chi',' ,')
!   write(*,*) 'lookup',s%lookup()
!   call s%split('heto=cho','=')
!   write(*,*) 'lookup',s%lookup()
!   call s%clear()
    call s%textwrap('aaa bbb cc d eeeee ff gg hhh i jjj k',5)
    do i=1,s%size()
    print*,s%at(i)
    enddo
  end subroutine test_vector_chr
!
  subroutine test_hash()
  use spur_hashtable
  type(hashtable) :: s
    write(*,*) 'test :: hashtable'
    write(*,*) s%max_size()
    write(*,*) s%size(),s%capacity(),s%empty()
    write(*,*) s%total(),s%memory(),s%maxlen()
    call s%push('1','one')
    write(*,*) s%size(),s%capacity(),s%empty(),s%total(),s%memory(),s%maxlen()
    call s%push('2','two')
    write(*,*) s%size(),s%capacity(),s%empty(),s%total(),s%memory(),s%maxlen()
    call s%push('03','three')
    write(*,*) s%size(),s%capacity(),s%empty(),s%total(),s%memory(),s%maxlen()
    call s%push(' 4','four')
    write(*,*) s%size(),s%capacity(),s%empty(),s%total(),s%memory(),s%maxlen()
    call s%push('55555','five')
    write(*,*) s%size(),s%capacity(),s%empty(),s%total(),s%memory(),s%maxlen()
    print*,s%at('1')
    print*,s%at('2')
    print*,s%at('03')
    print*,s%at(' 4')
    print*,s%at('55555')
    write(*,*) s%total(),s%memory(),s%maxlen()
    print*,s%key_lookup()
    print*,s%lookup()
  end subroutine test_hash
end program spur03_test
