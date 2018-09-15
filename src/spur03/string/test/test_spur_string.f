program main
use spur_string
implicit none
character(20),parameter :: string = '      AAA   bb  Cc  '
character(20),parameter :: upper  = '      AAA   BB  CC  '
character(20),parameter :: lower  = '      aaa   bb  cc  '
character(12),parameter :: adtrim = 'AAA   bb  Cc'
integer,parameter       :: length = 20, trimed = 12
logical       :: sccess
integer       :: stat
!
  stat   = 101 ; sccess = test_neaten() ; call abrt('function neaten')
  stat   = 102 ; sccess = test_digit()  ; call abrt('function digit ')
!
contains
  logical function test_neaten result(res)
    res = len(neaten(string))==trimed
    res = res.and.neaten(string)==adtrim
    res = res.and.neaten(string,'upper')==neaten(upper)
    res = res.and.neaten(string,'lower')==neaten(lower)
!
    res = res.and.len(large(string))==length
    res = res.and.large(string)==upper
!
    res = res.and.len(small(string))==length
    res = res.and.small(string)==lower
  end function test_neaten
!
  logical function test_digit result(res)
! check for digit_byte, digit_int2, digit_int4, digit_int8, digit_chr
  integer(1) :: i1
  integer(2) :: i2
  integer(4) :: i4
  integer(8) :: i8
    i1  = 20
    i2  = 1030
    i4  = 209322229
    i8  = 203333333883014
    res = digit(string)==trimed
    res = res.and.digit(i1)==2
    res = res.and.digit(-i1)==3
    res = res.and.digit(i2)==4
    res = res.and.digit(-i2)==5
    res = res.and.digit(i4)==9
    res = res.and.digit(-i4)==10
    res = res.and.digit(i8)==15
    res = res.and.digit(-i8)==16
  end function test_digit
!
  subroutine abrt(message)
  use,intrinsic :: ISO_FORTRAN_ENV, only : STDERR => ERROR_UNIT
  character(*),intent(in) :: message
    if(sccess) RETURN
    write(STDERR,'(a)') ' ERROR [test_spur_string] '//message
    call exit(stat)
  end subroutine abrt
end program main
