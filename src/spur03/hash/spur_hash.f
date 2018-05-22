module spur_hash
use ISO_C_BINDING
implicit none
private
public   :: fnv32
  interface
    function get_hash_fnv_32(str,bound) bind(c)
      import c_int,c_char
      character(*,c_char)   :: str
      integer(c_int) :: bound
    end function get_hash_fnv_32
  end interface

contains
  function fnv32(str,bound) result(res)
  character(*),intent(in)         :: str
  integer,intent(in)              :: bound
  character(:,c_char),allocatable :: str_c
  integer(c_int)                  :: bound_c,res_c,tmp
  integer                         :: res
  bound_c = bound
!  str_c = str//c_null_char
    tmp = ichar(str(1:1))
    res_c = get_hash_fnv_32(c_str('aaa'),bound)
    res   = res_c
  end function fnv32
!
  function c_str(str)
  character(*,c_char),intent(in)  :: str
  character(:,c_char),allocatable :: c_str
    c_str = str//c_null_char
  end function c_str
end module spur_hash
