module spur_env
  implicit none
  private
  public  :: GETENV
contains
  function GETENV(VAR) result(res)
  character(*),intent(in)  :: VAR
  character(:),allocatable :: res
  integer                  :: le,st
  intrinsic get_environment_variable
    call get_environment_variable(VAR,status=st,length=le)
    if(st==0)then
      allocate(character(le)::res)
      call get_environment_variable(VAR,value=res)
    else
      allocate(character(0)::res)
    endif
  end function GETENV
end module spur_env
