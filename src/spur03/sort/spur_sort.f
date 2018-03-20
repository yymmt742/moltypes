module spur_sort
  implicit none
  public sort
  interface sort
    module procedure  sort_int4, sort_real, sort_dble, sort_chr
  end interface sort
contains
  pure subroutine sort_chr(a,reverse)
  character(*),intent(inout)  :: a(:)
  logical,intent(in),optional :: reverse
    if(present(reverse))then
      if(reverse)then
        call qs_chr_down(a,1,size(a),len(a))
        return
      endif
    endif
    call qs_chr_up(a,1,size(a),len(a))
  end subroutine sort_chr
!
  pure recursive subroutine qs_chr_up(a,first,last,n)
  integer,intent(in)         :: first,last,n
  character(n),intent(inout) :: a(:)
  character(n)               :: x,t
  integer                    :: i,j
    x=a(int((first+last)/2))
    i=first
    j=last
    do
      do while (a(i)<x)
        i=i+1
      enddo
      do while (a(j)>x)
        j=j-1
      enddo
      if(i>=j)exit
      t=a(i) ; a(i)=a(j) ; a(j)=t
      i=i+1
      j=j-1
    enddo
    if(first<i-1) call qs_chr_up(a,first,i-1,n)
    if(j+1<last)  call qs_chr_up(a,j+1,last,n)
  end subroutine qs_chr_up
!
  pure recursive subroutine qs_chr_down(a,first,last,n)
  integer,intent(in)         :: first,last,n
  character(n),intent(inout) :: a(:)
  character(n)               :: x,t
  integer                    :: i,j
    x=a(int((first+last)/2))
    i=first
    j=last
    do
      do while (a(i)>x)
        i=i+1
      enddo
      do while (a(j)<x)
        j=j-1
      enddo
      if(i>=j)exit
      t=a(i) ; a(i)=a(j) ; a(j)=t
      i=i+1
      j=j-1
    enddo
    if(first<i-1) call qs_chr_down(a,first,i-1,n)
    if(j+1<last)  call qs_chr_down(a,j+1,last,n)
  end subroutine qs_chr_down
!
  pure subroutine sort_int4(a,reverse)
  integer(4),intent(inout)    :: a(:)
  logical,intent(in),optional :: reverse
    if(present(reverse))then
      if(reverse)then
        call qs_int4_down(a,1,size(a))
        return
      endif
    endif
    call qs_int4_up(a,1,size(a))
  end subroutine sort_int4
!
  pure recursive subroutine qs_int4_up(a,first,last)
  integer,intent(in)       :: first,last
  integer(4),intent(inout) :: a(:)
  integer(4)               :: x,t
  integer                  :: i,j
    x=a(int((first+last)/2))
    i=first
    j=last
    do
      do while (a(i)<x)
        i=i+1
      enddo
      do while (a(j)>x)
        j=j-1
      enddo
      if(i>=j)exit
      t=a(i) ; a(i)=a(j) ; a(j)=t
      i=i+1
      j=j-1
    enddo
    if(first<i-1) call qs_int4_up(a,first,i-1)
    if(j+1<last)  call qs_int4_up(a,j+1,last)
  end subroutine qs_int4_up
!
  pure recursive subroutine qs_int4_down(a,first,last)
  integer,intent(in)       :: first,last
  integer(4),intent(inout) :: a(:)
  integer(4)               :: x,t
  integer                  :: i,j
    x=a(int((first+last)/2))
    i=first
    j=last
    do
      do while (a(i)>x)
        i=i+1
      enddo
      do while (a(j)<x)
        j=j-1
      enddo
      if(i>=j)exit
      t=a(i) ; a(i)=a(j) ; a(j)=t
      i=i+1
      j=j-1
    enddo
    if(first<i-1) call qs_int4_down(a,first,i-1)
    if(j+1<last)  call qs_int4_down(a,j+1,last)
  end subroutine qs_int4_down
!
  pure subroutine sort_real(a,reverse)
  real,intent(inout)          :: a(:)
  logical,intent(in),optional :: reverse
    if(present(reverse))then
      if(reverse)then
        call qs_real_down(a,1,size(a))
        return
      endif
    endif
    call qs_real_up(a,1,size(a))
  end subroutine sort_real
!
  pure recursive subroutine qs_real_up(a,first,last)
  integer,intent(in)       :: first,last
  real,intent(inout)       :: a(:)
  real                     :: x,t
  integer                  :: i,j
    x=a(int((first+last)/2))
    i=first
    j=last
    do
      do while (a(i)<x)
        i=i+1
      enddo
      do while (a(j)>x)
        j=j-1
      enddo
      if(i>=j)exit
      t=a(i) ; a(i)=a(j) ; a(j)=t
      i=i+1
      j=j-1
    enddo
    if(first<i-1) call qs_real_up(a,first,i-1)
    if(j+1<last)  call qs_real_up(a,j+1,last)
  end subroutine qs_real_up
!
  pure recursive subroutine qs_real_down(a,first,last)
  integer,intent(in)       :: first,last
  real,intent(inout)       :: a(:)
  real                     :: x,t
  integer                  :: i,j
    x=a(int((first+last)/2))
    i=first
    j=last
    do
      do while (a(i)>x)
        i=i+1
      enddo
      do while (a(j)<x)
        j=j-1
      enddo
      if(i>=j)exit
      t=a(i) ; a(i)=a(j) ; a(j)=t
      i=i+1
      j=j-1
    enddo
    if(first<i-1) call qs_real_down(a,first,i-1)
    if(j+1<last)  call qs_real_down(a,j+1,last)
  end subroutine qs_real_down
!
  pure subroutine sort_dble(a,reverse)
  double precision,intent(inout) :: a(:)
  logical,intent(in),optional    :: reverse
    if(present(reverse))then
      if(reverse)then
        call qs_dble_down(a,1,size(a))
        return
      endif
    endif
    call qs_dble_up(a,1,size(a))
  end subroutine sort_dble
!
  pure recursive subroutine qs_dble_up(a,first,last)
  integer,intent(in)             :: first,last
  double precision,intent(inout) :: a(:)
  double precision               :: x,t
  integer                        :: i,j
    x=a(int((first+last)/2))
    i=first
    j=last
    do
      do while (a(i)<x)
        i=i+1
      enddo
      do while (a(j)>x)
        j=j-1
      enddo
      if(i>=j)exit
      t=a(i) ; a(i)=a(j) ; a(j)=t
      i=i+1
      j=j-1
    enddo
    if(first<i-1) call qs_dble_up(a,first,i-1)
    if(j+1<last)  call qs_dble_up(a,j+1,last)
  end subroutine qs_dble_up
!
  pure recursive subroutine qs_dble_down(a,first,last)
  integer,intent(in)             :: first,last
  double precision,intent(inout) :: a(:)
  double precision               :: x,t
  integer                        :: i,j
    x=a(int((first+last)/2))
    i=first
    j=last
    do
      do while (a(i)>x)
        i=i+1
      enddo
      do while (a(j)<x)
        j=j-1
      enddo
      if(i>=j)exit
      t=a(i) ; a(i)=a(j) ; a(j)=t
      i=i+1
      j=j-1
    enddo
    if(first<i-1) call qs_dble_down(a,first,i-1)
    if(j+1<last)  call qs_dble_down(a,j+1,last)
  end subroutine qs_dble_down
end module spur_sort
