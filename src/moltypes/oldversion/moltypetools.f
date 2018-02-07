  pure subroutine TrjCenterCoordinates(this)
  class(trajectory),intent(inout)  :: this
  integer                          :: i,j,iframe
  double precision                 :: COM(3),REVN
    if(count(this%selection)<=0.or.this%NTRJ<=0)RETURN
!
    REVN = 1.d0 / dble(count(this%selection))
!
    do j=1,this%NTRJ
      COM = COMCALC(this%NATM,this%coordinates(j))
      do i=1,size(this%selection)
        this%xyz(:,i,j) = this%xyz(:,i,j) - COM
      enddo
    enddo
    RETURN
  contains
    pure function COMCALC(natm,xyz) result (res)
    integer,intent(in)          :: natm
    double precision,intent(in) :: xyz(3,natm)
    double precision            :: res(3)
    integer                     :: i
      res = 0.d0
      do i=1,natm
        res = res + xyz(:,i)
      enddo
      res = res * REVN
    end function COMCALC
  end subroutine TrjCenterCoordinates
!
  pure subroutine TrjUnwrap(this,amount)
  class(trajectory),intent(inout)      :: this
  double precision,intent(in),optional :: amount
    if(this%NATM<=0.or.this%NTRJ<=0)RETURN
    if(present(amount))then
      call Unwrap(this%xyz,this%box,size(this%selection),this%NTRJ,maxval([amount,0.2d0],1))
    else
      call Unwrap(this%xyz,this%box,size(this%selection),this%NTRJ,0.50d0)
    endif
  contains
    pure subroutine Unwrap(xyz,box,NATM,NTRJ,AMOUNT)
    double precision,intent(inout)       :: xyz(3,NATM,NTRJ)
    double precision,intent(in)          :: box(3,NTRJ)
    integer,intent(in)                   :: NATM,NTRJ
    double precision,intent(in)          :: AMOUNT
    double precision                     :: test(3)
    double precision                     :: offset(3,NATM),prev(3,NATM),width(3)
    integer                              :: i,j,k
      offset = 0.d0
      do k=2,NTRJ
        prev(:,:) = xyz(:,:,k-1)
        width = box(:,k) * AMOUNT
        do j=1,NATM
          xyz(:,j,k) = xyz(:,j,k) + offset(:,j)*box(:,k)
          test(:) = xyz(:,j,k) - prev(:,j)
          do i=1,3
            if(test(i)>width(i))then
              offset(i,j) = offset(i,j) - 1.d0
              xyz(i,j,k)  = xyz(i,j,k) - box(i,k)
            elseif(test(i)<-width(i))then
              offset(i,j) = offset(i,j) + 1.d0
              xyz(i,j,k)  = xyz(i,j,k) + box(i,k)
            endif
          enddo
        enddo
      enddo
    end subroutine Unwrap
  end subroutine TrjUnwrap
!
  pure function TrjCenterResidue(this,ntrj) result(res)
  class(Trajectory),intent(in)  :: this
  integer,intent(in)            :: ntrj
  double precision,allocatable  :: res(:,:)
    if(this%NATM<=0.or.this%NTRJ<ntrj.or.ntrj<=0)then
      allocate(res(3,1)) ; res = 0.d0 ; RETURN
    endif
    call ComCalc(count(this%top%mask),this%top%nres,TrjCoordinates(this,ntrj),this%top%residue%lookup(),res)
  contains
    pure subroutine ComCalc(NATM,NRES,CRD,RESID,res)
    integer,intent(in)                       :: NATM,NRES
    double precision,intent(in)              :: CRD(3,NATM)
    integer,intent(in)                       :: RESID(NATM)
    double precision,intent(out),allocatable :: res(:,:)
    integer                                  :: NUM(NRES)
    double precision                         :: COM(3,NRES)
    integer                                  :: i,j
      COM = 0.d0 ; NUM = 0
      do i=1,NATM
        NUM(RESID(i)) = NUM(RESID(i)) + 1
        COM(:,RESID(i)) = COM(:,RESID(i)) + CRD(:,i)
      enddo
      allocate(res(3,count(NUM>0)))
      j = 0
      do i=1,NRES
        if(NUM(i)==0)CYCLE
        j = j + 1
        RES(:,j) = COM(:,i) / dble(NUM(i))
      enddo
    end subroutine ComCalc
  end function TrjCenterResidue
