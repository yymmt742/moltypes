  pure subroutine MolAssign(this,mol)
  class(molinfo),intent(inout)         :: this
  class(molinfo),intent(in)            :: mol
    call MolinfoDestractor(this)
    if(allocated(mol%UnitName))then
      allocate(character(len(mol%UnitName))::this%UnitName) ; this%UnitName(:) = mol%UnitName
    endif
    if(allocated(mol%Title))then
      allocate(character(len(mol%Title))::this%Title)       ; this%Title(:)    = mol%Title
    endif
    if(allocated(mol%Caption))then
      allocate(character(len(mol%Caption))::this%Caption)   ; this%Caption(:)  = mol%Caption
    endif
    if(allocated(mol%mask))then
      allocate(this%mask(size(mol%mask)))                   ; this%mask(:)     = mol%mask(:)
    endif
!
    this%NATM    = mol%NATM    ; this%NRES    = mol%NRES
    this%AtmName = mol%AtmName ; this%ResName = mol%ResName ; this%AtmType = mol%AtmType
    this%UniqAtm = mol%UniqAtm ; this%UniqRes = mol%UniqRes ; this%Uniqtyp = mol%Uniqtyp
    this%chg     = mol%chg     ; this%mass    = mol%mass    ; this%vdw     = mol%vdw
    this%residue = mol%residue ; this%idx     = mol%idx     ; this%element = mol%element
    this%bond    = mol%bond    ; this%angle   = mol%angle   ; this%dihed   = mol%dihed
    RETURN
  end subroutine MolAssign
!
pure subroutine TrjAssignTrj(this,trj)
class(trajectory),intent(inout) :: this
class(trajectory),intent(in)    :: trj
  call TrjDestractor(this)
  if(trj%top%NATM<=0)RETURN
  this%top   = trj%top
  this%NATM  = trj%NATM
  this%NTRJ  = trj%NTRJ
  this%Stack = trj%Stack
!
  if(this%NATM<=0)RETURN
  allocate(this%selection(this%NATM))
  allocate(this%xyz(3,this%NATM,this%stack))
  allocate(this%box(3,this%stack))
  this%selection = trj%selection
  this%xyz       = trj%xyz
  this%Box       = trj%Box
  RETURN
end subroutine TrjAssignTRJ
!
pure subroutine TrjAssignRCRD(this,xyz)
class(trajectory),intent(inout)         :: this
real,intent(in)                         :: xyz(:,:)
integer                                 :: lb(2),ub(2)
  call TrjDestractor(this)
  lb = [lbound(xyz,1),lbound(xyz,2)]
  ub = [ubound(xyz,1),ubound(xyz,2)] + 1
  if(ub(1)-lb(1)/=3)RETURN
  this%top%NATM = ub(2)-lb(2)
  allocate(this%top%mask(this%top%NATM),this%selection(this%top%NATM))
  this%top%mask(:) = .TRUE. ; this%selection(:) = .TRUE.
  this%NATM = this%top%NATM
  this%NTRJ = 1
  call TrjExpand(this)
  this%xyz(1:3,1:this%NATM,1)  = xyz
  this%Box(:,this%NTRJ) = 0.d0
  RETURN
end subroutine TrjAssignRCRD
!
pure subroutine TrjAssignRTRJ(this,xyz)
class(trajectory),intent(inout)         :: this
real, intent(in)                        :: xyz(:,:,:)
integer                                 :: lb(3),ub(3)
  call TrjDestractor(this)
  lb = [lbound(xyz,1),lbound(xyz,2),lbound(xyz,3)]
  ub = [ubound(xyz,1),ubound(xyz,2),ubound(xyz,3)] + 1
  if(ub(1)-lb(1)/=3)RETURN
  this%top%NATM = ub(2)-lb(2)
  allocate(this%top%mask(this%top%NATM),this%selection(this%top%NATM))
  this%top%mask(:) = .TRUE. ; this%selection(:) = .TRUE.
  this%NATM = this%top%NATM
  this%NTRJ = ub(3)-lb(3)
  call TrjExpand(this)
  this%xyz(1:3,1:this%NATM,1:this%NTRJ)  = xyz
  this%Box(:,this%NTRJ) = 0.d0
  RETURN
end subroutine TrjAssignRTRJ
!
pure subroutine TrjAssignDCRD(this,xyz)
class(trajectory),intent(inout)         :: this
double precision, intent(in)            :: xyz(:,:)
integer                                 :: lb(2),ub(2)
  call TrjDestractor(this)
  lb = [lbound(xyz,1),lbound(xyz,2)]
  ub = [ubound(xyz,1),ubound(xyz,2)] + 1
  if(ub(1)-lb(1)/=3)RETURN
  this%top%NATM = ub(2)-lb(2)
  allocate(this%top%mask(this%top%NATM),this%selection(this%top%NATM))
  this%top%mask(:) = .TRUE. ; this%selection(:) = .TRUE.
  this%NATM = this%top%NATM
  this%NTRJ = 1
  call TrjExpand(this)
  this%xyz(1:3,1:this%NATM,1)  = xyz
  this%Box(:,this%NTRJ) = 0.d0
  RETURN
end subroutine TrjAssignDCRD
!
pure subroutine TrjAssignDTRJ(this,xyz)
class(trajectory),intent(inout)         :: this
double precision,intent(in)             :: xyz(:,:,:)
integer                                 :: lb(3),ub(3)
  call TrjDestractor(this)
  lb = [lbound(xyz,1),lbound(xyz,2),lbound(xyz,3)]
  ub = [ubound(xyz,1),ubound(xyz,2),ubound(xyz,3)] + 1
  if(ub(1)-lb(1)/=3)RETURN
  this%top%NATM = ub(2)-lb(2)
  allocate(this%top%mask(this%top%NATM),this%selection(this%top%NATM))
  this%top%mask(:) = .TRUE. ; this%selection(:) = .TRUE.
  this%NATM = this%top%NATM
  this%NTRJ = ub(3)-lb(3)
  call TrjExpand(this)
  this%xyz(1:3,1:this%NATM,1:this%NTRJ)  = xyz
  this%Box(:,this%NTRJ) = 0.d0
  RETURN
end subroutine TrjAssignDTRJ
!
pure function TrjAdd(t1,t2) result(res)
class(trajectory),intent(in)  :: t1,t2
double precision              :: res(3,LtOrOne(t1%NATM,t2%NATM),LtOrOne(t1%NTRJ,t2%NTRJ))
integer                       :: natm,ntrj
  natm = minval([t1%NATM,t2%NATM],1) ; ntrj = minval([t1%NTRJ,t2%NTRJ],1)
  if(minval([natm,ntrj],1)<1)then ; res = 0.d0 ; RETURN ; endif
  call ADD(3,NATM,NTRJ,t1%xyz(1:3,1:natm,1:ntrj),t2%xyz(1:3,1:natm,1:ntrj),res)
contains
  pure subroutine ADD(SPATIAL,NATM,NTRJ,xyz1,xyz2,res)
  integer,intent(in)          :: SPATIAL,NATM,NTRJ
  double precision,intent(in) :: xyz1(SPATIAL,NATM,NTRJ),xyz2(SPATIAL,NATM,NTRJ)
  double precision,intent(out):: res(SPATIAL,NATM,NTRJ)
    res = xyz1 + xyz2 ; RETURN
  end subroutine ADD
end function TrjAdd
!
pure function TrjSub(t1,t2) result(res)
class(trajectory),intent(in)  :: t1,t2
double precision              :: res(3,LtOrOne(t1%NATM,t2%NATM),LtOrOne(t1%NTRJ,t2%NTRJ))
integer                       :: natm,ntrj
  natm = minval([t1%NATM,t2%NATM],1) ; ntrj = minval([t1%NTRJ,t2%NTRJ],1)
  if(minval([natm,ntrj],1)<1)then ; res = 0.d0 ; RETURN ; endif
  call SUB(3,NATM,NTRJ,t1%xyz(:,1:natm,1:ntrj),t2%xyz(:,1:natm,1:ntrj),res)
contains
  pure subroutine SUB(SPATIAL,NATM,NTRJ,xyz1,xyz2,res)
  integer,intent(in)          :: SPATIAL,NATM,NTRJ
  double precision,intent(in) :: xyz1(SPATIAL,NATM,NTRJ),xyz2(SPATIAL,NATM,NTRJ)
  double precision,intent(out):: res(SPATIAL,NATM,NTRJ)
    res = xyz1 - xyz2 ; RETURN
  end subroutine SUB
end function TrjSub
!
pure function TrjMul(t1,t2) result(res)
class(trajectory),intent(in)  :: t1,t2
double precision              :: res(3,LtOrOne(t1%NATM,t2%NATM),LtOrOne(t1%NTRJ,t2%NTRJ))
integer                       :: natm,ntrj
  natm = minval([t1%NATM,t2%NATM],1) ; ntrj = minval([t1%NTRJ,t2%NTRJ],1)
  if(minval([natm,ntrj],1)<1)then ; res = 0.d0 ; RETURN ; endif
  call MUL(3,NATM,NTRJ,t1%xyz(1:3,1:natm,1:ntrj),t2%xyz(:,1:natm,1:ntrj),res)
contains
  pure subroutine MUL(SPATIAL,NATM,NTRJ,xyz1,xyz2,res)
  integer,intent(in)          :: SPATIAL,NATM,NTRJ
  double precision,intent(in) :: xyz1(SPATIAL,NATM,NTRJ),xyz2(SPATIAL,NATM,NTRJ)
  double precision,intent(out):: res(SPATIAL,NATM,NTRJ)
    res = xyz1 * xyz2 ; RETURN
  end subroutine MUL
end function TrjMul
!
pure function TrjDiv(t1,t2) result(res)
class(trajectory),intent(in)  :: t1,t2
double precision              :: res(3,LtOrOne(t1%NATM,t2%NATM),LtOrOne(t1%NTRJ,t2%NTRJ))
integer                       :: natm,ntrj
  natm = minval([t1%NATM,t2%NATM],1) ; ntrj = minval([t1%NTRJ,t2%NTRJ],1)
  if(minval([natm,ntrj],1)<1)then ; res = 0.d0 ; RETURN ; endif
  call DIV(3,NATM,NTRJ,t1%xyz(1:3,1:natm,1:ntrj),t2%xyz(:,1:natm,1:ntrj),res)
contains
  pure subroutine DIV(SPATIAL,NATM,NTRJ,xyz1,xyz2,res)
  integer,intent(in)          :: SPATIAL,NATM,NTRJ
  double precision,intent(in) :: xyz1(SPATIAL,NATM,NTRJ),xyz2(SPATIAL,NATM,NTRJ)
  double precision,intent(out):: res(SPATIAL,NATM,NTRJ)
    res = xyz1 / xyz2 ; RETURN
  end subroutine DIV
end function TrjDiv
!
pure function TrjDot(t1,t2) result(res)
class(trajectory),intent(in)  :: t1,t2
double precision              :: res(LtOrOne(t1%NATM,t2%NATM),LtOrOne(t1%NTRJ,t2%NTRJ))
integer                       :: natm,ntrj
  natm = minval([t1%NATM,t2%NATM],1) ; ntrj = minval([t1%NTRJ,t2%NTRJ],1)
  if(minval([natm,ntrj],1)<1)then ; res = 0.d0 ; RETURN ; endif
  call DOT(3,NATM,NTRJ,t1%xyz(1:3,1:natm,1:ntrj),t2%xyz(:,1:natm,1:ntrj),res)
contains
  pure subroutine DOT(SPATIAL,NATM,NTRJ,xyz1,xyz2,res)
  integer,intent(in)          :: SPATIAL,NATM,NTRJ
  double precision,intent(in) :: xyz1(SPATIAL,NATM,NTRJ),xyz2(SPATIAL,NATM,NTRJ)
  double precision,intent(out):: res(NATM,NTRJ)
  integer                     :: i,j
    do j = 1,ntrj
      do i = 1,natm
        res(i,j) = dot_product(xyz1(:,i,j),xyz2(:,i,j))
      enddo
    enddo
  end subroutine DOT
end function TrjDot
!
pure function TrjCross(t1,t2) result(res)
class(trajectory),intent(in)  :: t1,t2
double precision              :: res(3,LtOrOne(t1%NATM,t2%NATM),LtOrOne(t1%NTRJ,t2%NTRJ))
integer                       :: natm,ntrj
  natm = minval([t1%NATM,t2%NATM],1) ; ntrj = minval([t1%NTRJ,t2%NTRJ],1)
  if(minval([natm,ntrj],1)<1)then ; res = 0.d0 ; RETURN ; endif
  call CROSS(3,NATM,NTRJ,t1%xyz(1:3,1:natm,1:ntrj),t2%xyz(:,1:natm,1:ntrj),res)
contains
  pure subroutine Cross(SPATIAL,NATM,NTRJ,xyz1,xyz2,res)
  integer,intent(in)          :: SPATIAL,NATM,NTRJ
  double precision,intent(in) :: xyz1(SPATIAL,NATM,NTRJ),xyz2(SPATIAL,NATM,NTRJ)
  double precision,intent(out):: res(SPATIAL,NATM,NTRJ)
  integer                     :: i,j
    do j = 1,ntrj
      do i = 1,natm
        res(1,i,j)=xyz1(2,i,j)*xyz2(3,i,j)-xyz1(3,i,j)*xyz2(2,i,j)
        res(2,i,j)=xyz1(3,i,j)*xyz2(1,i,j)-xyz1(1,i,j)*xyz2(3,i,j)
        res(3,i,j)=xyz1(1,i,j)*xyz2(2,i,j)-xyz1(2,i,j)*xyz2(1,i,j)
      enddo
    enddo
  end subroutine Cross
end function TrjCross
!
pure integer function LtOrOne(i1,i2) result(res)
integer,intent(in) :: i1,i2
  res = maxval([minval([i1,i2],1),1],1) ; RETURN
end function LtOrOne
!
