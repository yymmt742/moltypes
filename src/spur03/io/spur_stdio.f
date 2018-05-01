module spur_stdio
use,intrinsic :: ISO_FORTRAN_ENV, only : STDERR => ERROR_UNIT, &
&                                        STDOUT => OUTPUT_UNIT,&
&                                        STDINP => INPUT_UNIT
use spur_vector_chr
use spur_string
use spur_pathname
use spur_ioerrorhandler
  implicit none
  private
  public :: stdio
!
  integer,parameter         :: DEVNULL     = -1
  integer,parameter         :: DEVLB       = 10
  integer,parameter         :: DEVUB       = 99
  integer,parameter         :: DEVVACANCY  = DEVUB - DEVLB + 1
  logical,parameter         :: ADVANCE_DEF = .TRUE.
  integer,parameter         :: IFMT_DEF(1) = [8]
  integer,parameter         :: LOAD_MEM    =  0
!
!---
  integer,parameter         :: MAXLINE    = 65536
  logical,save              :: DEVTABLE(DEVLB:DEVUB) = .true.
!
  type,extends(pathname) :: stdio
    private
    type(vector_chr)       :: baff
    logical                :: adv = ADVANCE_DEF
    integer                :: dev = DEVNULL, stat=IO_CLOSING, iseek=0
  contains
    procedure         :: connect        => StdioConnect
    procedure         :: append         => StdioAppend
    procedure         :: streamseek     => StdioStreamSeek
    procedure         :: generate       => StdioGenerate
    procedure         :: seek           => StdioSeek
    procedure         :: quit           => StdioQuit
    procedure         :: devn           => StdioDevN
    procedure         :: delete         => StdioDelete
    procedure         :: load           => StdioLoad
    procedure         :: goback         => StdioGoback
    procedure         :: goforward      => StdioGoforward
    procedure         :: reload         => StdioReload
    procedure         :: nlines         => StdioNLines
    procedure         :: CurrentAddress => StdioCurrentAddress
    procedure         :: seeksuccess    => StdioSeekSuccess
    procedure,private :: StdioGets_chr
    procedure,private :: StdioGets_chrv
    procedure,private :: StdioGets_int
    procedure,private :: StdioGets_intv
    procedure,private :: StdioGets_real
    procedure,private :: StdioGets_realv
    generic           :: gets => StdioGets_chr, StdioGets_chrv,&
                              &  StdioGets_int, StdioGets_intv,&
                              &  StdioGets_real,StdioGets_realv
    procedure,private :: StdioPuts_chr
    procedure,private :: StdioPuts_chrv
    procedure,private :: StdioPuts_int
    procedure,private :: StdioPuts_intv
    generic           :: puts => StdioPuts_chr,StdioPuts_chrv,&
                              &  StdioPuts_int,StdioPuts_intv
    procedure         :: break          => StdioBreak
    procedure         :: isreading      => StdioIsReading
    procedure         :: isnotreading   => StdioIsNotReading
    procedure         :: iswriting      => StdioIsWriting
    procedure         :: isnotwriting   => StdioIsNotWriting
    procedure         :: isopen         => StdioIsOpen
    procedure         :: isclose        => StdioIsClose
    procedure         :: iseof          => StdioIsEOF
    procedure         :: iserr          => StdioIsERR
    procedure         :: clear          => StdioClear
    procedure         :: lookup         => StdioLookup
    final             :: StdioDestractor
  end type stdio
contains
  subroutine StdioGenerate(this)
  class(stdio),intent(inout)       :: this
  integer                          :: is,system
    call RoutineNameIs('STDIO_GENERATE')
    if(CheckAbort(this,this%is()=='',IO_EMPTYPATH))RETURN
    if(CheckOpen(this%stat)) call this%quit()
!
    if(this%dirname()/='')then
      is = system('mkdir -p '//this%dirname())
      if(CheckAbort(this,is/=0,IO_MKDIRERR))RETURN
    endif
!
    call GetDevN(this) ; if(CheckAbort(this))RETURN
    open(this%dev, FILE=this%is(), STATUS="REPLACE",ACTION="WRITE",POSITION="ASIS",IOSTAT=is)
    call SetIostat(this,is) ; if(CheckAbort(this,ierr=IO_OPENERR))RETURN
  end subroutine StdioGenerate
!
  subroutine StdioAppend(this)
  class(stdio),intent(inout)        :: this
  integer                           :: is
    if(this%is()=='')then
      this%stat = IO_EMPTYPATH ; call this%quit() ; this%dev = STDOUT ; RETURN
    endif
    call RoutineNameIs('STDIO_APPEND')
    if(CheckAbort(this,this%isExist().and.this%isnotWritable(),IO_NOTWRITABLE)) RETURN
    if(CheckOpen(this%stat)) call this%quit()
!
    call GetDevN(this) ; if(CheckAbort(this))RETURN
!
    if(this%isExist())then
      open(this%dev,FILE=this%is(),STATUS="OLD",ACTION="WRITE",POSITION="APPEND",IOSTAT=is)
    else
      open(this%dev,FILE=this%is(),STATUS="REPLACE",ACTION="WRITE",POSITION="ASIS",IOSTAT=is)
    endif
    call SetIostat(this,is) ; if(CheckAbort(this,ierr=IO_OPENERR))RETURN
  end subroutine StdioAppend
!
  subroutine StdioConnect(this)
  class(stdio),intent(inout)        :: this
  integer                           :: is
    if(this%is()=='')then
      this%stat = IO_EMPTYPATH ; call this%quit() ; this%dev = STDINP ; RETURN
    endif
    call RoutineNameIs('STDIO_CONNECT')
    if(CheckAbort(this,this%isExist().and.this%isnotReadable(),IO_NOTREADABLE)) RETURN
    if(CheckOpen(this%stat)) call this%quit()
!
    call GetDevN(this) ; if(CheckAbort(this))RETURN
!
    open(this%dev, FILE=this%is(), STATUS="OLD",ACTION="READ",POSITION="ASIS",IOSTAT=is)
    call SetIostat(this,is) ; if(CheckAbort(this,ierr=IO_OPENERR))RETURN
  end subroutine StdioConnect
!
  function StdioStreamSeek(this,substring,reload) result(res)
  class(stdio),intent(inout)           :: this
  character(*),intent(in)              :: substring
  logical,intent(in),optional          :: reload
  character(:),allocatable             :: res
  character(MAXLINE)                   :: line
  integer                              :: id,is
    allocate(character(0)::res)
    if(CheckAbort(this,len_trim(substring)==0,ierr=IO_ILEGALOPT)) RETURN
    if(CheckAbort(this,this%isnotReading(),ierr=IO_DISCONNECT))   RETURN
!
    if(present(reload))then
      if(reload.and.this%isOpen()) rewind(this%dev) ; this%iseek = 0
    endif
!
    do
      this%iseek = this%iseek + 1
      read(this%dev,'(A)',END=100,iostat=is) line
      id = index(trim(line),substring)
      if(id>0)then
        res = trim(line) ; RETURN
      endif
    enddo
    RETURN
100 call this%quit()
  end function StdioStreamSeek
!
  subroutine SetIostat(this,is)
  class(stdio),intent(inout) :: this
  integer,intent(in)         :: is
  character(9)               :: act
    if(is==0)then
      INQUIRE(this%dev,ACTION=act)
      if(act=='READ')then
        this%stat = IO_READING
      elseif(act=='READWRITE'.or.act=='WRITE')then
        this%stat = IO_WRITING
      else
        this%stat = IO_CLOSING
      endif
    elseif(is>0) then
      this%stat = IO_ABNORMAL
    else
      this%stat = IO_ENDOFFILE
    endif
    call this%check()
  end subroutine SetIostat
!
  subroutine StdioBreak(this)
  class(stdio),intent(inout) :: this
  integer                    :: is
    if(this%isnotWriting()) call StdioAppend(this)
    call RoutineNameIs('STDIO_BREAK')
    write(this%dev,'(A)',iostat=is,ADVANCE='YES') ''
    call SetIostat(this,is)
    if(CheckAbort(this,is/=0,IO_WRITERR)) RETURN
  end subroutine StdioBreak
!
  subroutine StdioPuts_chr(this,str,break)
  class(stdio),intent(inout)       :: this
  character(*),intent(in)          :: str
  logical,intent(in),optional      :: Break
  character(3)                     :: adv
  integer                          :: is
    if(this%isnotWriting()) call StdioAppend(this)
    call RoutineNameIs('STDIO_PUTS')
    adv = ADVANCE_YESNO(this%adv)
    if(present(Break)) adv = ADVANCE_YESNO(Break)
    write(this%dev,'(A)',iostat=is,ADVANCE=adv) str
    call SetIostat(this,is)
    if(CheckAbort(this,is/=0,IO_WRITERR)) RETURN
  end subroutine StdioPuts_chr
!
  subroutine StdioPuts_chrv(this,str,break)
  class(stdio),intent(inout)       :: this
  character(*),intent(in)          :: str(:)
  logical,intent(in),optional      :: Break
  character(3)                     :: adv
  integer                          :: is
    if(this%isnotWriting()) call StdioAppend(this)
    call RoutineNameIs('STDIO_PUTS')
    adv = ADVANCE_YESNO(this%adv)
    if(present(Break)) adv = ADVANCE_YESNO(Break)
    write(this%dev,'(A)',iostat=is,ADVANCE=adv) str
    call SetIostat(this,is)
    if(CheckAbort(this,is/=0,IO_WRITERR)) RETURN
  end subroutine StdioPuts_chrv
!
  subroutine StdioPuts_int(this,val,ifmt,break)
  class(stdio),intent(inout)       :: this
  integer,intent(in)               :: val
  integer,intent(in),optional       :: ifmt(:)
  logical,intent(in),optional      :: Break
  character(3)                     :: adv
  character(:),allocatable         :: lfmt
  integer                          :: is
    if(this%isnotWriting()) call StdioAppend(this)
    call RoutineNameIs('STDIO_PUTS')
    adv = ADVANCE_YESNO(this%adv)
    if(present(Break)) adv = ADVANCE_YESNO(Break)
!
    allocate(character(0)::lfmt)
    if(present(ifmt))then
      lfmt = '('//STRFMT('I',ifmt)//')'
    else
      lfmt = '('//STRFMT('I',IFMT_DEF)//')'
    endif
!
    write(this%dev,lfmt,iostat=is,ADVANCE=adv) val
    call SetIostat(this,is)
    if(CheckAbort(this,is/=0,IO_WRITERR)) RETURN
  end subroutine StdioPuts_int
!
  subroutine StdioPuts_intv(this,val,ifmt,break,num)
  class(stdio),intent(inout)       :: this
  integer,intent(in)               :: val(:)
  integer,intent(in),optional      :: ifmt(:),num
  logical,intent(in),optional      :: Break
  character(3)                     :: adv
  character(:),allocatable         :: lfmt
  integer                          :: is,lnum
    if(this%isnotWriting()) call StdioAppend(this)
    call RoutineNameIs('STDIO_PUTS')
    adv = ADVANCE_YESNO(this%adv)
    if(present(Break)) adv = ADVANCE_YESNO(Break)
!
    lnum = size(val) ; if(present(num)) lnum = num
    allocate(character(0)::lfmt)
    if(present(ifmt))then
      lfmt = '('//STRFMT('I',ifmt,lnum)//')'
    else
      lfmt = '('//STRFMT('I',IFMT_DEF,lnum)//')'
    endif
!
    write(this%dev,lfmt,iostat=is,ADVANCE=adv) val
    call SetIostat(this,is)
    if(CheckAbort(this,is/=0,IO_WRITERR)) RETURN
  end subroutine StdioPuts_intv
!
  pure function ADVANCE_YESNO(l) result(res)
  logical,intent(in) :: l
  character(3)       :: res
    if(l)then ; res = 'YES' ; else ; res = 'NO ' ; endif
  end function ADVANCE_YESNO
!
  pure function STRFMT(x,ifmt,num) result(res)
  use spur_string
  character(1),intent(in)     :: x
  integer,intent(in)          :: ifmt(:)
  integer,intent(in),optional :: num
  character(:),allocatable    :: res
    allocate(character(0)::res)
    if(size(ifmt)==0)then
      res = x
    elseif(size(ifmt)==1)then
      res = x//ToStr(ifmt(1))
    elseif(size(ifmt)>=2)then
      res = x//ToStr(ifmt(1))//'.'//ToStr(ifmt(2))
    endif
    if(present(num)) res = ToStr(num)//res
  end function STRFMT
!
  subroutine StdioLoad(this,maxn,maxb)
  class(stdio),intent(inout)  :: this
  integer,intent(in),optional :: maxn,maxb
  integer                     :: is,bload,lload
  integer                     :: lmax,bmax,lsign,bsign
  character(MAXLINE)          :: line
    call this%baff%clear() ; this%iseek = 0
    if(this%isnotReading()) call this%Connect()
    if(this%stat==IO_EMPTYPATH)then
      call this%quit() ; RETURN
    endif
    call RoutineNameIs('STDIO_LOAD')
    lload = 0 ; lmax = 0 ; lsign = 0
    bload = 0 ; bmax = 0 ; bsign = 0
    if(present(maxn)) lmax = maxval([maxn,0],1)
    if(present(maxb)) bmax = maxval([maxb,0],1)
    if(bmax>0) bsign = 1
    if(lmax>0) lsign = 1
!
    do
      read(this%dev,'(A)',IOSTAT=is,END=100) line
      if(CheckAbort(this,is>0,IO_READERR)) EXIT
      call this%baff%push(trim(line))
      bload = bload + bsign * len_trim(line)
      lload = lload + lsign
      if(lload>lmax.or.bload>bmax) RETURN
    enddo
    RETURN
100 call this%quit()
  end subroutine StdioLoad
!
  function StdioGets_chr(this) result(res)
  class(stdio),intent(inout)        :: this
  character(:),allocatable          :: res
    allocate(character(0)::res)
    if(this%baff%size()==0) RETURN
    this%iseek = this%iseek + 1
    res = this%baff%at(this%iseek)
  end function StdioGets_chr
!
  function StdioGets_chrv(this,num,dumm) result(res)
  class(stdio),intent(inout) :: this
  integer,intent(in)         :: num
  character(*),intent(in)    :: dumm
  character(len(dumm))       :: res(num)
    res = dumm ; if(this%baff%size()==0) RETURN
    this%iseek = this%iseek + 1
    call readfix(this%baff%at(this%iseek),num,len(dumm),dumm,res)
  contains
    pure subroutine readfix(string,num,dig,dumm,res)
    character(*),intent(in)      :: string
    integer,intent(in)           :: num,dig
    character(dig),intent(in)    :: dumm
    character(dig),intent(inout) :: res(num)
    integer                      :: i,j,nlen
      j = 0 ; nlen = len_trim(string)
      do i=1,nlen,dig
        j = j + 1 ; if(j>num) RETURN
        res(j) = string(i:minval([i+dig-1,nlen],1))
      enddo
    end subroutine readfix
  end function StdioGets_chrv
!
  integer function StdioGets_int(this,dumm) result(res)
  class(stdio),intent(inout) :: this
  integer,intent(in)         :: dumm
    res = dumm ; if(this%baff%size()==0) RETURN
    this%iseek = this%iseek + 1
    res = ToNum(this%baff%at(this%iseek),dumm)
  end function StdioGets_int
!
  function StdioGets_intv(this,num,dumm,dig) result(res)
  class(stdio),intent(inout)  :: this
  integer,intent(in)          :: dumm,num
  integer,intent(in),optional :: dig
  integer                     :: res(num)
    res = dumm ; if(this%baff%size()==0) RETURN
    this%iseek = this%iseek + 1
    if(present(dig))then
      call readfix(this%baff%at(this%iseek),num,dig,dumm,res)
    else
      call readfree(this%baff%at(this%iseek),num,dumm,res)
    endif
  contains
    pure subroutine readfix(string,num,dig,dumm,res)
    character(*),intent(in) :: string
    integer,intent(in)      :: num,dig,dumm
    integer,intent(inout)   :: res(num)
    integer                 :: i,j,nlen
      j = 0 ; nlen = len_trim(string)
      do i=1,nlen,dig
        j = j + 1 ; if(j>num) RETURN
        res(j) = ToNum(string(i:minval([i+dig-1,nlen],1)),dumm)
      enddo
    end subroutine readfix
!
    pure subroutine readfree(string,num,dumm,res)
    character(*),intent(in) :: string
    integer,intent(in)      :: num,dumm
    integer,intent(inout)   :: res(num)
    integer                 :: i
    type(vector_chr)        :: sp
    call sp%split(string,delimiter=' ,')
    do i=1,minval([sp%size(),num],1)
      res(i) = ToNum(sp%at(i),dumm)
    enddo
    end subroutine readfree
  end function StdioGets_intv
!
!
  integer function StdioGets_real(this,dumm) result(res)
  class(stdio),intent(inout) :: this
  real,intent(in)            :: dumm
    res = dumm ; if(this%baff%size()==0) RETURN
    this%iseek = this%iseek + 1
    res = ToNum(this%baff%at(this%iseek),dumm)
  end function StdioGets_real
!
  function StdioGets_realv(this,num,dumm,dig) result(res)
  class(stdio),intent(inout)  :: this
  integer,intent(in)          :: num
  real,intent(in)             :: dumm
  integer,intent(in),optional :: dig
  real                        :: res(num)
    res = dumm ; if(this%baff%size()==0) RETURN
    this%iseek = this%iseek + 1
    if(present(dig))then
      call readfix(this%baff%at(this%iseek),num,dig,dumm,res)
    else
      call readfree(this%baff%at(this%iseek),num,dumm,res)
    endif
  contains
    pure subroutine readfix(string,num,dig,dumm,res)
    character(*),intent(in) :: string
    integer,intent(in)      :: num,dig
    real,intent(in)         :: dumm
    real,intent(inout)      :: res(num)
    integer                 :: i,j,nlen
      j = 0 ; nlen = len_trim(string)
      do i=1,nlen,dig
        j = j + 1 ; if(j>num) RETURN
        res(j) = ToNum(string(i:minval([i+dig-1,nlen],1)),dumm)
      enddo
    end subroutine readfix
!
    pure subroutine readfree(string,num,dumm,res)
    character(*),intent(in) :: string
    integer,intent(in)      :: num
    real,intent(in)         :: dumm
    real,intent(inout)      :: res(num)
    integer                 :: i
    type(vector_chr)        :: sp
    call sp%split(string,delimiter=' ,')
    do i=1,minval([sp%size(),num],1)
      res(i) = ToNum(sp%at(i),dumm)
    enddo
    end subroutine readfree
  end function StdioGets_realv
!
  function StdioSeek(this,substring,reload) result(res)
  class(stdio),intent(inout)           :: this
  character(*),intent(in)              :: substring
  logical,intent(in),optional          :: reload
  character(:),allocatable             :: res
  integer                              :: id
    allocate(character(0)::res) ; if(len_trim(substring)==0) RETURN
    if(this%baff%size()==0) RETURN
    if(present(reload))then
      if(reload) this%iseek = 0
    endif
    if(this%iseek>this%baff%size()) RETURN
!
    do
      this%iseek = this%iseek + 1
      if(this%iseek>this%baff%size()) EXIT
      id    = index(this%baff%at(this%iseek),substring)
      if(id>0)then
        res = this%baff%at(this%iseek) ; RETURN
      endif
    enddo
  end function StdioSeek
!
  pure logical function StdioSeekSuccess(this) result(res)
  class(stdio),intent(in) :: this
    res = this%iseek<this%baff%size()
  end function StdioSeekSuccess
!
  pure integer function StdioNLines(this) result(res)
  class(stdio),intent(in) :: this
    res = this%baff%size()
  end function StdioNLines
!
  pure integer function StdioCurrentAddress(this) result(res)
  class(stdio),intent(in) :: this
    res = this%iseek
  end function StdioCurrentAddress
!
  pure subroutine StdioGoback(this,num)
  class(Stdio),intent(inout)        :: this
  integer,intent(in),optional       :: num
    if(present(num))then
      this%iseek = this%iseek - num
    else
      this%iseek = this%iseek - 1
    endif
    if(this%iseek<0) this%iseek = 0
    if(this%iseek>this%baff%size()) this%iseek = this%baff%size()
  end subroutine StdioGoback
!
  pure subroutine StdioGoForward(this,num)
  class(Stdio),intent(inout)        :: this
  integer,intent(in),optional       :: num
    if(present(num))then
      this%iseek = this%iseek + num
    else
      this%iseek = this%iseek + 1
    endif
    if(this%iseek<0) this%iseek = 0
    if(this%iseek>this%baff%size()) this%iseek = this%baff%size()
  end subroutine StdioGoForward
!
  pure subroutine StdioReload(this)
  class(Stdio),intent(inout) :: this
    this%iseek = 0
  end subroutine StdioReload
!
  pure function StdioLookUp(this) result(res)
  class(Stdio),intent(in)       :: this
  character(this%baff%maxlen()) :: res(this%baff%size())
  integer                       :: i
    res = this%baff%lookup()
  end function StdioLookUp
!
! pure function StdioGets_idx(this,idx) result(res)
! class(stdio),intent(in)           :: this
! integer,intent(in)                :: idx
! character(:),allocatable          :: res
!   allocate(character(0)::res)
!   if(this%baff%size()==0) RETURN
!   res = this%baff%at(idx)
! end function StdioGets_idx
!
  subroutine StdioDelete(this,path)
  class(Stdio),intent(inout)       :: this
  character(*),intent(in),optional :: path
  integer                          :: is
    if(present(path)) call this%Fetch(path)
    if(this%isnotExist()) RETURN
    if(this%isClose()) call this%Connect()
    call RoutineNameIs('STDIO_DELETE')
    close(this%dev,STATUS="DELETE",IOSTAT=is) ; call SetIostat(this,is)
    DEVTABLE(this%dev) = .true. ; this%dev = DEVNULL
  end subroutine StdioDelete
!
  pure integer function StdioDevN(this) result(res)
  class(stdio),intent(in)       :: this
    if(this%isClose())then ; res = STDOUT ;else ; res = this%dev ; endif
  end function StdioDevN
!
  subroutine GetDevN(this)
  type(stdio),intent(inout) :: this
  integer                   :: i,dev
  real                      :: rnd
    call random_number(rnd)
    dev = modulo(int(rnd*1000),DEVVACANCY)+1+DEVLB
    do i=dev,DEVLB,-1
      if(DEVTABLE(i))then
        this%dev = i ; DEVTABLE(i) = .false. ; RETURN
      endif
    enddo
    do i=dev,DEVUB,1
      if(DEVTABLE(i))then
        this%dev = i ; DEVTABLE(i) = .false. ; RETURN
      endif
    enddo
    this%dev = IO_DEVERR
  end subroutine GetDevN
!
  subroutine StdioQuit(this)
  class(stdio),intent(inout) :: this
  integer                    :: is
    call RoutineNameIs('STDIO_QUIT')
    if(this%dev==DEVNULL) RETURN
    close(this%dev,IOSTAT=is)
    call SetIostat(this,is)
    DEVTABLE(this%dev) = .true. ; this%dev = DEVNULL
  end subroutine StdioQuit
!
  pure logical function StdioIsReading(this) result(res)
  class(stdio),intent(in)    :: this
    res = this%stat==IO_READING
  end function StdioIsReading
!
  pure logical function StdioIsNotReading(this) result(res)
  class(stdio),intent(in)    :: this
    res = this%stat/=IO_READING
  end function StdioIsNotReading
!
  pure logical function StdioIsWriting(this) result(res)
  class(stdio),intent(in)    :: this
    res = this%stat==IO_WRITING
  end function StdioIsWriting
!
  pure logical function StdioIsNotWriting(this) result(res)
  class(stdio),intent(in)    :: this
    res = this%stat/=IO_WRITING
  end function StdioIsNotWriting
!
  pure logical function StdioIsOpen(this) result(res)
  class(stdio),intent(in)    :: this
    res = CheckOpen(this%stat)
  end function StdioIsOpen
!
  pure logical function StdioIsClose(this) result(res)
  class(stdio),intent(in)    :: this
    res = .not.this%isOpen()
  end function StdioIsClose
!
  pure logical function StdioIsEOF(this) result(res)
  class(stdio),intent(in)    :: this
    res = this%stat==IO_ENDOFFILE
  end function StdioIsEOF
!
  pure logical function StdioIsERR(this) result(res)
  class(stdio),intent(in)    :: this
    res = .not.CheckSanity(this%stat)
  end function StdioIsERR
!
  logical function CheckAbort(this,test,ierr) result(res)
  type(stdio),intent(inout)        :: this
  logical,intent(in),optional      :: test
  integer,intent(in),optional      :: ierr
    if(present(test))then
      res = test
    else
      res = CheckError(this%stat)
    endif
    if(.not.res) RETURN
    if(present(ierr)) this%stat = ierr
    if(this%terminates_at_abnormal)then
      call IO_echo_errmsg(this%stat,this%is())
      call StdioDestractor(this) ; call exit(ierr)
    endif
  end function CheckAbort
!
  subroutine StdioClear(this)
  class(stdio),intent(inout)  :: this
    call StdioDestractor(this)
  end subroutine StdioClear
!
  subroutine StdioDestractor(this)
  type(stdio),intent(inout)  :: this
    call this%baff%clear()
    this%adv  = ADVANCE_DEF
    this%dev  = DEVNULL
    this%stat = IO_CLOSING
    this%iseek=0
    call this%quit()
    call this%PathNameClear()
  end subroutine StdioDestractor
end module spur_stdio
