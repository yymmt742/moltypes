program pcut
use,intrinsic :: ISO_FORTRAN_ENV, only : STDOUT => OUTPUT_UNIT
use spur_string
implicit none
double precision,parameter :: StdChgR    = 18.2223                  ! For Amber Unit
double precision,parameter :: StdChg     = 1.0/StdChgR              !    1.0/18.2223
character(4),parameter     :: KEY_NAME    = 'name'
character(4),parameter     :: KEY_TYPE    = 'type'
character(5),parameter     :: KEY_RESID   = 'resid'
character(6),parameter     :: KEY_CHARGE  = 'charge'
character(7),parameter     :: KEY_ELEMENT = 'element'
character(4),parameter     :: KEY_MASS    = 'mass'
character(7),parameter     :: KEY_RESNAME = 'resname'
  call compute_pcut()
contains
  subroutine compute_pcut()
  use moltypes_amberprmtop
  use moltypes_readmask
  use spur_optparse
  type(amberprmtop)                :: prm
  type(readmask)                   :: mp
  type(optparse)                   :: op
    call op%add_option("-m",narg=1,metavar='string',def=['all'], help='mask string.')
    call op%add_option("-o",narg=1,metavar='string', help='output path.')
    call op%parser()
    if(op%narg()==0) call op%call_usage()
!
    prm%terminates_at_abnormal = .TRUE.
    call prm%load(op%args(1))
!
    if(op%option("-m"))then
      call mp%init(prm%natoms())
      call mp%def_keyword(KEY_NAME,   prm%IGRAPH)
      call mp%def_keyword(KEY_TYPE,   prm%ISYMBL)
      call mp%def_keyword(KEY_RESNAME,prm%resname())
      call mp%def_keyword(KEY_RESID,  prm%resid())
      call mp%def_keyword(KEY_ELEMENT,prm%ATNUM)
      call mp%def_keyword(KEY_CHARGE, real(prm%CHARGE*StdChg))
      call mp%def_keyword(KEY_MASS,   real(prm%AMASS))
      call prm%export(op%optargs('-o',1),mask=mp%parse(op%optargs('-m',1)))
    else
      call prm%export(op%optargs('-o',1))
    endif
  end subroutine compute_pcut
end program pcut
