! Language    : Fortran90
! Description : main modules for PSC
! Lastchange  : 2018, Dec. 25, 15:46
! =================================================================
! MODULE cs_psc %{{{
MODULE cs_psc
  IMPLICIT NONE
  SAVE
  ! always use below keywords as intrinsics
  INTRINSIC :: DBLE, DABS, DSQRT, DEXP, DLOG, DLOG10
  INTRINSIC :: DSIN, DCOS, DTAN, DASIN, DACOS, DATAN, DATAN2
  INTRINSIC :: DSINH, DCOSH, DTANH, DMOD, DSIGN
  INTRINSIC :: INT, NINT, FLOOR, CEILING, IABS, MOD, ISIGN
  INTRINSIC :: TRIM, LEN_TRIM, ADJUSTL, ACHAR, IACHAR
  INTRINSIC :: SCAN, VERIFY
  INTRINSIC :: ALLOCATED, SIZE, LBOUND, UBOUND, SHAPE, RESHAPE
  INTRINSIC :: MATMUL, DOT_PRODUCT, TRANSPOSE, MINLOC, MAXLOC
  INTRINSIC :: MINVAL, MAXVAL, COUNT, ALL, ANY, SUM
  INTRINSIC :: MODULO
  INTRINSIC :: CMPLX, REAL, DIMAG, CONJG
  !INTRINSIC :: IARGC
  ! significant parameters
  CHARACTER( LEN= 20 ), PARAMETER :: pscauthor = "XXXXXXXX"
  CHARACTER( LEN= 20 ), PARAMETER :: pscversion = "1.e"
  CHARACTER( LEN= 20 ), PARAMETER :: pscupdate = "2018, Dec. 25"
  INTEGER( KIND= 4 ), PARAMETER :: log_underf = -323
  INTEGER( KIND= 4 ), PARAMETER :: log_overf  = +323
  INTEGER( KIND= 4 ), PARAMETER :: psc_strlen = 4000
  INTEGER( KIND= 4 ), PARAMETER :: psc_wordlen = 200
  ! array size
  INTEGER( KIND= 4 ), PARAMETER :: psc_maxtag = 8
  INTEGER( KIND= 4 ), PARAMETER :: stackmax = 1000
  INTEGER( KIND= 4 ), PARAMETER :: macromax = 1000
  INTEGER( KIND= 4 ), PARAMETER :: mapmax = 100
  INTEGER( KIND= 4 ), PARAMETER :: varmax = 200
  ! types
  TYPE mapdict
    CHARACTER( LEN= psc_wordlen ) :: lhs
    CHARACTER( LEN= psc_strlen ) :: rhs
    LOGICAL :: no
  END TYPE
  TYPE vardict
    CHARACTER( LEN= 100 ) :: tag
    INTEGER( KIND= 4 ) :: loc
    DOUBLE PRECISION :: val
  END TYPE
  TYPE device
    INTEGER( KIND= 4 ) :: devnum
    CHARACTER( LEN= 500 ) :: tag
  END TYPE
  TYPE(device), PARAMETER :: dev_stder = device( 0, 'stder' )
  TYPE(device), PARAMETER :: dev_stdin = device( 5, 'stdin' )
  TYPE(device), PARAMETER :: dev_stdou = device( 6, 'stdou' )
END MODULE
!%}}}
! MODULE cv_psc %{{{
MODULE cv_psc
  USE cs_psc
  IMPLICIT NONE
  SAVE
  ! significat flags
  LOGICAL :: quiet   = .FALSE.
  LOGICAL :: color   = .FALSE.
  LOGICAL :: autonl  = .FALSE.
  !
  LOGICAL :: super_quiet = .FALSE.
  LOGICAL :: quiet_load  = .FALSE.
  LOGICAL :: error_quiet = .FALSE.
  LOGICAL :: error_cont  = .FALSE.
  !
  LOGICAL :: debug_buffer_start = .FALSE.
  LOGICAL :: debug_buffer_read  = .FALSE.
  LOGICAL :: debug_buffer_clean = .FALSE.
  LOGICAL :: debug_buffer_map   = .FALSE.
  LOGICAL :: debug_buffer_trim  = .FALSE.
  LOGICAL :: debug_buffer_last  = .FALSE.
  LOGICAL :: debug_buffer_do    = .FALSE.
  LOGICAL :: debug_loop  = .FALSE.
  LOGICAL :: debug_level = .FALSE.
  LOGICAL :: debug_commd = .FALSE.
  LOGICAL :: debug_stack = .FALSE.
  !
  LOGICAL :: file_exec   = .FALSE.
  LOGICAL :: script_exec = .FALSE.
  LOGICAL :: file_pexe   = .FALSE.
  LOGICAL :: script_pexe = .FALSE.
  !
  INTEGER( KIND= 4 ) :: id_comm
  INTEGER( KIND= 4 ) :: exec_mode
  CHARACTER( LEN= 100 ) :: maintag_comm
  CHARACTER( LEN= 100 ) :: subtag_comm( psc_maxtag )
  CHARACTER( LEN= 200 ) :: help_comm
  CHARACTER( LEN= 200 ) :: helpja_comm
  CHARACTER( LEN= 200 ) :: usage_comm
  ! colors; hold fg-colors(RGB) and bg-colors(RGB), -1 if default
  INTEGER( KIND= 4 ) :: cl_reset(6)
  INTEGER( KIND= 4 ) :: cl_prompt(6)
  INTEGER( KIND= 4 ) :: cl_histnum(6)
  INTEGER( KIND= 4 ) :: cl_histcom(6)
  INTEGER( KIND= 4 ) :: cl_histstack(6)
  INTEGER( KIND= 4 ) :: cl_loadcom(6)
  INTEGER( KIND= 4 ) :: cl_listcom(6)
  INTEGER( KIND= 4 ) :: cl_liststack(6)
  INTEGER( KIND= 4 ) :: cl_helptitle(6)
  INTEGER( KIND= 4 ) :: cl_helpcontents(6)
  INTEGER( KIND= 4 ) :: cl_error(6)
  INTEGER( KIND= 4 ) :: cl_fatalerror(6)
  !
  CHARACTER( LEN= 100 ) :: prompt_normal
  CHARACTER( LEN= 100 ) :: prompt_special
  ! current condition
  INTEGER( KIND= 4 ) :: macrosize ! string stack size
  INTEGER( KIND= 4 ) :: histsize ! history size
  INTEGER( KIND= 4 ) :: mapsize ! number of maps defined
  INTEGER( KIND= 4 ) :: varsize ! number of variable defined
  INTEGER( KIND= 4 ) :: stz ! stack size
  DOUBLE PRECISION :: stack(1:stackmax) ! stacked values
  CHARACTER( LEN= psc_strlen ) :: cmd ! command
  CHARACTER( LEN= psc_strlen ) :: macro(1:macromax) ! string stack for each depth
  TYPE(mapdict) :: map(1:mapmax) ! defined maps
  TYPE(vardict) :: var(1:varmax) ! defined variables
  CHARACTER( LEN= 100 ) :: errmsg(1:50) ! error messages for each error-code
  ! syntax analysis
  CHARACTER( LEN= psc_strlen ) :: cword ! current reading word
  CHARACTER( LEN= psc_strlen ) :: script ! temporaly script
  CHARACTER( LEN= psc_strlen ) :: bufin ! command buffer for read
  INTEGER( KIND= 4 )  :: wlen ! word length
  INTEGER( KIND= 4 )  :: loclev ! localization level
  LOGICAL             :: stackchanged ! check if stack has changed by newest command
  LOGICAL             :: iferror      ! check whether error happens in routines call
  LOGICAL             :: ifloop       ! check whether system execution is in loop or not
  DOUBLE PRECISION    :: incunit ! incrementation unit
  ! print format
  CHARACTER( LEN= psc_wordlen ) :: sepstr ! separator for PSC-print
  INTEGER( KIND= 4 )   :: decimals ! decimals for PSC-print
  CHARACTER( LEN= 50 ) :: fmt_a ! format for PSC-print
  ! input & output files
  INTEGER( KIND= 4 ) :: intfsave
  CHARACTER( LEN= 500 ) :: fsave
  TYPE(device) :: dev_in(0:100) ! input devices
  TYPE(device) :: dev_ou(0:100) ! output devices
  TYPE(device) :: dev_ld(0:100) ! load devices
  CHARACTER( LEN= psc_strlen ) :: holdbuffer(0:100)
  ! Note: read from holdbuffer (if it is not empty) first
  !         before reading normal buffer
  INTEGER( KIND= 4 ) :: lines(0:100) ! lines in input files
  INTEGER( KIND= 4 ) :: codepoint(0:100) ! reading lines
  INTEGER( KIND= 4 ) :: iinnow
  INTEGER( KIND= 4 ) :: iounow
  INTEGER( KIND= 4 ) :: ildnow
  ! time
  INTEGER( KIND= 4 ) :: datime(1:8)
  CHARACTER( LEN= 10 ) :: systime(1:3)
  CHARACTER( LEN= 5 ) :: dtm(1:8)
  !
  INTEGER( KIND= 4 ), ALLOCATABLE :: prime(:)
END MODULE
!%}}}

