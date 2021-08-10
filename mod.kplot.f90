! Language   : Fortran90
! Description: modules for KPLOT
! Lastchange : 2019, Jan. 22, 14:11
! =================================================================
! MODULE cs_kplot %{{{
MODULE cs_kplot
IMPLICIT NONE
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
PUBLIC
SAVE
CHARACTER(LEN=40),PARAMETER :: kpl_author = 'XXXXXXXX'
CHARACTER(LEN=40),PARAMETER :: kpl_update = '2019, Jan. 22'
INTEGER,          PARAMETER :: kpl_logmin = -300   ! should be greater than -323 for IEEE754 double
INTEGER,          PARAMETER :: kpl_rndmin = -45  ! should be greater than  -52 for IEEE754 double
DOUBLE PRECISION, PARAMETER :: kpl_conspi = 4.d0 * DATAN( 1.d0 )
DOUBLE PRECISION, PARAMETER :: kpl_small  = -1.d300 ! used when finding maximum
DOUBLE PRECISION, PARAMETER :: kpl_large  = +1.d300 ! used when finding minimum
INTEGER         , PARAMETER :: kpl_fnumax = 5 ! max number of data files
INTEGER         , PARAMETER :: kpl_stylemax = 50 ! max number of styles
INTEGER         , PARAMETER :: kpl_styledescstrlenmax = 200 ! max string length for line style description
END MODULE
!%}}}
! MODULE cv_kplot %{{{
MODULE cv_kplot
USE cs_kplot
IMPLICIT NONE
SAVE
! ---------------- important
CHARACTER(LEN=500)  :: filename(kpl_fnumax) ! filename for each data
DOUBLE PRECISION    :: pct(kpl_fnumax)      ! percentage display on each file
INTEGER             :: p_mode       ! plot mode
character(len=100)  :: plotmode ! plot mode as string
character(len=100)  :: usingstr ! using from string
INTEGER             :: rowarr(kpl_fnumax)    ! rows for each file DIM(fnum)
INTEGER             :: colmax       ! max column number
INTEGER             :: repnum       ! number of replot required
INTEGER,ALLOCATABLE :: using(:)     ! "using" array (this is used like Gnuplot)
CHARACTER(LEN=1)    :: commentchar  ! comment CHARACTER in datafile
CHARACTER(LEN=1)    :: vertbarchar  ! vertical bar CHARACTER in datafile
! ---------------- real data
DOUBLE PRECISION,ALLOCATABLE :: kpldata(:,:,:)   ! plot data DIM(fnum,colmax,max(rowarr))
LOGICAL,         ALLOCATABLE :: fildata(:,:,:)   ! detect data fill state
LOGICAL,         ALLOCATABLE :: empdata(:,:,:)   ! detect data vacancy (=empty data)
DOUBLE PRECISION :: vbar(kpl_fnumax,100)  ! vertical bars(fnu,100)
INTEGER          :: vbarnum(kpl_fnumax)   ! number of vertical bar(fnum)
CHARACTER(LEN=500) :: vbarlab(kpl_fnumax,100) ! vertical bar labels
!
DOUBLE PRECISION,ALLOCATABLE :: distance(:)      ! distance list
INTEGER,         ALLOCATABLE :: bondgroup(:,:,:) ! bond group(fnum,colmax,ibmax)
DOUBLE PRECISION,ALLOCATABLE :: bondangle(:,:,:) ! angles for each bonds
DOUBLE PRECISION             :: cubenode(8,3)    ! cube node coordinates (for 3dplot)
! ---------------- paper
CHARACTER(LEN=20)  :: papertype  ! A4, B5, A5(default), userdefined, etc
INTEGER            :: brdt       ! border type
DOUBLE PRECISION   :: ppx, ppy   ! paper size (in pixels)
INTEGER            :: tpst       ! title, key position
DOUBLE PRECISION   :: trx, try   ! translation (in pixels)
DOUBLE PRECISION   :: glbc(3)    ! global background color
DOUBLE PRECISION   :: bakc(3)    ! background color
CHARACTER(LEN=500) :: title      ! title
CHARACTER(LEN=500) :: key(1:100)    !  key
CHARACTER(LEN=500) :: xlab, ylab, y2lb, zlab !  axis-label
CHARACTER(LEN=20)  :: xtlf, ytlf, ztlf, atlf   ! tics label format
! ---------------- margin
DOUBLE PRECISION   :: gxmg, gymg        ! graph-border margin from paper-end
DOUBLE PRECISION   :: lxmg, lymg, ly2mg ! axis-label margin (not tics-label margin!)
DOUBLE PRECISION   :: ttmg, kymg        ! title, key margin from graph-border
DOUBLE PRECISION   :: mpmg              ! multiplot margin between graph-border
DOUBLE PRECISION   :: tcmg              ! tics margin from graph-border
DOUBLE PRECISION   :: ytcmg             ! ytics label margin
DOUBLE PRECISION   :: bxmg, bymg        ! box margin (for colorbar plotmode)
DOUBLE PRECISION   :: clmg              ! colorbar margin from graph-border
DOUBLE PRECISION   :: bbxmg, bbymg      ! boundingbox margin from graph-border
DOUBLE PRECISION   :: kyfw              ! key font width margin
! ---------------- kern
DOUBLE PRECISION   :: ttkx, ttky   ! title kern
DOUBLE PRECISION   :: kykx, kyky   ! key kern
DOUBLE PRECISION   :: xlkx, xlky   ! x label kern
DOUBLE PRECISION   :: ylkx, ylky   ! y label kern
DOUBLE PRECISION   :: y2lkx, y2lky ! y2 label kern
DOUBLE PRECISION   :: tckx, tcky   ! tics label kern
! ---------------- range
INTEGER             :: xr_m               ! xrange division mode
INTEGER             :: xdiv(kpl_fnumax)
INTEGER             :: mxdiv(kpl_fnumax)
INTEGER             :: xstep(kpl_fnumax)
INTEGER             :: yr_m, ydiv, mydiv, ystep  ! yrange division mode
INTEGER             :: ar_m, adiv, madiv, astep  ! bar range division
INTEGER             :: zr_m, zdiv, mzdiv, zstep  ! zrange division mode
INTEGER             :: mxdiv_m, mydiv_m, madiv_m   ! m-division algorithm
INTEGER             :: ibmax ! max number of bonds
DOUBLE PRECISION    :: br_r  ! bondrange ratio
DOUBLE PRECISION    :: bondlenmax
DOUBLE PRECISION:: xmin(kpl_fnumax), xmax(kpl_fnumax)  ! xrange
DOUBLE PRECISION:: xmin_value(kpl_fnumax), xmax_value(kpl_fnumax)  ! xrange given
LOGICAL :: xmin_flag(kpl_fnumax), xmax_flag(kpl_fnumax)
DOUBLE PRECISION    :: xr_r  ! xrange ratio
DOUBLE PRECISION    :: ymin, ymax, yr_r     ! yrange and ratio
DOUBLE PRECISION    :: ymin_value, ymax_value
LOGICAL :: ymin_flag, ymax_flag
DOUBLE PRECISION    :: amin, amax, ar_r     ! brange and ratio
DOUBLE PRECISION    :: zmin, zmax, zr_r     ! zrange and ratio
DOUBLE PRECISION    :: ctx, szx             ! real (=paper) x, center and size
DOUBLE PRECISION    :: cty, szy, dy         ! real (=paper) y, center and size
    ! there's no real-zrange because the paper is always in a plane!
INTEGER :: clbn              ! color bar division
DOUBLE PRECISION   :: pprat  ! perspective ratio: 0>parallel, positive>normal, negative>
DOUBLE PRECISION   :: mist ! exponentially tend to background color
DOUBLE PRECISION   :: lineacceptrange ! 1.0>normal clip
DOUBLE PRECISION   :: pointacceptrange ! 1.0>normal clip
LOGICAL :: connectpair(0:9,0:9) ! connect (i,j) atom type : for p_mode==5
INTEGER :: usingclusternum ! number of "using clusters" <= 20
INTEGER :: usinginit(20) ! cluster info
INTEGER :: usingstep(20) ! cluster info
INTEGER :: usingfinal(20) ! cluster info
! extend array size whenever you want more atom species
! ---------------- flags
LOGICAL :: sizesqr      ! make papersize definition square
LOGICAL :: rotmedia     ! rotate papersize definition
LOGICAL :: realratio    ! real-ratio-plot
LOGICAL :: showline     ! show lines
LOGICAL :: showpoint    ! show points
LOGICAL :: showbond     ! show nearest-neighbour bonds with lines (only when showline=.TRUE.)
LOGICAL :: showcube     ! show cube (for 3dplot)
LOGICAL :: implicitx    ! USE implicit x-value (INTEGER counted)
LOGICAL :: showxtics    ! show x tics
LOGICAL :: showytics    ! show y tics
LOGICAL :: showxtlabs   ! show x tics labels
LOGICAL :: showytlabs   ! show y tics labels
LOGICAL :: showztlabs   ! show z tics labels
LOGICAL :: rotxtlabs    ! rotate x tics labels
LOGICAL :: rotytlabs    ! rotate y tics labels
LOGICAL :: showcolorbar ! show color bar
LOGICAL :: showkey      ! show key
LOGICAL :: showkeybox   ! show key box
LOGICAL :: cutextrazero ! cut off extra zero in tics labels
LOGICAL :: hidden3d     ! hide backward object
LOGICAL :: printerror   ! if false, just return error-message in "errmsg" and not print
LOGICAL :: autofix      ! fix kplot options and not warn
LOGICAL :: implicitbb   ! USE implicit bounding box generation
LOGICAL :: hiddenbond   ! hide bonds with connecting-points
LOGICAL :: pseudoshade  ! use pseudo-shading technology
LOGICAL :: morecomment  ! validate extra comments in EPS file
LOGICAL :: cutzerobox   ! cut box with value zero in box-plot mode
! ---------------- line, point
! line type, point type
INTEGER            :: bdlt                ! border linetype
INTEGER            :: tclt, mtclt         ! tics   linetype
INTEGER            :: gxlt, gylt          ! grid   linetyle
INTEGER            :: vblt                ! vbar   linetype
! styles
INTEGER            :: stlt, stpt, stcl, stsz    ! global style settings
INTEGER            :: ltnum, ptnum, clnum, ftnum, sznum ! number of style types
DOUBLE PRECISION   :: stylecolors(kpl_stylemax,3)   ! stylecolors
CHARACTER(LEN=kpl_styledescstrlenmax) :: stylelines(kpl_stylemax) ! stylelines
CHARACTER(LEN=kpl_styledescstrlenmax) :: stylepoints(kpl_stylemax) ! stylepoints
CHARACTER(LEN=kpl_styledescstrlenmax) :: stylepoints2(kpl_stylemax) ! stylepoints
CHARACTER(LEN=kpl_styledescstrlenmax) :: stylefonts(kpl_stylemax) ! stylefonts
DOUBLE PRECISION   :: stylesize(kpl_stylemax) ! style (point) size
! line color
DOUBLE PRECISION   :: linc(3), pntc(3)    ! line, point linecolor
DOUBLE PRECISION   :: bdlc(3)             ! border line color
DOUBLE PRECISION   :: tclc(3), mtclc(3)   ! tics   line color
DOUBLE PRECISION   :: gxlc(3), gylc(3)    ! grid   line color
DOUBLE PRECISION   :: vblc(3)             ! vbar   line color
DOUBLE PRECISION   :: edgc(3)             ! edge color (used for pseudoshade)
! line width
DOUBLE PRECISION   :: linw, ptlw          ! line, point linewidth
DOUBLE PRECISION   :: bdlw                ! border linewidth
DOUBLE PRECISION   :: tclw, mtclw         ! tics   linewidth
DOUBLE PRECISION   :: gxlw, gylw          ! grid   linewidth
DOUBLE PRECISION   :: vblw                ! vbar   linewidth
! line length
DOUBLE PRECISION   :: tcll, mtcll         ! tics line length
DOUBLE PRECISION   :: ctll, cmtll         ! color bar tics line length
DOUBLE PRECISION   :: kyll                ! key line length
! size
DOUBLE PRECISION   :: pnts                ! point size
DOUBLE PRECISION   :: ttfs, kyfs          ! title, key  fontsize
DOUBLE PRECISION   :: tcfs, lbfs          ! ticslabel, axislabel fontsize
DOUBLE PRECISION   :: clbx, clby          ! color bar size
! angle
DOUBLE PRECISION   :: angth, angph        ! angles for 3D-plot
! font
INTEGER           :: ttft, kyft          ! title, key fonttype
INTEGER           :: tcft                ! ticslabel fonttype
INTEGER           :: xlft, ylft, y2lft   ! axislabel fonttype
INTEGER           :: vbft                ! vertbar fonttype
DOUBLE PRECISION  :: ttfc(3), kyfc(3)    ! title, key fontcolor
DOUBLE PRECISION  :: tcfc(3)             ! ticslabel font color
DOUBLE PRECISION  :: xlfc(3), ylfc(3), y2lfc(3), zlfc(3)   ! axislabel font color
! hide
DOUBLE PRECISION  :: linh, pnth   ! line hide width, point hide widht
! ---------------- system variables
LOGICAL :: initialized=.FALSE.  ! detect whether kplot variables are initialized yet
CHARACTER(LEN=10)  :: hint(kpl_fnumax) ! INTEGER strings
DOUBLE PRECISION, ALLOCATABLE :: data_for_scan(:)
END MODULE
!%}}}
! MODULE cl_kplot %{{{
MODULE cl_kplot
IMPLICIT NONE
SAVE
DOUBLE PRECISION,PARAMETER:: kpcolor_k(3) = 0.d0 ! black
DOUBLE PRECISION,PARAMETER:: kpcolor_r(3) = (/ 1.d0, 0.d0, 0.d0 /) ! red
DOUBLE PRECISION,PARAMETER:: kpcolor_g(3) = (/ 0.d0, 1.d0, 0.d0 /) ! green
DOUBLE PRECISION,PARAMETER:: kpcolor_b(3) = (/ 0.d0, 0.d0, 1.d0 /) ! blue
DOUBLE PRECISION,PARAMETER:: kpcolor_m(3) = &
& (/ 1.d0/DSQRT(2.d0), 0.d0, 1.d0/DSQRT(2.d0) /) ! magenta
DOUBLE PRECISION, PARAMETER:: kpcolor_c(3) = &
& (/ 0.d0, 1.d0/DSQRT(2.d0), 1.d0/DSQRT(2.d0) /) ! cyan
DOUBLE PRECISION, PARAMETER:: kpcolor_y(3) = &
& (/ 1.d0/DSQRT(2.d0), 1.d0/DSQRT(2.d0), 0.d0 /) ! yellow
DOUBLE PRECISION,PARAMETER:: kpcolor_w(3) = 1.d0
END MODULE
!%}}}

