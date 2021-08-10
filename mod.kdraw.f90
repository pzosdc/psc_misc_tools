! Language    : Fortran90
! Description : modules for KDRAW
! Lastchange  : 2018, Mar. 15, 09:27
! =================================================================

! MODULE cs_kdraw %{{{
MODULE cs_kdraw
  IMPLICIT NONE
  INTRINSIC :: DSIN, DCOS, DTAN, DATAN
  INTRINSIC :: DLOG, dlog10, DEXP
  INTRINSIC :: DBLE, INT, NINT, FLOAT, CEILING
  INTRINSIC :: LEN, LEN_TRIM, ADJUSTL, TRIM
  INTRINSIC :: ACHAR, IACHAR
  SAVE
  CHARACTER( LEN= 20 ), PARAMETER :: kdraw_author = "XXXXXXXXX"
  CHARACTER( LEN= 20 ), PARAMETER :: kdraw_version = "1.1"
  CHARACTER( LEN= 20 ), PARAMETER :: kdraw_version_accept(1) = (/ "1.1" /)
  CHARACTER( LEN= 20 ), PARAMETER :: kdraw_update = "2018, Mar. 15"
  INTEGER, PARAMETER :: kdraw_stackmax = 100
  INTEGER, PARAMETER :: kdraw_strlenmax = 500
  INTEGER, PARAMETER :: kdraw_layersmax = 10
END MODULE
!%}}}
! MODULE cv_kdraw %{{{
MODULE cv_kdraw
  USE cs_kdraw
  IMPLICIT NONE
  SAVE
  ! plot parameters
  CHARACTER( LEN= 2 ) :: pap
  CHARACTER( LEN= 2 ) :: rev
  CHARACTER( LEN= 7 ) :: bgcol, fgcol
  ! layers
  INTEGER :: currentlayerid
  CHARACTER( LEN= kdraw_strlenmax ) :: layernamelist(0:kdraw_layersmax)
  LOGICAL :: layerdisplay(0:kdraw_layersmax)
  !
  DOUBLE PRECISION :: papx, papy
  DOUBLE PRECISION :: marx, mary
  INTEGER :: grdx, grdy
  DOUBLE PRECISION :: arrowdepth, arrowwidth, arrowthick
  DOUBLE PRECISION :: arrowback, arrowturn, arrowkern
  DOUBLE PRECISION :: circleradius
  DOUBLE PRECISION :: fontsize
  DOUBLE PRECISION :: stroke_rgb(3)
  DOUBLE PRECISION :: fill_rgb(3)
  CHARACTER( LEN= kdraw_strlenmax ) :: fontname
  DOUBLE PRECISION :: linewidth
  INTEGER( KIND= SELECTED_INT_KIND(1) ) :: linecap
  INTEGER( KIND= SELECTED_INT_KIND(1) ) :: linejoin
  !
  ! file I/O
  CHARACTER( LEN= 1 ) :: cchar
  CHARACTER( LEN= kdraw_strlenmax ) :: cword
  INTEGER :: cwordlen
  CHARACTER( LEN= kdraw_strlenmax ) :: ccomd
  LOGICAL :: end_word, clear_stack, comment
  INTEGER( KIND= 4 ) :: ilevel
  ! stack list
  DOUBLE PRECISION :: rstack(kdraw_stackmax)
  CHARACTER( LEN= kdraw_strlenmax ) :: cstack(kdraw_stackmax)
  LOGICAL :: fstack(kdraw_stackmax)
  INTEGER( KIND= 4 ) :: rsmax, csmax, fsmax
  !
  LOGICAL :: version_accepted
  LOGICAL :: warn_unknowncommand
  LOGICAL :: warn_unknownlayername
  LOGICAL :: warn_insufficientstack
  !
  LOGICAL :: flaga, flagb
  INTEGER( KIND= 4 ) :: iios
  INTEGER( KIND= 8 ) :: irec
  INTEGER( KIND= 4 ) :: iread, iwrite
END MODULE
!%}}}

