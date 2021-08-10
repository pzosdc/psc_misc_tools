! Language    : Fortran90
! Description : major subroutines for PSC
! Lastchange  : 2018, Nov. 16, 14:41
! ===================================================================

! basic operations
! SUBROUTINE psc_clean %{{{
SUBROUTINE psc_clean
! clean up all complicated situations before end PSC
USE cv_psc
IMPLICIT NONE
INTEGER :: idoa
DO idoa = iinnow, 1, -1
  CLOSE( dev_in( idoa ) % devnum )
END DO
DO idoa = iounow, 1, -1
  CLOSE( dev_ou( idoa ) % devnum )
END DO
DO idoa = ildnow, 1, -1
  CLOSE( dev_ld( idoa ) % devnum )
END DO
IF( ALLOCATED( prime     ) ) DEALLOCATE( prime     )
RETURN
END SUBROUTINE
!%}}}
! SUBROUTINE psc_error(command,inum) %{{{
SUBROUTINE psc_error(command,inum)
USE cv_psc
IMPLICIT NONE
CHARACTER( LEN= psc_strlen ),INTENT( IN ) :: command
INTEGER,INTENT( IN ) :: inum
CHARACTER( LEN= 50 ) :: stmpa
INTEGER :: idoa

IF(.NOT.error_quiet)THEN
  WRITE(*,'(A$)') ' '
  CALL psc_printcolor( cl_error )
  WRITE(*,'(A$)') 'PSC Error:'
  CALL psc_printcolor( cl_reset )
  IF( ildnow > 0 )THEN
    WRITE(*,'(A$)') ' in file "'//TRIM(dev_ld(ildnow)%tag)//'": '
    WRITE(*,'(A,I6,A$)') ' line ', codepoint(ildnow) ,': '
  END IF
  WRITE(*,'(A$)') ' command "'//TRIM(command)//'": '
END IF

WRITE(stmpa,'(I2.2)') inum
IF(.NOT.error_quiet)THEN
  IF(inum<100)THEN
    WRITE(*,'(A)') 'E'//TRIM(stmpa)//': '//TRIM(errmsg(inum))
  ELSE
    WRITE(*,'(A,I3)') 'PSC Fatal Error ',inum
    WRITE(*,'(A$)') ' '
    CALL psc_printcolor( cl_fatalerror )
    WRITE(*,'(A$)') 'Fatal Error'
    CALL psc_printcolor( cl_reset )
    WRITE(*,'(A)') ' Please inform PSC-author when you observed this message'
    WRITE(*,'(A)') ' Author: '//TRIM(pscauthor)//', '//TRIM(pscupdate)
  END IF
END IF

IF( exec_mode == 1 )THEN
  ! return to normal mode if in command-help mode
  exec_mode = 0
END IF
IF( .NOT. error_cont )THEN
  ! force exit load
  IF( ildnow > 0 )THEN
    DO idoa = ildnow, 1, -1
      IF( .NOT. error_quiet )THEN
        CALL psc_printcolor( cl_error )
        WRITE(*,'(A$)') ' Load from file "'//TRIM(dev_ld(idoa)%tag)//'" suspended'
        CALL psc_printcolor( cl_reset )
        WRITE(*,'(A)')
      END IF
      IF( dev_ld(idoa)%devnum /= dev_stdin%devnum ) THEN
        CLOSE( dev_ld(idoa)%devnum )
      END IF
    END DO
    ildnow = 0
  END IF
  ! throw away current buffer
  IF( bufin /= '' )THEN
    bufin = ''
    IF( .NOT. error_quiet )THEN
      CALL psc_printcolor( cl_error )
      WRITE(*,'(A$)') ' Line input buffer cleared'
      CALL psc_printcolor( cl_reset )
      WRITE(*,'(A)')
    END IF
  END IF
END IF
RETURN
END SUBROUTINE
!%}}}
! SUBROUTINE psc_inform(msgstrin) %{{{
SUBROUTINE psc_inform(msgstrin)
USE cv_psc
IMPLICIT NONE
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: msgstrin
IF( quiet       ) RETURN
IF( super_quiet ) RETURN
IF( file_exec   ) RETURN
IF( script_exec ) RETURN
IF( file_pexe   ) RETURN
IF( script_pexe ) RETURN
WRITE(*,*) TRIM( msgstrin )
RETURN
END SUBROUTINE
!%}}}
! SUBROUTINE psc_cmdhelp %{{{
SUBROUTINE psc_cmdhelp
USE cv_psc
IMPLICIT NONE
INTEGER :: idoa
WRITE(*,'(A$)') '(' // TRIM ( ADJUSTL ( maintag_comm ) ) // ')'
DO idoa = 1 , psc_maxtag
  IF ( subtag_comm(idoa) == '' ) EXIT
  WRITE(*,'(A$)') ', (' // TRIM ( ADJUSTL ( subtag_comm(idoa) ) ) // ')'
END DO
WRITE(*,*)
IF( usage_comm /= '' )THEN
  WRITE(*,'(A)') '[usage] ' // TRIM ( ADJUSTL ( usage_comm ) )
END IF
IF( help_comm /= '' )THEN
  WRITE(*,'(A)') '[description] ' // TRIM ( ADJUSTL ( help_comm ) )
END IF
exec_mode = 0
RETURN
END SUBROUTINE
!%}}}

! stack operations
! SUBROUTINE psc_stack_append %{{{
SUBROUTINE psc_stack_append
USE cv_psc
IMPLICIT NONE
! max対策
IF( stz >= stackmax )THEN
  IF( error_cont )THEN
    ! cut extra-stack values automatically
    ! [TODO] 要修正
    stz = stackmax - 1
    iferror = .FALSE.
  ELSE
    ! error stop
    CALL psc_error( cword, 6 )
    ! 元プログラムでcycleする判定に使う
    iferror = .TRUE.
    RETURN
  END IF
END IF
stz = stz + 1
stackchanged = .TRUE.
RETURN
END SUBROUTINE
!%}}}
! SUBROUTINE psc_stack_remove %{{{
SUBROUTINE psc_stack_remove
USE cv_psc
IMPLICIT NONE
stz = stz - 1
stackchanged = .TRUE.
RETURN
END SUBROUTINE
!%}}}
! SUBROUTINE psc_strst_append %{{{
SUBROUTINE psc_strst_append
USE cv_psc
IMPLICIT NONE
IF( macrosize >= macromax )THEN
  IF( error_cont )THEN
    macrosize = macromax - 1
    iferror = .FALSE.
  ELSE
    CALL psc_error( cword, 7 )
    ! 元プログラムでcycleする判定に使う
    iferror = .TRUE.
    RETURN
  END IF
END IF
macro(2:macrosize+1) = macro(:macrosize)
macrosize = macrosize + 1
RETURN
END SUBROUTINE
!%}}}
! SUBROUTINE psc_strst_remove %{{{
SUBROUTINE psc_strst_remove
USE cv_psc
IMPLICIT NONE
macro(:macrosize-1) = macro(2:macrosize)
macrosize = macrosize - 1
RETURN
END SUBROUTINE
!%}}}

! buffer operations
! SUBROUTINE psc_buf_readnext %{{{
SUBROUTINE psc_buf_readnext
! read next buffer as 'bufin'
USE cv_psc
IMPLICIT NONE
INTEGER :: ierror
DO
  ! show prompt
  IF( .NOT. super_quiet )THEN
    IF((dev_ld(ildnow)%tag == dev_stdin%tag).OR.(.NOT.quiet_load))THEN
      ! show prompt
      CALL psc_printcolor( cl_prompt )
      CALL psc_printprompt
      CALL psc_printcolor( cl_reset )
      WRITE(*,'(A$)') ' '
      ! end prompt
    END IF
  END IF
  ! read buffer string
  IF( dev_ld(ildnow) % devnum == 0 )THEN
    READ( *, '(A)', IOSTAT= ierror ) bufin
  ELSE
    READ( dev_ld(ildnow)%devnum, '(A)', IOSTAT= ierror ) bufin
  END IF
  IF( ierror > 0 )THEN
    ! fatal error
    WRITE(0,'(A)') ' Fatal Error while reading file "' &
    & // TRIM(dev_ld(ildnow)%tag) // '"'
  ELSE IF( ierror < 0 )THEN
    ! EOF
    CLOSE( dev_ld(ildnow)%devnum )
    ! Note: ここの処理はdebugフラグで指定すべき？
    IF( (.NOT.super_quiet) .AND. (.NOT.quiet_load) )THEN
      WRITE(*,*)
      WRITE(*,*) 'Load Done Successfully from file "' &
      & // TRIM(dev_ld(ildnow)%tag) // '"'
    END IF
    ildnow = ildnow - 1
    IF( ildnow == 0 )THEN
      IF( file_exec )STOP
      IF( file_pexe .AND. script_exec )THEN
        file_pexe = .FALSE.
        bufin = script
        EXIT
      END IF
      IF( script_exec ) STOP
    END IF
    ! load original buffer (= buffer string before using 'load' command)
    bufin = holdbuffer(ildnow)
  ELSE
    ! success read (but may be NULL string)
    codepoint(ildnow) = codepoint(ildnow) + 1
    IF( .NOT. super_quiet )THEN
      IF( (ildnow>0) .AND. (.NOT.quiet_load) )THEN
        CALL psc_printcolor( cl_loadcom )
        WRITE(*,'(A)') TRIM(bufin)
        CALL psc_printcolor( cl_reset )
      END IF
    END IF
  END IF
  ! cycle if NULL
  IF( bufin /= '' ) EXIT
END DO
RETURN
END SUBROUTINE
!%}}}
! SUBROUTINE psc_buf_rmtopsep %{{{
SUBROUTINE psc_buf_rmtopsep
! remove top separators on current buffer
USE cv_psc
IMPLICIT NONE
INTEGER :: inta
INTEGER :: itmpa
DO
  inta = VERIFY( bufin, ' ;,'//ACHAR(10)//ACHAR(9) )
  IF( inta == 0 ) inta = LEN_TRIM( bufin ) + 1
  IF( inta > 1 )THEN
    bufin(1:inta-1) = ''
    bufin = ADJUSTL( bufin )
  ENDIF
  ! remove comment
  IF( ( bufin(1:1) == '"' ) .OR. ( bufin(1:1) == "'" )  )THEN
    ! Comment-out (till newline if exist)
    itmpa = INDEX( bufin, ACHAR(10) )
    IF( itmpa > 0 )THEN
      ! remove comment
      bufin(1:itmpa) = ''
      bufin = ADJUSTL( bufin )
    ELSE
      bufin = ''
      EXIT
    END IF
  ELSE
    EXIT
  END IF
END DO
RETURN
END SUBROUTINE
!%}}}
! SUBROUTINE psc_buf_gettoklen %{{{
SUBROUTINE psc_buf_gettoklen
! get first token length on current buffer
! return as 'wlen'
! return negative if the token not end yet
USE cv_psc
IMPLICIT NONE
CHARACTER( LEN= psc_strlen ) :: stmpa
INTEGER :: itmpa

IF(bufin(1:1)=='\')THEN
  ! SPECIAL token start from Backslash
  ! 1文字目がバックスラッシュなら
  ! もう一度バックスラッシュまたは改行文字またはタブ文字が出現するまで待つ
  ! 出現しない場合は全てを１つのトークンとみなす
  ! この機能は記号コーディングに使用する
  IF( SCAN(bufin(2:2), '0123456789' // &
  &'abcdefghijklmnopqrstuvwxyz'//&
  &'ABCDEFGHIJKLMNOPQRSTUVWXYZ' ) == 1 )THEN
    ! [a-zA-Z] comes after \ will be regarded as string command
    ! string can also include _ after its first character
    itmpa = 1+VERIFY(bufin(2:),'0123456789'//&
    &'abcdefghijklmnopqrstuvwxyz'//&
    &'ABCDEFGHIJKLMNOPQRSTUVWXYZ'//'_')
    bufin(itmpa+1:) = bufin(itmpa:)
    bufin(1:1) = '('
    bufin(itmpa:itmpa) = ')'
    wlen = itmpa
    RETURN
  END IF
  stmpa = bufin
  stmpa(1:1) = ''
  itmpa = SCAN(stmpa,'\'//ACHAR(10)//ACHAR(9))
  IF(itmpa==0)THEN
    wlen = LEN_TRIM(bufin)
    RETURN
  ELSE IF(itmpa==2)THEN
    ! 連続して２つのバックスラッシュが現れた場合に限りその２文字を1つのトークンと見なす
    ! これによって記号コーディングモードを終了できる
    wlen = 2
    RETURN
  ELSE
    wlen = itmpa-1
    RETURN
  END IF
END IF
! ==================== 区切り文字の後のトークン区切りの判定
! 最初の文字が特殊文字の場合は特殊コマンドの終了位置をトークン区切りにする
IF(SCAN(bufin(1:1),'={}[]<>!/%^@&|*#')>0)THEN
  wlen = 1
  IF(bufin(1:2)=='==')wlen=2
  IF(bufin(1:3)=='===')wlen=3
  IF(bufin(1:2)=='[[')wlen=2
  IF(bufin(1:2)==']]')wlen=2
  IF(bufin(1:2)=='**')wlen=2
  IF(bufin(1:1)=='#')THEN
    IF(bufin(1:2)=='##')THEN
      wlen = 2
    ELSE
      ! detect color with HTML-format (like '#00ff00' , '#0f0')
      itmpa = VERIFY(bufin(2:7),'0123456789abcdefABCDEF')
      !         1 2 3 4 5 6 7
      !         # 0 0 f f 0 0
      IF(itmpa==0)THEN
        wlen = 7
      ELSE IF(itmpa>=4)THEN
        wlen = 4
      END IF
    END IF
  END IF
  RETURN
END IF
IF(SCAN(bufin(1:1),'+-')>0)THEN
  ! '+'または'-'の場合はそのあとに数が続く場合負の実数と解釈することに注意
  IF(SCAN(bufin(2:2),'0123456789')==0)THEN
    wlen = 1
    RETURN
  END IF
END IF
! ==================== 区切り文字の前のトークン区切りの判定
! allow character ';', ',', ' ', NL and tab for separators
! separate before special token even if not separated by separators
! アンダースコアをトークン区切りにするとdebug_bufinなどが区切られてしまう
wlen = LEN_TRIM(bufin)
itmpa = SCAN(bufin,';, \=({}[]<>!/%^@&|*#"'//ACHAR(10)//ACHAR(9))
IF(itmpa>1)THEN
  wlen = itmpa-1
  ! does not return here
END IF
! '-'と'+'は実数の指数部の場合もあるので注意
itmpa = INDEX(bufin,'+')
IF((itmpa>1).AND.(itmpa-1<wlen))THEN
  IF(SCAN(bufin(itmpa-1:itmpa-1),'leEdD')==0)THEN
    wlen = itmpa-1
  END IF
END IF
itmpa = INDEX(bufin,'-')
IF((itmpa>1).AND.(itmpa-1<wlen))THEN
  IF(SCAN(bufin(itmpa-1:itmpa-1),'leEdD')==0)THEN
    wlen = itmpa-1
  END IF
END IF
RETURN
END SUBROUTINE
!%}}}
! SUBROUTINE psc_buf_getstrlen %{{{
SUBROUTINE psc_buf_getstrlen
! detect string length
! bufin : current buffer (begin from '(')
! wlen : string length (negative for the case string does not end yet)
USE cv_psc
IMPLICIT NONE
LOGICAL :: comment_now
LOGICAL :: escape_now
INTEGER :: clevel
INTEGER :: idoa, iamax
CHARACTER( LEN= 1 ) :: ctmpa

! init
comment_now = .FALSE.
escape_now = .FALSE.
clevel = 1
iamax = LEN_TRIM(bufin)
! start
idoa = 2
DO
  ctmpa = bufin(idoa:idoa)
  IF(comment_now)THEN
    IF(bufin(idoa:idoa+1)=='\\')THEN
      idoa = idoa+2
      comment_now= .FALSE.
      CYCLE
    ELSE IF(bufin(idoa:idoa)==ACHAR(10))THEN
      comment_now= .FALSE.
    END IF
  ELSE IF(escape_now)THEN
    IF(bufin(idoa:idoa+1)=='\\')THEN
      idoa = idoa+2
      escape_now= .FALSE.
      CYCLE
    ELSE IF(bufin(idoa:idoa)==ACHAR(10))THEN
      escape_now= .FALSE.
    END IF
  ELSE
    IF(ctmpa==')')clevel=clevel-1
    IF(ctmpa=='(')clevel=clevel+1
    IF(clevel == 0)THEN
      wlen = idoa
      RETURN
    END IF
    IF((ctmpa=="'").OR.(ctmpa=='"'))comment_now=.TRUE.
    IF(ctmpa=='\')THEN
      IF(bufin(idoa:idoa+1)=='\\')THEN
        idoa = idoa+2
        CYCLE
      END IF
      escape_now= .TRUE.
    END IF
  END IF
  idoa = idoa+1
  IF(idoa>LEN_TRIM(bufin))EXIT
END DO
wlen = -clevel
RETURN
END SUBROUTINE
!%}}}

! printing
! SUBROUTINE psc_printcolor( colorin ) %{{{
SUBROUTINE psc_printcolor( colorin )
USE cv_psc, ONLY: color
IMPLICIT NONE
INTEGER( KIND= 4 ), INTENT( IN ) :: colorin(6)
CHARACTER( LEN= 4 ) :: r_fg, g_fg, b_fg
CHARACTER( LEN= 4 ) :: r_bg, g_bg, b_bg
CHARACTER( LEN= 50 ) :: fgstr
CHARACTER( LEN= 50 ) :: bgstr

IF( .NOT. color ) RETURN
WRITE( r_fg, '(I4)' ) colorin(1)
WRITE( g_fg, '(I4)' ) colorin(2)
WRITE( b_fg, '(I4)' ) colorin(3)
WRITE( r_bg, '(I4)' ) colorin(4)
WRITE( g_bg, '(I4)' ) colorin(5)
WRITE( b_bg, '(I4)' ) colorin(6)
r_fg = TRIM( ADJUSTL( r_fg ) ) // ';'
g_fg = TRIM( ADJUSTL( g_fg ) ) // ';'
b_fg = TRIM( ADJUSTL( b_fg ) )
r_bg = TRIM( ADJUSTL( r_bg ) ) // ';'
g_bg = TRIM( ADJUSTL( g_bg ) ) // ';'
b_bg = TRIM( ADJUSTL( b_bg ) )
fgstr = '[38;2;' // TRIM( r_fg ) // TRIM( g_fg ) // TRIM( b_fg ) // 'm'
bgstr = '[48;2;' // TRIM( r_bg ) // TRIM( g_bg ) // TRIM( b_bg ) // 'm'

WRITE( *, '(A$)' ) ACHAR(27) // '[m'

IF( ALL( colorin(1:3) >= 0 ) .AND. ALL( colorin(1:3) <= 255 ) )&
& WRITE( *, '(A$)' ) ACHAR(27) // TRIM( fgstr )
IF( ALL( colorin(4:6) >= 0 ) .AND. ALL( colorin(4:6) <= 255 ) )&
& WRITE( *, '(A$)' ) ACHAR(27) // TRIM( bgstr )

RETURN
END SUBROUTINE
!%}}}
! SUBROUTINE psc_printprompt %{{{
SUBROUTINE psc_printprompt
USE cv_psc
IMPLICIT NONE
CHARACTER( LEN= 100 ) :: stmpa, stmpb
IF( bufin == '' )THEN
  ! PS1
  stmpa = prompt_normal
ELSE
  ! PS2
  stmpa = prompt_special
END IF
DO
  IF( stmpa == '' ) EXIT
  IF( stmpa(1:2) == '/r' )THEN
    ! 実数型スタックサイズに置換
    WRITE( stmpb, * ) stz
    stmpb = ADJUSTL( stmpb )
    WRITE( *, '(A$)' ) TRIM( stmpb )
    stmpa = stmpa(3:)
  ELSE IF( stmpa(1:2) == '/s' )THEN
    ! 文字列型スタックサイズに置換
    WRITE( stmpb, * ) macrosize
    stmpb = ADJUSTL( stmpb )
    WRITE( *, '(A$)' ) TRIM( stmpb )
    stmpa = stmpa(3:)
  ELSE
    WRITE( *, '(A1$)' ) stmpa(1:1)
    stmpa = stmpa(2:)
  END IF
END DO
!DO idoa=1,loclev
!  WRITE(*,'(A$)') "'"
!END DO
RETURN
END SUBROUTINE
!%}}}
! SUBROUTINE psc_dispstacklist() %{{{
SUBROUTINE psc_dispstacklist()
USE cv_psc
IMPLICIT NONE
INTEGER( KIND = 4) :: idoa
CHARACTER( LEN = psc_strlen ) :: stmpa
CALL psc_printcolor( cl_listcom )
WRITE(*,'(A23$)') TRIM(cword) // ' '
CALL psc_printcolor( cl_liststack )
DO idoa = 1, stz
  CALL psc_printnum(stack(idoa), fmt_a, stmpa, &
  & dev_stdou%devnum, .FALSE., sepstr)
END DO
CALL psc_printcolor( cl_reset )
WRITE(*,*)
RETURN
END SUBROUTINE
!%}}}
! SUBROUTINE psc_printnum(realin,fmtstr,strout,intfout,use_nl,sepstr) %{{{
SUBROUTINE psc_printnum(realin,fmtstr,strout,intfout,use_nl,sepstr)
USE cs_psc, ONLY: psc_strlen, psc_wordlen
IMPLICIT NONE
! function
CHARACTER( LEN= 100 ), EXTERNAL :: hexwrite
CHARACTER( LEN= 100 ), EXTERNAL :: basewrite
! arguments
DOUBLE PRECISION, INTENT( IN ) :: realin
CHARACTER( LEN= * ), INTENT( IN ) :: fmtstr
CHARACTER( LEN= psc_strlen ), INTENT( OUT ) :: strout
INTEGER( KIND= 4 ), INTENT( IN ) :: intfout
LOGICAL, INTENT( IN ) :: use_nl
CHARACTER( LEN= psc_wordlen ), INTENT( IN ) :: sepstr
! local variables
CHARACTER( LEN= 100 ) :: fmtbase, width, fmtfull
CHARACTER( LEN= 100 ) :: fmtdefault
CHARACTER( LEN= 100 ) :: stmpa, stmpb
INTEGER( KIND= 4 ) :: iround
INTEGER( KIND= 4 ) :: itmpa, itmpb
INTEGER( KIND= 4 ) :: idoa
INTEGER( KIND= 4 ) :: ibase
INTEGER( KIND= 4 ) :: icap
LOGICAL :: cutexzero

cutexzero = .TRUE.
fmtdefault = 'g'
width = ''
iround = NINT( realin )
icap = 0
! detect width info %{{{
itmpa = SCAN( fmtstr, '0123456789.' )
IF( itmpa > 0 )THEN
  ! 注意: 指数部の桁数の指定に'Ew.dEe'のようにEを使用することがある
  itmpb = itmpa - 1 + VERIFY( fmtstr(itmpa:), '0123456789.E' )
  width = fmtstr(itmpa:itmpb-1)
END IF
!%}}}
! detect output format %{{{
IF     ( INDEX( fmtstr, 'auto'  ) > 0 )THEN ! auto
  fmtbase = fmtdefault
ELSE IF( INDEX( fmtstr, 'logic' ) > 0 )THEN ! logic (= true or false)
  fmtbase = 'logic'
ELSE IF( INDEX( fmtstr, 'Logic' ) > 0 )THEN ! Logic (= True or False)
  fmtbase = 'logic'
  icap = 1
ELSE IF( INDEX( fmtstr, 'LOGIC' ) > 0 )THEN ! LOGIC (= TRUE or FALSE)
  fmtbase = 'logic'
  icap = 2
ELSE IF( INDEX( fmtstr, 'roman' ) > 0 )THEN ! roman (using [ivxlcdm])
  fmtbase = 'roman'
ELSE IF( INDEX( fmtstr, 'ROMAN' ) > 0 )THEN ! ROMAN (using [IVXLCDM])
  fmtbase = 'roman'
  icap = 1
ELSE IF( INDEX( fmtstr, 'base' ) > 0 )THEN ! base
  fmtbase = 'base'
  itmpa = INDEX( width, '.' )
  IF( itmpa > 0 )THEN
    READ( width(1:itmpa-1), * ) ibase
    width = width(itmpa+1:)
  ELSE
    READ( width, * ) ibase
    width = ''
  END IF
ELSE IF( INDEX( fmtstr, 'BASE' ) > 0 )THEN ! BASE
  fmtbase = 'base'
  itmpa = INDEX( width, '.' )
  IF( itmpa > 0 )THEN
    READ( width(1:itmpa-1), * ) ibase
    width = width(itmpa+1:)
  ELSE
    READ( width, * ) ibase
    width = ''
  END IF
  icap = 1
ELSE IF( SCAN( fmtstr,  'l' ) > 0 )THEN ! l format (logical= t or f)
  fmtbase = 'l'
ELSE IF( SCAN( fmtstr,  'L' ) > 0 )THEN ! L format (logical= T or F)
  fmtbase = 'l'
  icap = 1
ELSE IF( SCAN( fmtstr, 'fF' ) > 0 )THEN ! F format (fixed point)
  fmtbase = 'f'
ELSE IF( SCAN( fmtstr, 'gG' ) > 0 )THEN ! G format
  fmtbase = 'g'
ELSE IF( SCAN( fmtstr, 'nN' ) > 0 )THEN ! EN format (Engineering)
  fmtbase = 'en'
ELSE IF( SCAN( fmtstr, 'sS' ) > 0 )THEN ! ES format (Science)
  fmtbase = 'es'
ELSE IF( SCAN( fmtstr, 'eE' ) > 0 )THEN ! E format
  fmtbase = 'e'
ELSE IF( SCAN( fmtstr, 'dD' ) > 0 )THEN ! D format
  fmtbase = 'd'
ELSE IF( SCAN( fmtstr, 'iI' ) > 0 )THEN ! I format (integer using [-0-9])
  fmtbase = 'i'
ELSE IF( SCAN( fmtstr,  'z' ) > 0 )THEN ! z format (hex using [0-9a-f])
  fmtbase = 'z'
ELSE IF( SCAN( fmtstr,  'Z' ) > 0 )THEN ! Z format (Hex using [0-9A-F])
  fmtbase = 'z'
  icap = 1
ELSE IF( SCAN( fmtstr, 'bB' ) > 0 )THEN ! B format (bin using [01])
  fmtbase = 'b'
ELSE IF( SCAN( fmtstr, 'oO' ) > 0 )THEN ! O format (octal using [0-7])
  fmtbase = 'o'
ELSE
  fmtbase = fmtdefault
END IF
!%}}}
fmtfull = '(' // TRIM( fmtbase ) // TRIM( width ) // ',")")'
! start printing (long if)
IF( .FALSE. )THEN
! logic %{{{
ELSE IF( fmtbase == 'logic' )THEN
  ! logic format
  IF( iround /= 0 )THEN
    SELECT CASE( icap )
      CASE( 0 ) ; stmpa = 'true'
      CASE( 1 ) ; stmpa = 'True'
      CASE( 2 ) ; stmpa = 'TRUE'
    END SELECT
  ELSE
    SELECT CASE( icap )
      CASE( 0 ) ; stmpa = 'false'
      CASE( 1 ) ; stmpa = 'False'
      CASE( 2 ) ; stmpa = 'FALSE'
    END SELECT
  END IF
  IF( width /= '' )THEN
    WRITE(strout,'(A'//TRIM(width)//'$)') TRIM(stmpa) // ')'
  ELSE
    strout = TRIM(stmpa) // ')'
  END IF
!%}}}
! roman %{{{
ELSE IF( fmtbase == 'roman' )THEN
  ! roman format
  ! Note: here is a list of roman notations
  !          i :    1
  !          v :    5
  !          x :   10
  !          l :   50
  !          c :  100
  !          d :  500
  !          m : 1000
  SELECT CASE( icap )
    CASE( 0 ) ; stmpb = 'ivxlcdm'
    CASE( 1 ) ; stmpb = 'IVXLCDM'
  END SELECT
  IF( iround <= 0 .OR. iround >= 4000 )THEN
    ! Note: the longest expression is 'mmmcccxxxiii'
    stmpa = '************'
  ELSE
    stmpa = ''
    DO idoa = 1, iround / 1000
      stmpa = TRIM( stmpa ) // stmpb(7:7)
    END DO
    iround = MOD( iround, 1000 )
    !------------------------------
    IF( iround >= 900 )THEN
      stmpa = TRIM( stmpa ) // stmpb(5:5) // stmpb(7:7)
      iround = iround - 900
    ELSE
      IF( iround >= 500 )THEN
        stmpa = TRIM( stmpa ) // stmpb(6:6)
        iround = iround - 500
      ELSE IF( iround >= 400 )THEN
        stmpa = TRIM( stmpa ) // stmpb(5:5) // stmpb(6:6)
        iround = iround - 400
      END IF
      DO idoa = 1, iround / 100
        stmpa = TRIM( stmpa ) // stmpb(5:5)
      END DO
      iround = MOD( iround, 100 )
    END IF
    !------------------------------
    IF( iround >= 90 )THEN
      stmpa = TRIM( stmpa ) // stmpb(3:3) // stmpb(5:5)
      iround = iround - 90
    ELSE
      IF( iround >= 50 )THEN
        stmpa = TRIM( stmpa ) // stmpb(4:4)
        iround = iround - 50
      ELSE IF( iround >= 40 )THEN
        stmpa = TRIM( stmpa ) // stmpb(3:3) // stmpb(4:4)
        iround = iround - 40
      END IF
      DO idoa = 1, iround / 10
        stmpa = TRIM( stmpa ) // stmpb(3:3)
      END DO
      iround = MOD( iround, 10 )
    END IF
    !------------------------------
    IF( iround == 9 )THEN
      stmpa = TRIM( stmpa ) // stmpb(1:1) // stmpb(3:3)
      iround = iround - 9
    ELSE
      IF( iround >= 5 )THEN
        stmpa = TRIM( stmpa ) // stmpb(2:2)
        iround = iround - 5
      ELSEIF( iround == 4 )THEN
        stmpa = TRIM( stmpa ) // stmpb(1:1) // stmpb(2:2)
        iround = iround - 4
      END IF
      DO idoa = 1, iround
        stmpa = TRIM( stmpa ) // stmpb(1:1)
      END DO
    END IF
    !------------------------------
  END IF
  IF( width /= '' )THEN
    WRITE(strout,'(A'//TRIM(width)//'$)') TRIM(stmpa) // ')'
  ELSE
    strout = TRIM(stmpa) // ')'
  END IF
!%}}}
! base %{{{
ELSE IF( fmtbase == 'base' )THEN
  ! base format
  IF( ibase > 36 )THEN
    stmpa = '************'
  ELSE
    IF( icap == 1 )THEN
      stmpa = basewrite( ibase, iround, .TRUE. )
    ELSE
      stmpa = basewrite( ibase, iround, .FALSE. )
    END IF
    IF( width /= '' )THEN
      WRITE(strout,'(A'//TRIM(width)//'$)') TRIM(stmpa) // ')'
    ELSE
      strout = ADJUSTL( stmpa ) // ')'
    END IF
  END IF
!%}}}
! l %{{{
ELSE IF( fmtbase == 'l' )THEN
  ! fortran-like 'l' format
  IF( iround /= 0 )THEN
    SELECT CASE( icap )
      CASE( 0 ) ; stmpa = 't'
      CASE( 1 ) ; stmpa = 'T'
    END SELECT
  ELSE
    SELECT CASE( icap )
      CASE( 0 ) ; stmpa = 'f'
      CASE( 1 ) ; stmpa = 'F'
    END SELECT
  END IF
  IF( width /= '' )THEN
    WRITE(strout,'(A'//TRIM(width)//'$)') TRIM(stmpa) // ')'
  ELSE
    strout = TRIM(stmpa) // ')'
  END IF
!%}}}
! f %{{{
ELSE IF( fmtbase == 'f' )THEN
  ! fortran 'f' format
  IF( width /= '' )THEN
    WRITE(strout,fmtfull) realin
  ELSE
    WRITE(stmpa,'(F32.15)') realin
    strout = ADJUSTL(stmpa)
    IF(cutexzero) CALL psc_cut_extra_zero(strout)
    strout = TRIM(strout) // ')'
  END IF
!%}}}
! g %{{{
ELSE IF( fmtbase == 'g' )THEN
  ! fortran 'g' format
  IF( width /= '' )THEN
    WRITE(strout,fmtfull) realin
  ELSE
    WRITE(stmpa,'(G32.15)') realin
    strout = ADJUSTL(stmpa)
    IF(cutexzero) CALL psc_cut_extra_zero(strout)
    strout = TRIM(strout) // ')'
  END IF
!%}}}
! en %{{{
ELSE IF( fmtbase == 'en' )THEN
  ! fortran 'en' format
  IF( width /= '' )THEN
    WRITE(strout,fmtfull) realin
  ELSE
    WRITE(stmpa,'(EN32.15)') realin
    strout = ADJUSTL(stmpa)
    IF(cutexzero) CALL psc_cut_extra_zero(strout)
    strout = TRIM(strout) // ')'
  END IF
!%}}}
! es %{{{
ELSE IF( fmtbase == 'es' )THEN
  ! fortran 'es' format
  IF( width /= '' )THEN
    WRITE(strout,fmtfull) realin
  ELSE
    WRITE(stmpa,'(ES32.15)') realin
    strout = ADJUSTL(stmpa)
    IF(cutexzero) CALL psc_cut_extra_zero(strout)
    strout = TRIM(strout) // ')'
  END IF
!%}}}
! e %{{{
ELSE IF( fmtbase == 'e' )THEN
  ! fortran 'e' format
  IF( width /= '' )THEN
    WRITE(strout,fmtfull) realin
  ELSE
    WRITE(stmpa,'(E32.15)') realin
    strout = ADJUSTL(stmpa)
    IF(cutexzero) CALL psc_cut_extra_zero(strout)
    strout = TRIM(strout) // ')'
  END IF
!%}}}
! d %{{{
ELSE IF( fmtbase == 'd' )THEN
  ! fortran 'd' format
  IF( width /= '' )THEN
    WRITE(strout,fmtfull) realin
  ELSE
    WRITE(stmpa,'(D32.15)') realin
    strout = ADJUSTL(stmpa)
    IF(cutexzero) CALL psc_cut_extra_zero(strout)
    strout = TRIM(strout) // ')'
  END IF
!%}}}
! i %{{{
ELSE IF( fmtbase == 'i' )THEN
  ! fortran 'i' format
  ! Note:  -(2.15*10^(9)) ~ (2.15*10^(9)) -> need only 10+1 digits
  !        ('i' format uses minus mark to describe negative integer)
  IF( width /= '' )THEN
    WRITE(strout,fmtfull) iround
  ELSE
    WRITE(stmpa,'(I12)') iround
    strout = ADJUSTL(stmpa)
    strout = TRIM(strout) // ')'
  END IF
!%}}}
! z %{{{
ELSE IF( fmtbase == 'z' )THEN
  ! fortran 'z' format
  ! Note:  -(8*16^(7)) ~ (8*16^(7)-1) -> need only 8 digits
  IF( icap == 1 )THEN
    stmpa = basewrite( 16, iround, .TRUE. )
  ELSE
    stmpa = basewrite( 16, iround, .FALSE. )
  END IF
  IF( width /= '' )THEN
    WRITE(strout,'(A'//TRIM(width)//')') TRIM(stmpa)
  ELSE
    strout = ADJUSTL( stmpa )
  END IF
  strout = TRIM(strout) // ')'
!%}}}
! b %{{{
ELSE IF( fmtbase == 'b' )THEN
  ! fortran 'b' format
  ! Note:  -(2^(31)) ~ (2^(31)-1) -> need only 32 digits
  IF( width /= '' )THEN
    WRITE(strout,fmtfull) iround
  ELSE
    WRITE(stmpa,'(B40)') iround
    strout = ADJUSTL(stmpa)
    IF(cutexzero) CALL psc_cut_extra_zero(strout)
    strout = TRIM(strout) // ')'
  END IF
!%}}}
! o %{{{
ELSE IF( fmtbase == 'o' )THEN
  ! fortran 'o' format
  ! Note: -(2*8^(10)) ~ (2*8^(10)-1) -> need only 11 digits
  IF( width /= '' )THEN
    WRITE(strout,fmtfull) iround
  ELSE
    WRITE(stmpa,'(O20)') iround
    strout = ADJUSTL(stmpa)
    IF(cutexzero) CALL psc_cut_extra_zero(strout)
    strout = TRIM(strout) // ')'
  END IF
!%}}}
END IF
! final %{{{
itmpa = LEN_TRIM(strout)
itmpb = LEN_TRIM(sepstr)
strout = strout(1:itmpa-1) // sepstr(1:itmpb-1) // ')'
itmpa = itmpa + itmpb - 1

IF( intfout > 0 )THEN
  WRITE(intfout,'(A$)') strout(1:itmpa-1)
  IF(use_nl) WRITE(intfout,*)
END IF
!%}}}

RETURN
END SUBROUTINE
!%}}}
! SUBROUTINE psc_cut_extra_zero(strinout) %{{{
SUBROUTINE psc_cut_extra_zero(strinout)
! this routine cuts extra 'zero' in the numeric expression
! example:
!   '3.27000'      -> '3.27'
!   '24.0000000'   -> '24'
!   '7.50000E+00'  -> '7.5E+00'
USE cs_psc
IMPLICIT NONE
CHARACTER( LEN= psc_strlen ), INTENT( INOUT ) :: strinout
INTEGER :: itmpa
CHARACTER( LEN= 100 ) :: stmpa

IF( INDEX(strinout,'.') == 0 )RETURN
IF(SCAN(strinout,'eEdD')>0)THEN
  ! with-index-representation
  itmpa = SCAN(strinout,'eEdD')
  stmpa = strinout(1:itmpa-1)
  stmpa( VERIFY(stmpa,'0 ',.TRUE.)+1: ) = ''
  stmpa( VERIFY(stmpa,'. ',.TRUE.)+1: ) = ''
  strinout = TRIM(stmpa) // TRIM(strinout(itmpa:))
ELSE
  ! without-index-representation
  strinout(VERIFY(strinout,'0 ',.TRUE.)+1:) = ''
  strinout(VERIFY(strinout,'. ',.TRUE.)+1:) = ''
END IF
RETURN
END SUBROUTINE
!%}}}

! others
! SUBROUTINE read_html_color(strin,rout,gout,bout) %{{{
SUBROUTINE read_html_color(strin,rout,gout,bout)
USE cs_psc
IMPLICIT NONE
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: strin
DOUBLE PRECISION, INTENT( OUT ) :: rout, gout, bout
INTEGER( KIND= 4 ), EXTERNAL :: hexmap, hex2map
DOUBLE PRECISION :: htmlcolors(3)
INTEGER( KIND= 4 ) :: idoa
CHARACTER( LEN= 1 ) :: ctmpa
CHARACTER( LEN= 2 ) :: xtmpa
IF( LEN_TRIM( strin ) == 4 )THEN
  DO idoa = 1, 3
    ctmpa = strin(idoa+1:idoa+1)
    htmlcolors(idoa) = DBLE( hexmap(ctmpa) ) / 15.d0
  END DO
ELSE
  DO idoa = 1, 3
    xtmpa = strin(idoa*2:idoa*2+1)
    htmlcolors(idoa) = DBLE( hex2map(xtmpa) ) / 255.d0
  END DO
ENDIF
rout = htmlcolors(1)
gout = htmlcolors(2)
bout = htmlcolors(3)
RETURN
END SUBROUTINE
!%}}}
! SUBROUTINE transform_hex_chars(strio) %{{{
SUBROUTINE transform_hex_chars(strio,wordlen)
USE cs_psc
IMPLICIT NONE
CHARACTER( LEN= psc_strlen ), INTENT( INOUT ) :: strio
INTEGER( KIND= 4 ), INTENT( INOUT ) :: wordlen
INTEGER( KIND= 4 ), EXTERNAL :: hex2map
INTEGER( KIND= 4 ) :: idoa
CHARACTER( LEN= 2 ) :: xxtemp
idoa = 1
DO
  xxtemp = strio(idoa+2:idoa+3)
  IF( ( strio(idoa:idoa+1) == '/x' ) .AND. ( VERIFY( xxtemp, '0123456789abcdefABCDEF' ) == 0 ) )THEN
    strio = strio(1:idoa-1) // ACHAR( hex2map( xxtemp ) ) // strio(idoa+4:)
    wordlen = wordlen - 3
  ENDIF
  idoa = idoa + 1
  IF( idoa > LEN_TRIM( strio ) - 3 ) EXIT
END DO
RETURN
END SUBROUTINE
!%}}}
! SUBROUTINE random_seed_initializer %{{{
SUBROUTINE random_seed_initializer
USE cv_psc
IMPLICIT NONE
INTEGER :: seedsize
INTEGER, ALLOCATABLE :: seed(:)
! initialize random seeds
CALL RANDOM_SEED( SIZE= seedsize )
ALLOCATE( seed(seedsize) )
CALL DATE_AND_TIME(systime(1),systime(2),systime(3),datime)
CALL RANDOM_SEED( get= seed )
seed(1) = datime(5)*10000+datime(6)*100+datime(7)+datime(8)*1000000
seed(seedsize) = datime(7)-datime(8)*10000
CALL RANDOM_SEED( put= seed )
DEALLOCATE( seed )
RETURN
END SUBROUTINE
!%}}}

! SUBROUTINE psc_comm_comm(word) %{{{
SUBROUTINE psc_comm_comm(word)
! common commands
USE cv_psc
IMPLICIT NONE
CHARACTER( LEN= psc_strlen ), INTENT( INOUT ) :: word
INTEGER :: inta,intb,intc
DOUBLE PRECISION :: rtempa
CHARACTER( LEN= 100 ) :: stmpa

! version %{{{
IF((word=='version'))THEN
  WRITE(*,'(A)') ' PSC version ' // TRIM( pscversion )
  WRITE(*,'(A)') ' update ' // TRIM( pscupdate )
  WRITE(*,'(A)') ' created by ' // TRIM( pscauthor )
  RETURN
END IF
!%}}}
! vos diger %{{{
IF((word=='vos'//"diger"))THEN
  WRITE(*,'(A)') '==============================================================='
  WRITE(*,'(A)') '         This is supplemental description of PSC               '
  WRITE(*,'(A)') '            Command        Operation                           '
  WRITE(*,'(A)') 'author,version,\(''-'')?   show information about author         '
  WRITE(*,'(A)') '          vos'//'diger,\!?   show this command list                '
  WRITE(*,'(A)') '                banner   show banner                           '
  WRITE(*,'(A)') '             flow,walk   show random-walk-like curves          '
  WRITE(*,'(A)') '    stars,numbers,snow   show random points                    '
  WRITE(*,'(A)') '==============================================================='
  RETURN
END IF
!%}}}
! banner %{{{
IF((word=='banner'))THEN
  IF(color)THEN
    WRITE(*,'(A)') ACHAR(27)//'[38;2;120;060;200m======================================'
    WRITE(*,'(A)') ACHAR(27)//'[38;2;000;000;000m                                      '
    WRITE(*,'(A)') ACHAR(27)//'[38;2;080;040;255m  PPPPPPP      SSSSSS       CCCCC     '
    WRITE(*,'(A)') ACHAR(27)//'[38;2;000;080;200m  P      P    S      S     C    CC    '
    WRITE(*,'(A)') ACHAR(27)//'[38;2;000;190;190m  P       P   S           C           '
    WRITE(*,'(A)') ACHAR(27)//'[38;2;000;200;080m  P      P     SS         C           '
    WRITE(*,'(A)') ACHAR(27)//'[38;2;030;255;030m  PPPPPPP        SSS      C           '
    WRITE(*,'(A)') ACHAR(27)//'[38;2;080;200;000m  P                 S     C           '
    WRITE(*,'(A)') ACHAR(27)//'[38;2;190;190;020m  P                  S    C           '
    WRITE(*,'(A)') ACHAR(27)//'[38;2;200;080;020m  P           SS     S     C     C    '
    WRITE(*,'(A)') ACHAR(27)//'[38;2;255;080;080m  P             SSSSS       CCCCC     '
    WRITE(*,'(A)') ACHAR(27)//'[38;2;000;000;000m                                      '
    WRITE(*,'(A)') ACHAR(27)//'[38;2;200;120;120m======================================'
  ELSE
    WRITE(*,'(A)') '======================================'
    WRITE(*,'(A)') '                                      '
    WRITE(*,'(A)') '  PPPPPPP      SSSSSS       CCCCC     '
    WRITE(*,'(A)') '  P      P    S      S     C    CC    '
    WRITE(*,'(A)') '  P       P   S           C           '
    WRITE(*,'(A)') '  P      P     SS         C           '
    WRITE(*,'(A)') '  PPPPPPP        SSS      C           '
    WRITE(*,'(A)') '  P                 S     C           '
    WRITE(*,'(A)') '  P                  S    C           '
    WRITE(*,'(A)') '  P           SS     S     C     C    '
    WRITE(*,'(A)') '  P             SSSSS       CCCCC     '
    WRITE(*,'(A)') '                                      '
    WRITE(*,'(A)') '======================================'
  END IF
  ! '==========================================================================='
  ! '                                                                           '
  ! '  PPPPPPP      SSSSSS               CCCCC               ll                 '
  ! '  P      P    S      S             C    CC               l                 '
  ! '  P       P   S                   C                      l                 '
  ! '  P      P     SS                 C           aaaaa      l        cccc     '
  ! '  PPPPPPP        SSS    -------   C          a     a     l       c   cc    '
  ! '  P                 S             C             aaaa     l      c          '
  ! '  P                  S            C          aaa   a     l      c          '
  ! '  P           SS     S             C     C   a     a     ll      c    c    '
  ! '  P             SSSSS               CCCCC     aaaaa aa    lll     cccc     '
  ! '                                                                           '
  ! '==========================================================================='
  RETURN
END IF
!%}}}
! flow %{{{
IF((word=='flow'))THEN
  CALL RANDOM_NUMBER(rtempa)
  inta = INT(140.d0*rtempa)+1
  IF(.TRUE.)THEN
    DO intc=1,200
      CALL RANDOM_NUMBER(rtempa)
      IF((rtempa>0.95d0))THEN
        inta = inta+3
      ELSE IF((rtempa>0.85d0))THEN
        inta = inta+2
      ELSE IF((rtempa>0.7d0))THEN
        inta = inta+1
      ELSE IF((rtempa<0.05d0))THEN
        inta = inta-3
      ELSE IF((rtempa<0.15d0))THEN
        inta = inta-2
      ELSE IF((rtempa<0.3d0))THEN
        inta = inta-1
      END IF
      IF(inta<1)inta=inta+140
      IF(inta>140)inta=inta-140
      DO intb=1,inta
        WRITE(*,'(A$)') ' '
      END DO
      DO intb=1,1000000
        ! time control
        rtempa = DSIN(rtempa)
      END DO
      WRITE(*,'(A)') 'o'
    END DO
  END IF
  RETURN
END IF
!%}}}
! stars %{{{
IF((word=='stars'))THEN
  IF(.TRUE.)THEN
    DO intc=1,1000
      ! 位置の決定
      CALL RANDOM_NUMBER(rtempa)
      inta = int(40.d0*rtempa)
      DO intb=1,inta-1
        WRITE(*,'(A$)') ' '
      END DO
      ! 文字色の決定
      CALL RANDOM_NUMBER(rtempa)
      inta = INT(255.d0*rtempa)
      WRITE(stmpa,'(I3)') inta
      WRITE(*,'(A$)') ACHAR(27)//'[38;05;'//TRIM(stmpa)//'m'
      WRITE(*,'(A$)') TRIM(stmpa)//' '
      DO intb=1,500
        CALL RANDOM_NUMBER(rtempa)
      END DO
    END DO
    WRITE(*,'(A)') ACHAR(27)//'[m'
  END IF
  RETURN
END IF
!%}}}
! now %{{{
IF((word=='now'))THEN
  CALL DATE_AND_TIME(systime(1),systime(2),systime(3),datime)
  WRITE(dtm(1),'(I4)') datime(1)
  DO inta=2,7
    WRITE(dtm(inta),'(I2.2)') datime(inta)
  END DO
  WRITE(dtm(8),'(I3.3)') datime(8)
  stmpa = ''
  stmpa = TRIM(stmpa)//TRIM(dtm(1))//'-'//TRIM(dtm(2))//'-'//TRIM(dtm(3))
  stmpa = TRIM(stmpa)//' '//TRIM(dtm(5))//':'//TRIM(dtm(6))//':'//TRIM(dtm(7))//'.'//TRIM(dtm(8))
  word = stmpa
  RETURN
END IF
!%}}}
RETURN
END SUBROUTINE
!%}}}
! SUBROUTINE makeprimelist( ipmax, primelist, ierr ) %{{{
SUBROUTINE makeprimelist( ipmax, primelist, ierr )
! make list of prime numbers
! ierr == 0 : successful return
! ierr == 1 : has overflow while calculating prime-lists
IMPLICIT NONE
INTEGER, INTENT( IN ) :: ipmax
INTEGER, INTENT( OUT ) :: primelist( 1 : ipmax )
INTEGER, INTENT( OUT ) :: ierr
INTRINSIC :: FLOOR, DSQRT, DBLE, HUGE
INTEGER :: inow
INTEGER :: isqr
LOGICAL :: isprime
INTEGER :: idop, jdop
! single precision integer must be less than 2^(31)
! but this is enough for this purpose I think

primelist( : ) = 0
primelist( 1 ) = 2
inow = 3

idop = 2
DO
  isqr = FLOOR( DSQRT( DBLE( inow ) ) )
  isprime = .TRUE.
  DO jdop = 1, idop - 1
    IF( MOD( inow, primelist( jdop ) ) == 0 )THEN
      isprime = .FALSE.
      EXIT
    END IF
    IF( primelist( jdop ) >= isqr ) EXIT
  END DO
  IF( isprime )THEN
    primelist( idop ) = inow
    IF( idop == ipmax ) EXIT
    idop = idop + 1
  END IF
  IF( inow >= HUGE( inow ) - 2 )THEN
    ierr = 1
    EXIT
  END IF
  inow = inow + 2
END DO

ierr = 0
RETURN
END SUBROUTINE
!%}}}

! kplot related routines
! SUBROUTINE kplot_initializer %{{{
SUBROUTINE kplot_initializer
USE cv_kplot
IMPLICIT NONE
CALL initialize_kplot_options()
! この後に追加の設定をかけば既定のプロット設定を変更できる
printerror = .FALSE.
!stpt = 1
!xlab = 'xxx'
!ylab = 'yyy'
!title = 'ttt'
RETURN
END SUBROUTINE
!%}}}

! SUBROUTINE setkpopt(cmdin,tagin) %{{{
SUBROUTINE setkpopt(cmdin,tagin)
USE cs_psc, ONLY: psc_strlen
USE cv_psc, ONLY: stack, stz, macro, macrosize
USE cv_psc, ONLY: psc_cword => cword
USE cv_kplot
IMPLICIT NONE
LOGICAL, EXTERNAL :: setkpopt_typecheck
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: cmdin
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: tagin
CHARACTER( LEN = psc_strlen ) :: stmpa
INTEGER :: itype
DOUBLE PRECISION :: rtmpa
INTEGER :: itmpa
LOGICAL :: ltmpa
DOUBLE PRECISION :: rin, gin, bin
IF( macrosize >= 2 )THEN
  stmpa = macro(2)
  stmpa = stmpa(1:LEN_TRIM(stmpa)-1)
END IF
IF( stz >= 1 )THEN
  rtmpa = stack(stz)
  itmpa = NINT(stack(stz))
  ltmpa = ( itmpa /= 0 )
  !ltmpa = .TRUE.
  !IF( itmpa /= 0 ) ltmpa = .FALSE.
END IF
IF( stz >= 3 )THEN
  rin = stack(stz-2)
  gin = stack(stz-1)
  bin = stack(stz-0)
END IF
IF(.FALSE.)THEN
! str(1) %{{{
ELSE IF(tagin=='commentchar')THEN
  itype = 1; IF( setkpopt_typecheck(itype) ) commentchar = stmpa(1:1)
ELSE IF(tagin=='vertbarchar')THEN
  itype = 1; IF( setkpopt_typecheck(itype) ) vertbarchar = stmpa(1:1)
ELSE IF(tagin=='papertype'  )THEN
  itype = 1; IF( setkpopt_typecheck(itype) ) papertype = TRIM( stmpa )
ELSE IF(tagin=='title'      )THEN
  itype = 1; IF( setkpopt_typecheck(itype) ) title     = TRIM( stmpa )
ELSE IF(tagin=='xlab'       )THEN
  itype = 1; IF( setkpopt_typecheck(itype) ) xlab      = TRIM( stmpa )
ELSE IF(tagin=='ylab'       )THEN
  itype = 1; IF( setkpopt_typecheck(itype) ) ylab      = TRIM( stmpa )
ELSE IF(tagin=='zlab'       )THEN
  itype = 1; IF( setkpopt_typecheck(itype) ) zlab      = TRIM( stmpa )
ELSE IF(tagin=='xtlf'       )THEN
  itype = 1; IF( setkpopt_typecheck(itype) ) xtlf      = TRIM( stmpa )
ELSE IF(tagin=='ytlf'       )THEN
  itype = 1; IF( setkpopt_typecheck(itype) ) ytlf      = TRIM( stmpa )
ELSE IF(tagin=='ztlf'       )THEN
  itype = 1; IF( setkpopt_typecheck(itype) ) ztlf      = TRIM( stmpa )
ELSE IF(tagin=='atlf'       )THEN
  itype = 1; IF( setkpopt_typecheck(itype) ) atlf      = TRIM( stmpa )
ELSE IF(tagin=='plotmode'   )THEN
  itype = 1; IF( setkpopt_typecheck(itype) ) plotmode  = TRIM( stmpa )
ELSE IF(tagin=='using'      )THEN
  itype = 1; IF( setkpopt_typecheck(itype) ) usingstr  = TRIM( stmpa )
!%}}}
! int(2) %{{{
ELSE IF(tagin=='p_mode'   )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) p_mode     = itmpa
ELSE IF(tagin=='tpst'     )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) tpst       = itmpa
ELSE IF(tagin=='brdt'     )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) brdt       = itmpa
ELSE IF(tagin=='stpt'     )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) stpt       = itmpa
ELSE IF(tagin=='stlt'     )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) stlt       = itmpa
ELSE IF(tagin=='stcl'     )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) stcl       = itmpa
ELSE IF(tagin=='stsz'     )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) stsz       = itmpa
ELSE IF(tagin=='tclt'     )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) tclt       = itmpa
ELSE IF(tagin=='mtclt'    )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) mtclt      = itmpa
ELSE IF(tagin=='bdlt'     )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) bdlt       = itmpa
ELSE IF(tagin=='gxlt'     )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) gxlt       = itmpa
ELSE IF(tagin=='gylt'     )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) gylt       = itmpa
ELSE IF(tagin=='vblt'     )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) vblt       = itmpa
ELSE IF(tagin=='xr_m'     )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) xr_m       = itmpa
ELSE IF(tagin=='yr_m'     )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) yr_m       = itmpa
ELSE IF(tagin=='zr_m'     )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) zr_m       = itmpa
ELSE IF(tagin=='ar_m'     )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) ar_m       = itmpa
ELSE IF(tagin=='xdiv'     )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) xdiv(:)    = itmpa
ELSE IF(tagin=='mxdiv'    )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) mxdiv_m    = itmpa
ELSE IF(tagin=='ydiv'     )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) ydiv       = itmpa
ELSE IF(tagin=='mydiv'    )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) mydiv_m    = itmpa
ELSE IF(tagin=='adiv'     )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) adiv       = itmpa
ELSE IF(tagin=='madiv'    )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) madiv_m    = itmpa
ELSE IF(tagin=='zdiv'     )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) zdiv       = itmpa
ELSE IF(tagin=='xstep'     )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) xstep(:)   = itmpa
ELSE IF(tagin=='ystep'     )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) ystep      = itmpa
ELSE IF(tagin=='zstep'     )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) zstep      = itmpa
ELSE IF(tagin=='astep'     )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) astep      = itmpa
ELSE IF(tagin=='ibmax'    )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) ibmax      = itmpa
ELSE IF(tagin=='clbn'     )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) clbn       = itmpa
ELSE IF(tagin=='ltnum'    )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) ltnum      = itmpa
ELSE IF(tagin=='ptnum'    )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) ptnum      = itmpa
ELSE IF(tagin=='clnum'    )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) clnum      = itmpa
ELSE IF(tagin=='ftnum'    )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) ftnum      = itmpa
ELSE IF(tagin=='sznum'    )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) sznum      = itmpa
ELSE IF(tagin=='xlft'     )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) xlft       = itmpa
ELSE IF(tagin=='ylft'     )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) ylft       = itmpa
ELSE IF(tagin=='y2lft'    )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) y2lft      = itmpa
ELSE IF(tagin=='tcft'     )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) tcft       = itmpa
ELSE IF(tagin=='vbft'     )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) vbft       = itmpa
ELSE IF(tagin=='ttft'     )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) ttft       = itmpa
ELSE IF(tagin=='kyft'     )THEN
  itype = 2; IF( setkpopt_typecheck(itype) ) kyft       = itmpa
!%}}}
! flag(3) %{{{
ELSE IF(tagin== 'sizesqr'        )THEN
  itype = 3; IF( setkpopt_typecheck(itype) ) sizesqr        =ltmpa
ELSE IF(tagin== 'realratio'      )THEN
  itype = 3; IF( setkpopt_typecheck(itype) ) realratio      =ltmpa
ELSE IF(tagin== 'rotmedia'       )THEN
  itype = 3; IF( setkpopt_typecheck(itype) ) rotmedia       =ltmpa
ELSE IF(tagin== 'implicitbb'     )THEN
  itype = 3; IF( setkpopt_typecheck(itype) ) implicitbb     =ltmpa
ELSE IF(tagin== 'showline'       )THEN
  itype = 3; IF( setkpopt_typecheck(itype) ) showline       =ltmpa
ELSE IF(tagin== 'showpoint'      )THEN
  itype = 3; IF( setkpopt_typecheck(itype) ) showpoint      =ltmpa
ELSE IF(tagin== 'showbond'       )THEN
  itype = 3; IF( setkpopt_typecheck(itype) ) showbond       =ltmpa
ELSE IF(tagin== 'showcube'       )THEN
  itype = 3; IF( setkpopt_typecheck(itype) ) showcube       =ltmpa
ELSE IF(tagin== 'showxtics'      )THEN
  itype = 3; IF( setkpopt_typecheck(itype) ) showxtics      =ltmpa
ELSE IF(tagin== 'showytics'      )THEN
  itype = 3; IF( setkpopt_typecheck(itype) ) showytics      =ltmpa
ELSE IF(tagin== 'showxtlabs'     )THEN
  itype = 3; IF( setkpopt_typecheck(itype) ) showxtlabs     =ltmpa
ELSE IF(tagin== 'showytlabs'     )THEN
  itype = 3; IF( setkpopt_typecheck(itype) ) showytlabs     =ltmpa
ELSE IF(tagin== 'showztlabs'     )THEN
  itype = 3; IF( setkpopt_typecheck(itype) ) showztlabs     =ltmpa
ELSE IF(tagin== 'rotxtlabs'      )THEN
  itype = 3; IF( setkpopt_typecheck(itype) ) rotxtlabs      =ltmpa
ELSE IF(tagin== 'rotytlabs'      )THEN
  itype = 3; IF( setkpopt_typecheck(itype) ) rotytlabs      =ltmpa
ELSE IF(tagin== 'showcolorbar'   )THEN
  itype = 3; IF( setkpopt_typecheck(itype) ) showcolorbar   =ltmpa
ELSE IF(tagin== 'showkey'        )THEN
  itype = 3; IF( setkpopt_typecheck(itype) ) showkey        =ltmpa
ELSE IF(tagin== 'showkeybox'     )THEN
  itype = 3; IF( setkpopt_typecheck(itype) ) showkeybox     =ltmpa
ELSE IF(tagin== 'cutextrazero'   )THEN
  itype = 3; IF( setkpopt_typecheck(itype) ) cutextrazero   =ltmpa
ELSE IF(tagin== 'implicitx'      )THEN
  itype = 3; IF( setkpopt_typecheck(itype) ) implicitx      =ltmpa
ELSE IF(tagin== 'hidden3d'       )THEN
  itype = 3; IF( setkpopt_typecheck(itype) ) hidden3d       =ltmpa
ELSE IF(tagin== 'autofix'        )THEN
  itype = 3; IF( setkpopt_typecheck(itype) ) autofix        =ltmpa
ELSE IF(tagin== 'hiddenbond'     )THEN
  itype = 3; IF( setkpopt_typecheck(itype) ) hiddenbond     =ltmpa
ELSE IF(tagin== 'pseudoshade'    )THEN
  itype = 3; IF( setkpopt_typecheck(itype) ) pseudoshade    =ltmpa
ELSE IF(tagin== 'morecomment'    )THEN
  itype = 3; IF( setkpopt_typecheck(itype) ) morecomment    =ltmpa
!%}}}
! real(4) %{{{
ELSE IF(tagin== 'ppx'    )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) papertype = 'userdefined'
                                  ppx    = rtmpa
ELSE IF(tagin== 'ppy'    )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) papertype = 'userdefined'
                                  ppy    = rtmpa
ELSE IF(tagin== 'xr_r'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) xr_r   = rtmpa
ELSE IF(tagin== 'yr_r'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) yr_r   = rtmpa
ELSE IF(tagin== 'zr_r'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) zr_r   = rtmpa
ELSE IF(tagin== 'ar_r'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) ar_r   = rtmpa
ELSE IF(tagin== 'br_r'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) br_r   = rtmpa
ELSE IF(tagin== 'bondlenmax'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) bondlenmax   = rtmpa
ELSE IF(tagin== 'xmin'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) xmin_value(:) = rtmpa
                                  xmin_flag(:)  = .TRUE.
ELSE IF(tagin== 'xmax'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) xmax_value(:) = rtmpa
                                  xmax_flag(:)  = .TRUE.
ELSE IF(tagin== 'ymin'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) ymin_value    = rtmpa
                                  ymin_flag     = .TRUE.
                                  yr_m          = 2
ELSE IF(tagin== 'ymax'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) ymax_value    = rtmpa
                                  ymax_flag     = .TRUE.
                                  yr_m          = 2
ELSE IF(tagin== 'amin'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) amin    = rtmpa
ELSE IF(tagin== 'amax'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) amax    = rtmpa
ELSE IF(tagin== 'zmin'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) zmin    = rtmpa
ELSE IF(tagin== 'zmax'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) zmax    = rtmpa
ELSE IF(tagin== 'angth'  )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) angth   = rtmpa
ELSE IF(tagin== 'angph'  )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) angph   = rtmpa
ELSE IF(tagin== 'pprat'  )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) pprat   = rtmpa
ELSE IF(tagin== 'mist'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) mist    = rtmpa
ELSE IF(tagin== 'lineacceptrange'  )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) lineacceptrange   = rtmpa
ELSE IF(tagin== 'pointacceptrange'  )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) pointacceptrange   = rtmpa
ELSE IF(tagin== 'pnts'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) pnts    = rtmpa
ELSE IF(tagin== 'pnth'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) pnth    = rtmpa
ELSE IF(tagin== 'ptlw'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) ptlw    = rtmpa
ELSE IF(tagin== 'linw'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) linw    = rtmpa
ELSE IF(tagin== 'linh'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) linh    = rtmpa
ELSE IF(tagin== 'bdlw'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) bdlw    = rtmpa
ELSE IF(tagin== 'gxlw'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) gxlw    = rtmpa
ELSE IF(tagin== 'gylw'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) gylw    = rtmpa
ELSE IF(tagin== 'vblw'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) vblw    = rtmpa
ELSE IF(tagin== 'tclw'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) tclw    = rtmpa
ELSE IF(tagin== 'mtclw'  )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) mtclw   = rtmpa
ELSE IF(tagin== 'clmg'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) clmg    = rtmpa
ELSE IF(tagin== 'gxmg'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) gxmg    = rtmpa
ELSE IF(tagin== 'gymg'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) gymg    = rtmpa
ELSE IF(tagin== 'kymg'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) kymg    = rtmpa
ELSE IF(tagin== 'tcll'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) tcll    = rtmpa
ELSE IF(tagin== 'mtcll'   )THEN
  itype = 4; IF( setkpopt_typecheck(itype) ) mtcll   = rtmpa
ELSE IF(tagin== 'kyll'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) kyll    = rtmpa
ELSE IF(tagin== 'tcfs'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) tcfs    = rtmpa
ELSE IF(tagin== 'tcmg'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) tcmg    = rtmpa
ELSE IF(tagin== 'lbfs'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) lbfs    = rtmpa
ELSE IF(tagin== 'lxmg'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) lxmg    = rtmpa
ELSE IF(tagin== 'lymg'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) lymg    = rtmpa
ELSE IF(tagin== 'bbxmg'  )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) bbxmg   = rtmpa
ELSE IF(tagin== 'bbymg'  )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) bbymg   = rtmpa
ELSE IF(tagin== 'trx'    )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) trx     = rtmpa
ELSE IF(tagin== 'try'    )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) try     = rtmpa
ELSE IF(tagin== 'ttkx'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) ttkx    = rtmpa
ELSE IF(tagin== 'ttky'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) ttky    = rtmpa
ELSE IF(tagin== 'kykx'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) kykx    = rtmpa
ELSE IF(tagin== 'kyky'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) kyky    = rtmpa
ELSE IF(tagin== 'xlkx'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) xlkx    = rtmpa
ELSE IF(tagin== 'xlky'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) xlky    = rtmpa
ELSE IF(tagin== 'ylkx'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) ylkx    = rtmpa
ELSE IF(tagin== 'ylky'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) ylky    = rtmpa
ELSE IF(tagin== 'tckx'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) tckx    = rtmpa
ELSE IF(tagin== 'tcky'   )THEN 
  itype = 4; IF( setkpopt_typecheck(itype) ) tcky    = rtmpa
!%}}}
! color(5) %{{{
ELSE IF(tagin== 'bakc' )THEN
  itype = 5; IF( setkpopt_typecheck(itype) ) bakc(:)  = (/ rin, gin, bin /)
ELSE IF(tagin== 'glbc' )THEN
  itype = 5; IF( setkpopt_typecheck(itype) ) glbc(:)  = (/ rin, gin, bin /)
ELSE IF(tagin== 'bdlc' )THEN
  itype = 5; IF( setkpopt_typecheck(itype) ) bdlc(:)  = (/ rin, gin, bin /)
ELSE IF(tagin== 'tcfc' )THEN
  itype = 5; IF( setkpopt_typecheck(itype) ) tcfc(:)  = (/ rin, gin, bin /)
ELSE IF(tagin== 'tclc' )THEN
  itype = 5; IF( setkpopt_typecheck(itype) ) tclc(:)  = (/ rin, gin, bin /)
ELSE IF(tagin== 'mtclc')THEN
  itype = 5; IF( setkpopt_typecheck(itype) ) mtclc(:) = (/ rin, gin, bin /)
ELSE IF(tagin== 'linc' )THEN
  itype = 5; IF( setkpopt_typecheck(itype) ) linc(:)  = (/ rin, gin, bin /)
ELSE IF(tagin== 'pntc' )THEN
  itype = 5; IF( setkpopt_typecheck(itype) ) pntc(:)  = (/ rin, gin, bin /)
ELSE IF(tagin== 'gxlc' )THEN
  itype = 5; IF( setkpopt_typecheck(itype) ) gxlc(:)  = (/ rin, gin, bin /)
ELSE IF(tagin== 'gylc' )THEN
  itype = 5; IF( setkpopt_typecheck(itype) ) gylc(:)  = (/ rin, gin, bin /)
ELSE IF(tagin== 'vblc' )THEN
  itype = 5; IF( setkpopt_typecheck(itype) ) vblc(:)  = (/ rin, gin, bin /)
ELSE IF(tagin== 'edgc' )THEN
  itype = 5; IF( setkpopt_typecheck(itype) ) edgc(:)  = (/ rin, gin, bin /)
!%}}}
! string with integer(6) %{{{
ELSE IF(tagin== 'vbarlab' )THEN
  itype = 6
  IF( setkpopt_typecheck(itype) ) vbarlab(:,itmpa) = TRIM(stmpa)
ELSE IF(tagin== 'key' )THEN
  itype = 6
  IF( setkpopt_typecheck(itype) ) key(itmpa) = TRIM(stmpa)
ELSE IF(tagin== 'styleline' )THEN
  itype = 6
  IF( setkpopt_typecheck(itype) ) stylelines(itmpa) = TRIM(stmpa)
ELSE IF(tagin== 'stylepoint' )THEN
  itype = 6
  IF( setkpopt_typecheck(itype) ) stylepoints(itmpa) = TRIM(stmpa)
ELSE IF(tagin== 'stylepoint2' )THEN
  itype = 6
  IF( setkpopt_typecheck(itype) ) stylepoints2(itmpa) = TRIM(stmpa)
ELSE IF(tagin== 'stylefont' )THEN
  itype = 6
  IF( setkpopt_typecheck(itype) ) stylefonts(itmpa) = TRIM(stmpa)
!%}}}
ELSE
  CALL psc_error(cmdin,15)
  RETURN
END IF
! 正常終了した場合のスタック破棄(ただし正常動作の場合のみ)
IF( setkpopt_typecheck(itype) )THEN
  IF( itype == 1 )THEN ! string
    CALL psc_strst_remove
  ELSE IF( itype == 2 .OR. itype == 3 .OR. itype == 4 )THEN ! integer|flag|real
    CALL psc_stack_remove
  ELSE IF( itype == 5 )THEN ! color
    CALL psc_stack_remove
    CALL psc_stack_remove
    CALL psc_stack_remove
  ELSE IF( itype == 6 )THEN ! string with integer
    CALL psc_stack_remove
    CALL psc_strst_remove
  END IF
ELSE
  IF( itype == 1 )THEN ! string option
    CALL psc_error( psc_cword, 5 )
  ELSE IF( itype == 2 .OR. itype == 3 .OR. itype == 4 )THEN ! integer|flag|real
    CALL psc_error( psc_cword, 3 )
  ELSE IF( itype == 5 )THEN ! color
    CALL psc_error( psc_cword, 3 )
  ELSE IF( itype == 6 )THEN ! string with integer
    CALL psc_error( psc_cword, 3 )
  END IF
END IF
RETURN
END SUBROUTINE
!%}}}
! FUNCTION setkpopt_typecheck(itypein) %{{{
LOGICAL FUNCTION setkpopt_typecheck(itypein) RESULT ( res )
USE cv_psc
IMPLICIT NONE
INTEGER, INTENT( IN ) :: itypein
res = .FALSE.
IF( itypein == 1 )THEN ! string option
  IF( macrosize >= 2 )THEN
    res = .TRUE.
    RETURN
  ELSE
    res = .FALSE.
    RETURN
  END IF
ELSE IF( itypein == 2 .OR. itypein == 3 .OR. itypein == 4 )THEN ! integer|flag|real option
  IF( stz >= 1 )THEN
    res = .TRUE.
    RETURN
  ELSE
    res = .FALSE.
    RETURN
  END IF
ELSE IF( itypein == 5 )THEN ! color
  IF( stz >= 3 )THEN
    res = .TRUE.
    RETURN
  ELSE
    res = .FALSE.
    RETURN
  END IF
ELSE IF( itypein == 6 )THEN ! string with integer
  IF( stz >= 1 .AND. macrosize >= 1 )THEN
    res = .TRUE.
    RETURN
  ELSE
    res = .FALSE.
    RETURN
  END IF
END IF
END FUNCTION
!%}}}
! SUBROUTINE setkpopt_string(cmdin,tagin,strin) %{{{
SUBROUTINE setkpopt_string(cmdin,tagin,strin)
USE cs_psc, ONLY: psc_strlen
USE cv_kplot
IMPLICIT NONE
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: cmdin
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: tagin
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: strin
IF     (tagin=='commentchar')THEN; commentchar = strin(1:1)
ELSE IF(tagin=='vertbarchar')THEN; vertbarchar = strin(1:1)
ELSE IF(tagin=='papertype'  )THEN; papertype = TRIM( strin )
ELSE IF(tagin=='title'      )THEN; title     = TRIM( strin )
ELSE IF(tagin=='xlab'       )THEN; xlab      = TRIM( strin )
ELSE IF(tagin=='ylab'       )THEN; ylab      = TRIM( strin )
ELSE IF(tagin=='zlab'       )THEN; zlab      = TRIM( strin )
ELSE IF(tagin=='xtlf'       )THEN; xtlf      = TRIM( strin )
ELSE IF(tagin=='ytlf'       )THEN; ytlf      = TRIM( strin )
ELSE IF(tagin=='ztlf'       )THEN; ztlf      = TRIM( strin )
ELSE IF(tagin=='atlf'       )THEN; atlf      = TRIM( strin )
ELSE IF(tagin=='plotmode'   )THEN; plotmode  = TRIM( strin )
ELSE IF(tagin=='using'      )THEN; usingstr  = TRIM( strin )
ELSE
  CALL psc_error(cmdin,15)
END IF
RETURN
END SUBROUTINE
!%}}}
! SUBROUTINE setkpopt_integer(cmdin,tagin,intin) %{{{
SUBROUTINE setkpopt_integer(cmdin,tagin,intin)
USE cs_psc, ONLY: psc_strlen
USE cv_kplot
IMPLICIT NONE
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: cmdin
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: tagin
INTEGER, INTENT( IN ) :: intin
IF     (tagin=='p_mode'   )THEN; p_mode     = intin
ELSE IF(tagin=='tpst'     )THEN; tpst       = intin
ELSE IF(tagin=='brdt'     )THEN; brdt       = intin
ELSE IF(tagin=='stpt'     )THEN; stpt       = intin
ELSE IF(tagin=='stlt'     )THEN; stlt       = intin
ELSE IF(tagin=='stcl'     )THEN; stcl       = intin
ELSE IF(tagin=='stsz'     )THEN; stsz       = intin
ELSE IF(tagin=='tclt'     )THEN; tclt       = intin
ELSE IF(tagin=='mtclt'    )THEN; mtclt      = intin
ELSE IF(tagin=='bdlt'     )THEN; bdlt       = intin
ELSE IF(tagin=='gxlt'     )THEN; gxlt       = intin
ELSE IF(tagin=='gylt'     )THEN; gylt       = intin
ELSE IF(tagin=='vblt'     )THEN; vblt       = intin
ELSE IF(tagin=='xr_m'     )THEN; xr_m       = intin
ELSE IF(tagin=='yr_m'     )THEN; yr_m       = intin
ELSE IF(tagin=='zr_m'     )THEN; zr_m       = intin
ELSE IF(tagin=='ar_m'     )THEN; ar_m       = intin
ELSE IF(tagin=='xdiv'     )THEN; xdiv(:)    = intin
ELSE IF(tagin=='mxdiv'    )THEN; mxdiv_m    = intin
ELSE IF(tagin=='ydiv'     )THEN; ydiv       = intin
ELSE IF(tagin=='mydiv'    )THEN; mydiv_m    = intin
ELSE IF(tagin=='adiv'     )THEN; adiv       = intin
ELSE IF(tagin=='madiv'    )THEN; madiv_m    = intin
ELSE IF(tagin=='zdiv'     )THEN; zdiv       = intin
ELSE IF(tagin=='xstep'    )THEN; xstep(:)   = intin
ELSE IF(tagin=='ystep'    )THEN; ystep      = intin
ELSE IF(tagin=='zstep'    )THEN; zstep      = intin
ELSE IF(tagin=='astep'    )THEN; astep      = intin
ELSE IF(tagin=='ibmax'    )THEN; ibmax      = intin
ELSE IF(tagin=='clbn'     )THEN; clbn       = intin
ELSE IF(tagin=='ltnum'    )THEN; ltnum      = intin
ELSE IF(tagin=='ptnum'    )THEN; ptnum      = intin
ELSE IF(tagin=='clnum'    )THEN; clnum      = intin
ELSE IF(tagin=='ftnum'    )THEN; ftnum      = intin
ELSE IF(tagin=='sznum'    )THEN; sznum      = intin
ELSE IF(tagin=='xlft'     )THEN; xlft       = intin
ELSE IF(tagin=='ylft'     )THEN; ylft       = intin
ELSE IF(tagin=='y2lft'    )THEN; y2lft       = intin
ELSE IF(tagin=='tcft'     )THEN; tcft       = intin
ELSE IF(tagin=='ttft'     )THEN; ttft       = intin
ELSE IF(tagin=='kyft'     )THEN; kyft       = intin
ELSE IF(tagin=='cnodemax'    )THEN
  CALL kplot_removedoption('cnodemax')
  WRITE (*,*) 'Please use INTEGER "clnum" option'
  CALL psc_error(cmdin,15)
ELSE IF(tagin=='kpst'    )THEN
  CALL kplot_removedoption('kpst')
  CALL psc_error(cmdin,15)
ELSE
  CALL psc_error(cmdin,15)
END IF
RETURN
END SUBROUTINE
!%}}}
! SUBROUTINE setkpopt_flag(cmdin,tagin,flagin) %{{{
SUBROUTINE setkpopt_flag(cmdin,tagin,flagin)
USE cs_psc, ONLY:psc_strlen
USE cv_kplot
IMPLICIT NONE
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: cmdin
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: tagin
LOGICAL, INTENT( IN ) :: flagin
IF     (tagin== 'sizesqr'        )THEN; sizesqr        =flagin
ELSE IF(tagin== 'realratio'      )THEN; realratio      =flagin
ELSE IF(tagin== 'rotmedia'       )THEN; rotmedia       =flagin
ELSE IF(tagin== 'implicitbb'     )THEN; implicitbb     =flagin
ELSE IF(tagin== 'showline'       )THEN; showline       =flagin
ELSE IF(tagin== 'showpoint'      )THEN; showpoint      =flagin
ELSE IF(tagin== 'showbond'       )THEN; showbond       =flagin
ELSE IF(tagin== 'showcube'       )THEN; showcube       =flagin
ELSE IF(tagin== 'showxtics'      )THEN; showxtics      =flagin
ELSE IF(tagin== 'showytics'      )THEN; showytics      =flagin
ELSE IF(tagin== 'showxtlabs'     )THEN; showxtlabs     =flagin
ELSE IF(tagin== 'showytlabs'     )THEN; showytlabs     =flagin
ELSE IF(tagin== 'showztlabs'     )THEN; showztlabs     =flagin
ELSE IF(tagin== 'rotxtlabs'      )THEN; rotxtlabs      =flagin
ELSE IF(tagin== 'rotytlabs'      )THEN; rotytlabs      =flagin
ELSE IF(tagin== 'showcolorbar'   )THEN; showcolorbar   =flagin
ELSE IF(tagin== 'showkey'        )THEN; showkey        =flagin
ELSE IF(tagin== 'showkeybox'     )THEN; showkeybox     =flagin
ELSE IF(tagin== 'cutextrazero'   )THEN; cutextrazero   =flagin
ELSE IF(tagin== 'implicitx'      )THEN; implicitx      =flagin
ELSE IF(tagin== 'hidden3d'       )THEN; hidden3d       =flagin
ELSE IF(tagin== 'autofix'        )THEN; autofix        =flagin
ELSE IF(tagin== 'hiddenbond'     )THEN; hiddenbond     =flagin
ELSE IF(tagin== 'pseudoshade'    )THEN; pseudoshade    =flagin
ELSE IF(tagin== 'morecomment'    )THEN; morecomment    =flagin
ELSE IF(tagin== 'negaposi'       )THEN
  IF(flagin)THEN
    bakc(:) = 1.d0-bakc(:)
    glbc(:) = 1.d0-glbc(:)
    linc(:) = 1.d0-linc(:)
    pntc(:) = 1.d0-pntc(:)
    bdlc(:) = 1.d0-bdlc(:)
    tcfc(:) = 1.d0-tcfc(:)
    tclc(:) = 1.d0-tclc(:)
    mtclc(:)= 1.d0-mtclc(:)
    gxlc(:) = 1.d0-gxlc(:)
    gylc(:) = 1.d0-gylc(:)
    vblc(:) = 1.d0-vblc(:)

    ttfc(:) = 1.d0-ttfc(:)
    kyfc(:) = 1.d0-kyfc(:)
    xlfc(:) = 1.d0-xlfc(:)
    ylfc(:) = 1.d0-ylfc(:)
    y2lfc(:)= 1.d0-y2lfc(:)
    zlfc(:) = 1.d0-zlfc(:)
    stylecolors(:,:)=1.d0-stylecolors(:,:)
  END IF
ELSE IF(tagin== 'colorfull'    )THEN
  CALL kplot_removedoption('colorfull')
  WRITE (*,*) 'Please use INTEGER "stcl" option'
  CALL psc_error(cmdin,15)
ELSE
  CALL psc_error(cmdin,15)
END IF
RETURN
END SUBROUTINE
!%}}}
! SUBROUTINE setkpopt_real(cmdin,tagin,realin) %{{{
SUBROUTINE setkpopt_real(cmdin,tagin,realin)
USE cs_psc, ONLY: psc_strlen
USE cv_kplot
IMPLICIT NONE
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: cmdin
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: tagin
DOUBLE PRECISION, INTENT( IN ) :: realin
IF     (tagin== 'ppx'    )THEN ;  papertype = 'userdefined'
                                  ppx    = realin
ELSE IF(tagin== 'ppy'    )THEN ;  papertype = 'userdefined'
                                  ppy    = realin
ELSE IF(tagin== 'xr_r'   )THEN ;  xr_r   = realin
ELSE IF(tagin== 'yr_r'   )THEN ;  yr_r   = realin
ELSE IF(tagin== 'zr_r'   )THEN ;  zr_r   = realin
ELSE IF(tagin== 'ar_r'   )THEN ;  ar_r   = realin
ELSE IF(tagin== 'br_r'   )THEN ;  br_r   = realin
ELSE IF(tagin== 'bondlenmax'   )THEN ;  bondlenmax   = realin
ELSE IF(tagin== 'xmin'   )THEN ;  xmin_value(:) = realin
                                  xmin_flag(:)  = .TRUE.
ELSE IF(tagin== 'xmax'   )THEN ;  xmax_value(:) = realin
                                  xmax_flag(:)  = .TRUE.
ELSE IF(tagin== 'ymin'   )THEN ;  ymin_value    = realin
                                  ymin_flag     = .TRUE.
                                  yr_m          = 2
ELSE IF(tagin== 'ymax'   )THEN ;  ymax_value    = realin
                                  ymax_flag     = .TRUE.
                                  yr_m          = 2
ELSE IF(tagin== 'amin'   )THEN ;  amin    = realin
ELSE IF(tagin== 'amax'   )THEN ;  amax    = realin
ELSE IF(tagin== 'zmin'   )THEN ;  zmin    = realin
ELSE IF(tagin== 'zmax'   )THEN ;  zmax    = realin
ELSE IF(tagin== 'angth'  )THEN ;  angth   = realin
ELSE IF(tagin== 'angph'  )THEN ;  angph   = realin
ELSE IF(tagin== 'pprat'  )THEN ;  pprat   = realin
ELSE IF(tagin== 'mist'   )THEN ;  mist    = realin
ELSE IF(tagin== 'lineacceptrange'  )THEN ;  lineacceptrange   = realin
ELSE IF(tagin== 'pointacceptrange'  )THEN ;  pointacceptrange   = realin
ELSE IF(tagin== 'pnts'   )THEN ;  pnts    = realin
ELSE IF(tagin== 'pnth'   )THEN ;  pnth    = realin
ELSE IF(tagin== 'ptlw'   )THEN ;  ptlw    = realin
ELSE IF(tagin== 'linw'   )THEN ;  linw    = realin
ELSE IF(tagin== 'linh'   )THEN ;  linh    = realin
ELSE IF(tagin== 'bdlw'   )THEN ;  bdlw    = realin
ELSE IF(tagin== 'gxlw'   )THEN ;  gxlw    = realin
ELSE IF(tagin== 'gylw'   )THEN ;  gylw    = realin
ELSE IF(tagin== 'vblw'   )THEN ;  vblw    = realin
ELSE IF(tagin== 'tclw'   )THEN ;  tclw    = realin
ELSE IF(tagin== 'mtclw'  )THEN ;  mtclw   = realin
ELSE IF(tagin== 'clmg'   )THEN ;  clmg    = realin
ELSE IF(tagin== 'gxmg'   )THEN ;  gxmg    = realin
ELSE IF(tagin== 'gymg'   )THEN ;  gymg    = realin
ELSE IF(tagin== 'kymg'   )THEN ;  kymg    = realin
ELSE IF(tagin== 'tcll'   )THEN ;  tcll    = realin
ELSE IF(tagin== 'mtcll'   )THEN;  mtcll   = realin
ELSE IF(tagin== 'kyll'   )THEN ;  kyll    = realin
ELSE IF(tagin== 'tcfs'   )THEN ;  tcfs    = realin
ELSE IF(tagin== 'tcmg'   )THEN ;  tcmg    = realin
ELSE IF(tagin== 'lbfs'   )THEN ;  lbfs    = realin
ELSE IF(tagin== 'lxmg'   )THEN ;  lxmg    = realin
ELSE IF(tagin== 'lymg'   )THEN ;  lymg    = realin
ELSE IF(tagin== 'bbxmg'  )THEN ;  bbxmg   = realin
ELSE IF(tagin== 'bbymg'  )THEN ;  bbymg   = realin
ELSE IF(tagin== 'trx'    )THEN ;  trx     = realin
ELSE IF(tagin== 'try'    )THEN ;  try     = realin
ELSE IF(tagin== 'ttkx'   )THEN ;  ttkx    = realin
ELSE IF(tagin== 'ttky'   )THEN ;  ttky    = realin
ELSE IF(tagin== 'kykx'   )THEN ;  kykx    = realin
ELSE IF(tagin== 'kyky'   )THEN ;  kyky    = realin
ELSE IF(tagin== 'xlkx'   )THEN ;  xlkx    = realin
ELSE IF(tagin== 'xlky'   )THEN ;  xlky    = realin
ELSE IF(tagin== 'ylkx'   )THEN ;  ylkx    = realin
ELSE IF(tagin== 'ylky'   )THEN ;  ylky    = realin
ELSE IF(tagin== 'tckx'   )THEN ;  tckx    = realin
ELSE IF(tagin== 'tcky'   )THEN ;  tcky    = realin
ELSE IF(tagin== 'everykernx'  )THEN
  CALL kplot_removedoption('everykernx')
  WRITE(*,*) 'Please use REAL option: "ttkx", "kykx", "xlkx", "ylkx", "tckx"'
  CALL psc_error(cmdin,15)
ELSE IF(tagin== 'everykerny'  )THEN
  CALL kplot_removedoption('everykernx')
  WRITE(*,*) 'Please use REAL option: "ttky", "kyky", "xlky", "ylky", "tcky"'
  CALL psc_error(cmdin,15)
ELSE IF(tagin== 'tcl'  )THEN
  CALL kplot_removedoption('tcl')
  WRITE(*,*) 'Please use REAL option: "tcll"'
  CALL psc_error(cmdin,15)
ELSE IF(tagin== 'mtcl'  )THEN
  CALL kplot_removedoption('mtcl')
  WRITE(*,*) 'Please use REAL option: "mtcll"'
  CALL psc_error(cmdin,15)
ELSE IF(tagin== 'kyl'  )THEN
  CALL kplot_removedoption('kyl')
  WRITE(*,*) 'Please use REAL option: "kyll"'
  CALL psc_error(cmdin,15)
ELSE
  CALL psc_error(cmdin,15)
END IF
RETURN
END SUBROUTINE
!%}}}
! SUBROUTINE setkpopt_color(cmdin,tagin,rin,gin,bin) %{{{
SUBROUTINE setkpopt_color(cmdin,tagin,rin,gin,bin)
USE cs_psc, ONLY: psc_strlen
USE cv_kplot
IMPLICIT NONE
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: cmdin
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: tagin
DOUBLE PRECISION, INTENT( IN ) :: rin, gin, bin
IF     (tagin== 'bakc' )THEN;  bakc(:)= (/ rin, gin, bin /)
ELSE IF(tagin== 'glbc' )THEN;  glbc(:)= (/ rin, gin, bin /)
ELSE IF(tagin== 'bdlc' )THEN;  bdlc(:)= (/ rin, gin, bin /)
ELSE IF(tagin== 'tcfc' )THEN;  tcfc(:)= (/ rin, gin, bin /)
ELSE IF(tagin== 'tclc' )THEN;  tclc(:)= (/ rin, gin, bin /)
ELSE IF(tagin== 'mtclc')THEN; mtclc(:)= (/ rin, gin, bin /)
ELSE IF(tagin== 'linc' )THEN;  linc(:)= (/ rin, gin, bin /)
ELSE IF(tagin== 'pntc' )THEN;  pntc(:)= (/ rin, gin, bin /)
ELSE IF(tagin== 'gxlc' )THEN;  gxlc(:)= (/ rin, gin, bin /)
ELSE IF(tagin== 'gylc' )THEN;  gylc(:)= (/ rin, gin, bin /)
ELSE IF(tagin== 'vblc' )THEN;  vblc(:)= (/ rin, gin, bin /)
ELSE IF(tagin== 'edgc' )THEN;  edgc(:)= (/ rin, gin, bin /)
ELSE
  CALL psc_error(cmdin,15)
END IF
RETURN
END SUBROUTINE
!%}}}
! SUBROUTINE setkpopt_stylecolor(cmdin,intin,rin,gin,bin) %{{{
SUBROUTINE setkpopt_stylecolor(cmdin,intin,rin,gin,bin)
USE cs_psc, ONLY: psc_strlen
USE cv_kplot
IMPLICIT NONE
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: cmdin
INTEGER( KIND= 4 ), INTENT( IN ) :: intin
DOUBLE PRECISION, INTENT( IN ) :: rin, gin, bin
IF( intin >= 0 .AND. intin <= UBOUND( stylecolors, 1 ) )THEN
  stylecolors( intin, : ) = (/ rin, gin, bin /)
ELSE
  CALL psc_error( cmdin, 15 )
END IF
RETURN
END SUBROUTINE
!%}}}
! SUBROUTINE setkpopt_styleline(cmdin,intin,strin) %{{{
SUBROUTINE setkpopt_styleline(cmdin,intin,strin)
USE cs_psc, ONLY: psc_strlen
USE cv_kplot
IMPLICIT NONE
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: cmdin
INTEGER( KIND= 4 ), INTENT( IN ) :: intin
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: strin
IF( intin >= 0 .AND. intin <= UBOUND( stylelines, 1 ) )THEN
  stylelines( intin ) = TRIM(strin)
ELSE
  CALL psc_error(cmdin,15)
END IF
RETURN
END SUBROUTINE
!%}}}
! SUBROUTINE setkpopt_stylepoint(cmdin,intin,strin) %{{{
SUBROUTINE setkpopt_stylepoint(cmdin,intin,strin)
USE cs_psc, ONLY: psc_strlen
USE cv_kplot
IMPLICIT NONE
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: cmdin
INTEGER( KIND= 4 ), INTENT( IN ) :: intin
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: strin
IF( intin >= 0 .AND. intin <= UBOUND( stylepoints, 1 ) )THEN
  stylepoints( intin ) = TRIM(strin)
ELSE
  CALL psc_error(cmdin,15)
END IF
RETURN
END SUBROUTINE
!%}}}
! SUBROUTINE setkpopt_stylepoint2(cmdin,intin,strin) %{{{
SUBROUTINE setkpopt_stylepoint2(cmdin,intin,strin)
USE cs_psc, ONLY: psc_strlen
USE cv_kplot
IMPLICIT NONE
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: cmdin
INTEGER( KIND= 4 ), INTENT( IN ) :: intin
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: strin
IF( intin >= 0 .AND. intin <= UBOUND( stylepoints2, 1 ) )THEN
  stylepoints2( intin ) = TRIM(strin)
ELSE
  CALL psc_error(cmdin,15)
END IF
RETURN
END SUBROUTINE
!%}}}
! SUBROUTINE setkpopt_stylefont(cmdin,intin,strin) %{{{
SUBROUTINE setkpopt_stylefont(cmdin,intin,strin)
USE cs_psc, ONLY: psc_strlen
USE cv_kplot
IMPLICIT NONE
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: cmdin
INTEGER( KIND= 4 ), INTENT( IN ) :: intin
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: strin
IF( intin >= 0 .AND. intin <= UBOUND( stylefonts, 1 ) )THEN
  stylefonts( intin ) = TRIM(strin)
ELSE
  CALL psc_error(cmdin,15)
END IF
RETURN
END SUBROUTINE
!%}}}
! SUBROUTINE setkpopt_stylesize(cmdin,intin,rin) %{{{
SUBROUTINE setkpopt_stylesize(cmdin,intin,rin)
USE cs_psc, ONLY: psc_strlen
USE cv_kplot
IMPLICIT NONE
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: cmdin
INTEGER( KIND= 4 ), INTENT( IN ) :: intin
DOUBLE PRECISION, INTENT( IN ) :: rin
IF( intin >= 0 .AND. intin <= UBOUND( stylesize, 1 ) )THEN
  stylesize( intin ) = rin
ELSE
  CALL psc_error( cmdin, 15 )
END IF
RETURN
END SUBROUTINE
!%}}}

! SUBROUTINE getkpopt_string(cmdin,tagin,strout) %{{{
SUBROUTINE getkpopt_string(cmdin,tagin,strout)
USE cs_psc, ONLY: psc_strlen
USE cv_kplot
IMPLICIT NONE
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: cmdin
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: tagin
CHARACTER( LEN= psc_strlen ), INTENT( OUT ) :: strout
IF     (tagin=='commentchar')THEN; strout= commentchar
ELSE IF(tagin=='vertbarchar')THEN; strout= vertbarchar
ELSE IF(tagin=='papertype'  )THEN; strout= papertype
ELSE IF(tagin=='title'      )THEN; strout= title
ELSE IF(tagin=='xlab'       )THEN; strout= xlab
ELSE IF(tagin=='ylab'       )THEN; strout= ylab
ELSE IF(tagin=='zlab'       )THEN; strout= zlab
ELSE IF(tagin=='xtlf'       )THEN; strout= xtlf
ELSE IF(tagin=='ytlf'       )THEN; strout= ytlf
ELSE IF(tagin=='ztlf'       )THEN; strout= ztlf
ELSE IF(tagin=='atlf'       )THEN; strout= atlf
ELSE IF(tagin=='plotmode'   )THEN; strout= plotmode
ELSE IF(tagin=='using'      )THEN; strout= usingstr
ELSE
  CALL psc_error(cmdin,15)
END IF
RETURN
END SUBROUTINE
!%}}}
! SUBROUTINE getkpopt_integer(cmdin,tagin,intout) %{{{
SUBROUTINE getkpopt_integer(cmdin,tagin,intout)
USE cs_psc, ONLY: psc_strlen
USE cv_kplot
IMPLICIT NONE
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: cmdin
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: tagin
INTEGER, INTENT( OUT ) :: intout
IF     (tagin=='p_mode'   )THEN; intout =     p_mode
ELSE IF(tagin=='tpst'     )THEN; intout =       tpst
ELSE IF(tagin=='brdt'     )THEN; intout =       brdt
ELSE IF(tagin=='stpt'     )THEN; intout =       stpt
ELSE IF(tagin=='stlt'     )THEN; intout =       stlt
ELSE IF(tagin=='stcl'     )THEN; intout =       stcl
ELSE IF(tagin=='stsz'     )THEN; intout =       stsz
ELSE IF(tagin=='tclt'     )THEN; intout =       tclt
ELSE IF(tagin=='mtclt'    )THEN; intout =      mtclt
ELSE IF(tagin=='bdlt'     )THEN; intout =       bdlt
ELSE IF(tagin=='gxlt'     )THEN; intout =       gxlt
ELSE IF(tagin=='gylt'     )THEN; intout =       gylt
ELSE IF(tagin=='vblt'     )THEN; intout =       vblt
ELSE IF(tagin=='xr_m'     )THEN; intout =       xr_m
ELSE IF(tagin=='yr_m'     )THEN; intout =       yr_m
ELSE IF(tagin=='zr_m'     )THEN; intout =       zr_m
ELSE IF(tagin=='ar_m'     )THEN; intout =       ar_m
ELSE IF(tagin=='xdiv'     )THEN; intout =    xdiv(1)
ELSE IF(tagin=='mxdiv'    )THEN; intout =    mxdiv_m
ELSE IF(tagin=='ydiv'     )THEN; intout =       ydiv
ELSE IF(tagin=='mydiv'    )THEN; intout =    mydiv_m
ELSE IF(tagin=='adiv'     )THEN; intout =       adiv
ELSE IF(tagin=='madiv'    )THEN; intout =    madiv_m
ELSE IF(tagin=='zdiv'     )THEN; intout =       zdiv
ELSE IF(tagin=='xstep'    )THEN; intout =   xstep(1)
ELSE IF(tagin=='ystep'    )THEN; intout =      ystep
ELSE IF(tagin=='zstep'    )THEN; intout =      zstep
ELSE IF(tagin=='astep'    )THEN; intout =      astep
ELSE IF(tagin=='ibmax'    )THEN; intout =      ibmax
ELSE IF(tagin=='clbn'     )THEN; intout =       clbn
ELSE IF(tagin=='ltnum'    )THEN; intout =      ltnum
ELSE IF(tagin=='ptnum'    )THEN; intout =      ptnum
ELSE IF(tagin=='clnum'    )THEN; intout =      clnum
ELSE IF(tagin=='ftnum'    )THEN; intout =      ftnum
ELSE IF(tagin=='sznum'    )THEN; intout =      sznum
ELSE IF(tagin=='xlft'     )THEN; intout =       xlft
ELSE IF(tagin=='ylft'     )THEN; intout =       ylft
ELSE IF(tagin=='y2lft'    )THEN; intout =      y2lft
ELSE IF(tagin=='tcft'     )THEN; intout =       tcft
ELSE IF(tagin=='ttft'     )THEN; intout =       ttft
ELSE IF(tagin=='kyft'     )THEN; intout =       kyft
ELSE IF(tagin=='cnodemax'    )THEN
  CALL kplot_removedoption('cnodemax')
  WRITE(*,*) 'Please use INTEGER "clnum" option'
  CALL psc_error(cmdin,15)
ELSE IF(tagin=='kpst'    )THEN
  CALL kplot_removedoption('kpst')
  CALL psc_error(cmdin,15)
ELSE
  CALL psc_error(cmdin,15)
END IF
RETURN
END SUBROUTINE
!%}}}
! SUBROUTINE getkpopt_flag(cmdin,tagin,flagout) %{{{
SUBROUTINE getkpopt_flag(cmdin,tagin,flagout)
USE cs_psc, ONLY:psc_strlen
USE cv_kplot
IMPLICIT NONE
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: cmdin
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: tagin
LOGICAL, INTENT( OUT ) :: flagout
IF     (tagin== 'sizesqr'        )THEN; flagout =       sizesqr
ELSE IF(tagin== 'realratio'      )THEN; flagout =     realratio
ELSE IF(tagin== 'rotmedia'       )THEN; flagout =      rotmedia
ELSE IF(tagin== 'implicitbb'     )THEN; flagout =    implicitbb
ELSE IF(tagin== 'showline'       )THEN; flagout =      showline
ELSE IF(tagin== 'showpoint'      )THEN; flagout =     showpoint
ELSE IF(tagin== 'showbond'       )THEN; flagout =      showbond
ELSE IF(tagin== 'showcube'       )THEN; flagout =      showcube
ELSE IF(tagin== 'showxtics'      )THEN; flagout =     showxtics
ELSE IF(tagin== 'showytics'      )THEN; flagout =     showytics
ELSE IF(tagin== 'showxtlabs'     )THEN; flagout =    showxtlabs
ELSE IF(tagin== 'showytlabs'     )THEN; flagout =    showytlabs
ELSE IF(tagin== 'showztlabs'     )THEN; flagout =    showztlabs
ELSE IF(tagin== 'rotxtlabs'      )THEN; flagout =     rotxtlabs
ELSE IF(tagin== 'rotytlabs'      )THEN; flagout =     rotytlabs
ELSE IF(tagin== 'showcolorbar'   )THEN; flagout =  showcolorbar
ELSE IF(tagin== 'showkey'        )THEN; flagout =       showkey
ELSE IF(tagin== 'showkeybox'     )THEN; flagout =    showkeybox
ELSE IF(tagin== 'cutextrazero'   )THEN; flagout =  cutextrazero
ELSE IF(tagin== 'implicitx'      )THEN; flagout =     implicitx
ELSE IF(tagin== 'hidden3d'       )THEN; flagout =      hidden3d
ELSE IF(tagin== 'autofix'        )THEN; flagout =       autofix
ELSE IF(tagin== 'hiddenbond'     )THEN; flagout =    hiddenbond
ELSE IF(tagin== 'pseudoshade'    )THEN; flagout =   pseudoshade
ELSE IF(tagin== 'morecomment'    )THEN; flagout =   morecomment
ELSE IF(tagin== 'colorfull'    )THEN
  CALL kplot_removedoption('colorfull')
  WRITE(*,*) 'Please use INTEGER "stcl" option'
  CALL psc_error(cmdin,15)
ELSE
  CALL psc_error(cmdin,15)
END IF
RETURN
END SUBROUTINE
!%}}}
! SUBROUTINE getkpopt_real(cmdin,tagin,realout) %{{{
SUBROUTINE getkpopt_real(cmdin,tagin,realout)
USE cs_psc, ONLY: psc_strlen
USE cv_kplot
IMPLICIT NONE
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: cmdin
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: tagin
DOUBLE PRECISION, INTENT( OUT ) :: realout
IF     (tagin== 'ppx'  )THEN ;  realout =  ppx
ELSE IF(tagin== 'ppy'  )THEN ;  realout =  ppy
ELSE IF(tagin== 'xr_r' )THEN ;  realout =  xr_r
ELSE IF(tagin== 'yr_r' )THEN ;  realout =  yr_r
ELSE IF(tagin== 'zr_r' )THEN ;  realout =  zr_r
ELSE IF(tagin== 'br_r' )THEN ;  realout =  br_r
ELSE IF(tagin== 'bondlenmax' )THEN ;  realout =  bondlenmax
ELSE IF(tagin== 'ar_r' )THEN ;  realout =  ar_r
ELSE IF(tagin== 'xmin' )THEN
  IF( xmin_flag(1) )THEN
    realout = xmin_value(1)
  ELSE
    CALL psc_error(tagin, 22 )
  END IF
ELSE IF(tagin== 'xmax' )THEN
  IF( xmax_flag(1) )THEN
    realout = xmax_value(1)
  ELSE
    CALL psc_error(tagin, 22 )
  END IF
ELSE IF(tagin== 'ymin'  )THEN ;  realout =  ymin
ELSE IF(tagin== 'ymax'  )THEN ;  realout =  ymax
ELSE IF(tagin== 'amin'  )THEN ;  realout =  amin
ELSE IF(tagin== 'amax'  )THEN ;  realout =  amax
ELSE IF(tagin== 'zmin'  )THEN ;  realout =  zmin
ELSE IF(tagin== 'zmax'  )THEN ;  realout =  zmax
ELSE IF(tagin== 'angth' )THEN ;  realout = angth
ELSE IF(tagin== 'angph' )THEN ;  realout = angph
ELSE IF(tagin== 'pprat' )THEN ;  realout = pprat
ELSE IF(tagin== 'mist'  )THEN ;  realout = mist
ELSE IF(tagin== 'lineacceptrange' )THEN ;  realout = lineacceptrange
ELSE IF(tagin== 'pointacceptrange' )THEN ;  realout = pointacceptrange
ELSE IF(tagin== 'pnts'  )THEN ;  realout =  pnts
ELSE IF(tagin== 'pnth'  )THEN ;  realout =  pnth
ELSE IF(tagin== 'ptlw'  )THEN ;  realout =  ptlw
ELSE IF(tagin== 'linw'  )THEN ;  realout =  linw
ELSE IF(tagin== 'linh'  )THEN ;  realout =  linh
ELSE IF(tagin== 'bdlw'  )THEN ;  realout =  bdlw
ELSE IF(tagin== 'gxlw'  )THEN ;  realout =  gxlw
ELSE IF(tagin== 'gylw'  )THEN ;  realout =  gylw
ELSE IF(tagin== 'vblw'  )THEN ;  realout =  vblw
ELSE IF(tagin== 'tclw'  )THEN ;  realout =  tclw
ELSE IF(tagin== 'mtclw' )THEN ;  realout =  mtclw
ELSE IF(tagin== 'clmg'  )THEN ;  realout =  clmg
ELSE IF(tagin== 'gxmg'  )THEN ;  realout =  gxmg
ELSE IF(tagin== 'gymg'  )THEN ;  realout =  gymg
ELSE IF(tagin== 'kymg'  )THEN ;  realout =  kymg
ELSE IF(tagin== 'tcll'  )THEN ;  realout =   tcll
ELSE IF(tagin== 'mtcll'  )THEN ;  realout =  mtcll
ELSE IF(tagin== 'kyll'  )THEN ;  realout =   kyll
ELSE IF(tagin== 'tcfs'  )THEN ;  realout =  tcfs
ELSE IF(tagin== 'tcmg'  )THEN ;  realout =  tcmg
ELSE IF(tagin== 'lbfs'  )THEN ;  realout =  lbfs
ELSE IF(tagin== 'lxmg'  )THEN ;  realout =  lxmg
ELSE IF(tagin== 'lymg'  )THEN ;  realout =  lymg
ELSE IF(tagin== 'bbxmg' )THEN ;  realout =  bbxmg
ELSE IF(tagin== 'bbymg' )THEN ;  realout =  bbymg
ELSE IF(tagin== 'trx'   )THEN ;  realout =  trx
ELSE IF(tagin== 'try'   )THEN ;  realout =  try
ELSE IF(tagin== 'ttkx'  )THEN ;  realout =  ttkx
ELSE IF(tagin== 'ttky'  )THEN ;  realout =  ttky
ELSE IF(tagin== 'kykx'  )THEN ;  realout =  kykx
ELSE IF(tagin== 'kyky'  )THEN ;  realout =  kyky
ELSE IF(tagin== 'xlkx'  )THEN ;  realout =  xlkx
ELSE IF(tagin== 'xlky'  )THEN ;  realout =  xlky
ELSE IF(tagin== 'ylkx'  )THEN ;  realout =  ylkx
ELSE IF(tagin== 'ylky'  )THEN ;  realout =  ylky
ELSE IF(tagin== 'tckx'  )THEN ;  realout =  tckx
ELSE IF(tagin== 'tcky'  )THEN ;  realout =  tcky
ELSE IF(tagin== 'everykernx'  )THEN
  CALL kplot_removedoption('everykernx')
  WRITE(*,*) 'Please use REAL option: "ttkx", "kykx", "xlkx", "ylkx", "tckx"'
  CALL psc_error(cmdin,15)
ELSE IF(tagin== 'everykerny'  )THEN
  CALL kplot_removedoption('everykernx')
  WRITE(*,*) 'Please use REAL option: "ttky", "kyky", "xlky", "ylky", "tcky"'
  CALL psc_error(cmdin,15)
ELSE IF(tagin== 'tcl'  )THEN
  CALL kplot_removedoption('tcl')
  WRITE(*,*) 'Please use REAL option: "tcll"'
  CALL psc_error(cmdin,15)
ELSE IF(tagin== 'mtcl'  )THEN
  CALL kplot_removedoption('mtcl')
  WRITE(*,*) 'Please use REAL option: "mtcll"'
  CALL psc_error(cmdin,15)
ELSE IF(tagin== 'kyl'  )THEN
  CALL kplot_removedoption('kyl')
  WRITE(*,*) 'Please use REAL option: "kyll"'
  CALL psc_error(cmdin,15)
ELSE
  CALL psc_error(cmdin,15)
END IF
RETURN
END SUBROUTINE
!%}}}
! SUBROUTINE getkpopt_color(cmdin,tagin,rin,gin,bin) %{{{
SUBROUTINE getkpopt_color(cmdin,tagin,rout,gout,bout)
USE cs_psc, ONLY: psc_strlen
USE cv_kplot
IMPLICIT NONE
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: cmdin
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: tagin
DOUBLE PRECISION, INTENT( OUT ) :: rout, gout, bout
DOUBLE PRECISION :: color_temp(3)
IF     (tagin== 'bakc' )THEN; color_temp(:)=  bakc(:)
ELSE IF(tagin== 'glbc' )THEN; color_temp(:)=  glbc(:)
ELSE IF(tagin== 'bdlc' )THEN; color_temp(:)=  bdlc(:)
ELSE IF(tagin== 'tcfc' )THEN; color_temp(:)=  tcfc(:)
ELSE IF(tagin== 'tclc' )THEN; color_temp(:)=  tclc(:)
ELSE IF(tagin== 'mtclc')THEN; color_temp(:)= mtclc(:)
ELSE IF(tagin== 'linc' )THEN; color_temp(:)=  linc(:)
ELSE IF(tagin== 'pntc' )THEN; color_temp(:)=  pntc(:)
ELSE IF(tagin== 'gxlc' )THEN; color_temp(:)=  gxlc(:)
ELSE IF(tagin== 'gylc' )THEN; color_temp(:)=  gylc(:)
ELSE IF(tagin== 'vblc' )THEN; color_temp(:)=  vblc(:)
ELSE IF(tagin== 'edgc' )THEN; color_temp(:)=  edgc(:)
ELSE
  CALL psc_error(cmdin,15)
END IF
rout = color_temp(1)
gout = color_temp(2)
bout = color_temp(3)
RETURN
END SUBROUTINE
!%}}}
! SUBROUTINE getkpopt_stylecolor(cmdin,intin,rout,gout,bout) %{{{
SUBROUTINE getkpopt_stylecolor(cmdin,intin,rout,gout,bout)
USE cs_psc, ONLY: psc_strlen
USE cv_kplot
IMPLICIT NONE
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: cmdin
INTEGER( KIND= 4 ), INTENT( IN ) :: intin
DOUBLE PRECISION, INTENT( OUT ) :: rout, gout, bout
IF( intin >= 0 .AND. intin <= UBOUND( stylecolors, 1 ) )THEN
  rout = stylecolors( intin, 1 )
  gout = stylecolors( intin, 2 )
  bout = stylecolors( intin, 3 )
ELSE
  CALL psc_error( cmdin, 15 )
END IF
RETURN
END SUBROUTINE
!%}}}
! SUBROUTINE getkpopt_styleline(cmdin,intin,strout) %{{{
SUBROUTINE getkpopt_styleline(cmdin,intin,strout)
USE cs_psc, ONLY: psc_strlen
USE cv_kplot
IMPLICIT NONE
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: cmdin
INTEGER( KIND= 4 ), INTENT( IN ) :: intin
CHARACTER( LEN= psc_strlen ), INTENT( OUT ) :: strout
IF( intin >= 0 .AND. intin <= UBOUND( stylelines, 1 ) )THEN
  strout = stylelines( intin )
ELSE
  CALL psc_error(cmdin,15)
END IF
RETURN
END SUBROUTINE
!%}}}
! SUBROUTINE getkpopt_stylepoint(cmdin,intin,strout) %{{{
SUBROUTINE getkpopt_stylepoint(cmdin,intin,strout)
USE cs_psc, ONLY: psc_strlen
USE cv_kplot
IMPLICIT NONE
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: cmdin
INTEGER( KIND= 4 ), INTENT( IN ) :: intin
CHARACTER( LEN= psc_strlen ), INTENT( OUT ) :: strout
IF( intin >= 0 .AND. intin <= UBOUND( stylepoints, 1 ) )THEN
  strout = stylepoints( intin )
ELSE
  CALL psc_error(cmdin,15)
END IF
RETURN
END SUBROUTINE
!%}}}
! SUBROUTINE getkpopt_stylepoint2(cmdin,intin,strout) %{{{
SUBROUTINE getkpopt_stylepoint2(cmdin,intin,strout)
USE cs_psc, ONLY: psc_strlen
USE cv_kplot
IMPLICIT NONE
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: cmdin
INTEGER( KIND= 4 ), INTENT( IN ) :: intin
CHARACTER( LEN= psc_strlen ), INTENT( OUT ) :: strout
IF( intin >= 0 .AND. intin <= UBOUND( stylepoints2, 1 ) )THEN
  strout = stylepoints2( intin )
ELSE
  CALL psc_error(cmdin,15)
END IF
RETURN
END SUBROUTINE
!%}}}
! SUBROUTINE getkpopt_stylefont(cmdin,intin,strout) %{{{
SUBROUTINE getkpopt_stylefont(cmdin,intin,strout)
USE cs_psc, ONLY: psc_strlen
USE cv_kplot
IMPLICIT NONE
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: cmdin
INTEGER( KIND= 4 ), INTENT( IN ) :: intin
CHARACTER( LEN= psc_strlen ), INTENT( OUT ) :: strout
IF( intin >= 0 .AND. intin <= UBOUND( stylefonts, 1 ) )THEN
  strout = stylefonts( intin )
ELSE
  CALL psc_error(cmdin,15)
END IF
RETURN
END SUBROUTINE
!%}}}
! SUBROUTINE getkpopt_stylesize(cmdin,intin,rout) %{{{
SUBROUTINE getkpopt_stylesize(cmdin,intin,rout)
USE cs_psc, ONLY: psc_strlen
USE cv_kplot
IMPLICIT NONE
CHARACTER( LEN= psc_strlen ), INTENT( IN ) :: cmdin
INTEGER( KIND= 4 ), INTENT( IN ) :: intin
DOUBLE PRECISION, INTENT( OUT ) :: rout
IF( intin >= 0 .AND. intin <= UBOUND( stylesize, 1 ) )THEN
  rout = stylesize( intin )
ELSE
  CALL psc_error( cmdin, 15 )
END IF
RETURN
END SUBROUTINE
!%}}}

! SUBROUTINE kplot_removedoption(strin) %{{{
SUBROUTINE kplot_removedoption(strin)
USE cs_psc, ONLY: psc_strlen
IMPLICIT NONE
CHARACTER( LEN= * ), INTENT( IN ) :: strin
WRITE(*,*) 'Error: kplot option "' // TRIM(strin) // '" removed'
RETURN
END SUBROUTINE
!%}}}

! functions
! LOGICAL psc_isreal(str_input) %{{{
LOGICAL FUNCTION psc_isreal(str_input)
USE cs_psc
IMPLICIT NONE
CHARACTER(LEN = *) :: str_input
INTEGER( KIND = 4 ) :: level, icount
level = 1
icount = 1
DO
  SELECT CASE(level)
  CASE(1) ! 符号または数字または小数点を求めている段階
    IF( SCAN(str_input(icount:icount),'+-')/=0 )THEN
      level = 2
    ELSE IF( SCAN(str_input(icount:icount),'0123456789')/=0 )THEN
      level = 3
    ELSE IF( SCAN(str_input(icount:icount),'.')/=0 )THEN
      level = 4
    ELSE
      psc_isreal = .FALSE.
      RETURN
    END IF
  CASE(2) ! 符号を観測し、数字または小数点を求めている段階
    IF( SCAN(str_input(icount:icount),'0123456789')/=0 )THEN
      level = 3
    ELSE IF( SCAN(str_input(icount:icount),'.')/=0 )THEN
      level = 4
    ELSE
      psc_isreal = .FALSE.
      RETURN
    END IF
  CASE(3) ! 小数点を含まない数字を観測し、数字または小数点または指数記号または数字の終了を求めている段階
    IF( SCAN(str_input(icount:icount),'0123456789')/=0 )THEN
      CONTINUE
    ELSE IF( SCAN(str_input(icount:icount),'.')/=0 )THEN
      level = 4
    ELSE IF( SCAN(str_input(icount:icount),'dDeE')/=0 )THEN
      level = 5
    ELSE
      psc_isreal = .FALSE.
      RETURN
    END IF
  CASE(4) ! 小数点を含む数字を観測し数字または指数記号または数字の終了を求めている段階
    IF( SCAN(str_input(icount:icount),'0123456789')/=0 )THEN
      CONTINUE
    ELSE IF( SCAN(str_input(icount:icount),'dDeE')/=0 )THEN
      level = 5
    ELSE
      psc_isreal = .FALSE.
      RETURN
    END IF
  CASE(5) ! 指数記号を観測し、指数符号または数字を求めている段階
    IF( SCAN(str_input(icount:icount),'+-')/=0 )THEN
      level = 6
    ELSE IF( SCAN(str_input(icount:icount),'0123456789')/=0 )THEN
      level = 7
    ELSE
      psc_isreal = .FALSE.
      RETURN
    END IF
  CASE(6) ! 指数符号を観測し、数字を求めている段階
    IF( SCAN(str_input(icount:icount),'0123456789')/=0 )THEN
      level = 7
    ELSE
      psc_isreal = .FALSE.
      RETURN
    END IF
  CASE(7) ! 指数の数字を観測し、数字または数字の終了を求めている段階
    IF( SCAN(str_input(icount:icount),'0123456789')/=0 )THEN
      CONTINUE
    ELSE
      psc_isreal = .FALSE.
      RETURN
    END IF
  END SELECT
  icount = icount + 1
  IF( icount>LEN_TRIM(str_input))EXIT
END DO
psc_isreal = .TRUE.
RETURN
END FUNCTION
!%}}}
! INTEGER psc_free_unit() %{{{
INTEGER( KIND = 4 ) FUNCTION psc_free_unit() RESULT ( res )
! returns not-associated device number
! return 0 when no number available
USE cs_psc
IMPLICIT NONE
INTEGER( KIND = 4 ) :: idoa
LOGICAL :: ltma
DO idoa =  11,  99
  INQUIRE( UNIT= idoa, OPENED = ltma )
  IF( .NOT. ltma  )THEN
    res = idoa
    RETURN
  ENDIF
END DO
res = 0
RETURN
END FUNCTION
!%}}}
! INTEGER hex2map(xxin) %{{{
INTEGER FUNCTION hex2map(xxin)
IMPLICIT NONE
INTEGER,EXTERNAL :: hexmap
CHARACTER( LEN= 2 ) :: xxin
INTEGER :: itmpa, itmpb
itmpa = hexmap(xxin(1:1))
itmpb = hexmap(xxin(2:2))
hex2map= itmpa*16+itmpb
RETURN
END FUNCTION
!%}}}
! INTEGER hexmap(charin) %{{{
INTEGER FUNCTION hexmap(charin)
IMPLICIT NONE
CHARACTER( LEN= 1 ), INTENT( IN ) :: charin
INTEGER,EXTERNAL :: basemap
hexmap = basemap(16,charin)
RETURN
END FUNCTION
!%}}}
! INTEGER basemap(base,charin) %{{{
INTEGER FUNCTION basemap(base,charin)
IMPLICIT NONE
INTEGER( KIND= 4 ), INTENT( IN ) :: base
CHARACTER( LEN= 1 ), INTENT( IN ) :: charin
CHARACTER( LEN= 50 ) :: charray1, charray2
INTEGER( KIND= 4 ) :: idoa
basemap = 0
charray1 = '0123456789abcdefghijklmnopqrstuvwxyz'
charray2 = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'
DO idoa = 1, base
  IF( charin == charray1(idoa:idoa) )THEN
    basemap = idoa - 1
    RETURN
  ELSE IF( charin == charray2(idoa:idoa) )THEN
    basemap = idoa - 1 
    RETURN
  END IF
END DO
RETURN
END FUNCTION
!%}}}
! CHARACTER( LEN= 100 ) basewrite(base,intin,cap) %{{{
CHARACTER( LEN= 100 ) FUNCTION basewrite(base,intin,cap)
IMPLICIT NONE
INTEGER( KIND= 4 ), INTENT( IN ) :: base
INTEGER( KIND= 4 ), INTENT( IN ) :: intin
LOGICAL, INTENT( IN ) :: cap
INTEGER( KIND= 4 ) :: icopy
CHARACTER( LEN= 50 ) :: charray1, charray2, charray
INTEGER( KIND= 4 ) :: idoa
INTEGER( KIND= 4 ) :: inow
INTEGER( KIND= 4 ) :: icount
CHARACTER( LEN= 100 ) :: stmpa
charray1 = '0123456789abcdefghijklmnopqrstuvwxyz'
charray2 = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'
IF( cap )THEN
  charray = charray2
ELSE
  charray = charray1
END IF
icopy = intin
IF( icopy < 0 )THEN
  stmpa = '-'
  icopy = - icopy
ELSE
  stmpa = ''
END IF
idoa = 0
DO
  IF( base ** idoa > icopy )THEN
    inow = idoa
    EXIT
  END IF
  idoa = idoa + 1
END DO
idoa = inow - 1
icount = 0
DO
  IF( idoa == 0 ) EXIT
  IF( icopy >= base ** idoa )THEN
    icount = icount + 1
    icopy = icopy - ( base ** idoa )
  ELSE
    stmpa = TRIM(stmpa) // charray(icount+1:icount+1)
    icount = 0
    idoa = idoa - 1
  END IF
END DO
stmpa = TRIM(stmpa) // charray(icopy+1:icopy+1)
basewrite = stmpa
RETURN
END FUNCTION
!%}}}

!EOF
