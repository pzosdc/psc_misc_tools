! Language    : Fortran90
! Description : KDRAW
! Lastchange  : 2018, Aug. 24, 16:29
! =================================================================
! development history %{{{
!d(  1)= d(20170702,1,'言語定義の大規模変更')
!d(  2)= d(20170702,1,'開発記録作成開始')
!d(  3)= d(20170705,1,'矢印描画と円描画の実装')
!d(  4)= d(20170705,2,'サブルーチンや変数の整備')
!d(  5)= d(20170705,3,'文字列描画、スクリプト描画の実装')
!d(  6)= d(20170725,1,'文字サイズ、円サイズ指定の実装')
!d(  7)= d(20170801,1,'矢印描画の改良')
!d(  8)= d(20170801,2,'線幅の指定を相対長さ単位に変更')
!d(  9)= d(20170801,3,'moveto,lineto,arc,closepath,fill,stroke等の基本的な描画命令の実装')
!d( 10)= d(20170802,1,'矢印描画コマンドで線幅指定やfill指定ができるように改良')
!d( 11)= d(20170802,2,'未定義コマンドやスタック不足時に警告を表示できるようにする')
!d( 12)= d(20170807,1,'矢印描画コマンドに細かい調整パラメタを追加')
!d( 13)= d(20170828,1,'バージョン対応確認コマンドの実装')
!d( 14)= d(20170828,2,'原点移動コマンドの実装')
!d( 15)= d(20170831,1,'starコマンドの簡易実装')
!d( 16)= d(20170904,1,'starコマンドの完成')
!d( 17)= d(20170907,1,'starfillコマンドの実装')
!d( 18)= d(20170929,1,'starコマンドのバグ修正')
!d( 19)= d(20170929,2,'レイヤー機能を実装し、プリアンブルで描画レイヤーを選択できるようにする')
!d( 20)= d(20171003,1,'スタック操作コマンド簡易実装(動作未確認)')
!d( 21)= d(20171003,2,'getlen実装修正(動作未確認)')
!d( 22)= d(20171124,1,'linecap実装')
!d( 23)= d(20180310,1,'fontsize,fontname,linedashの実装')
!d( 23)= d(20180310,2,'linejoinを実装')
!d( 24)= d(20180311,1,'strokergb,fillrgb,fillandの実装')
!d( 25)= d(20180311,2,'linedashのバグ修正')
!d( 26)= d(20180311,3,'add,sub,mul,divの実装')
!d( 27)= d(20180315,1,'roll_realの実装')
!d( 28)= d(20180824,1,'HTML形式の色指定と名前形式の色指定')
!%}}}
! todo %{{{
! できればsvg形式の出力もサポートしたい
! objectを等間隔に描画する機能(できればループを使用せずに)...あまり低級な処理は実装しないほうが良いかも
! starコマンドで曲線を描画できるように
! #fff形式の色指定、色名による指定
! 簡易resetコマンド
!%}}}

SUBROUTINE kdraw(inputfile,outputfile,ierr)
! usage %{{{
! ===== PARAMETERS =====
! inputfile : kdr file name for input
! outputfile : eps file name for output
! ierr :
!        input 0 for normal draw
!        input 1 for display menu
!        output 0 for success plot
!        output 2 for same input and output filename
!        output 3 for inputfile not exist
! ===== FILE DESCRIPTION =====
! !----------preamble
! {VALX} {VALY}   papersize : 用紙サイズの指定
! {VALX} {VALY}   margin    : マージン指定
! {INTX} {INTY}   grid : グリッド数の指定
! ({string})      version : バージョンの確認
! ({layername}) {displayflag} layer : レイヤー名の宣言と描画フラグの指定
! enddraw : 描画プログラムの強制終了
! begindraw : プリアンブルの終了
! !----------main
! ({layername}) layer : 指定したレイヤー名のレイヤー描画を開始
! dup_real    : 実数スタックの複製
! dup_char    : 文字列スタックの複製
! dup_flag    : フラグスタックの複製
! exch_real    : 実数スタックの交換
! exch_char    : 文字列スタックの交換
! exch_flag    : フラグスタックの交換
! roll_real    : 実数スタックのroll
! {RCOLOR} {GCOLOR} {BCOLOR}  rgb  : 色の指定
! {VAL}                     linewidth : 線幅の指定
! {VAL}                     linecap   : 線端の指定
! {VAL}                     linejoin  : 接続点の指定
! ( {VAL} {VAL} ... ) {VAL} linedash  : 線種の指定
! {VAL}  circleradius : 円の半径指定
! {VAL}      fontsize : フォントサイズの指定
! ({string}) fontname : フォント種類の指定
! {VAL}    arrowdepth : 矢印の深さ指定
! {VAL}    arrowwidth : 矢印の幅指定
! {VAL}    arrowthick : 矢印の線幅指定
! {VAL}    arrowback  : 矢印の戻り幅指定
! {VAL}    arrowturn  : 矢印の返り幅指定
! {VAL}    arrowkern  : 矢印の送り幅指定
! ({script})  script : スクリプトの直接記述
! gsave              : (gsave) script と同じ
! grestore           : (grestore) script と同じ
! closepath          : (closepath) script と同じ
! fill               : (fill) script と同じ
! stroke             : (stroke) script と同じ
! {XMIN} {YMIN} {XMAX} {YMAX}   lattice       : 格子を描画
! {XMIN} {YMIN} {XREL} {YREL}   rlattice      : 格子を描画
! {XMIN} {YMIN} {XMAX} {YMAX}   box           : 長方形の枠を描画
! {XMIN} {YMIN} {XREL} {YREL}   rbox          : 長方形の枠を描画
! {XMIN} {YMIN} {XMAX} {YMAX}   boxfill       : 長方形の内部を塗りつぶす
! {XMIN} {YMIN} {XREL} {YREL}   rboxfill      : 長方形の内部を塗りつぶす
! {XMIN} {YMIN} {XMAX} {YMAX}   boxfilland    : 長方形の内部を塗りつぶしてから描画
! {XMIN} {YMIN} {XREL} {YREL}   rboxfilland   : 長方形の内部を塗りつぶしてから描画
! {XMIN} {YMIN} {XMAX} {YMAX}   line       : 線を描画
! {XMIN} {YMIN} {XREL} {YREL}   rline      : 線を描画
! {XMIN} {YMIN} {XMAX} {YMAX}   arrow         : 矢印を描画
! {XMIN} {YMIN} {XREL} {YREL}   rarrow        : 矢印を描画
! {XMIN} {YMIN} {XMAX} {YMAX}   arrowfill     : 矢印の内部を塗りつぶす
! {XMIN} {YMIN} {XREL} {YREL}   rarrowfill    : 矢印の内部を塗りつぶす
! {XMIN} {YMIN} {XMAX} {YMAX}   arrowfilland  : 矢印の内部を塗りつぶしてから描画
! {XMIN} {YMIN} {XREL} {YREL}   rarrowfilland : 矢印の内部を塗りつぶしてから描画
! {XCENTER} {YCENTER}           circle        : 円を描画
! {XCENTER} {YCENTER}           circlefill    : 円の内部を塗りつぶす
! {XCENTER} {YCENTER}           circlefilland : 円の内部を塗りつぶしてから描画
! {x} {y} {n} {m} ( r_1 t_1 r_2 t_2 ... r_m t_m ) star        : 回転対称多角形を描画
! {x} {y} {n} {m} ( r_1 t_1 r_2 t_2 ... r_m t_m ) starfill    : 回転対称多角形の内部を塗りつぶす
! {x} {y} {n} {m} ( r_1 t_1 r_2 t_2 ... r_m t_m ) starfilland : 回転対称多角形の内部を塗りつぶしてから描画
! {X} {Y} ({string})            string       : 文字列を中央揃えで描画
! {X} {Y}                            moveto  : 移動指定
! {X} {Y}                            lineto  : 線分指定
! {X} {Y} {THETA_INIT} {THETA_FINAL} arc     : 円弧指定
! {X} {Y} {THETA_INIT} {THETA_FINAL} arcn    : 円弧指定
!
! [TODO] 以下のリストは未実装コマンド
! [note] ヤジリンや流れるループのようなマス内矢印の実装はとりあえず後回し
!
! {X1} {Y1} {X2} {Y2} ... {Xn} {Yn} {n}  polygon      : 多角形の枠を描画
! {X1} {Y1} {X2} {Y2} ... {Xn} {Yn} {n}  polygonfill  : 多角形の内部を塗りつぶす
! {X1} {Y1} {X2} {Y2} ... {Xn} {Yn} {n}  polyline     : 連続線を描画
! {X1} {Y1} {X2} {Y2} ... {Xn} {Yn} {n}  polyarrow    : 連続線矢印を描画

! enddraw : 描画の終了
!%}}}
! declaration %{{{
USE cs_psc, only: pscversion, pscupdate
USE cv_kdraw
IMPLICIT NONE
CHARACTER( LEN= * ), INTENT( IN ) :: inputfile
CHARACTER( LEN= * ), INTENT( IN ) :: outputfile
INTEGER, INTENT( INOUT ) :: ierr
CHARACTER( LEN= 500 ) :: infile, outfile
LOGICAL, EXTERNAL :: kd_isspace
INTEGER, EXTERNAL :: kd_new_devnum
!%}}}
! preparation %{{{
IF(ierr==1)THEN
  WRITE(*,'(A)') ' kdraw reference'
  WRITE(*,'(A)') '--------------------------------------------'
  WRITE(*,'(A)') '%{comment}          : コメント'
  WRITE(*,'(A)') '{x} {y}  papersize  : 用紙サイズを指定'
  WRITE(*,'(A)') '{x} {y}  margin     : マージンを指定'
  WRITE(*,'(A)') '{i} {j}  grid       : グリッドを指定'
  WRITE(*,'(A)') '({layername}) {displayflag} layer : レイヤー名の宣言と描画フラグの指定'
  WRITE(*,'(A)') 'begindraw           : プリアンブルの終了'
  WRITE(*,'(A)') '--------------------------------------------'
  WRITE(*,'(A)') '({layername}) layer   : 指定したレイヤー名のレイヤー描画を開始'
  WRITE(*,'(A)') 'dup_real    : 実数スタックの複製'
  WRITE(*,'(A)') 'dup_char    : 文字列スタックの複製'
  WRITE(*,'(A)') 'dup_flag    : フラグスタックの複製'
  WRITE(*,'(A)') 'exch_real    : 実数スタックの交換'
  WRITE(*,'(A)') 'exch_char    : 文字列スタックの交換'
  WRITE(*,'(A)') 'exch_flag    : フラグスタックの交換'
  WRITE(*,'(A)') 'roll_real    : 実数スタックのroll'
  WRITE(*,'(A)') 'add          : 加算'
  WRITE(*,'(A)') 'sub          : 減算'
  WRITE(*,'(A)') 'mul          : 乗算'
  WRITE(*,'(A)') 'div          : 除算'
  WRITE(*,'(A)') '{r} {g} {b} rgb       : 色の指定'
  WRITE(*,'(A)') '{r} {g} {b} strokergb : 描画色の指定'
  WRITE(*,'(A)') '{r} {g} {b} fillrgb   : 塗りつぶし色の指定'
  WRITE(*,'(A)') '{value} linewidth           : 線幅の指定'
  WRITE(*,'(A)') '{value} linecap             : 線端の指定'
  WRITE(*,'(A)') '{value} linejoin            : 接続点の指定'
  WRITE(*,'(A)') '({values}) {value} linedash : 線種の指定'
  WRITE(*,'(A)') '{value} circleradius  : 円の半径の指定'
  WRITE(*,'(A)') '{value} fontsize      : フォントサイズの指定'
  WRITE(*,'(A)') '({string}) fontname   : フォント種類の指定'
  WRITE(*,'(A)') '{value} arrowdepth    : 矢印の深さ指定'
  WRITE(*,'(A)') '{value} arrowwidth    : 矢印の幅指定'
  WRITE(*,'(A)') '{value} arrowthick    : 矢印の線幅指定'
  WRITE(*,'(A)') '{value} arrowback     : 矢印の戻り幅指定'
  WRITE(*,'(A)') '{value} arrowturn     : 矢印の返り幅指定'
  WRITE(*,'(A)') '{value} arrowkern     : 矢印の送り幅指定'
  WRITE(*,'(A)') '({script}) script     : スクリプトの直接指定'
  WRITE(*,'(A)') 'gsave                 : (gsave) script と同じ'
  WRITE(*,'(A)') 'grestore              : (grestore) script と同じ'
  WRITE(*,'(A)') 'closepath             : (closepath) script と同じ'
  WRITE(*,'(A)') 'fill                  : (fill) script と同じ'
  WRITE(*,'(A)') 'stroke                : (stroke) script と同じ'
  WRITE(*,'(A)') '{X} {Y} moveto                          : 移動指定'
  WRITE(*,'(A)') '{X} {Y} lineto                          : 線分指定'
  WRITE(*,'(A)') '{X} {Y} {THETA_INIT} {THETA_FINAL} arc  : 円弧指定'
  WRITE(*,'(A)') '{X} {Y} {THETA_INIT} {THETA_FINAL} arcn : 円弧指定'
  WRITE(*,'(A)') '{XMIN} {YMIN} {XMAX} {YMAX} lattice  : 格子を描画'
  WRITE(*,'(A)') '{XMIN} {YMIN} {XMAX} {YMAX} box         : 長方形の枠を描画'
  WRITE(*,'(A)') '{XMIN} {YMIN} {XMAX} {YMAX} boxfill     : 長方形内部を塗りつぶす'
  WRITE(*,'(A)') '{XMIN} {YMIN} {XMAX} {YMAX} boxfilland  : 長方形内部を塗りつぶてから描画'
  WRITE(*,'(A)') '{XMIN} {YMIN} {XMAX} {YMAX} line     : 直線を描画'
  WRITE(*,'(A)') '{XMIN} {YMIN} {XMAX} {YMAX} arrow        : 矢印を描画'
  WRITE(*,'(A)') '{XMIN} {YMIN} {XMAX} {YMAX} arrowfill    : 矢印の内部を塗りつぶす'
  WRITE(*,'(A)') '{XMIN} {YMIN} {XMAX} {YMAX} arrowfilland : 矢印の内部を塗りつぶしてから描画'
  WRITE(*,'(A)') '{X} {Y} circle         : 円を描画'
  WRITE(*,'(A)') '{X} {Y} circlefill     : 円の内部を塗りつぶす'
  WRITE(*,'(A)') '{X} {Y} circlefilland  : 円の内部を塗りつぶしてから描画'
  WRITE(*,'(A)') '{x} {y} {n} {m} ( r_1 t_1 r_2 t_2 ... r_m t_m ) star'
  WRITE(*,'(A)') '  : 回転対称多角形を描画'
  WRITE(*,'(A)') '{x} {y} {n} {m} ( r_1 t_1 r_2 t_2 ... r_m t_m ) starfill'
  WRITE(*,'(A)') '  : 回転対称多角形の内部を塗りつぶす'
  WRITE(*,'(A)') '{x} {y} {n} {m} ( r_1 t_1 r_2 t_2 ... r_m t_m ) starfilland'
  WRITE(*,'(A)') '  : 回転対称多角形の内部を塗りつぶしてから描画'
  WRITE(*,'(A)') '{X} {Y} ({string}) string            : 文字列を描画'
  WRITE(*,'(A)') 'enddraw   : 描画の終了'
  RETURN
ENDIF
infile = TRIM(ADJUSTL(inputfile))
outfile = TRIM(ADJUSTL(outputfile))
IF(infile==outfile)THEN
  ierr=2 ! Error: filename for input and output must differ
  RETURN
ENDIF
!%}}}
! init %{{{
version_accepted = .TRUE.
warn_unknowncommand = .TRUE.
warn_unknownlayername = .TRUE.
warn_insufficientstack = .TRUE.
papx = 200.d0
papy = 200.d0
marx = 10.d0
mary = 10.d0
grdx = 10
grdy = 10
CALL kd_layerinitparams
layernamelist(:) = ''
layerdisplay(:) = .TRUE.
layernamelist(0) = 'default'
layerdisplay(0) = .TRUE.
!%}}}
! open input file %{{{
IF( infile == 'STDIN' .OR. infile == 'stdin' .OR. infile == '' )THEN
  iread=5
ELSE
  INQUIRE(FILE=TRIM(infile),EXIST=flaga,OPENED=flagb)
  IF(.NOT.flaga)THEN
    ierr=3 ! Error: inputfile not exist
    RETURN
  ENDIF
  IF(     flagb)THEN
    INQUIRE(FILE=TRIM(infile),NUMBER=iios)
    CLOSE(iios)
  ENDIF
  iread=kd_new_devnum()
  OPEN(iread,FILE=TRIM(infile),ACCESS='direct',FORM='unformatted',RECL=1)
END IF
!%}}}
! kdraw prolog %{{{
CALL kd_clearstack
irec = 0
ilevel = 0
cword = ''
cwordlen = 0
comment = .FALSE.
end_word = .FALSE.
clear_stack = .FALSE.
currentlayerid = 1 ! only in next loop, this variable express smallest layer id not used
DO
  ! read %{{{
  irec=irec+1
  READ(iread,REC=irec,IOSTAT=iios)cchar
  IF(iios/=0)EXIT
  !%}}}
  ! first IF %{{{
  IF((ilevel==0).AND.kd_isspace(cchar))THEN
    end_word=.TRUE.
  ELSE IF((ilevel==0).AND.(cchar=='%'))THEN
    end_word=.TRUE.
    comment=.TRUE.
  ELSE IF((cchar==ACHAR(10)))THEN
    IF(ilevel/=0)THEN
      cword = ''
      cwordlen = 0
      ilevel=0
    END IF
    end_word=.TRUE.
    clear_stack=.TRUE.
    comment=.FALSE.
  ELSE IF(comment)THEN
  ELSE IF((ilevel>0).OR.(cchar=='(').OR.(cchar==')'))THEN
    IF(cchar=='(')THEN
      ilevel=ilevel+1
    ELSE IF(cchar==')')THEN
      ilevel=ilevel-1
    END IF
    cwordlen = cwordlen + 1
    cword(cwordlen:cwordlen) = cchar
    IF(ilevel==0)THEN
      end_word=.TRUE.
    END IF
  ELSE
    cwordlen = cwordlen + 1
    cword(cwordlen:cwordlen) = cchar
  END IF
  !%}}}
  ! second IF %{{{
  IF(end_word)THEN
    !cword = TRIM(ADJUSTL(cword))
    ccomd=''
    CALL kd_readcword
    cword = ''
    cwordlen = 0
    end_word=.FALSE.
    IF(ccomd/='')THEN
      IF(ccomd=='papersize')THEN
        IF(rsmax>=2)THEN
          papx = rstack(rsmax-1)
          papy = rstack(rsmax)
          CALL kd_killrealstack()
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='margin')THEN
        IF(rsmax>=2)THEN
          marx = rstack(rsmax-1)
          mary = rstack(rsmax)
          CALL kd_killrealstack()
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='grid')THEN
        IF(rsmax>=2)THEN
          grdx = NINT(rstack(rsmax-1))
          grdy = NINT(rstack(rsmax))
          CALL kd_killrealstack()
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='layer')THEN
        IF(csmax>=1.AND.fsmax>=1)THEN
          layernamelist(currentlayerid) = cstack(csmax)
          layerdisplay(currentlayerid) = fstack(fsmax)
          currentlayerid = currentlayerid + 1
          ! only in next loop, this variable express smallest layer id not used
          CALL kd_killcharstack()
          CALL kd_killflagstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='checkversion')THEN
        IF(csmax>=1)THEN
          CALL kd_checkversion()
          IF( version_accepted )THEN
            WRITE(*,*) 'KDRAW version accepted'
          ELSE
            WRITE(*,*) 'KDRAW version not accepted'
            WRITE(*,*) 'KDRAW terminated'
            RETURN
          END IF
          CALL kd_killcharstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='enddraw')THEN
        ! draw nothing and exit
        WRITE(*,*) 'KDRAW terminated'
        RETURN
      ELSE IF(ccomd=='begindraw')THEN
        EXIT
      ELSE
        CALL kd_warnunknowncommand
      END IF
    END IF
  END IF
  !%}}}
  ! clear_stack %{{{
  IF(clear_stack)THEN
    CALL kd_clearstack
    clear_stack=.FALSE.
  ENDIF
  !%}}}
END DO
!%}}}
! open output file %{{{
IF( outfile == 'STDOUT' .OR. outfile == 'stdout' .OR. outfile == '' )THEN
  iwrite=6
ELSE
  iwrite=kd_new_devnum()
  OPEN(iwrite,FILE=TRIM(outfile),STATUS='replace')
END IF
!%}}}
! initial DSC %{{{
WRITE(iwrite,'(A)') '%!PS-Adobe-3.0 EPSF-3.0'
WRITE(iwrite,'(A)') '%%Title: '//TRIM(outfile)
WRITE(iwrite,'(A)') '%%Creator: kdraw under PSC version ' // &
& TRIM(pscversion) // ' ' // TRIM(pscupdate)
WRITE(iwrite,'(A)') '%%Orientation: Portrait'
WRITE(iwrite,'(A,2I5)') '%%BoundingBox: 0 0', NINT(papx), NINT(papy)
WRITE(iwrite,'(A)') '%%Pages: 1'
WRITE(iwrite,'(A)') '%%PageOrder: Ascend'
WRITE(iwrite,'(A)') '%%EndComments'
!%}}}
! prolog %{{{
WRITE(iwrite,'(A)') '%%BeginProlog'
WRITE(iwrite,'(A)') '/kdrdict 256 dict def'
WRITE(iwrite,'(A)') 'kdrdict begin'
! parameters
WRITE(iwrite,'(A,F16.4,A)') '/papx ',papx,' def'
WRITE(iwrite,'(A,F16.4,A)') '/papy ',papy,' def'
WRITE(iwrite,'(A,F16.4,A)') '/marx ',marx,' def'
WRITE(iwrite,'(A,F16.4,A)') '/mary ',mary,' def'
WRITE(iwrite,'(A,I8,A)') '/grdx ',grdx,' def'
WRITE(iwrite,'(A,I8,A)') '/grdy ',grdy,' def'
! macros
WRITE(iwrite,'(A)') '/originx 0.0 def'
WRITE(iwrite,'(A)') '/originy 0.0 def'
WRITE(iwrite,'(A)') '/origin { /originy exch def /originx exch def } def'
WRITE(iwrite,'(A)') '/widx papx marx 2 mul sub def'
WRITE(iwrite,'(A)') '/widy papy mary 2 mul sub def'
WRITE(iwrite,'(A)') '/getx { originx add grdx div widx mul marx add } def'
WRITE(iwrite,'(A)') '/gety { originy add grdy div widy mul mary add } def'
WRITE(iwrite,'(A)') '/getxy { gety exch getx exch } def'
WRITE(iwrite,'(A)') '/getlen { widx grdx div mul } def' ! 元:{ getx 0 getx sub }
WRITE(iwrite,'(A)') '/arrowrealdepth { arrowdepth getlen } def'
WRITE(iwrite,'(A)') '/arrowrealwidth { arrowwidth getlen } def'
WRITE(iwrite,'(A)') '/arrowrealthick { arrowthick getlen } def'
WRITE(iwrite,'(A)') '/arrowrealback { arrowback getlen } def'
WRITE(iwrite,'(A)') '/arrowrealturn { arrowturn getlen } def'
WRITE(iwrite,'(A)') '/arrowrealkern { arrowkern getlen } def'
WRITE(iwrite,'(A)') '/circlerealradius { circleradius getlen } def'
WRITE(iwrite,'(A)') '/linerealwidth { linewidth getlen } def'
WRITE(iwrite,'(A)') '/fontrealsize { fontsize getlen } def'
WRITE(iwrite,'(A)') '/usermoveto { getxy moveto } def'
WRITE(iwrite,'(A)') '/userlineto { getxy lineto } def'
WRITE(iwrite,'(A)') '/usertranslate { getxy translate } def'
WRITE(iwrite,'(A)') '/setlinedash {'
WRITE(iwrite,'(A)') '  3 dict begin'
WRITE(iwrite,'(A)') '  /dist exch def'
WRITE(iwrite,'(A)') '  /arr exch def'
WRITE(iwrite,'(A)') '  0 1 arr length 1 sub'
WRITE(iwrite,'(A)') '  {'
WRITE(iwrite,'(A)') '    /i exch def'
WRITE(iwrite,'(A)') '    arr i arr i get getlen put'
WRITE(iwrite,'(A)') '  } for'
WRITE(iwrite,'(A)') '  arr dist getlen setdash'
WRITE(iwrite,'(A)') '  end'
WRITE(iwrite,'(A)') '} def'
! arc %{{{
WRITE(iwrite,'(A)') '/userarc {'
WRITE(iwrite,'(A)') '  4 dict begin'
WRITE(iwrite,'(A)') '  /thetafinal exch def'
WRITE(iwrite,'(A)') '  /thetainit exch def'
WRITE(iwrite,'(A)') '  /y exch def'
WRITE(iwrite,'(A)') '  /x exch def'
WRITE(iwrite,'(A)') '  x getx y gety circlerealradius thetainit thetafinal arc'
WRITE(iwrite,'(A)') '  end'
WRITE(iwrite,'(A)') '} def'
WRITE(iwrite,'(A)') '/userarcn {'
WRITE(iwrite,'(A)') '  4 dict begin'
WRITE(iwrite,'(A)') '  /thetafinal exch def'
WRITE(iwrite,'(A)') '  /thetainit exch def'
WRITE(iwrite,'(A)') '  /y exch def'
WRITE(iwrite,'(A)') '  /x exch def'
WRITE(iwrite,'(A)') '  x getx y gety circlerealradius thetainit thetafinal arcn'
WRITE(iwrite,'(A)') '  end'
WRITE(iwrite,'(A)') '} def'
!%}}}
! line %{{{
WRITE(iwrite,'(A)') '/linedraw {'
WRITE(iwrite,'(A)') '  usermoveto userlineto stroke'
WRITE(iwrite,'(A)') '} def'
WRITE(iwrite,'(A)') '/rlinedraw {'
WRITE(iwrite,'(A)') '  4 dict begin'
WRITE(iwrite,'(A)') '  /yrel exch def'
WRITE(iwrite,'(A)') '  /xrel exch def'
WRITE(iwrite,'(A)') '  /ymin exch def'
WRITE(iwrite,'(A)') '  /xmin exch def'
WRITE(iwrite,'(A)') '  newpath'
WRITE(iwrite,'(A)') '  xmin ymin usermoveto'
WRITE(iwrite,'(A)') '  xmin xrel add ymin yrel add userlineto'
WRITE(iwrite,'(A)') '  stroke'
WRITE(iwrite,'(A)') '  end'
WRITE(iwrite,'(A)') '} def'
!%}}}
! arrow %{{{
WRITE(iwrite,'(A)') '/arrowpath {'
WRITE(iwrite,'(A)') '  7 dict begin'
WRITE(iwrite,'(A)') '  /ymax exch def'
WRITE(iwrite,'(A)') '  /xmax exch def'
WRITE(iwrite,'(A)') '  /ymin exch def'
WRITE(iwrite,'(A)') '  /xmin exch def'
WRITE(iwrite,'(A)') '  /dx xmax getx xmin getx sub def'
WRITE(iwrite,'(A)') '  /dy ymax gety ymin gety sub def'
WRITE(iwrite,'(A)') '  /len dx dx mul dy dy mul add sqrt arrowrealkern add def'
WRITE(iwrite,'(A)') '  newpath'
WRITE(iwrite,'(A)') '  xmin ymin usertranslate'
WRITE(iwrite,'(A)') '  dy dx atan rotate'
WRITE(iwrite,'(A)') '  0 arrowrealback sub arrowrealthick -2 div moveto'
WRITE(iwrite,'(A)') '  0 arrowrealback sub arrowrealthick 2 div lineto'
WRITE(iwrite,'(A)') '  len arrowrealdepth sub arrowrealturn add arrowrealthick 2 div lineto'
WRITE(iwrite,'(A)') '  len arrowrealdepth sub arrowrealwidth 2 div lineto'
WRITE(iwrite,'(A)') '  len 0 lineto'
WRITE(iwrite,'(A)') '  len arrowrealdepth sub arrowrealwidth -2 div lineto'
WRITE(iwrite,'(A)') '  len arrowrealdepth sub arrowrealturn add arrowrealthick -2 div lineto'
WRITE(iwrite,'(A)') '  closepath'
WRITE(iwrite,'(A)') '  end'
WRITE(iwrite,'(A)') '} def'
WRITE(iwrite,'(A)') '/arrowdraw {'
WRITE(iwrite,'(A)') '  4 dict begin'
WRITE(iwrite,'(A)') '  /ymax exch def'
WRITE(iwrite,'(A)') '  /xmax exch def'
WRITE(iwrite,'(A)') '  /ymin exch def'
WRITE(iwrite,'(A)') '  /xmin exch def'
WRITE(iwrite,'(A)') '  gsave'
WRITE(iwrite,'(A)') '  xmin ymin xmax ymax arrowpath stroke'
WRITE(iwrite,'(A)') '  grestore'
WRITE(iwrite,'(A)') '  end'
WRITE(iwrite,'(A)') '} def'
WRITE(iwrite,'(A)') '/rarrowdraw {'
WRITE(iwrite,'(A)') '  4 dict begin'
WRITE(iwrite,'(A)') '  /yrel exch def'
WRITE(iwrite,'(A)') '  /xrel exch def'
WRITE(iwrite,'(A)') '  /ymin exch def'
WRITE(iwrite,'(A)') '  /xmin exch def'
WRITE(iwrite,'(A)') '  gsave'
WRITE(iwrite,'(A)') '  xmin ymin xmin xrel add ymin yrel add arrowpath stroke'
WRITE(iwrite,'(A)') '  grestore'
WRITE(iwrite,'(A)') '  end'
WRITE(iwrite,'(A)') '} def'
WRITE(iwrite,'(A)') '/arrowfill {'
WRITE(iwrite,'(A)') '  4 dict begin'
WRITE(iwrite,'(A)') '  /ymax exch def'
WRITE(iwrite,'(A)') '  /xmax exch def'
WRITE(iwrite,'(A)') '  /ymin exch def'
WRITE(iwrite,'(A)') '  /xmin exch def'
WRITE(iwrite,'(A)') '  gsave'
WRITE(iwrite,'(A)') '  xmin ymin xmax ymax arrowpath fill'
WRITE(iwrite,'(A)') '  grestore'
WRITE(iwrite,'(A)') '  end'
WRITE(iwrite,'(A)') '} def'
WRITE(iwrite,'(A)') '/rarrowfill {'
WRITE(iwrite,'(A)') '  4 dict begin'
WRITE(iwrite,'(A)') '  /yrel exch def'
WRITE(iwrite,'(A)') '  /xrel exch def'
WRITE(iwrite,'(A)') '  /ymin exch def'
WRITE(iwrite,'(A)') '  /xmin exch def'
WRITE(iwrite,'(A)') '  gsave'
WRITE(iwrite,'(A)') '  xmin ymin xmin xrel add ymin yrel add arrowpath fill'
WRITE(iwrite,'(A)') '  grestore'
WRITE(iwrite,'(A)') '  end'
WRITE(iwrite,'(A)') '} def'
!%}}}
! box %{{{
WRITE(iwrite,'(A)') '/boxdraw {'
WRITE(iwrite,'(A)') '  4 dict begin'
WRITE(iwrite,'(A)') '  /ymax exch def'
WRITE(iwrite,'(A)') '  /xmax exch def'
WRITE(iwrite,'(A)') '  /ymin exch def'
WRITE(iwrite,'(A)') '  /xmin exch def'
WRITE(iwrite,'(A)') '  newpath'
WRITE(iwrite,'(A)') '  xmin ymin usermoveto'
WRITE(iwrite,'(A)') '  xmin ymax userlineto'
WRITE(iwrite,'(A)') '  xmax ymax userlineto'
WRITE(iwrite,'(A)') '  xmax ymin userlineto'
WRITE(iwrite,'(A)') '  closepath'
WRITE(iwrite,'(A)') '  stroke'
WRITE(iwrite,'(A)') '  end'
WRITE(iwrite,'(A)') '} def'
WRITE(iwrite,'(A)') '/rboxdraw {'
WRITE(iwrite,'(A)') '  4 dict begin'
WRITE(iwrite,'(A)') '  /yrel exch def'
WRITE(iwrite,'(A)') '  /xrel exch def'
WRITE(iwrite,'(A)') '  /ymin exch def'
WRITE(iwrite,'(A)') '  /xmin exch def'
WRITE(iwrite,'(A)') '  xmin ymin xmin xrel add ymin yrel add boxdraw'
WRITE(iwrite,'(A)') '  end'
WRITE(iwrite,'(A)') '} def'
WRITE(iwrite,'(A)') '/boxfill {'
WRITE(iwrite,'(A)') '  4 dict begin'
WRITE(iwrite,'(A)') '  /ymax exch def'
WRITE(iwrite,'(A)') '  /xmax exch def'
WRITE(iwrite,'(A)') '  /ymin exch def'
WRITE(iwrite,'(A)') '  /xmin exch def'
WRITE(iwrite,'(A)') '  newpath'
WRITE(iwrite,'(A)') '  xmin ymin usermoveto'
WRITE(iwrite,'(A)') '  xmin ymax userlineto'
WRITE(iwrite,'(A)') '  xmax ymax userlineto'
WRITE(iwrite,'(A)') '  xmax ymin userlineto'
WRITE(iwrite,'(A)') '  closepath'
WRITE(iwrite,'(A)') '  fill'
WRITE(iwrite,'(A)') '  end'
WRITE(iwrite,'(A)') '} def'
WRITE(iwrite,'(A)') '/rboxfill {'
WRITE(iwrite,'(A)') '  6 dict begin'
WRITE(iwrite,'(A)') '  /yrel exch def'
WRITE(iwrite,'(A)') '  /xrel exch def'
WRITE(iwrite,'(A)') '  /ymin exch def'
WRITE(iwrite,'(A)') '  /xmin exch def'
WRITE(iwrite,'(A)') '  /xmax xmin xrel add def'
WRITE(iwrite,'(A)') '  /ymax ymin yrel add def'
WRITE(iwrite,'(A)') '  xmin ymin xmax ymax boxfill'
WRITE(iwrite,'(A)') '  end'
WRITE(iwrite,'(A)') '} def'
!%}}}
! lattice %{{{
WRITE(iwrite,'(A)') '/latticedraw {'
WRITE(iwrite,'(A)') '  6 dict begin'
WRITE(iwrite,'(A)') '  /ymax exch def'
WRITE(iwrite,'(A)') '  /xmax exch def'
WRITE(iwrite,'(A)') '  /ymin exch def'
WRITE(iwrite,'(A)') '  /xmin exch def'
WRITE(iwrite,'(A)') '  /xrel xmax xmin sub def'
WRITE(iwrite,'(A)') '  /yrel ymax ymin sub def'
WRITE(iwrite,'(A)') '  xmin ymin xrel yrel rlatticedraw'
WRITE(iwrite,'(A)') '  end'
WRITE(iwrite,'(A)') '} def'
WRITE(iwrite,'(A)') '/rlatticedraw {'
WRITE(iwrite,'(A)') '  4 dict begin'
WRITE(iwrite,'(A)') '  /yrel exch def'
WRITE(iwrite,'(A)') '  /xrel exch def'
WRITE(iwrite,'(A)') '  /ymin exch def'
WRITE(iwrite,'(A)') '  /xmin exch def'
WRITE(iwrite,'(A)') '  newpath'
WRITE(iwrite,'(A)') '  0 1 xrel {'
WRITE(iwrite,'(A)') '    xmin add getx dup'
WRITE(iwrite,'(A)') '    ymin gety moveto'
WRITE(iwrite,'(A)') '    ymin yrel add gety lineto'
WRITE(iwrite,'(A)') '    stroke'
WRITE(iwrite,'(A)') '  } for'
WRITE(iwrite,'(A)') '  0 1 yrel {'
WRITE(iwrite,'(A)') '    ymin add gety dup'
WRITE(iwrite,'(A)') '    xmin getx exch moveto'
WRITE(iwrite,'(A)') '    xmin xrel add getx exch lineto'
WRITE(iwrite,'(A)') '    stroke'
WRITE(iwrite,'(A)') '  } for'
WRITE(iwrite,'(A)') '  end'
WRITE(iwrite,'(A)') '} def'
!%}}}
! circle %{{{
WRITE(iwrite,'(A)') '/circledraw {'
WRITE(iwrite,'(A)') '  2 dict begin'
WRITE(iwrite,'(A)') '  /y exch def'
WRITE(iwrite,'(A)') '  /x exch def'
WRITE(iwrite,'(A)') '  x circleradius add y usermoveto'
WRITE(iwrite,'(A)') '  x getx y gety circlerealradius 0 360 arc stroke'
WRITE(iwrite,'(A)') '  end'
WRITE(iwrite,'(A)') '} def'
WRITE(iwrite,'(A)') '/circlefill {'
WRITE(iwrite,'(A)') '  2 dict begin'
WRITE(iwrite,'(A)') '  /y exch def'
WRITE(iwrite,'(A)') '  /x exch def'
WRITE(iwrite,'(A)') '  x circleradius add y usermoveto'
WRITE(iwrite,'(A)') '  x getx y gety circlerealradius 0 360 arc fill'
WRITE(iwrite,'(A)') '  end'
WRITE(iwrite,'(A)') '} def'
!%}}}
! star %{{{
WRITE(iwrite,'(A)') '/starpath {'
WRITE(iwrite,'(A)') '  % [usage] x y n m [ r_1 t_1 r_2 t_2 ... r_m t_m ] starpath'
WRITE(iwrite,'(A)') '  9 dict begin'
WRITE(iwrite,'(A)') '  /arr exch def'
WRITE(iwrite,'(A)') '  /m exch def'
WRITE(iwrite,'(A)') '  /n exch def'
WRITE(iwrite,'(A)') '  /ybase exch def'
WRITE(iwrite,'(A)') '  /xbase exch def'
WRITE(iwrite,'(A)') '  /dtheta 360 n div def'
WRITE(iwrite,'(A)') '  /theta 0 def'
WRITE(iwrite,'(A)') '  1 1 n {'
WRITE(iwrite,'(A)') '    /i exch def'
WRITE(iwrite,'(A)') '    1 1 m {'
WRITE(iwrite,'(A)') '      /j exch def'
WRITE(iwrite,'(A)') '      arr j 2 mul 2 sub get'
WRITE(iwrite,'(A)') '      arr j 2 mul 1 sub get dtheta mul theta add'
WRITE(iwrite,'(A)') '      2 copy'
WRITE(iwrite,'(A)') '      cos mul xbase add'
WRITE(iwrite,'(A)') '      3 1 roll'
WRITE(iwrite,'(A)') '      sin mul ybase add'
WRITE(iwrite,'(A)') '      i 1 eq j 1 eq and {'
WRITE(iwrite,'(A)') '        usermoveto'
WRITE(iwrite,'(A)') '      }{'
WRITE(iwrite,'(A)') '        userlineto'
WRITE(iwrite,'(A)') '      } ifelse'
WRITE(iwrite,'(A)') '    } for'
WRITE(iwrite,'(A)') '    /theta theta dtheta add def'
WRITE(iwrite,'(A)') '  } for'
WRITE(iwrite,'(A)') '  closepath'
WRITE(iwrite,'(A)') '  end'
WRITE(iwrite,'(A)') '} def'
WRITE(iwrite,'(A)') '/stardraw {'
WRITE(iwrite,'(A)') '  % [usage] x y n m [ r_1 t_1 r_2 t_2 ... r_m t_m ] stardraw'
WRITE(iwrite,'(A)') '  gsave'
WRITE(iwrite,'(A)') '  starpath stroke'
WRITE(iwrite,'(A)') '  grestore'
WRITE(iwrite,'(A)') '} def'
WRITE(iwrite,'(A)') '/starfill {'
WRITE(iwrite,'(A)') '  % [usage] x y n m [ r_1 t_1 r_2 t_2 ... r_m t_m ] stardraw'
WRITE(iwrite,'(A)') '  gsave'
WRITE(iwrite,'(A)') '  starpath fill'
WRITE(iwrite,'(A)') '  grestore'
WRITE(iwrite,'(A)') '} def'
!%}}}
! string %{{{
WRITE(iwrite,'(A)') '/stringdraw {'
WRITE(iwrite,'(A)') '  3 dict begin'
WRITE(iwrite,'(A)') '  /str exch def'
WRITE(iwrite,'(A)') '  /y exch def'
WRITE(iwrite,'(A)') '  /x exch def'
WRITE(iwrite,'(A)') '  x y usermoveto'
WRITE(iwrite,'(A)') '  str stringwidth pop -2 div'
WRITE(iwrite,'(A)') '  str true charpath pathbbox 4 1 roll pop 3 1 roll pop exch sub -2 div'
WRITE(iwrite,'(A)') '  newpath'
WRITE(iwrite,'(A)') '  x getx y gety moveto'
WRITE(iwrite,'(A)') '  rmoveto'
WRITE(iwrite,'(A)') '  str show'
WRITE(iwrite,'(A)') '  end'
WRITE(iwrite,'(A)') '} def'
!%}}}
WRITE(iwrite,'(A)') 'end'
WRITE(iwrite,'(A)') '%%EndProlog'
!%}}}
WRITE(iwrite,'(A)') '%%Page: 1'
! Page Setup %{{{
WRITE(iwrite,'(A)') '%%BeginPageSetup'
WRITE(iwrite,'(A)') '/kdrsave save def'
WRITE(iwrite,'(A)') 'gsave'
WRITE(iwrite,'(A)') 'clipsave'
WRITE(iwrite,'(A)') 'kdrdict begin'
CALL kd_layerprintparams
WRITE(iwrite,'(A)') 'linerealwidth setlinewidth'
WRITE(iwrite,'(A)') '%%EndPageSetup'
!%}}}
! loop prep %{{{
CALL kd_clearstack
ilevel = 0
cword = ''
cwordlen = 0
comment = .FALSE.
end_word = .FALSE.
clear_stack = .FALSE.
currentlayerid = 0
!%}}}
DO
  ! read %{{{
  irec=irec+1
  READ(iread,REC=irec,IOSTAT=iios)cchar
  IF(iios/=0)EXIT
  !%}}}
  ! first IF %{{{
  IF((ilevel==0).AND.kd_isspace(cchar))THEN
    end_word=.TRUE.
  ELSE IF((ilevel==0).AND.(cchar=='%'))THEN
    end_word=.TRUE.
    comment=.TRUE.
  ELSE IF((cchar==ACHAR(10)))THEN
    IF(ilevel/=0)THEN
      cword = ''
      cwordlen = 0
      ilevel=0
    END IF
    end_word=.TRUE.
    clear_stack=.TRUE.
    comment=.FALSE.
  ELSE IF(comment)THEN
  ELSE IF((ilevel>0).OR.(cchar=='(').OR.(cchar==')'))THEN
    IF(cchar=='(')THEN
      ilevel=ilevel+1
    ELSE IF(cchar==')')THEN
      ilevel=ilevel-1
    END IF
    cwordlen = cwordlen + 1
    cword(cwordlen:cwordlen) = cchar
    IF(ilevel==0)THEN
      end_word=.TRUE.
    END IF
  ELSE
    cwordlen = cwordlen + 1
    cword(cwordlen:cwordlen) = cchar
  END IF
  !%}}}
  ! second IF %{{{
  IF(end_word)THEN
    ccomd=''
    CALL kd_readcword
    cword = ''
    cwordlen = 0
    end_word=.FALSE.
    IF(ccomd/='')THEN
      IF(.not.layerdisplay(currentlayerid))THEN
        IF(ccomd/='enddraw'.and.ccomd/='layer')THEN
          CYCLE
        END IF
      END IF
      IF(ccomd=='enddraw')THEN
        EXIT
      ! basics %{{{
      ELSE IF(ccomd=='layer')THEN
        IF(csmax>=1)THEN
          CALL kd_getlayeridfromname(cstack(csmax))
          IF(currentlayerid==-1)THEN
            CALL kd_warnunknownlayername(cstack(csmax))
            currentlayerid = 0
          ELSE
            CALL kd_layerinitparams
            CALL kd_layerprintparams
          END IF
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='origin')THEN
        IF(rsmax>=2)THEN
          WRITE(iwrite,'(2F14.4,A)') &
          & rstack(rsmax-1:rsmax), ' origin'
          CALL kd_killrealstack()
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      !%}}}
      ! stack operations %{{{
      ELSE IF(ccomd=='dup_real')THEN
        IF(rsmax>=1)THEN
          CALL kd_duplicaterealstack
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='dup_char')THEN
        IF(csmax>=1)THEN
          CALL kd_duplicatecharstack
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='dup_flag')THEN
        IF(fsmax>=1)THEN
          CALL kd_duplicateflagstack
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='exch_real')THEN
        IF(rsmax>=2)THEN
          CALL kd_exchangerealstack
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='exch_char')THEN
        IF(csmax>=2)THEN
          CALL kd_exchangecharstack
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='exch_flag')THEN
        IF(fsmax>=2)THEN
          CALL kd_exchangeflagstack
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='roll_real')THEN
        IF(rsmax>=4)THEN
          CALL kd_rollrealstack
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      !%}}}
      ! stack math %{{{
      ! [TODO] オーバーフロー対策
      ELSE IF(ccomd=='add')THEN
        IF(rsmax>=2)THEN
          rstack(rsmax-1) = rstack(rsmax-1) + rstack(rsmax)
          rsmax = rsmax - 1
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='sub')THEN
        IF(rsmax>=2)THEN
          rstack(rsmax-1) = rstack(rsmax-1) - rstack(rsmax)
          rsmax = rsmax - 1
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='mul')THEN
        IF(rsmax>=2)THEN
          rstack(rsmax-1) = rstack(rsmax-1) * rstack(rsmax)
          rsmax = rsmax - 1
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='div')THEN
        IF(rsmax>=2)THEN
          rstack(rsmax-1) = rstack(rsmax-1) / rstack(rsmax)
          rsmax = rsmax - 1
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      !%}}}
      ! lattice %{{{
      ELSE IF(ccomd=='lattice')THEN
        IF(rsmax>=4)THEN
          WRITE(iwrite,'(2F14.4,2I8,A)') rstack(rsmax-3:rsmax-2), &
          & NINT(rstack(rsmax-1:rsmax)), ' latticedraw'
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='rlattice')THEN
        IF(rsmax>=4)THEN
          WRITE(iwrite,'(4F14.4,A)') rstack(rsmax-3:rsmax), ' rlatticedraw'
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      !%}}}
      ! box %{{{
      ELSE IF(ccomd=='box')THEN
        IF(rsmax>=4)THEN
          WRITE(iwrite,'(4F14.4,A)') rstack(rsmax-3:rsmax), ' boxdraw'
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='rbox')THEN
        IF(rsmax>=4)THEN
          WRITE(iwrite,'(4F14.4,A)') rstack(rsmax-3:rsmax), ' rboxdraw'
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='boxfill')THEN
        IF(rsmax>=4)THEN
          WRITE(iwrite,'(A)') 'fillrgb setrgbcolor'
          WRITE(iwrite,'(4F14.4,A)') rstack(rsmax-3:rsmax), ' boxfill'
          WRITE(iwrite,'(A)') 'strokergb setrgbcolor'
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='rboxfill')THEN
        IF(rsmax>=4)THEN
          WRITE(iwrite,'(A)') 'fillrgb setrgbcolor'
          WRITE(iwrite,'(4F14.4,A)') rstack(rsmax-3:rsmax), ' rboxfill'
          WRITE(iwrite,'(A)') 'strokergb setrgbcolor'
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='boxfilland')THEN
        IF(rsmax>=4)THEN
          WRITE(iwrite,'(A)') 'fillrgb setrgbcolor'
          WRITE(iwrite,'(4F14.4,A)') rstack(rsmax-3:rsmax), ' boxfill'
          WRITE(iwrite,'(A)') 'strokergb setrgbcolor'
          WRITE(iwrite,'(4F14.4,A)') rstack(rsmax-3:rsmax), ' boxdraw'
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='rboxfilland')THEN
        IF(rsmax>=4)THEN
          WRITE(iwrite,'(A)') 'fillrgb setrgbcolor'
          WRITE(iwrite,'(4F14.4,A)') rstack(rsmax-3:rsmax), ' rboxfill'
          WRITE(iwrite,'(A)') 'strokergb setrgbcolor'
          WRITE(iwrite,'(4F14.4,A)') rstack(rsmax-3:rsmax), ' rboxdraw'
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      !%}}}
      ! line %{{{
      ELSE IF(ccomd=='line')THEN
        IF(rsmax>=4)THEN
          WRITE(iwrite,'(4F14.4,A)') rstack(rsmax-3:rsmax), ' linedraw'
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='rline')THEN
        IF(rsmax>=4)THEN
          WRITE(iwrite,'(4F14.4,A)') rstack(rsmax-3:rsmax), ' rlinedraw'
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      !%}}}
      ! arrow %{{{
      ELSE IF(ccomd=='arrow')THEN
        IF(rsmax>=4)THEN
          WRITE(iwrite,'(4F14.4,A)') rstack(rsmax-3:rsmax), ' arrowdraw'
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='rarrow')THEN
        IF(rsmax>=4)THEN
          WRITE(iwrite,'(4F14.4,A)') rstack(rsmax-3:rsmax), ' rarrowdraw'
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='arrowfill')THEN
        IF(rsmax>=4)THEN
          WRITE(iwrite,'(A)') 'fillrgb setrgbcolor'
          WRITE(iwrite,'(4F14.4,A)') rstack(rsmax-3:rsmax), ' arrowfill'
          WRITE(iwrite,'(A)') 'strokergb setrgbcolor'
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='rarrowfill')THEN
        IF(rsmax>=4)THEN
          WRITE(iwrite,'(A)') 'fillrgb setrgbcolor'
          WRITE(iwrite,'(4F14.4,A)') rstack(rsmax-3:rsmax), ' rarrowfill'
          WRITE(iwrite,'(A)') 'strokergb setrgbcolor'
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='arrowfilland')THEN
        IF(rsmax>=4)THEN
          WRITE(iwrite,'(A)') 'fillrgb setrgbcolor'
          WRITE(iwrite,'(4F14.4,A)') rstack(rsmax-3:rsmax), ' arrowfill'
          WRITE(iwrite,'(A)') 'strokergb setrgbcolor'
          WRITE(iwrite,'(4F14.4,A)') rstack(rsmax-3:rsmax), ' arrowdraw'
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='rarrowfill')THEN
        IF(rsmax>=4)THEN
          WRITE(iwrite,'(A)') 'fillrgb setrgbcolor'
          WRITE(iwrite,'(4F14.4,A)') rstack(rsmax-3:rsmax), ' rarrowfill'
          WRITE(iwrite,'(A)') 'strokergb setrgbcolor'
          WRITE(iwrite,'(4F14.4,A)') rstack(rsmax-3:rsmax), ' rarrowdraw'
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      !%}}}
      ! circle %{{{
      ELSE IF(ccomd=='circle')THEN
        IF(rsmax>=2)THEN
          WRITE(iwrite,'(2F14.4,A)') rstack(rsmax-1:rsmax), ' circledraw'
          CALL kd_killrealstack()
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='circlefill')THEN
        IF(rsmax>=2)THEN
          WRITE(iwrite,'(A)') 'fillrgb setrgbcolor'
          WRITE(iwrite,'(2F14.4,A)') rstack(rsmax-1:rsmax), ' circlefill'
          WRITE(iwrite,'(A)') 'strokergb setrgbcolor'
          CALL kd_killrealstack()
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='circlefilland')THEN
        IF(rsmax>=2)THEN
          WRITE(iwrite,'(A)') 'fillrgb setrgbcolor'
          WRITE(iwrite,'(2F14.4,A)') rstack(rsmax-1:rsmax), ' circlefill'
          WRITE(iwrite,'(A)') 'strokergb setrgbcolor'
          WRITE(iwrite,'(2F14.4,A)') rstack(rsmax-1:rsmax), ' circledraw'
          CALL kd_killrealstack()
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      !%}}}
      ! star %{{{
      ELSE IF(ccomd=='star')THEN
        IF(rsmax>=4.AND.csmax>=1)THEN
          WRITE(iwrite,'(2F14.4$)') rstack(rsmax-3:rsmax-2)
          WRITE(iwrite,'(2I4$)') NINT(rstack(rsmax-1)), NINT(rstack(rsmax))
          WRITE(iwrite,'(A$)') ' [ '
          WRITE(iwrite,'(A$)') TRIM(cstack(csmax))
          WRITE(iwrite,'(A)') ' ] stardraw'
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killcharstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='starfill')THEN
        IF(rsmax>=4.AND.csmax>=1)THEN
          WRITE(iwrite,'(A)') 'fillrgb setrgbcolor'
          WRITE(iwrite,'(2F14.4$)') rstack(rsmax-3:rsmax-2)
          WRITE(iwrite,'(2I4$)') NINT(rstack(rsmax-1)), NINT(rstack(rsmax))
          WRITE(iwrite,'(A$)') ' [ '
          WRITE(iwrite,'(A$)') TRIM(cstack(csmax))
          WRITE(iwrite,'(A)') ' ] starfill'
          WRITE(iwrite,'(A)') 'strokergb setrgbcolor'
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killcharstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='starfilland')THEN
        IF(rsmax>=4.AND.csmax>=1)THEN
          WRITE(iwrite,'(A)') 'fillrgb setrgbcolor'
          WRITE(iwrite,'(2F14.4$)') rstack(rsmax-3:rsmax-2)
          WRITE(iwrite,'(2I4$)') NINT(rstack(rsmax-1)), NINT(rstack(rsmax))
          WRITE(iwrite,'(A$)') ' [ '
          WRITE(iwrite,'(A$)') TRIM(cstack(csmax))
          WRITE(iwrite,'(A)') ' ] starfill'
          WRITE(iwrite,'(A)') 'strokergb setrgbcolor'
          WRITE(iwrite,'(2F14.4$)') rstack(rsmax-3:rsmax-2)
          WRITE(iwrite,'(2I4$)') NINT(rstack(rsmax-1)), NINT(rstack(rsmax))
          WRITE(iwrite,'(A$)') ' [ '
          WRITE(iwrite,'(A$)') TRIM(cstack(csmax))
          WRITE(iwrite,'(A)') ' ] stardraw'
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killcharstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      !%}}}
      ! string %{{{
      ELSE IF(ccomd=='string')THEN
        IF(rsmax>=2.AND.csmax>=1)THEN
          WRITE(iwrite,'(2F14.4,A)') rstack(rsmax-1:rsmax), &
          & ' (' // TRIM(cstack(csmax)) // ') stringdraw'
          CALL kd_killcharstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      !%}}}
      ! direct drawings %{{{
      ELSE IF(ccomd=='script')THEN
        IF(csmax>=1)THEN
          WRITE(iwrite,'(A)') TRIM(cstack(csmax))
          CALL kd_killcharstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='gsave')THEN
        WRITE(iwrite,'(A)') 'gsave'
      ELSE IF(ccomd=='grestore')THEN
        WRITE(iwrite,'(A)') 'grestore'
      ELSE IF(ccomd=='closepath')THEN
        WRITE(iwrite,'(A)') 'closepath'
      ELSE IF(ccomd=='fill')THEN
        WRITE(iwrite,'(A)') 'fill'
      ELSE IF(ccomd=='stroke')THEN
        WRITE(iwrite,'(A)') 'stroke'
      ELSE IF(ccomd=='moveto')THEN
        IF(rsmax>=2)THEN
          WRITE(iwrite,'(2F16.4,A)') rstack(rsmax-1), rstack(rsmax), ' usermoveto'
          CALL kd_killrealstack()
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='lineto')THEN
        IF(rsmax>=2)THEN
          WRITE(iwrite,'(2F16.4,A)') rstack(rsmax-1), rstack(rsmax), ' userlineto'
          CALL kd_killrealstack()
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='arc')THEN
        IF(rsmax>=4)THEN
          ! {x} {y} {theta_init} {theta_final} arc
          WRITE(iwrite,'(4F16.4,A)') rstack(rsmax-3), rstack(rsmax-2), &
          & rstack(rsmax-1), rstack(rsmax), ' userarc'
          CALL kd_killrealstack()
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='arcn')THEN
        IF(rsmax>=4)THEN
          ! {x} {y} {theta_init} {theta_final} arcn
          WRITE(iwrite,'(4F16.4,A)') rstack(rsmax-3), rstack(rsmax-2), &
          & rstack(rsmax-1), rstack(rsmax), ' userarcn'
          CALL kd_killrealstack()
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      !%}}}
      ! kdraw parameter settings %{{{
      ! rgb set %{{{
      ELSE IF(ccomd=='rgb')THEN
        ! 通常のrgbコマンドはstroke_rgbとfill_rgbの両方を同時に指定値に設定する
        ! これは旧版との互換性のため
        IF(rsmax>=3)THEN
          stroke_rgb(1:3) = rstack(rsmax-2:rsmax)
          fill_rgb(1:3) = rstack(rsmax-2:rsmax)
          WRITE(iwrite,'(A,3F16.4,A)') '/strokergb {', stroke_rgb(1:3),'} def'
          WRITE(iwrite,'(A,3F16.4,A)') '/fillrgb {', fill_rgb(1:3),'} def'
          WRITE(iwrite,'(A)') 'strokergb setrgbcolor'
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='strokergb')THEN
        ! strokergbまたはfillrgbで色が設定された場合、既定の色はstrokergbの方になる
        ! fill時だけ色を変更する
        IF(rsmax>=3)THEN
          stroke_rgb(1:3) = rstack(rsmax-2:rsmax)
          WRITE(iwrite,'(A,3F16.4,A)') '/strokergb {', stroke_rgb(1:3),'} def'
          WRITE(iwrite,'(A)') 'strokergb setrgbcolor'
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='fillrgb')THEN
        IF(rsmax>=3)THEN
          fill_rgb(1:3) = rstack(rsmax-2:rsmax)
          WRITE(iwrite,'(A,3F16.4,A)') '/fillrgb {', fill_rgb(1:3),'} def'
          CALL kd_killrealstack()
          CALL kd_killrealstack()
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
        !%}}}
      ! line set %{{{
      ELSE IF(ccomd=='linewidth')THEN
        IF(rsmax>=1)THEN
          linewidth = rstack(rsmax)
          WRITE(iwrite,'(A,F16.4,A)') '/linewidth ',linewidth,' def'
          WRITE(iwrite,'(A)') 'linerealwidth setlinewidth'
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='linecap')THEN
        IF(rsmax>=1)THEN
          linecap = 1
          IF(NINT(rstack(rsmax))==0) linecap = 0
          WRITE(iwrite,'(I1,A)') linecap, ' setlinecap'
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='linejoin')THEN
        IF(rsmax>=1)THEN
          linejoin = NINT(rstack(rsmax),SELECTED_INT_KIND(1))
          WRITE(iwrite,'(I1,A)') linejoin, ' setlinejoin'
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='linedash')THEN
        IF(rsmax>=1.AND.csmax>=1)THEN
          WRITE(iwrite,'(A$)') '[ ' // TRIM(cstack(csmax)) // ' ] '
          WRITE(iwrite,'(F16.4,A)') rstack(rsmax), ' setlinedash'
          CALL kd_killrealstack()
          CALL kd_killcharstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
        !%}}}
      ! circle set %{{{
      ELSE IF(ccomd=='circleradius')THEN
        IF(rsmax>=1)THEN
          circleradius = rstack(rsmax)
          WRITE(iwrite,'(A,F16.4,A)') '/circleradius ',circleradius,' def'
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
        !%}}}
      ! arrow set %{{{
      ELSE IF(ccomd=='arrowdepth')THEN
        IF(rsmax>=1)THEN
          arrowdepth = rstack(rsmax)
          WRITE(iwrite,'(A,F16.4,A)') '/arrowdepth ',arrowdepth,' def'
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='arrowwidth')THEN
        IF(rsmax>=1)THEN
          arrowwidth = rstack(rsmax)
          WRITE(iwrite,'(A,F16.4,A)') '/arrowwidth ',arrowwidth,' def'
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='arrowthick')THEN
        IF(rsmax>=1)THEN
          arrowthick = rstack(rsmax)
          WRITE(iwrite,'(A,F16.4,A)') '/arrowthick ',arrowthick,' def'
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='arrowback')THEN
        IF(rsmax>=1)THEN
          arrowback = rstack(rsmax)
          WRITE(iwrite,'(A,F16.4,A)') '/arrowback ',arrowback,' def'
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='arrowturn')THEN
        IF(rsmax>=1)THEN
          arrowturn = rstack(rsmax)
          WRITE(iwrite,'(A,F16.4,A)') '/arrowturn ',arrowturn,' def'
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='arrowkern')THEN
        IF(rsmax>=1)THEN
          arrowkern = rstack(rsmax)
          WRITE(iwrite,'(A,F16.4,A)') '/arrowkern ',arrowkern,' def'
          CALL kd_killrealstack()
        ELSE
          CALL kd_warnstackinsufficient
        END IF
        !%}}}
      ! font set %{{{
      ELSE IF(ccomd=='fontsize')THEN
        IF(rsmax>=1)THEN
          fontsize = rstack(rsmax)
          WRITE(iwrite,'(A,F16.4,A)') '/fontsize ',fontsize,' def'
          CALL kd_killrealstack()
          CALL kd_setfont
        ELSE
          CALL kd_warnstackinsufficient
        END IF
      ELSE IF(ccomd=='fontname')THEN
        IF(csmax>=1)THEN
          fontname = cstack(csmax)
          CALL kd_killcharstack()
          CALL kd_setfont
        ELSE
          CALL kd_warnstackinsufficient
        END IF
        !%}}}
      !%}}}
      ELSE
        CALL kd_warnunknowncommand
      END IF
    END IF
  END IF
  !%}}}
  ! clear_stack %{{{
  IF(clear_stack)THEN
    CALL kd_clearstack
    clear_stack=.FALSE.
  ENDIF
  !%}}}
END DO
! end plot %{{{
WRITE(iwrite,'(A)') 'end'
WRITE(iwrite,'(A)') 'cliprestore'
WRITE(iwrite,'(A)') 'grestore'
WRITE(iwrite,'(A)') 'kdrsave restore'
WRITE(iwrite,'(A)') 'showpage'
WRITE(iwrite,'(A)') '%%EOF'
!%}}}
! end %{{{
CLOSE(iwrite)
CLOSE(iread)
ierr=0
!%}}}
RETURN
END SUBROUTINE

! =================================================================
! INTEGER kd_new_devnum() %{{{
INTEGER FUNCTION kd_new_devnum() RESULT ( res )
! returns not-associated device number
! return 0 when no number available
IMPLICIT NONE
INTEGER( KIND= 4 ) :: idoa
LOGICAL :: ltma
DO idoa = 11, 99
  INQUIRE( UNIT= idoa, OPENED= ltma )
  IF( .NOT. ltma )THEN
    res = idoa
    RETURN
  END IF
END DO
res = 0
RETURN
END FUNCTION
!%}}}
! LOGICAL kd_isspace(char_input) %{{{
LOGICAL FUNCTION kd_isspace(char_input)
IMPLICIT NONE
CHARACTER*1 :: char_input
INTEGER :: icc
LOGICAL :: logica
icc = IACHAR(char_input)
logica=.FALSE.
IF( icc == 32 ) logica = .TRUE. ! normal space
IF( icc ==  9 ) logica = .TRUE. ! horizontal tab
IF( icc == 11 ) logica = .TRUE. ! vertical tab
IF( icc == 12 ) logica = .TRUE. ! form feed
! Caution!! This does not contain newline character
kd_isspace=logica
RETURN
END FUNCTION
!%}}}
! INTEGER kd_hex2map(xxin) %{{{
INTEGER FUNCTION kd_hex2map(xxin)
IMPLICIT NONE
INTEGER,EXTERNAL :: kd_hexmap
CHARACTER( LEN= 2 ) :: xxin
INTEGER :: itmpa, itmpb
itmpa = kd_hexmap(xxin(1:1))
itmpb = kd_hexmap(xxin(2:2))
kd_hex2map= itmpa*16+itmpb
RETURN
END FUNCTION
!%}}}
! INTEGER kd_hexmap(charin) %{{{
INTEGER FUNCTION kd_hexmap(charin)
IMPLICIT NONE
CHARACTER( LEN= 1 ), INTENT( IN ) :: charin
INTEGER,EXTERNAL :: kd_basemap
kd_hexmap = kd_basemap(16,charin)
RETURN
END FUNCTION
!%}}}
! INTEGER kd_basemap(base,charin) %{{{
INTEGER FUNCTION kd_basemap(base,charin)
IMPLICIT NONE
INTEGER( KIND= 4 ), INTENT( IN ) :: base
CHARACTER( LEN= 1 ), INTENT( IN ) :: charin
CHARACTER( LEN= 50 ) :: charray1, charray2
INTEGER( KIND= 4 ) :: idoa
kd_basemap = 0
charray1 = '0123456789abcdefghijklmnopqrstuvwxyz'
charray2 = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'
DO idoa = 1, base
  IF( charin == charray1(idoa:idoa) )THEN
    kd_basemap = idoa - 1
    RETURN
  ELSE IF( charin == charray2(idoa:idoa) )THEN
    kd_basemap = idoa - 1 
    RETURN
  END IF
END DO
RETURN
END FUNCTION
!%}}}

! SUB kd_readhtmlcolor(strin,rout,gout,bout) %{{{
SUBROUTINE kd_readhtmlcolor(strin,rout,gout,bout)
USE cs_kdraw
IMPLICIT NONE
CHARACTER( LEN= kdraw_strlenmax ), INTENT( IN ) :: strin
DOUBLE PRECISION, INTENT( OUT ) :: rout, gout, bout
INTEGER( KIND= 4 ), EXTERNAL :: kd_hexmap, kd_hex2map
DOUBLE PRECISION :: htmlcolors(3)
INTEGER( KIND= 4 ) :: idoa
CHARACTER( LEN= 1 ) :: ctmpa
CHARACTER( LEN= 2 ) :: xtmpa
IF( LEN_TRIM( strin ) == 4 )THEN
  DO idoa = 1, 3
    ctmpa = strin(idoa+1:idoa+1)
    htmlcolors(idoa) = DBLE( kd_hexmap(ctmpa) ) / 15.d0
  END DO
ELSE
  DO idoa = 1, 3
    xtmpa = strin(idoa*2:idoa*2+1)
    htmlcolors(idoa) = DBLE( kd_hex2map(xtmpa) ) / 255.d0
  END DO
ENDIF
rout = htmlcolors(1)
gout = htmlcolors(2)
bout = htmlcolors(3)
RETURN
END SUBROUTINE
!%}}}

! SUB kd_checkversion%{{{
SUBROUTINE kd_checkversion
USE cv_kdraw
IMPLICIT NONE
INTEGER :: idoa
DO idoa = 1, UBOUND(kdraw_version_accept(:),1)
  IF( cstack(csmax) == kdraw_version_accept(idoa) )THEN
    version_accepted = .TRUE.
    RETURN
  END IF
END DO
version_accepted = .FALSE.
RETURN
END SUBROUTINE
!%}}}
! SUB kd_warnstackinsufficient%{{{
SUBROUTINE kd_warnstackinsufficient
USE cv_kdraw
IMPLICIT NONE
IF( warn_insufficientstack )THEN
  WRITE(*,*) 'KDRAW WARNING: ' // &
  & 'insufficient stack for command ''' // &
  & TRIM(ccomd) // ''' in kdraw body'
END IF
RETURN
END SUBROUTINE
!%}}}
! SUB kd_warnunknowncommand%{{{
SUBROUTINE kd_warnunknowncommand
USE cv_kdraw
IMPLICIT NONE
IF( warn_unknowncommand )THEN
  WRITE(*,*) 'KDRAW WARNING: ' // &
  & 'unkwon command ''' // &
  & TRIM(ccomd) // ''' in kdraw body'
END IF
RETURN
END SUBROUTINE
!%}}}
! SUB kd_warnunknownlayername(str_input) %{{{
SUBROUTINE kd_warnunknownlayername(str_input)
USE cv_kdraw
IMPLICIT NONE
CHARACTER(kdraw_strlenmax), INTENT(IN) :: str_input
IF( warn_unknownlayername )THEN
  WRITE(*,*) 'KDRAW WARNING: ' // &
  & 'unkwon layer name ''' // &
  & TRIM(str_input) // ''' in kdraw body'
END IF
RETURN
END SUBROUTINE
!%}}}

! SUB kd_clearstack %{{{
SUBROUTINE kd_clearstack
USE cv_kdraw
IMPLICIT NONE
rsmax = 0
csmax = 0
fsmax = 0
rstack(:) = 0.d0
cstack(:) = ''
fstack(:) = .FALSE.
RETURN
END SUBROUTINE
!%}}}
! SUB kd_killrealstack %{{{
SUBROUTINE kd_killrealstack
USE cv_kdraw
IMPLICIT NONE
! [TODO] rsmax==0の時のエラー処理(フールブルーフ)
rsmax = rsmax - 1
RETURN
END SUBROUTINE
!%}}}
! SUB kd_killcharstack %{{{
SUBROUTINE kd_killcharstack
USE cv_kdraw
IMPLICIT NONE
csmax = csmax - 1
RETURN
END SUBROUTINE
!%}}}
! SUB kd_killflagstack %{{{
SUBROUTINE kd_killflagstack
USE cv_kdraw
IMPLICIT NONE
fsmax = fsmax - 1
RETURN
END SUBROUTINE
!%}}}
! SUB kd_duplicaterealstack %{{{
SUBROUTINE kd_duplicaterealstack
USE cv_kdraw
IMPLICIT NONE
! [TODO] rsmax==0の時のエラー処理(フールブルーフ)
rsmax = rsmax + 1
rstack(rsmax) = rstack(rsmax-1)
RETURN
END SUBROUTINE
!%}}}
! SUB kd_duplicatecharstack %{{{
SUBROUTINE kd_duplicatecharstack
USE cv_kdraw
IMPLICIT NONE
csmax = csmax + 1
cstack(csmax) = cstack(csmax-1)
RETURN
END SUBROUTINE
!%}}}
! SUB kd_duplicateflagstack %{{{
SUBROUTINE kd_duplicateflagstack
USE cv_kdraw
IMPLICIT NONE
fsmax = fsmax + 1
fstack(fsmax) = fstack(fsmax-1)
RETURN
END SUBROUTINE
!%}}}
! SUB kd_exchangerealstack %{{{
SUBROUTINE kd_exchangerealstack
USE cv_kdraw
IMPLICIT NONE
DOUBLE PRECISION :: swap_tmp
! [TODO] foolproof
swap_tmp = rstack(rsmax)
rstack(rsmax) = rstack(rsmax-1)
rstack(rsmax-1) = swap_tmp
RETURN
END SUBROUTINE
!%}}}
! SUB kd_exchangecharstack %{{{
SUBROUTINE kd_exchangecharstack
USE cs_kdraw
USE cv_kdraw
IMPLICIT NONE
CHARACTER(LEN=kdraw_strlenmax) :: swap_tmp
swap_tmp = cstack(csmax)
cstack(csmax) = cstack(csmax-1)
cstack(csmax-1) = swap_tmp
RETURN
END SUBROUTINE
!%}}}
! SUB kd_exchangeflagstack %{{{
SUBROUTINE kd_exchangeflagstack
USE cs_kdraw
USE cv_kdraw
IMPLICIT NONE
LOGICAL :: swap_tmp
swap_tmp = fstack(fsmax)
fstack(fsmax) = fstack(fsmax-1)
fstack(fsmax-1) = swap_tmp
RETURN
END SUBROUTINE
!%}}}
! SUB kd_rollrealstack %{{{
SUBROUTINE kd_rollrealstack
USE cv_kdraw
IMPLICIT NONE
INTEGER :: nrange
INTEGER :: nstep
! [TODO] foolproof
nrange = NINT(rstack(rsmax-1))
nstep = NINT(rstack(rsmax))
rsmax = rsmax - 2
IF( nrange < 2 )THEN
  PRINT *, 'Error: roll range must be positive'
  RETURN
ELSE IF( ABS(nstep) >= nrange )THEN
  PRINT *, 'Error: abs(roll step) must be less than roll range'
  RETURN
END IF
IF( nstep > 0 )THEN
  ! スタック奥のオブジェクトを取り出して先頭以降に足してから全体をずらす
  rstack(rsmax+1:rsmax+nstep) = rstack(rsmax+1-nrange:rsmax+1-nrange+nstep)
  rstack(rsmax+1-nrange:rsmax) = rstack(rsmax+1-nrange+nstep:rsmax+nstep)
ELSE IF( nstep < 0 )THEN
  ! スタックをずらしてからはみ出たデータをスタック奥に送る
  nstep = -nstep
  rstack(rsmax+1-nrange+nstep:rsmax+nstep) = rstack(rsmax+1-nrange:rsmax)
  rstack(rsmax+1-nrange:rsmax+1-nrange+nstep) = rstack(rsmax+1:rsmax+nstep)
END IF
RETURN
END SUBROUTINE
!%}}}

! SUB kd_readcword %{{{
SUBROUTINE kd_readcword
USE cv_kdraw
IMPLICIT NONE
DOUBLE PRECISION :: rtmpa
INTEGER :: itmpa
DOUBLE PRECISION :: rcolor
DOUBLE PRECISION :: gcolor
DOUBLE PRECISION :: bcolor
IF( cword == '' ) RETURN
IF(cword(1:1)=='(')THEN
  csmax = csmax + 1
  itmpa = len_trim(cword)
  cstack(csmax) = cword(2:itmpa-1)
  RETURN
ELSE IF(cword(1:1)=='#')THEN
  CALL kd_readhtmlcolor(cword,rcolor,gcolor,bcolor)
  rsmax = rsmax + 1
  rstack(rsmax) = rcolor
  rsmax = rsmax + 1
  rstack(rsmax) = gcolor
  rsmax = rsmax + 1
  rstack(rsmax) = bcolor
  RETURN
ELSE IF(cword=='true'.or.cword=='True'.or.cword=='TRUE')THEN
  fsmax = fsmax + 1
  fstack(fsmax) = .TRUE.
  RETURN
ELSE IF(cword=='false'.or.cword=='False'.or.cword=='FALSE')THEN
  fsmax = fsmax + 1
  fstack(fsmax) = .FALSE.
  RETURN
ELSE IF(cword=='white')THEN
  rsmax = rsmax + 1
  rstack(rsmax) = 1.d0
  rsmax = rsmax + 1
  rstack(rsmax) = 1.d0
  rsmax = rsmax + 1
  rstack(rsmax) = 1.d0
  RETURN
ELSE IF(cword=='black')THEN
  rsmax = rsmax + 1
  rstack(rsmax) = 0.d0
  rsmax = rsmax + 1
  rstack(rsmax) = 0.d0
  rsmax = rsmax + 1
  rstack(rsmax) = 0.d0
  RETURN
END IF
READ( cword, *, IOSTAT= iios ) rtmpa
IF( iios == 0 )THEN
  rsmax = rsmax + 1
  rstack(rsmax) = rtmpa
  RETURN
END IF
! do nothing if failed to read
ccomd = TRIM(cword)
RETURN
END SUBROUTINE
!%}}}
! SUB kd_getlayeridfromname(str_input) %{{{
SUBROUTINE kd_getlayeridfromname(str_input)
USE cv_kdraw
IMPLICIT NONE
CHARACTER(kdraw_strlenmax), INTENT(IN) :: str_input
INTEGER :: idoa
DO idoa = 0, UBOUND(layernamelist(:),1)
  IF( str_input == layernamelist(idoa) )THEN
    currentlayerid = idoa
    RETURN
  END IF
END DO
currentlayerid = -1
RETURN
END SUBROUTINE
!%}}}
! SUB kd_layerinitparams %{{{
SUBROUTINE kd_layerinitparams
USE cv_kdraw
IMPLICIT NONE
arrowdepth = 0.3d0 ! 矢印先端の三角形の全長
arrowwidth = 0.4d0 ! 矢印先端の三角形の幅
arrowthick = 0.1d0 ! 矢印の線幅
arrowback = 0.d0 ! 矢印の始点と指定した始点のずれ
arrowturn = 0.d0 ! 矢印の線の三角形への食い込み幅
arrowkern = 0.d0 ! 矢印の終点と指定した終点のずれ 
circleradius = 0.4d0 ! 円の半径
linewidth = 0.03d0
linecap = 1
linejoin = 0
fontname = 'Times-Roman'
fontsize = 0.6d0
stroke_rgb(1:3) = 0.d0
fill_rgb(1:3) = 0.d0
RETURN
END SUBROUTINE
!%}}}
! SUB kd_layerprintparams %{{{
SUBROUTINE kd_layerprintparams
USE cv_kdraw
IMPLICIT NONE
WRITE(iwrite,'(A,F16.4,A)') '/arrowdepth ',arrowdepth,' def'
WRITE(iwrite,'(A,F16.4,A)') '/arrowwidth ',arrowwidth,' def'
WRITE(iwrite,'(A,F16.4,A)') '/arrowthick ',arrowthick,' def'
WRITE(iwrite,'(A,F16.4,A)') '/arrowback ',arrowback,' def'
WRITE(iwrite,'(A,F16.4,A)') '/arrowturn ',arrowturn,' def'
WRITE(iwrite,'(A,F16.4,A)') '/arrowkern ',arrowkern,' def'
WRITE(iwrite,'(A,F16.4,A)') '/circleradius ',circleradius,' def'
WRITE(iwrite,'(A,F16.4,A)') '/linewidth ',linewidth,' def'
WRITE(iwrite,'(A)') 'linerealwidth setlinewidth'
WRITE(iwrite,'(I1,A)') linecap, ' setlinecap'
WRITE(iwrite,'(I1,A)') linejoin, ' setlinejoin'
WRITE(iwrite,'(A)') '[] 0 setlinedash'
WRITE(iwrite,'(A,3F16.4,A)') '/strokergb {', stroke_rgb(1:3),'} def'
WRITE(iwrite,'(A,3F16.4,A)') '/fillrgb {', fill_rgb(1:3),'} def'
WRITE(iwrite,'(A)') 'strokergb setrgbcolor'
WRITE(iwrite,'(A,F16.4,A)') '/fontsize ',fontsize,' def'
CALL kd_setfont
RETURN
END SUBROUTINE
!%}}}
! SUB kd_setfont %{{{
SUBROUTINE kd_setfont
USE cv_kdraw
IMPLICIT NONE
WRITE(iwrite,'(A)') '/' // TRIM(fontname) // &
& ' findfont fontrealsize scalefont setfont'
RETURN
END SUBROUTINE
!%}}}

!EOF
