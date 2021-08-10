! Language   : Fortran90
! Description: subroutine to plot graph
! Lastchange : 2019, Jan. 28, 14:16
! =================================================================
! coding conventions %{{{
! このファイルはグラフ描画を目的とするkplotプロジェクトの
! 主要なソースコードを記述するにあたり、言語としてfortran90を使用し
! コーディングのフォーマットとして、基本的に、
! 親プロジェクトであるPSCプロジェクトのコーディングフォーマットに従う
! PSCプロジェクトにおけるコーディングフォーマットについては
! PSCの対応するコメントを参照せよ
!%}}}
! mode info %{{{
!   p_mode
!     1 : 2D             : [x y y y ...]
!     2 : 2D-color-bar   : [x a(y==#1) a(y==#2) a(y==#3) ...]
!     3 : 2D-color-point : [x y a]
!     4 : 3D             : [x y z] ! この場合は[x y z z z ...]のほうが自然
!     5 : 3D-color-point : [x y z a]
!     6 : 2D-color-face  : [x y]*[a]
!   xr_m
!     0 : auto
!     1 : ambiguous-rate
!     2 : ambiguous-value
!     3 : discrete-rate
!%}}}
! development history %{{{
! d(  1)= d(20150406,1,'データ区切り文字をsepで指定するように変更 -> 8月に廃止')
! d(  2)= d(20150406,2,'欠落データをゼロで代替するように変更')
! d(  3)= d(20150502,1,'欠落データの点を非表示に修正')
! d(  4)= d(20150502,2,'欠落データの線を分断するように修正')
! d(  5)= d(20150506,1,'using(:)などのallocationが早すぎるバグを修正')
! d(  6)= d(20150507,1,'変数名の大規模整理')
! d(  7)= d(20150521,1,'カラー形式の時の色範囲設定をy軸判定と同じにする')
! d(  8)= d(20150521,2,'カラーバー作成の準備開始')
! d(  9)= d(20150522,1,'カラーバー作成完了')
! d( 10)= d(20150523,1,'エラー発生時にメインに復帰できるよう修正')
! d( 11)= d(20150601,1,'RETURN前にDEALLOCATEするように修正')
! d( 12)= d(20150603,1,'initialize SUBROUTINEを作成し、外部からのオプション変更を容易に修正')
! d( 13)= d(20150709,1,'realratioフラグを設けて実比率可視化を可能にする')
! d( 14)= d(20150720,1,'グラフの背景色の指定を可能にする')
! d( 15)= d(20150722,1,'データファイルからバイナリ形式で全データの取得')
! d( 16)= d(20150826,1,'データファイルにNaNやInfが入っていても正常に動くように判定')
! d( 17)= d(20150826,2,'空白文字の判定としてタブ文字(HT,VT)やFFも許可')
! d( 18)= d(20150826,3,'一番最後のデータを読み込めないバグ修正')
! d( 19)= d(20151020,1,'プロットラインの結合形式を変更')
! d( 20)= d(20151020,2,'折り畳みを少し整理して簡潔にする')
! d( 21)= d(20151020,3,'日本語フォントを追加するが、実際の出力には未対応')
! d( 22)= d(20151023,1,'グラフラインが枠からはみ出ても境界まで直線を引く処理を追加するが未完成')
! d( 23)= d(20151023,2,'３次元データの描画機能を追加')
! d( 24)= d(20151028,1,'最近接格子点を接続する処理を追加')
! d( 25)= d(20151129,1,'ticsラベルの書式設定項目を追加')
! d( 26)= d(20151129,2,'ラベル末尾の余分なゼロをカットするオプションを追加')
! d( 27)= d(20151216,1,'mticsを指定するオプションとそのフールプルーフを追加')
! d( 28)= d(20151216,2,'データファイル中のコメント機能を実装')
! d( 29)= d(20151218,1,'ラベルの余分文字列カットオプションでドット文字もカットするように改良')
! d( 30)= d(20151218,2,'入力引数に依存する簡単なデバグ機能を作成')
! d( 31)= d(20151219,1,'暗黙のxデータを使用したプロットを実装')
! d( 32)= d(20151220,1,'データファイル中のコメント機能のバグ修正')
! d( 33)= d(20151220,2,'行ごとのデータ数が一致していなくてもエラーとせずプロットするように修正')
! d( 34)= d(20151225,1,'colorfull時の色設定をstylecolors配列としてまとめる')
! d( 35)= d(20151229,1,'ticsとticslabelの表示オプションを追加')
! d( 36)= d(20151230,1,'vbarを実装しバンド図の縦線描画を効率化')
! d( 37)= d(20151230,2,'文字列の実数判定を正規表現ライクな判定方式に修正')
! d( 38)= d(20151230,3,'コメント機能と垂直バー機能と空きデータ機能のバグを修正')
! d( 39)= d(20151231,1,'文字コード取得関数をIACHAR()組み込み関数に置き換え')
! d( 40)= d(20151231,2,'変数宣言部の大規模整理')
! d( 41)= d(20151231,3,'フールプルーフの範囲判定をルーチン化')
! d( 42)= d(20151231,4,'線種をpostscript手続き化して種類を4種から8種に増やす')
! d( 43)= d(20151231,5,'ボンド探索で近い順に採用するように改良')
! d( 44)= d(20160101,1,'3D plot のrealratio 対応に成功')
! d( 45)= d(20160101,2,'枠との交点を求めるgetcrosspoint()ルーチンの完成')
! d( 46)= d(20160101,3,'sizesqr を指定しなくてもrealratio の3D plotができるように修正')
! d( 47)= d(20160102,1,'簡易的な陰線処理モードの実装')
! d( 48)= d(20160102,2,'陰線処理モードのファイル書き出しをポストスクリプト手続き化')
! d( 49)= d(20160102,3,'色設定もポストスクリプト手続き化')
! d( 50)= d(20160106,1,'hidden3dモードの隠し幅をポストスクリプト変数化')
! d( 51)= d(20160106,2,'ファイル番号をポストスクリプト変数化して無駄を大きく改善')
! d( 52)= d(20160107,1,'colorboxプロットモードにおいてカラーバーの作成をポストスクリプトループ化')
! d( 53)= d(20160110,1,'3D plotで最大レンジの軸に合わせて拡大する処理の条件分枝のバグを修正')
! d( 54)= d(20160111,1,'3D plotで全体を囲む枠の描画を実装')
! d( 55)= d(20160119,1,'3D plotの空間枠線の描画処理の前後判定をルーチン化して実装')
! d( 56)= d(20160126,1,'tics labelの実数表示をルーチン化してcutexzeroを厳密化')
! d( 57)= d(20160326,2,'datafileに書式指定を直接含めるように修正')
! d( 58)= d(20160326,3,'xminとxmaxの片方のみの指定を可能にする')
! d( 59)= d(20160326,4,'上の実装に伴いxr_mの必要性が薄れる、削除する可能性あり')
! d( 60)= d(20160326,5,'初期化ルーチンの呼び出し時点でfnumが決まっていないとうまくいかないバグを発見')
! d( 61)= d(20160326,6,'上のバグの緊急対策として大きめの配列宣言で回避')
! d( 62)= d(20160425,1,'psc更新にあわせて軸ラベル出力ルーチンを改良')
! d( 63)= d(20170106,1,'プロット点の定義にclosepathを加えてずれを修正')
! d( 64)= d(20170120,1,'p_mode=2の時にバーのticsがおかしくなるバグ修正、しかしバグ多し')
! d( 65)= d(20170123,1,'上のバグを大幅に修正')
! d( 66)= d(20170126,1,'フォント追加')
! d( 67)= d(20170204,1,'BoundingBox関係のオプションを追加')
! d( 68)= d(20170204,2,'xr_mが2または3の時に本来の動作をするようにバグ修正')
! d( 69)= d(20170211,1,'p_mode==3でy軸分割がエラーになるのを修正')
! d( 70)= d(20170211,2,'p_mode==3でバーの範囲指定ができないバグ修正')
! d( 71)= d(20170310,1,'デフォルト設定の変更')
! d( 72)= d(20170312,1,'p_mode==5実装')
! d( 73)= d(20170312,2,'配列のallocationを整理')
! d( 74)= d(20170312,3,'x軸の分割アルゴリズムをy軸・z軸用のルーチンと統合')
! d( 75)= d(20170313,1,'マイナー修正')
! d( 76)= d(20170326,1,'box-plotモードで枠をはみ出すデータを描画しないよう修正')
! d( 77)= d(20170326,2,'2D-color-faceモード実装')
! d( 78)= d(20170424,1,'p_modeごとの最小列数フールプルーフのバグ修正')
! d( 79)= d(20170509,1,'3Dボンド表示モードで原子との接続部分のボンドを隠すように修正')
! d( 80)= d(20170511,1,'3Dボンド表示モードで疑似的なシェーディングを実装')
! d( 81)= d(20170517,1,'疑似シェーディングのエッジ処理を三角関数で滑らかにする')
! d( 82)= d(20170517,2,'疑似シェーディングのエッジ処理の色を指定できるようにする')
! d( 83)= d(20170517,3,'疑似シェーディングモード時にボンドの一部が描画領域からはずれた場合の処理を追加')
! d( 84)= d(20170517,4,'ボンドの重複描画を修正')
! d( 85)= d(20170520,1,'カラーバーのバグ修正')
! d( 86)= d(20170531,1,'ボンド描画モードを含む3D描画モードにおいて描画順序を厳密に修正')
! d( 87)= d(20170601,1,'用紙サイズのオプションを増強')
! d( 88)= d(20170605,1,'3Dボンド表示モードにおいて枠にかかるデータの切り取りをclipによる実装に変更')
! d( 89)= d(20170608,1,'グラフファイル中の余計なコメントを取り除くオプションを追加')
! d( 90)= d(20170608,2,'描画領域外のデータをグラフファイルに書き出さないように修正')
! d( 91)= d(20170608,3,'描画領域内にデータがないようなデータ列の描画処理前に点種・線種を宣言しないように修正')
! d( 92)= d(20170610,1,'EPS内のパラメタ宣言部の整備')
! d( 93)= d(20170610,2,'ソースコードのインデント整備')
! d( 94)= d(20170612,1,'点種指定を線種指定と同様にマクロ部にまとめる')
! d( 95)= d(20170614,1,'点・線・フォントのスタイル指定部を整備し描画をしない選択肢も追加')
! d( 96)= d(20170616,1,'細かな修正')
! d( 97)= d(20170616,2,'軸ラベル上で添え字やカーニング指定ができるように修正')
! d( 97)= d(20170616,3,'それぞれの軸ラベルに対し個別にフォント指定ができるように修正')
! d( 98)= d(20170619,1,'3Dボンド表示モードで遠方の点を小さく、遠方のボンドを細く描画する処理を追加')
! d( 99)= d(20170620,1,'点種を増強')
! d(100)= d(20170622,1,'マイナー修正')
! d(101)= d(20170623,1,'マイナー修正')
! d(102)= d(20170623,2,'DSCコメントの整備')
! d(103)= d(20170623,3,'文字列中フォント変更の実装')
! d(104)= d(20170626,1,'カラーバー表示オプション追加')
! d(105)= d(20170628,1,'空気遠近法の実装')
! d(106)= d(20170629,1,'遠近法と空気遠近法を修正')
! d(107)= d(20170705,1,'文字列を上下方向にも中央揃えするように修正')
! d(108)= d(20170711,1,'未使用ルーチン破棄')
! d(109)= d(20171113,1,'文字列における文字間マージンeverykernを実装')
! d(110)= d(20171122,1,'colorfull,cnodemaxを廃止してstcl,clnum等による実装に変更、stszを用意')
! d(111)= d(20171122,2,'スタイルサイズを実装し、プロット点のサイズをデータに応じて変更できるようにする')
! d(112)= d(20171124,1,'凡例表示機能を実装')
! d(113)= d(20171128,1,'マイナー修正')
! d(114)= d(20171130,1,'マイナー修正')
! d(115)= d(20171212,1,'既定の用紙サイズ設定を変更')
! d(116)= d(20171215,1,'文字列オブジェクト毎に異なる自動カーニング距離を指定できるようにする')
! d(117)= d(20171218,1,'文字列描画前後にgsave、grestoreするように修正')
! d(118)= d(20171218,2,'文字列描画マクロを９種類に増やし配置処理をより正確にする')
! d(119)= d(20171218,3,'ticsラベルを回転するフラグを追加')
! d(120)= d(20180116,1,'マイナー修正')
! d(121)= d(20180213,1,'マイナー修正')
! d(122)= d(20180314,1,'マイナー修正')
! d(123)= d(20180315,1,'同一列データ上で一時的に点をfillにする機能を仮実装')
! d(124)= d(20180321,1,'デフォルト設定を中心とした微調整')
! d(125)= d(20180517,1,'p_mode==5で原子の組み合わせごとにボンドをつなぐかどうかを設定できる機能を実装')
! d(126)= d(20180604,1,'データ処理部のルーチンを小分けして整理開始')
! d(127)= d(20180611,1,'データ処理部のルーチン整理完了')
! d(128)= d(20180618,1,'EPS出力部のルーチン整理開始')
! d(129)= d(20180626,1,'EPS出力部のルーチン整理が一段落')
! d(130)= d(20180629,1,'軸分割アルゴリズムの改良')
! d(131)= d(20180704,1,'vertbarラベルの表示を実装')
! d(132)= d(20180710,1,'bondlenmaxオプションの追加')
! d(133)= d(20180717,1,'usingオプションの追加')
! d(134)= d(20180718,1,'keyオプションの追加')
! d(135)= d(20180720,1,'軸分割アルゴリズムの改良と関連オプションの追加')
! d(136)= d(20180720,2,'EMPTYLINESマクロの修正')
! d(137)= d(20180723,1,'p_mode==2のticsラインとticsラベルのバグ修正')
! d(138)= d(20180731,1,'バグ修正')
! d(139)= d(20180803,1,'十分ゼロに近いboxの描画を抑制するフラグの追加')
! d(140)= d(20180816,1,'ルーチン整理')
! d(141)= d(20180823,1,'不要なDSCコメントの除去')
!%}}}
! ideas %{{{
! プロットの初期設定としていくつかのパターンを用意すると良いかも
! デフォルトの用紙サイズ等を変更した際にレイアウトが崩れる問題の対策がほしい
! Zバッファ
! 三角格子・極座標
! 3D面描画モード
! 点と線の接続境界線の修正
! 色塗りつぶしだけでなく周期パターンブラシも
! 指定関数の直接描画
! 移動平均・線形回帰・フィッティング
! 広角射影（魚眼）
! Windowsの改行0d0aに対応
! 文字列描画途中で10番目以降のスタイルフォントに切り替える機能がほしい: [1-9a-z]で36進数にするのが良いかも
! XYZファイルから直接描画する機能がほしい, できればAXYZにも対応してステップを指定して描画したい
! connectpairをPSCから指定できるように
!%}}}

!- main three routine --------------------------
! kplot %{{{
SUBROUTINE kplot(fnum,datafile,outputfile,ierror,errmsg)
USE cv_kplot
IMPLICIT NONE
INTERFACE
  SUBROUTINE cutoff_extrazero(strinout)
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(INOUT):: strinout
  END SUBROUTINE
END INTERFACE
INTEGER         ,INTENT(IN   ) :: fnum ! number of input data files
CHARACTER(LEN=*),INTENT(IN   ) :: datafile ! data file name
!  datafile example: 'data#[<format string>].dat'
!     here, use 'I' or 'Iw' for <format string> (w is INTEGER)
!     example:
!       'data[I].dat'  -> data1.dat data2.dat ...
!       'data[I3].dat' -> data001.dat, data002.dat ...
CHARACTER(LEN=*),INTENT(IN   ) :: outputfile ! output (eps) file name
INTEGER         ,INTENT(INOUT) :: ierror     ! error code integer
!  ierror input   0   : normal plot
!                 1   : global debug
!         output  1~500     : fool proof detected option error
!                 1001~1100 : other error
CHARACTER(LEN=*),INTENT(OUT) :: errmsg
INTEGER :: debugmode
debugmode=ierror
CALL kplot_datapart(debugmode,fnum,datafile,ierror,errmsg)
IF( ierror /= 0 )THEN
  IF(printerror) CALL print_error(ierror,errmsg)
  CALL dealloc_kp()
  RETURN
END IF
CALL kplot_epsgraphpart(debugmode,fnum,outputfile)
CALL dealloc_kp()
RETURN
END SUBROUTINE kplot
!%}}}

! SUB kplot_datapart(debugmode,fnum,datafile,ierror,errmsg) %{{{
SUBROUTINE kplot_datapart(debugmode,fnum,datafile,ierror,errmsg)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( IN ) :: fnum
CHARACTER( LEN = * ), INTENT( IN ) :: datafile
INTEGER, INTENT( OUT ) :: ierror
CHARACTER( LEN = * ), INTENT( OUT ) :: errmsg
CALL dealloc_kp()
IF( .NOT. initialized )THEN
  CALL initialize_kplot_options()
ENDIF
errmsg=''
ierror = 0
rowarr(:) = 0
CALL kplot_determine_pmode()
CALL kplot_foolproof(debugmode,fnum,ierror,errmsg)
IF( ierror /= 0 ) RETURN ! エラーは親ルーチン側で処理
CALL kplot_determine_papersize(debugmode,ierror,errmsg)
IF( ierror /= 0 ) RETURN
CALL kplot_determine_filename(debugmode,fnum,datafile,ierror,errmsg)
IF( ierror /= 0 ) RETURN
CALL kplot_determine_dataarraysize(debugmode,fnum,ierror,errmsg)
IF( ierror /= 0 ) RETURN
CALL kplot_prepare_import(debugmode,fnum)
CALL kplot_import_data(debugmode,fnum)
CALL kplot_detect_xrange(debugmode,fnum)
CALL kplot_detect_yrange(debugmode,fnum)
CALL kplot_detect_zrange(debugmode,fnum)
CALL kplot_make_cubenode(fnum)
CALL kplot_determine_bond(debugmode,fnum)
!fit arrangement: removed functional
CALL kplot_arrange_xdata(debugmode,fnum)
CALL kplot_arrange_yzadata(debugmode,ierror,errmsg)
CALL kplot_ratiokeep3d(debugmode,fnum)
CALL kplot_rotate3d(debugmode,fnum)
CALL kplot_bondreduce3d(debugmode,fnum)
CALL kplot_bondangle3d(debugmode,fnum)
CALL kplot_normalizeinone(debugmode,fnum)
CALL kplot_normalizeinthousand(debugmode,fnum)
CALL kplot_perspective3d(debugmode,fnum)
CALL kplot_ratiofix3d(debugmode,fnum)
CALL kplot_finaladjust(debugmode,fnum)
RETURN
END SUBROUTINE
!%}}}

!  SUB kplot_epsgraphpart(debugmode,fnum,outputfile) %{{{
SUBROUTINE kplot_epsgraphpart(debugmode,fnum,outputfile)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( IN ) :: fnum
CHARACTER( LEN = * ), INTENT( IN ) :: outputfile
INTEGER, EXTERNAL :: kplot_new_devnum
INTEGER :: ifgraph
ifgraph=kplot_new_devnum()
OPEN(ifgraph, FILE= TRIM(outputfile), STATUS= 'replace' )
CALL kplot_eps_initialdsc(debugmode,outputfile,ifgraph)
CALL kplot_eps_prolog(debugmode,fnum,ifgraph)
CALL kplot_eps_draw(debugmode,fnum,ifgraph)
CLOSE(ifgraph)
RETURN
END SUBROUTINE
!%}}}

!-----------

! SUB initialize_kplot_options() %{{{
SUBROUTINE initialize_kplot_options()
USE cv_kplot
USE cl_kplot
IMPLICIT NONE
INTEGER:: ifli

commentchar = '"'
vertbarchar = '|'
p_mode = 1
plotmode = '2D'
  ! 1 : 2D             [x y y y ...]
  ! 2 : 2D-color-bar   [x a(y==#1) a(y==#2) a(y==#3) ...]
  ! 3 : 2D-color-point [x y a]
  ! 4 : 3D             [x y z]
  ! 5 : 3D-color-point [x y z a]
xr_m = 1 ;yr_m = 1 ;zr_m = 1 ; ar_m = 1
   ! xyrange mode : 0>auto,           1>ambiguous-rate
   !                2>discrete-value, 3>discrete-rate
xr_r = 0.99d0 ;yr_r = 0.99d0; zr_r = 0.99d0  ! xyrange rate
                 ! (may overflow IF greater than one)
ar_r = 1.d0
br_r = 1.2d0
connectpair(:,:) = .TRUE. ! connect if true
bondlenmax = -5.d0 ! enabled if positive
!------------
connectpair(0,0) = .FALSE.
connectpair(1,1) = .FALSE.
connectpair(2,2) = .FALSE.
connectpair(3,3) = .FALSE.
connectpair(1,2) = .FALSE.
connectpair(2,1) = .FALSE.
connectpair(2,3) = .FALSE.
connectpair(3,2) = .FALSE.
connectpair(1,3) = .FALSE.
connectpair(3,1) = .FALSE.
!------------
DO ifli = 1, kpl_fnumax
  xdiv(ifli) = 0   ! x-grid division number (0>auto, other>discrete)
               ! IF xrange mode is auto, xdiv is ignored
  xmin_flag(ifli) = .FALSE. ! true if value are set
  xmax_flag(ifli) = .FALSE.
  xstep(ifli) = 1
END DO
ymin_flag = .FALSE.
ymax_flag = .FALSE.
ymin = -10.0d0 ;ymax = 10.0d0
zmin = -10.0d0 ;zmax = 10.0d0
amin = -10.d0 ; amax = 10.d0
ydiv = 0
zdiv = 0
adiv = 0
ystep = 1
zstep = 1
astep = 1
mxdiv_m = -1  ! negative>auto, otherwise>discrete
mydiv_m = -1  ! negative>auto, otherwise>discrete
madiv_m = -1  ! negative>auto, otherwise>discrete
   ! valid only when xyrange mode is not discrete or xydiv is 0
showline  = .TRUE.
showpoint = .TRUE.
showbond  = .FALSE.
showcube  = .TRUE.
showxtics = .TRUE.
showytics = .TRUE.
showxtlabs = .TRUE.
showytlabs = .TRUE.
showztlabs = .TRUE.
rotxtlabs = .FALSE.
rotytlabs = .FALSE.
showcolorbar = .TRUE.
showkey = .FALSE.
showkeybox = .TRUE.
morecomment = .TRUE.
cutzerobox = .TRUE.
   ! system flags
printerror = .TRUE.
autofix = .FALSE.
implicitbb = .FALSE.
stlt = 0 ! style line type
stpt = 0 ! style point type
stcl = 0 ! style color
stsz = 0 ! style size
!
ibmax = 10    ! max number of bonds
pnts = 1.20d0     ! pointsize (pixels radius)
angth = 0.20d0*kpl_conspi ! angles for 3D-plot
angph = 0.20d0*kpl_conspi ! angles for 3D-plot
pprat = 0.d0             ! perspective ratio: 0>parallel, positive>normall
mist = 0.d0 ! atmospheric scattering: 0>no effect, 1>exp(-x), 2>exp(-2x)
papertype = 'userdefined' !'A6'
ppx = 200.d0
ppy = 150.d0
rotmedia = .FALSE.    ! aspect of paper
sizesqr = .FALSE.     ! internal square plot
realratio = .FALSE.
hidden3d = .TRUE.
hiddenbond = .TRUE.
pseudoshade = .FALSE.
usingstr = '' ! ignored if length zero
usingclusternum = 0 ! ignored if zero
usinginit(:) = 0
usingstep(:) = 0
usingfinal(:) = 0
brdt = 15        ! setting border type with binary representation
                 !   0>noborder, 1>bottom, 2>left, 4>top, 8>right
tpst = 1      ! title position (0>bottom, 1>top)

 tclt = 1        ! tics line type (0>no, 1~8)
mtclt = 1        ! subtics line type (0>no, 1~8)
 bdlt = 1        ! border line type (0>no, 1~8)
 gxlt = 0        ! xgrid line type (0>no, 1~8)
 gylt = 0        ! ygrid line type
 vblt = 2        ! vertical bar line type  (0>no, 1~8)
! 線の太さもスタイル指定にしては？
 linw = 0.80d0   ! linewidth (pixels)
 ptlw = 0.40d0   ! pointlinewidth
 bdlw = 0.80d0   ! border linewidth
 tclw = 0.80d0   ! tics linewidth
mtclw = 0.80d0   ! subtics linewidth
 gxlw = 0.40d0   ! xgrid line width
 gylw = 0.40d0   ! ygrid line width
 vblw = 0.80d0   ! vbar  line width
 linh = 0.20d0   ! line hide ratio in hidden3d-mode
 pnth = 0.20d0   ! point hide ratio in hidden3d-mode
clbx = 5.00d0    ! color bar xsize
clby = 20.00d0    ! color bar ysize
clbn = 500        ! color bar division
lineacceptrange = 1.01d0 ! 1以上。大きいと大きい程枠外の線描画を丁寧に行う
pointacceptrange = 1.01d0 ! 1以上。大きいと大きい程枠外の点描画を丁寧に行う
! [TODO] これらの指定でパレット(スタイル色)が指定できるようにしたい
! 配列の一つ目の実数が-1なら第二引数をスタイル色指定整数と解釈するようにしたい
bakc(:) = kpcolor_w(:) ! background color
glbc(:) = kpcolor_w(:) ! global background color
linc(:) = kpcolor_k(:) ! line color
pntc(:) = kpcolor_k(:) ! point color
bdlc(:) = kpcolor_k(:) ! border line color
ttfc(:) = kpcolor_k(:) ! title font color
kyfc(:) = kpcolor_k(:) ! key font color
tclc(:) = kpcolor_k(:) ! tics line color
mtclc(:)= kpcolor_k(:) ! mtics line color
tcfc(:) = kpcolor_k(:) ! tics font color
gxlc(:) = kpcolor_k(:) ! x grid line color
gylc(:) = kpcolor_k(:) ! y grid line color
vblc(:) = kpcolor_k(:) ! vbar   line color
edgc(:) = (/ 0.3d0, 0.3d0, 0.3d0 /) ! edge color
xlfc(:) = kpcolor_k(:) ! x label font color
ylfc(:) = kpcolor_k(:) ! y label font color
y2lfc(:)= kpcolor_k(:) ! y2 label font color
zlfc(:) = kpcolor_k(:) ! z label font color
!cnodemax = 7   ! number of color node (for color-bar plot mode)
! cnodemaxではなくclnumやltnumを使用するように変更
sznum = 0
stylesize(:) = pnts
clnum = 7
stylecolors(:,:) = 0.d0
stylecolors(1,:) = kpcolor_k
stylecolors(2,:) = kpcolor_m
stylecolors(3,:) = kpcolor_b
stylecolors(4,:) = kpcolor_c
stylecolors(5,:) = kpcolor_g
stylecolors(6,:) = kpcolor_y
stylecolors(7,:) = kpcolor_r
ltnum = 9
stylelines(:) = '0 setlinecap []                  0 setdash'
stylelines(1) = '0 setlinecap []                  0 setdash'
stylelines(2) = '1 setlinecap [ 1 2 ]             0 setdash'
stylelines(3) = '1 setlinecap [ 6 2 2 2 ]         0 setdash'
stylelines(4) = '1 setlinecap [ 0 2 ]             0 setdash'
stylelines(5) = '0 setlinecap [ 2 3 ]             0 setdash'
stylelines(6) = '0 setlinecap [ 2 1 8 1 ]         0 setdash'
stylelines(7) = '0 setlinecap [ 2 2 2 2 2 2 8 2 ] 0 setdash'
stylelines(8) = '0 setlinecap [ 1 1 4 1 1 1 8 1 ] 0 setdash'
stylelines(9) = '1 setlinecap [ 0 4 ]             0 setdash'
ptnum = 13
stylepoints(:) = 'Ci stroke'
stylepoints( 1) = 'Ci stroke'
stylepoints( 2) = 'Sq stroke'
stylepoints( 3) = 'Di stroke'
stylepoints( 4) = 'Tt stroke'
stylepoints( 5) = 'Tb stroke'
stylepoints( 6) = 'Tl stroke'
stylepoints( 7) = 'Tr stroke'
!
! 六角形等は円と紛らわしいのでデフォルトで無効にしたほうが良いかも
stylepoints( 8) = 'Vh stroke'
stylepoints( 9) = 'Hh stroke'
stylepoints(10) = 'Ps stroke'
! 以下はfill不可能なポイント
stylepoints(11) = 'Cr stroke'
stylepoints(12) = 'Sc stroke'
stylepoints(13) = 'As stroke'
!
stylepoints2(:) = 'Ci fill'
stylepoints2( 1) = 'Ci fill'
stylepoints2( 2) = 'Sq fill'
stylepoints2( 3) = 'Di fill'
stylepoints2( 4) = 'Tt fill'
stylepoints2( 5) = 'Tb fill'
stylepoints2( 6) = 'Tl fill'
stylepoints2( 7) = 'Tr fill'
ftnum = 28
stylefonts( 1) = '/Times-Roman              SETFONT'
stylefonts( 2) = '/Times-Italic             SETFONT'
stylefonts( 3) = '/Times-Bold               SETFONT'
stylefonts( 4) = '/Times-BoldItalic         SETFONT'
stylefonts( 5) = '/Courier                  SETFONT'
stylefonts( 6) = '/Courier-Oblique          SETFONT'
stylefonts( 7) = '/Courier-Bold             SETFONT'
stylefonts( 8) = '/Courier-BoldOblique      SETFONT'
stylefonts( 9) = '/Helvetica                SETFONT'
stylefonts(10) = '/Helvetica-Oblique        SETFONT'
stylefonts(11) = '/Helvetica-Bold           SETFONT'
stylefonts(12) = '/Helvetica-BoldOblique    SETFONT'
stylefonts(13) = '/Symbol                   SETFONT'
stylefonts(14) = '/Arial                    SETFONT'
stylefonts(15) = '/Times-New-Roman          SETFONT'
stylefonts(16) = '/Monaco                   SETFONT'
stylefonts(17) = '/LetterGothic             SETFONT'
stylefonts(18) = '/LetterGothic-Bold        SETFONT'
stylefonts(19) = '/LetterGothic-Slanted     SETFONT'
stylefonts(20) = '/LetterGothic-BoldSlanted SETFONT'
stylefonts(21) = '/Ryumin-Light-H           SETFONT' ! JIS
stylefonts(22) = '/Ryumin-Light-V           SETFONT' ! JIS
stylefonts(23) = '/Ryumin-Light-RKSJ-H      SETFONT' ! SJIS
stylefonts(24) = '/Ryumin-Light-RKSJ-V      SETFONT' ! SJIS
stylefonts(25) = '/GothicBBB-Medium-H       SETFONT' ! JIS
stylefonts(26) = '/GothicBBB-Medium-V       SETFONT' ! JIS
stylefonts(27) = '/GothicBBB-Medium-RKSJ-H  SETFONT' ! SJIS
stylefonts(28) = '/GothicBBB-Medium-RKSJ-V  SETFONT' ! SJIS

ttkx = 0.d0
ttky = 0.d0
kykx = 0.d0
kyky = 0.d0
xlkx = 0.d0
xlky = 0.d0
ylkx = 0.d0
ylky = 0.d0
y2lkx = 0.d0
y2lky = 0.d0
tckx = -0.5d0
tcky = 0.d0

ttfs = 12.0d0   ! title fontsize (pixels (height))
kyfs = 6.0d0   ! key fontsize
tcfs = 6.0d0   ! tics fontsize
lbfs = 10.0d0   ! label font size

gxmg  = 30.0d0   ! margin from border to paper end (pixels width)
gymg  = 25.0d0
ttmg  = 10.0d0   ! title to border margin
kymg  =  2.5d0   ! key to border margin
lxmg  = 10.0d0   ! label to border margin
lymg  = 20.0d0
ly2mg = 25.0d0
bbxmg = 30.0d0   ! boundingbox margin from border
bbymg = 25.0d0
clmg  = 5.00d0    ! color bar to border margin
mpmg  = 10.00d0    ! multiplot margin
tcmg  =  3.00d0    ! tics margin from border
bxmg  =  0.00d0    ! box x margin
bymg  =  0.00d0    ! box y margin
ytcmg =  2.00d0    ! ytics label margin
! ytcmg は通常のマージンではない(通常のマージンはlymg)
! ytcmgはticsごとに設置されるラベルの間隔の最小許容値を与える
! ラベル間距離がそれよりも小さい場合は文字が重なっているとみなしてラベルを描画しない
kyfw =  40.d0      ! key line length
trx =  0.0d0     ! rest translation for Convenience
try =  0.0d0
tcll  =  2.00d0    ! tics line length
mtcll =  1.50d0    ! mtics line length
ctll  = -2.0d0     ! vert bar tics line length
cmtll = -1.5d0     ! vert bar mtics line length
kyll =  20.d0      ! key line length

xtlf = 'auto'      ! x tics label format (normal format OR 'auto')
ytlf = 'auto'      ! y tics label format
ztlf = 'auto'      ! z tics label format
atlf = 'auto'      ! bar tics label format
cutextrazero = .TRUE.  ! cut off extra zero ( ex. 3.1400000 -> 3.14 )

title= ''         ! title
key(:)  = ''      ! key
xlab = ''       ! x label
ylab = ''       ! y label
y2lb = ''       ! y2 label
zlab = ''       ! z label
vbarlab(:,:) = '' ! vert bar label

ttft = 1  ! title fonttype
kyft = 1  ! key fonttype
tcft = 1   ! tics fonttype
xlft = 1   ! x axis label font type
ylft = 1   ! y axis label font type
y2lft = 1   ! y2 axis label font type
vbft = 1   ! vertbar fonttype
! ==================================================================
initialized = .TRUE.
RETURN
END SUBROUTINE
!%}}}

!- data operation routine ----------------------
! SUB kplot_determine_pmode() %{{{
SUBROUTINE kplot_determine_pmode()
USE cv_kplot
IMPLICIT NONE
IF( p_mode == 1 )THEN
  ! p_modeが変更されていない場合はplotmodeの設定が優先される
  IF( plotmode == '2D' )THEN
    p_mode = 1
  ELSE IF( plotmode == '2D-color-bar' )THEN
    p_mode = 2
  ELSE IF( plotmode == '2D-color-point' )THEN
    p_mode = 3
  ELSE IF( plotmode == '3D' )THEN
    p_mode = 4
  ELSE IF( plotmode == '3D-color-point' )THEN
    p_mode = 5
  ELSE IF( plotmode == '2D-color-face' )THEN
    p_mode = 6
  END IF
END IF
RETURN
END SUBROUTINE
!%}}}
! SUB kplot_foolproof(debugmode,fnum,ierror,errmsg) %{{{
SUBROUTINE kplot_foolproof(debugmode,fnum,ierror,errmsg)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( IN ) :: fnum
INTEGER, INTENT( OUT ) :: ierror
CHARACTER(LEN=*),INTENT(OUT) :: errmsg
INTEGER :: ifli
INTEGER :: idoa
ierror = 0
! Error number: 1~500
IF( debugmode == 1 ) WRITE (*,*) 'Begin: Fool Proof'
!
IF(fnum>kpl_fnumax)THEN
  ! this error is FATAL because we cannot change 'fnum'
  ierror = 51
  WRITE(errmsg,*) 'Error fnum' // &
  '. Input integer between 0 and ', kpl_fnumax
  RETURN
END IF
! color type options Error : 11~50
DO idoa = 1, 3
  CALL dcheckrange(  linc(idoa),  'linc(:)',0.d0,1.d0,11,ierror,errmsg)
  CALL dcheckrange(  bakc(idoa),  'bakc(:)',0.d0,1.d0,12,ierror,errmsg)
  CALL dcheckrange(  pntc(idoa),  'pntc(:)',0.d0,1.d0,13,ierror,errmsg)
  CALL dcheckrange(  bdlc(idoa),  'bdlc(:)',0.d0,1.d0,14,ierror,errmsg)
  CALL dcheckrange(  ttfc(idoa),  'ttfc(:)',0.d0,1.d0,15,ierror,errmsg)
  CALL dcheckrange(  kyfc(idoa),  'kyfc(:)',0.d0,1.d0,16,ierror,errmsg)
  CALL dcheckrange(  tclc(idoa),  'tclc(:)',0.d0,1.d0,17,ierror,errmsg)
  CALL dcheckrange(  tcfc(idoa),  'tcfc(:)',0.d0,1.d0,18,ierror,errmsg)
  CALL dcheckrange(  gxlc(idoa),  'gxlc(:)',0.d0,1.d0,19,ierror,errmsg)
  CALL dcheckrange(  gylc(idoa),  'gylc(:)',0.d0,1.d0,20,ierror,errmsg)
  CALL dcheckrange(  xlfc(idoa),  'xlfc(:)',0.d0,1.d0,21,ierror,errmsg)
  CALL dcheckrange(  ylfc(idoa),  'ylfc(:)',0.d0,1.d0,22,ierror,errmsg)
  CALL dcheckrange( y2lfc(idoa), 'y2lfc(:)',0.d0,1.d0,23,ierror,errmsg)
  IF( ierror /= 0 ) RETURN
END DO
! INTEGER type options Error : 61~100
! linetype, pointtype
CALL icheckrange(  stlt,  'stlt',0, ltnum, 61,ierror,errmsg)
CALL icheckrange(  stpt,  'stpt',0, ptnum, 62,ierror,errmsg)
CALL icheckrange(  bdlt,  'bdlt',0, ltnum, 63,ierror,errmsg)
CALL icheckrange(  tclt,  'tclt',0, ltnum, 64,ierror,errmsg)
CALL icheckrange( mtclt, 'mtclt',0, ltnum, 65,ierror,errmsg)
CALL icheckrange(  gxlt,  'gxlt',0, ltnum, 66,ierror,errmsg)
CALL icheckrange(  gylt,  'gylt',0, ltnum, 67,ierror,errmsg)
CALL icheckrange(  vblt,  'vblt',0, ltnum, 68,ierror,errmsg)
! fonttype
CALL icheckrange(  tcft,  'tcft',1, ftnum, 69,ierror,errmsg)
CALL icheckrange(  ttft,  'ttft',1, ftnum, 70,ierror,errmsg)
CALL icheckrange(  kyft,  'kyft',1, ftnum, 71,ierror,errmsg)
CALL icheckrange(  xlft,  'xlft',1, ftnum, 72,ierror,errmsg)
CALL icheckrange(  ylft,  'ylft',1, ftnum, 72,ierror,errmsg)
CALL icheckrange(  y2lft,  'y2lft',1, ftnum, 72,ierror,errmsg)
! majortype 
CALL icheckrange(  brdt,  'brdt',0,15, 75,ierror,errmsg)
CALL icheckrange(  tpst,  'tpst',0, 1, 77,ierror,errmsg)
IF( ierror /= 0 ) RETURN

! division
DO ifli = 1, fnum
  CALL icheckrange(  xdiv(ifli),  'xdiv(:)',0,50,78,ierror,errmsg)
END DO
CALL icheckrange(    ydiv,    'ydiv',0, 50, 79,ierror,errmsg)
CALL icheckrange(    adiv,    'adiv',0, 50, 80,ierror,errmsg)
CALL icheckrange( mxdiv_m, 'mxdiv_m',-1, 50, 81,ierror,errmsg)
CALL icheckrange( mydiv_m, 'mydiv_m',-1, 50, 82,ierror,errmsg)
CALL icheckrange( madiv_m, 'madiv_m',-1, 50, 83,ierror,errmsg)
CALL icheckrange(   ibmax,   'ibmax',0, 20, 84,ierror,errmsg)
IF( ierror /= 0 ) RETURN

! line width options Error : 101~130
CALL dcheckrange(  linw,  'linw',0.d0,200.d0, 101,ierror,errmsg)
CALL dcheckrange(  ptlw,  'ptlw',0.d0,200.d0, 102,ierror,errmsg)
CALL dcheckrange(  bdlw,  'bdlw',0.d0,200.d0, 103,ierror,errmsg)
CALL dcheckrange(  tclw,  'tclw',0.d0,200.d0, 104,ierror,errmsg)
CALL dcheckrange( mtclw, 'mtclw',0.d0,200.d0, 105,ierror,errmsg)
CALL dcheckrange(  gxlw,  'gxlw',0.d0,200.d0, 106,ierror,errmsg)
CALL dcheckrange(  gylw,  'gylw',0.d0,200.d0, 107,ierror,errmsg)
CALL dcheckrange(  vblw,  'vblw',0.d0,200.d0, 108,ierror,errmsg)
IF( ierror /= 0 ) RETURN

! size, font-size options Error : 131~150
CALL dcheckrange(  pnts,  'pnts',0.d0,400.d0, 131,ierror,errmsg)
CALL dcheckrange(  ttfs,  'ttfs',0.d0,400.d0, 132,ierror,errmsg)
CALL dcheckrange(  kyfs,  'kyfs',0.d0,400.d0, 133,ierror,errmsg)
CALL dcheckrange(  tcfs,  'tcfs',0.d0,400.d0, 134,ierror,errmsg)
IF( ierror /= 0 ) RETURN

! margin Error : 151~160
IF( xr_m == 1 )THEN
  CALL dcheckrange(  xr_r,  'xr_r',1.d-8,1.d8, 151,ierror,errmsg)
END IF
IF( yr_m == 1 )THEN
  CALL dcheckrange(  yr_r,  'yr_r',1.d-8,1.d8, 152,ierror,errmsg)
END IF
RETURN
END SUBROUTINE
!%}}}
! SUB kplot_determine_papersize(debugmode,ierror,errmsg) %{{{
SUBROUTINE kplot_determine_papersize(debugmode,ierror,errmsg)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( OUT ) :: ierror
CHARACTER(LEN=*),INTENT(OUT) :: errmsg
DOUBLE PRECISION :: rtma
IF( debugmode == 1 ) WRITE (*,*) 'Begin: determine paper size'
! PostScript point (1pt=1/72in)
! 各種用紙サイズはISOとJISで規定されている
! ここではA4とB5用紙の値をもとに計算した値を使用している
IF(.FALSE.)THEN
ELSE IF( papertype == 'B0' )THEN
  ! https://ja.wikipedia.org/wiki/%E7%B4%99%E3%81%AE%E5%AF%B8%E6%B3%95
  ppx = 2914.00d0
  ppy = 4127.24d0
ELSE IF( papertype == 'A0' )THEN
  ppx = 2381.10d0
  ppy = 3367.56d0
ELSE IF( papertype == 'B1' )THEN
  ppx = 2063.62d0
  ppy = 2914.00d0
ELSE IF( papertype == 'A1' )THEN
  ppx = 1683.78d0
  ppy = 2381.10d0
ELSE IF( papertype == 'B2' )THEN
  ppx = 1457.00d0
  ppy = 2063.62d0
ELSE IF( papertype == 'A2' )THEN
  ppx = 1190.55d0
  ppy = 1683.78d0
ELSE IF( papertype == 'B3' )THEN
  ppx = 1031.81d0
  ppy = 1457.00d0
ELSE IF( papertype == 'A3' )THEN
  ppx = 841.89d0
  ppy = 1190.55d0
ELSE IF( papertype == 'B4' )THEN
  ppx = 728.50d0
  ppy = 1031.81d0
ELSE IF( papertype == 'A4' )THEN
  ppx = 595.28d0
  ppy = 841.89d0
ELSE IF( papertype == 'B5' )THEN
  ppx = 515.91d0
  ppy = 728.50d0
ELSE IF( papertype == 'A5' .OR. papertype == 'default' )THEN
  ppx = 419.53d0
  ppy = 595.28d0
ELSE IF( papertype == 'B6' )THEN
  ppx = 364.25d0
  ppy = 515.91d0
ELSE IF( papertype == 'A6' )THEN
  ppx = 297.64d0
  ppy = 419.53d0
ELSE IF( papertype == 'B7' )THEN
  ppx = 257.96d0
  ppy = 364.25d0
ELSE IF( papertype == 'A7' )THEN
  ppx = 209.76d0
  ppy = 297.64d0
ELSE IF( papertype == 'B8' )THEN
  ppx = 182.13d0
  ppy = 257.96d0
ELSE IF( papertype == 'A8' )THEN
  ppx = 148.82d0
  ppy = 209.76d0
ELSE IF( papertype == 'B9' )THEN
  ppx = 128.98d0
  ppy = 182.13d0
ELSE IF( papertype == 'A9' )THEN
  ppx = 104.88d0
  ppy = 148.82d0
ELSE IF( papertype == 'B10' )THEN
  ppx = 91.07d0
  ppy = 128.98d0
ELSE IF( papertype == 'A10' )THEN
  ppx = 74.41d0
  ppy = 104.88d0
ELSE IF( papertype == 'Letter' .OR. papertype == 'LTR' .OR. &
& papertype == 'ANSI-A' )THEN
  ! https://ja.wikipedia.org/wiki/%E3%83%AC%E3%82%BF%E3%83%BC%E3%82%B5%E3%82%A4%E3%82%BA
  ! 8.5in x 11in
  ppx = 612.d0
  ppy = 792.d0
ELSE IF( papertype == 'Legal' .OR. papertype == 'LGL' )THEN
  ! 8.5in x 14in
  ppx = 612.d0
  ppy = 1008.d0
ELSE IF( papertype == 'Tabloid' )THEN
  ! 11in x 17in
  ppx = 792.d0
  ppy = 1224.d0
ELSE IF( papertype == 'Ledger' .OR. papertype == 'LDR' .OR. &
& papertype == 'ANSI-B' )THEN
  ! 17in x 11in (Rotated Tabloid)
  ppx = 1224.d0
  ppy = 792.d0
ELSE IF( papertype == 'userdefined' )THEN
  CONTINUE
ELSE
  errmsg = 'Error: unknown papertype'
  ierror = 1014
  RETURN
END IF
IF(rotmedia)THEN
  rtma = ppx ; ppx = ppy ; ppy = rtma
END IF
ctx = ppx / 2.d0
cty = ppy / 2.d0
IF( gxmg >= ctx )THEN
  errmsg = 'Error: gxmg >= ctx'
  ierror = 1001
  RETURN
END IF
IF(gymg >= cty)THEN
  errmsg = 'Error: gymg >= cty'
  ierror = 1002
  RETURN
END IF
szx = ctx-gxmg  ! plot range
szy = cty-gymg
! size square
IF(sizesqr)THEN
  IF( szx < szy )THEN
    szy = szx
  ELSE
    szx = szy
  END IF
END IF
RETURN
END SUBROUTINE
!%}}}
! SUB kplot_determine_filename(debugmode,fnum,datafile,ierror,errmsg) %{{{
SUBROUTINE kplot_determine_filename(debugmode,fnum,datafile,ierror,errmsg)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( IN ) :: fnum
CHARACTER(LEN=*), INTENT( IN ) :: datafile
INTEGER, INTENT( OUT ) :: ierror
CHARACTER(LEN=*), INTENT( OUT ) :: errmsg
INTEGER           :: ifli
INTEGER           :: itma, itmb, itmc
CHARACTER(LEN=500):: stma, stmb, stmc
LOGICAL           :: ltma
IF( debugmode == 1 ) WRITE (*,*) 'Begin: determine filename'
ltma = .FALSE. ! detect success/fail for filename-replacement
filename(:) = TRIM( datafile )
ifli = 1
DO
  stma = filename(ifli)
  itma = INDEX(stma,'#[I')
  itmc = -1
  IF(itma>0)THEN
    itmb = INDEX(stma,']')
    IF( itmb > itma+3 )THEN
      stmb = ADJUSTL( stma(itma+3:itmb-1) )
      ! at this point, string 'stmb' must be consist of only by [0-9]
      IF( LEN_TRIM(stmb) == 0 )THEN
        itmc = 0
      ELSE IF( VERIFY( TRIM(stmb), '0123456789') == 0 )THEN
        READ( stmb, * ) itmc
      END IF
    END IF
  END IF
  IF( itmc >= 0 )THEN
    IF( itmc == 0 )THEN
      WRITE(stmc,'(I10)') ifli
      stmc = ADJUSTL(stmc)
    ELSE
      WRITE(stmb,'(I2)') itmc
      stmb = ADJUSTL(stmb)
      stmb = TRIM(stmb) // '.' // TRIM(stmb)
      WRITE(stmc,'(I'// TRIM(stmb) // ')') ifli
      stmc = ADJUSTL(stmc)
    END IF
    stma = stma(1:itma-1) // TRIM( stmc ) // TRIM( stma(itmb+1:) )
    filename(ifli) = TRIM( stma )
    ltma = .TRUE.
    CYCLE ! this allows user to include multiple <format-string> in string 'datafile'
  ELSE
    IF( ( fnum > 1 ) .AND. ( .NOT. ltma ) )THEN
      ierror = 1001
      errmsg = 'Error fnum or filename. Input format string #[Iw] if fnum>1'
      RETURN
    END IF
  END IF
  ifli = ifli + 1
  ltma = .FALSE.
  IF( ifli > fnum ) EXIT
END DO

DO ifli = 1, fnum
  INQUIRE( FILE= TRIM(filename(ifli)), EXIST= ltma )
  IF( .NOT. ltma )THEN
    errmsg = 'Error: no datafile '//TRIM(filename(ifli))
    ierror = 1003
    RETURN
  END IF
END DO
DO ifli = 1, fnum
  WRITE(stma,'(I10)') ifli
  hint(ifli) = TRIM(ADJUSTL(stma))
  pct(ifli) = 1.d0/DBLE(fnum)
END DO
RETURN
END SUBROUTINE
!%}}}
! SUB kplot_determine_dataarraysize(debugmode,fnum,ierror,errmsg) %{{{
SUBROUTINE kplot_determine_dataarraysize(debugmode,fnum,ierror,errmsg)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( IN ) :: fnum
INTEGER, INTENT( OUT ) :: ierror
CHARACTER(LEN=*), INTENT( OUT ) :: errmsg
INTEGER :: ifli       ! integer for files
INTEGER :: ifdata     ! integer for data
INTEGER :: iccA       ! integer for character-code
INTEGER :: itma, itmb
INTEGER, EXTERNAL :: kplot_new_devnum
LOGICAL, EXTERNAL :: isspace
INTEGER(KIND=8) :: irecord ! INTEGER to remember reading position
CHARACTER(LEN=1) :: cbyte ! current reading byte
INTEGER :: iline, iword ! line, word counter
LOGICAL :: fromdata, fromcomment ! flags used while reading datafiles
IF( debugmode == 1 ) WRITE (*,*) 'Begin: determine rowarr(:) and colmax'
colmax = -1
! for simplicity, we suppose 'colmax' is independent of files
!    (in fact, we just choose its minimum value)
! improvements required!
DO ifli = 1, fnum
  ifdata = kplot_new_devnum()
  OPEN( ifdata, FILE= TRIM(filename(ifli)), access= 'direct', FORM= 'unformatted', RECL= 1 )
  irecord = 0 ! count bytes             (for current file)
  iline = 0   ! count lines             (for current file)
  iword = 0   ! count words per line    (for current file)
  itmb = 0    ! temporary word counter  (for current line, current file)
  fromdata = .FALSE.
  fromcomment = .FALSE.
  ! vertical bar instructions(,which starts from vertbarchar) is treated as comment at here
  DO
    irecord = irecord+1
    READ( ifdata, REC= irecord, IOSTAT= itma ) cbyte
    iccA = IACHAR(cbyte)
    IF(itma /= 0)THEN
      EXIT  ! EOF (file end)
    END IF
    ! ===================
    ! long-IF start here
    ! ===================
    IF(fromcomment)THEN ! ================== current line is comment
      IF( iccA == 10 )THEN
        ! newline-char to end comment
        fromcomment = .FALSE.
        fromdata = .FALSE.
        IF( itmb == 0 )THEN
          ! line with no data
          CYCLE
        END IF
        iline = iline+1
        IF( itmb > iword ) iword = itmb
        itmb = 0
      END IF
      ! other char to DO nothing (CONTINUE comment)
    ELSE IF(( cbyte == commentchar ).OR.( cbyte == vertbarchar ))THEN ! ======= current line is comment
      IF(fromdata)THEN
        fromdata = .FALSE.
      END IF
      fromcomment = .TRUE.
    ELSE IF(fromdata)THEN ! ================= previous 1-byte is data
      IF( iccA == 10 )THEN
        ! here comes newline-char
        iline = iline + 1
        fromdata = .FALSE.
        IF(itmb>iword)iword = itmb
        itmb = 0
      ELSE IF(isspace(cbyte))THEN
        ! space separator to end data
        fromdata = .FALSE.
      ELSE
        ! data just continues
        CONTINUE
      END IF
    ELSE ! ================================= previous 1-byte is not data
      IF(isspace(cbyte))THEN
        ! space just continues
        CONTINUE
      ELSE IF( iccA == 10 )THEN
        ! here comes newline-char
        ! we really count lines with no data!
        iline = iline + 1
        IF(itmb>iword)iword = itmb
        itmb = 0
      ELSE
        ! data begins
        fromdata = .TRUE.
        itmb = itmb+1
      END IF
    END IF
    ! ===================
    ! long IF end here
    ! ===================
  END DO
  CLOSE(ifdata)
  ! USE implicit coordinates x
  IF(implicitx)THEN
    iword = iword+1
  END IF

  ! この段階でこのファイルの列数は決まったはず
  !  iline: このファイルの行数カウント用
  !  iword:: このファイルの列数保存用
  ! 全てのファイルで最小の列数を採用する
  IF( colmax == -1 )THEN
    ! これはifli==1と等価なはずである
    colmax = iword
  ELSE IF(iword<colmax)THEN
    colmax = iword
  END IF
  IF( colmax == 1 )THEN
    ! implictx を設定し忘れても1列データなら自動で判定する親切設計
    implicitx = .TRUE.
    colmax = 2
  END IF
  IF( ((p_mode==1.OR.p_mode==2).AND.(colmax<2)) .OR. &
  & ((p_mode==3.OR.p_mode==4).AND.(colmax<3)) .OR. &
  & ((p_mode==5).AND.(colmax<4)) .OR. &
  & ((p_mode==6).AND.(colmax<2)) )THEN
    ! p_modeにおいて要求される最低限の列数の判定
    errmsg = 'Error: too small colmax of data in '//TRIM(filename(ifli))
    ierror = 1007
    RETURN
  END IF
  !
  rowarr(ifli) = iline
  IF( rowarr(ifli) <= 1 )THEN
    errmsg = 'Error: too small column of data in '//TRIM(filename(ifli))
    ierror = 1008
    RETURN
  END IF
END DO
! 以上で全てのファイルに対し完全にcolmaxとcolが求まった
RETURN
END SUBROUTINE
!%}}}
! SUB kplot_prepare_import(debugmode,fnum) %{{{
SUBROUTINE kplot_prepare_import(debugmode,fnum)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( IN ) :: fnum
INTEGER :: idoa, idob
IF( debugmode == 1 ) WRITE (*,*) 'Begin: prepare before importing'
! do not read wasteful data from files, for each p_mode
IF(p_mode==3 .OR. p_mode==4)THEN
  colmax = 3
ELSE IF(p_mode==5)THEN
  colmax = 4
ELSE IF(p_mode==6)THEN
  colmax = 2
END IF
repnum = colmax-2
! Note that (replot num) = (plot num) - 1, not equal, so loops are always peculiar
CALL alloc_kp(fnum)
bondgroup(:,:,:) = 0
bondangle(:,:,:) = 0.d0
IF( usingstr == '' )THEN
  DO idob = 1, 1+repnum
    using(idob) = idob+1 ! colmaxs for first plot
  END DO
ELSE
  CALL kplot_parse_usingstr(debugmode)
  repnum = -1
  DO idoa = 1, usingclusternum
    !PRINT *, (/ ( idob, idob = usinginit(idoa), usingfinal(idoa), usingstep(idoa) ) /)
    DO idob = usinginit(idoa), usingfinal(idoa), usingstep(idoa)
      IF( idob < 1 .OR. idob > colmax ) CYCLE
      repnum = repnum + 1
      using(repnum+1) = idob
      !PRINT *, 'DEBUG07171454:', idob
    END DO
  END DO
END IF
RETURN
END SUBROUTINE
!%}}}
! SUB kplot_parse_usingstr(debugmode) %{{{
SUBROUTINE kplot_parse_usingstr(debugmode)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
character(len=100)  :: localusingstr
INTEGER :: currentclusternum
INTEGER :: ipos
IF( debugmode == 1 ) WRITE (*,*) 'Begin: prepare before importing'
! parse usingstr such as '2,5:10,12:2:18'
! three integer init:step:final separated by comma
! if only one integer, init=final
! if only two integer, step=1
localusingstr = usingstr
currentclusternum = 0
usingstep(:) = 1
DO
  IF( localusingstr == '' ) EXIT
  currentclusternum = currentclusternum + 1
  !PRINT *, 'DEBUG07171535:(' // TRIM(localusingstr) // ')'
  localusingstr = ADJUSTL(localusingstr)
  ipos = VERIFY(localusingstr,'0123456789')
  READ(localusingstr(1:ipos-1),*) usinginit(currentclusternum)
  IF( localusingstr(ipos:ipos) /= ':' )THEN
    usingfinal(currentclusternum) = usinginit(currentclusternum)
    localusingstr = localusingstr(ipos+1:)
    CYCLE
  END IF
  localusingstr = localusingstr(ipos+1:)
  ipos = VERIFY(localusingstr,'0123456789')
  IF( localusingstr(ipos:ipos) /= ':' )THEN
    READ(localusingstr(1:ipos-1),*) usingfinal(currentclusternum)
    localusingstr = localusingstr(ipos+1:)
    CYCLE
  END IF
  READ(localusingstr(1:ipos-1),*) usingstep(currentclusternum)
  localusingstr = localusingstr(ipos+1:)
  ipos = VERIFY(localusingstr,'0123456789')
  READ(localusingstr(1:ipos-1),*) usingfinal(currentclusternum)
  localusingstr = localusingstr(ipos+1:)
END DO
usingclusternum = currentclusternum
END SUBROUTINE
!%}}}
! SUB kplot_import_data(debugmode,fnum) %{{{
SUBROUTINE kplot_import_data(debugmode,fnum)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( IN ) :: fnum
INTEGER :: ifli       ! integer for files
INTEGER :: ifdata     ! integer for data
INTEGER :: iccA       ! integer for character-code
INTEGER :: itma, itmb
DOUBLE PRECISION  :: rtma
INTEGER, EXTERNAL :: kplot_new_devnum
LOGICAL, EXTERNAL :: isspace
LOGICAL, EXTERNAL :: isreal
INTEGER(KIND=8) :: irecord ! INTEGER to remember reading position
CHARACTER(LEN=500) :: cword ! current reading word
CHARACTER(LEN=1) :: cbyte ! current reading byte
INTEGER :: iline ! line, word counter
LOGICAL :: fromdata, fromcomment, fromvertbar ! flags used while reading datafiles
LOGICAL :: currentfilstate
IF( debugmode == 1 ) WRITE (*,*) 'Begin: import data'
empdata(:,:,:) = .TRUE.
fildata(:,:,:) = .FALSE.
currentfilstate = .FALSE. 
vbarnum(:) = 0
cbyte = ''
! can't treat vertical bar instructions as comment at here
DO ifli = 1, fnum
  ifdata = kplot_new_devnum()
  OPEN(ifdata, FILE= TRIM(filename(ifli)), access= 'direct', FORM= 'unformatted', RECL= 1)
  irecord = 0 ! count bytes             (for current file)
  iline = 1   ! count lines             (for current file)
  ! we don't USE 'iword' this time, instead we USE 'colmax'
  itmb = 0    ! temporary word counter  (for current line, current file)
  fromdata = .FALSE.
  fromcomment = .FALSE.
  fromvertbar = .FALSE.
  cword = ''  ! current reading word    (for current file)
  ! detect 'empdata' from iostat-option of READ-statement
  ! but be careful!: this method cannot detect NaN or Infty
  IF(implicitx)THEN
    itmb = itmb+1
    kpldata(ifli,itmb,iline) = DBLE(iline)
    empdata(ifli,itmb,iline) = .FALSE.
  END IF
  DO
    irecord = irecord+1
    READ(ifdata, REC= irecord, IOSTAT= itma ) cbyte
    iccA = IACHAR(cbyte)
    IF(itma /= 0)THEN
      EXIT  ! EOF (file end)
    END IF
    ! ===================
    ! long-IF start here
    ! ===================
    IF(fromcomment)THEN ! ================== current line is comment
      IF(iccA == 10)THEN
        ! newline-char to end comment
        fromcomment = .FALSE.
        fromdata = .FALSE.
        fromvertbar = .FALSE.
        IF(itmb == 0)THEN
          CONTINUE
        END IF
        IF(itmb /= 0) iline = iline+1
        itmb = 0
        IF(implicitx.AND.(iline <= rowarr(ifli)))THEN
          itmb = itmb+1
          kpldata(ifli,itmb,iline) = DBLE(iline)
          empdata(ifli,itmb,iline) = .FALSE.
        END IF
      END IF
      ! other char to DO nothing (CONTINUE comment)
    ELSE IF((cbyte == commentchar).OR.(cbyte == vertbarchar))THEN
      ! ======= current line is comment or vertbar
      IF(fromdata)THEN
        IF(isreal(cword))THEN
          READ(cword,*)rtma
          IF(iline>UBOUND(kpldata,3))THEN
            CYCLE
          END IF
          IF(itmb <= UBOUND(kpldata,2))THEN
            kpldata(ifli,itmb,iline) = rtma
            empdata(ifli,itmb,iline) = .FALSE.
            fildata(ifli,itmb,iline) = currentfilstate
            IF(fromvertbar)THEN
              vbarnum(ifli) = vbarnum(ifli)+1
              vbar(ifli,vbarnum(ifli)) = rtma
            END IF
          END IF
        ELSE
          empdata(ifli,itmb,iline) = .TRUE.
        END IF
        cword = ''
        fromdata = .FALSE.
      END IF
      IF(cbyte == commentchar) fromcomment = .TRUE.
      IF(cbyte == vertbarchar) fromvertbar = .TRUE.
    ELSE IF(fromdata)THEN
      ! ================= previous 1-byte is data
      IF(iccA == 10 .OR. iccA == 13 )THEN
        ! here comes newline-char
        IF(isreal(cword))THEN
          READ(cword,*)rtma
          IF(iline>UBOUND(kpldata,3))THEN
            CYCLE
          END IF
          IF(itmb <= UBOUND(kpldata,2))THEN
            kpldata(ifli,itmb,iline) = rtma
            empdata(ifli,itmb,iline) = .FALSE.
            fildata(ifli,itmb,iline) = currentfilstate
            IF(fromvertbar)THEN
              vbarnum(ifli) = vbarnum(ifli)+1
              vbar(ifli,vbarnum(ifli)) = rtma
            END IF
          END IF
        ELSE
          empdata(ifli,itmb,iline) = .TRUE.
        END IF
        cword = ''
        IF(.NOT.fromvertbar)iline = iline+1
        fromdata = .FALSE.
        fromcomment = .FALSE.
        fromvertbar = .FALSE.
        itmb = 0
        IF(implicitx.AND.(iline <= rowarr(ifli)))THEN
          IF(.NOT.fromvertbar)itmb = itmb+1
          kpldata(ifli,itmb,iline) = DBLE(iline)
          empdata(ifli,itmb,iline) = .FALSE.
        END IF
        fromvertbar = .FALSE.
      ELSE IF(isspace(cbyte))THEN
        ! space separator to end data
        IF(isreal(cword))THEN
          READ(cword,*)rtma
          IF(iline>UBOUND(kpldata,3))THEN
            CYCLE
          END IF
          IF(itmb <= UBOUND(kpldata,2))THEN
            kpldata(ifli,itmb,iline) = rtma
            empdata(ifli,itmb,iline) = .FALSE.
            fildata(ifli,itmb,iline) = currentfilstate
            IF(fromvertbar)THEN
              vbarnum(ifli) = vbarnum(ifli)+1
              vbar(ifli,vbarnum(ifli)) = rtma
            END IF
          END IF
        ELSE
          empdata(ifli,itmb,iline) = .TRUE.
        END IF
        cword = ''
        fromdata = .FALSE.
      ELSE
        ! data just continues
        cword = TRIM(cword)//cbyte
      END IF
    ELSE
      ! ================================= previous 1-byte is not data
      IF(isspace(cbyte))THEN
        ! space just continues
        CONTINUE
      ELSE IF(iccA == 10 .OR. iccA == 13 )THEN
        ! here comes newline-char
        IF((itmb == 0).OR.((implicitx).AND.(itmb == 1)))THEN
          ! line with no data => discrete data will separate lines
          iline = iline+1
        ELSE
          ! there are nonzero data on the line
          IF(.NOT.fromvertbar)iline = iline+1
        END IF
        itmb = 0
        IF(implicitx.AND.(iline <= rowarr(ifli)))THEN
          itmb = itmb+1
          kpldata(ifli,itmb,iline) = DBLE(iline)
          empdata(ifli,itmb,iline) = .FALSE.
        END IF
        fromdata = .FALSE.
        fromcomment = .FALSE.
        fromvertbar = .FALSE.
      ELSE IF(iccA == 40)THEN
        ! begin filldata from '(' character
        currentfilstate = .TRUE. 
        fromcomment = .TRUE.
      ELSE IF(iccA == 41)THEN
        ! begin filldata from ')' character
        currentfilstate = .FALSE. 
        fromcomment = .TRUE.
      ELSE
        ! data begins
        fromdata = .TRUE.
        cword = cbyte
        itmb = itmb+1
      END IF
    END IF
  END DO
  CLOSE(ifdata)
END DO
RETURN
END SUBROUTINE
!%}}}
! SUB kplot_detect_xrange(debugmode,fnum) %{{{
SUBROUTINE kplot_detect_xrange(debugmode,fnum)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( IN ) :: fnum
INTEGER :: ifli
INTEGER :: idoa
DOUBLE PRECISION  :: rtma, rtmb
! decide x-plot-range based on data and plot-mode
IF( debugmode == 1 ) WRITE (*,*) 'Begin: detect original xrange'
DO ifli = 1, fnum
  IF(mxdiv_m == -1)THEN
    mxdiv(ifli) = 0
  ELSE IF(mxdiv_m>=0)THEN
    mxdiv(ifli) = mxdiv_m
  END IF
END DO
! xdiv が 0 の場合後で自動決定を行う

DO ifli = 1, fnum
  IF(xmin_flag(ifli).OR.xmax_flag(ifli))THEN
    IF( xdiv(ifli) == 0 )THEN
      xdiv(ifli) = 1
      mxdiv(ifli) = 0
    END IF
  END IF
END DO

IF(p_mode == 2)THEN
  ! arrange x-data to center of each point
  DO ifli = 1,fnum
    rtma = 1.50d0*kpldata(ifli,1,1)-0.50d0*kpldata(ifli,1,2)
    DO idoa = 1,rowarr(ifli)-1
      rtmb = (kpldata(ifli,1,idoa)+kpldata(ifli,1,idoa+1))/2.0d0
      kpldata(ifli,1,idoa) = rtma
      rtma = rtmb
    END DO
    rtmb = 2.0d0*kpldata(ifli,1,rowarr(ifli))-rtma
    kpldata(ifli,1,rowarr(ifli)) = rtma
    kpldata(ifli,1,rowarr(ifli)+1) = rtmb
  END DO
END IF
! ========== decide true xrange
DO ifli = 1, fnum
  xmin(ifli) = kpl_large
  xmax(ifli) = kpl_small
  DO idoa = 1, rowarr(ifli)+1
    IF(idoa==rowarr(ifli)+1 .AND. (p_mode /= 2))EXIT
    ! x-dataに欠落がある場合はその行を無視する
    IF(empdata(ifli,1,idoa))CYCLE
    ! p_mode==6の場合、y-dataに欠陥のある行を無視する
    IF(p_mode==6.AND.empdata(ifli,2,idoa))CYCLE
    IF(xmin(ifli)>kpldata(ifli,1,idoa))THEN
      xmin(ifli) = kpldata(ifli,1,idoa)
    END IF
    IF(xmax(ifli)<kpldata(ifli,1,idoa))THEN
      xmax(ifli) = kpldata(ifli,1,idoa)
    END IF
  END DO
END DO
! at this point, xmax and xmin are original values
DO ifli = 1, fnum
  IF( xmin_flag(ifli) )THEN
    xmin(ifli) = xmin_value(ifli)
  END IF
  IF( xmax_flag(ifli) )THEN
    xmax(ifli) = xmax_value(ifli)
  END IF
END DO
RETURN
END SUBROUTINE
!%}}}
! SUB kplot_detect_yrange(debugmode,fnum) %{{{
SUBROUTINE kplot_detect_yrange(debugmode,fnum)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( IN ) :: fnum
INTEGER :: ifli
INTEGER :: idoa, idoc
DOUBLE PRECISION  :: rtma, rtmb
! decide y-plot-range based on data and plot-mode
IF( debugmode == 1 ) WRITE (*,*) 'Begin: detect original yrange'
IF( mydiv_m == -1 )THEN
  mydiv = 0  ! initialize
ELSE IF( mydiv_m >= 0 )THEN
  mydiv = mydiv_m
END IF
IF( madiv_m == -1 )THEN
  madiv = 1  ! initialize
ELSE IF( madiv_m >= 0 )THEN
  madiv = madiv_m
END IF
IF(yr_m <= 1)THEN
  ydiv = 0 ! automatic
END IF
IF(ar_m <= 1)THEN
  adiv = 0 ! automatic
END IF
IF(ymin_flag.OR.ymax_flag)THEN
  IF( ydiv == 0 )THEN
    ydiv = 1
    mydiv = 0
  END IF
END IF
IF(yr_m >= 2 .AND. ydiv == 0)THEN
  ydiv = 5
  WRITE(*,'(A)') 'Caution: not good setting: ydiv=0 while yr_m>=2'
  WRITE(*,'(A,I2)') 'ydiv is changed to ',ydiv
END IF
IF(ar_m >= 2 .AND. adiv == 0)THEN
  adiv = 5
  WRITE(*,'(A)') 'Caution: not good setting: adiv=0 while ar_m>=2.'
  WRITE(*,'(A,I2)') 'adiv is changed to ',adiv
END IF
IF(yr_m /= 2 .OR. (p_mode == 2 .AND. ar_m /= 2) )THEN
  ! ========== decide true yrange
  ymax = kpl_small
  ymin = kpl_large
  amax = kpl_small
  amin = kpl_large
  IF(p_mode == 1 .OR. p_mode == 2)THEN
    ! p_modeが1または2の時は列がたくさんあっても
    ! ２列目以降はすべてy座標とみなす
    ! [x y y y ...]
    DO idoc = 1, repnum+1
      CALL decide_range(using(idoc),fnum,rtma,rtmb)
      IF(rtma<ymin) ymin = rtma
      IF(rtmb>ymax) ymax = rtmb
    END DO
    IF( p_mode == 2 )THEN
      ! p_mode==2の場合は上記のyの値は実際にはカラーバーの値になる
      ! [x a(y==#1) a(y==#2) a(y==#3) ...]
      amax = ymax
      amin = ymin
    END IF
  ELSE IF(p_mode == 3 .OR. p_mode == 4)THEN
    ! p_mode={3,4}の時は２列目のデータのみをyとみなす
    ! [x y a] or [x y z]
    CALL decide_range(2,fnum,ymin,ymax)
  ELSE IF(p_mode == 5)THEN
    ! p_modeが5の場合は[x y z w]
    CALL decide_range(2,fnum,ymin,ymax)
    CALL decide_range(4,fnum,amin,amax)
  ELSE IF(p_mode == 6)THEN
    ! p_modeが6の場合はy-dataがemptyでない限り[x y]
    CALL decide_range(2,fnum,ymin,ymax)
    ! p_mode==6の場合y-dataがemptyの行はbar-rangeの決定に寄与する
    DO ifli = 1,fnum
      DO idoa = 1, rowarr(ifli)
        ! x-dataに欠落がある場合はその行を無視する
        IF(empdata(ifli,1,idoa))CYCLE
        ! p_mode==6の場合、y-dataに欠陥のある行のx-valueのみがbar-rangeに寄与する
        IF(.NOT.empdata(ifli,2,idoa))CYCLE
        IF(amin>kpldata(ifli,1,idoa))THEN
          amin = kpldata(ifli,1,idoa)
        END IF
        IF(amax<kpldata(ifli,1,idoa))THEN
          amax = kpldata(ifli,1,idoa)
        END IF
      END DO
    END DO
  END IF
  ! at this point, ymax and ymin are original values
END IF
DO ifli = 1, fnum
  IF( ymin_flag )THEN
    ymin = ymin_value
  END IF
  IF( ymax_flag )THEN
    ymax = ymax_value
  END IF
END DO
RETURN
END SUBROUTINE
!%}}}
! SUB kplot_detect_zrange(debugmode,fnum) %{{{
SUBROUTINE kplot_detect_zrange(debugmode,fnum)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( IN ) :: fnum
IF( p_mode == 1 ) RETURN
IF( p_mode == 2 ) RETURN
IF( p_mode == 3 .AND. ar_m == 2 ) RETURN
IF( p_mode == 6 ) RETURN
! decide z-plot-range based on data and plot-mode
IF( debugmode == 1 ) WRITE (*,*) 'Begin: detect original zrange'
mzdiv = 0
IF(zr_m <= 1) zdiv = 0
IF(zr_m >= 2 .AND. zdiv == 0)THEN
  zdiv = 5
  WRITE(*,'(A)') 'Caution: not good setting: zdiv=0 while zr_m>=2.'
  WRITE(*,'(A,I2)') 'zdiv is changed to ',zdiv
END IF
IF(zr_m /= 2)THEN
  ! ========== decide true zrange
  zmax = kpl_small
  zmin = kpl_large
  ! p_mode={3,4,5}なので３列目をzデータとみなす
  CALL decide_range(3,fnum,zmin,zmax)
  ! at this point, zmax and zmin are original values
END IF
IF( p_mode == 3 )THEN
  ! p_mode=3の時は[x y a]
  amax = zmax
  amin = zmin
END IF
RETURN
END SUBROUTINE
!%}}}
! SUB kplot_make_cubenode(fnum) %{{{
SUBROUTINE kplot_make_cubenode(fnum)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: fnum
INTEGER :: ifli
IF( p_mode == 1 ) RETURN
IF( p_mode == 2 ) RETURN
IF( p_mode == 3 ) RETURN
IF( p_mode == 6 ) RETURN
DO ifli = 1, fnum
  cubenode(1,:) = (/ xmin(ifli), ymin, zmin /)
  cubenode(2,:) = (/ xmin(ifli), ymin, zmax /)
  cubenode(3,:) = (/ xmin(ifli), ymax, zmin /)
  cubenode(4,:) = (/ xmin(ifli), ymax, zmax /)
  cubenode(5,:) = (/ xmax(ifli), ymin, zmin /)
  cubenode(6,:) = (/ xmax(ifli), ymin, zmax /)
  cubenode(7,:) = (/ xmax(ifli), ymax, zmin /)
  cubenode(8,:) = (/ xmax(ifli), ymax, zmax /)
END DO
RETURN
END SUBROUTINE
!%}}}
! SUB kplot_determine_bond(debugmode,fnum) %{{{
SUBROUTINE kplot_determine_bond(debugmode,fnum)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( IN ) :: fnum
INTEGER :: ifli
INTEGER :: idoa, idob
INTEGER :: itma
INTEGER :: infloopstoper
DOUBLE PRECISION  :: rtma, rtmb
IF( .NOT. showline ) RETURN
IF( .NOT. showbond ) RETURN
IF( p_mode == 2 ) RETURN
IF( p_mode == 6 ) RETURN
IF( debugmode == 1 ) WRITE (*,*) 'Begin: decide nearest bonds'
DO ifli = 1, fnum
  DO idoa = 1, rowarr(ifli)
    ! ここのempdataの判定は厳密ではないので要修正
    IF(empdata(ifli,1,idoa))CYCLE
    IF(empdata(ifli,2,idoa))CYCLE
    !WRITE(*,'(A,I4$)') 'DEBUG1805161520:',idoa
    rtma = kpl_large
    distance(:) = kpl_large
    DO idob = 1, rowarr(ifli)
      IF(empdata(ifli,1,idob))CYCLE
      IF(empdata(ifli,2,idob))CYCLE
      IF(idoa == idob)CYCLE
      rtmb = 0.d0
      rtmb = rtmb+((kpldata(ifli,1,idoa)-kpldata(ifli,1,idob))**2)
      rtmb = rtmb+((kpldata(ifli,2,idoa)-kpldata(ifli,2,idob))**2)
      ! 厳密にはp_mode==1に対しては列ごとに最近接点を求めるべき
      IF((p_mode == 3).OR.(p_mode == 4).OR.(p_mode==5))THEN
        rtmb = rtmb+((kpldata(ifli,3,idoa)-kpldata(ifli,3,idob))**2)
      END IF
      IF( p_mode == 5 )THEN
        IF( NINT(kpldata(ifli,4,idoa)) >= 0 .AND. NINT(kpldata(ifli,4,idoa)) <= 9 .AND. &
          &   NINT(kpldata(ifli,4,idob)) >= 0 .AND. NINT(kpldata(ifli,4,idob)) <= 9 &
        )THEN
          IF( .NOT. connectpair(NINT(kpldata(ifli,4,idoa)),NINT(kpldata(ifli,4,idob))) )CYCLE
        END IF
      END IF
      distance(idob) = rtmb
      IF(rtmb<rtma)rtma = rtmb
    END DO
    ! ボンド長は二乗されていることに注意
    ! この段階でノードidoaから最も近い結合の距離が求まった
    rtma = rtma*br_r*br_r  ! 許容距離
    ! ボンドを近い順にibmax個まで採用する
    ! ibmax個なければボンドはそこまで
    idob = 0
    DO infloopstoper = 1, rowarr(ifli)
      IF( idob >= ibmax ) EXIT
      IF( ALL(distance(:)==kpl_large) ) EXIT
      CALL getarrpos(SIZE(distance),distance,'min',itma)
      IF( distance(itma) > rtma ) EXIT
      IF( distance(itma) > bondlenmax ** 2 .AND. bondlenmax > 0.d0 ) EXIT
      idob = idob + 1
      bondgroup(ifli,idoa,idob) = itma
      distance(itma) = kpl_large
    END DO
  END DO
END DO
RETURN
END SUBROUTINE
!%}}}
! SUB kplot_arrange_xdata(debugmode,fnum) %{{{
SUBROUTINE kplot_arrange_xdata(debugmode,fnum)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( IN ) :: fnum
INTEGER :: ifli
DOUBLE PRECISION  :: rtma
IF( debugmode == 1 ) WRITE (*,*) 'Begin: arrange x-data'
! ========== arrange xrange
DO ifli = 1, fnum
  rtma = dmax1(DABS(xmax(ifli)),DABS(xmin(ifli)))
  IF(dlog10(rtma)<DBLE(kpl_logmin))THEN
    xmax(ifli) = 1.d0
    xmin(ifli) = -1.d0
    WRITE(*,'(A)') 'Caution: too small x data'
  END IF
  IF(DLOG(xmax(ifli)-xmin(ifli))-DLOG(rtma)<DLOG(2.0d0)*DBLE(kpl_rndmin))THEN
    xmax(ifli) = xmax(ifli)+1.d0
    xmin(ifli) = xmin(ifli)-1.d0
    WRITE(*,'(A)') 'Caution: too small xrange'
  END IF
  IF(MOD(xr_m,2) == 1)THEN
    ! 片方のみが指定された場合にも対応
    IF(xmax_flag(ifli))THEN
      IF(xmin_flag(ifli))THEN
        CYCLE
      ELSE
        xmin(ifli) = xmax(ifli) - (xmax(ifli)-xmin(ifli))/xr_r
      END IF
    ELSE
      IF(xmin_flag(ifli))THEN
        xmax(ifli) = xmin(ifli) + (xmax(ifli)-xmin(ifli))/xr_r
      ELSE
        xmin(ifli) = (xmax(ifli)+xmin(ifli))/2.0d0
        xmax(ifli) = (xmax(ifli)-xmin(ifli))/xr_r
        xmin(ifli) = xmin(ifli)-xmax(ifli)
        xmax(ifli) = xmin(ifli)+xmax(ifli)*2.0d0
      END IF
    END IF
    ! xmin and xmax are fixed for xr_rate
  END IF
END DO
! determine division
IF(p_mode == 4 .OR. p_mode == 5)THEN
  ! 3次元プロットの場合自動分割は行わないことにする
  ! 仮想的に値をいれておく
  xdiv(:) = 4
  mxdiv(:) = 2
ELSE IF(xr_m == 0 .OR. xr_m == 1 )THEN
  ! ========== auto division
  DO ifli = 1, fnum
    IF( xmin_flag(ifli) .OR. xmax_flag(ifli) ) CYCLE
    !-----------
    CALL auto_division(xr_m,xmin(ifli),xmax(ifli),&
    & xdiv(ifli),mxdiv(ifli))
    IF( mxdiv_m /= -1 ) mxdiv(ifli) = mxdiv_m
    IF( xr_m < 2 )THEN
      IF( xdiv(ifli) < 8 )THEN
        xstep(ifli) = 1
      ELSE IF( xdiv(ifli) < 16 )THEN
        xstep(ifli) = 2
      ELSE IF( xdiv(ifli) < 25 )THEN
        xstep(ifli) = 5
      ELSE
        xstep(ifli) = 10
      END IF
    END IF
  END DO
ELSE IF(xr_m == 2 .OR. xr_m == 3 )THEN
  DO ifli = 1, fnum
    IF( xdiv(ifli) == 0 ) xdiv(ifli) = 1
  END DO
END IF
RETURN
END SUBROUTINE
!%}}}
! SUB kplot_arrange_yzadata(debugmode,ierror,errmsg) %{{{
SUBROUTINE kplot_arrange_yzadata(debugmode,ierror,errmsg)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( OUT ) :: ierror
CHARACTER(LEN=*), INTENT( OUT ) :: errmsg
IF( debugmode == 1 ) WRITE (*,*) 'Begin: arrange y-data'
! y
IF(yr_m /= 2)THEN
  IF( p_mode /= 2 )THEN
    CALL arrange_range(yr_m,2,ymin,ymax,yr_r,ierror,errmsg)
    IF( ierror /= 0 )THEN
      WRITE(*,'(A)') TRIM(errmsg)
      ierror = 0
      errmsg = ''
    END IF
    ! at this point, ymin and ymax is fixed for yr_rate
    IF(p_mode == 4 .OR. p_mode == 5)THEN
      ! 仮想的に値をいれておく
      ydiv = 4
      mydiv = 0
    ELSE
      CALL auto_division(yr_m,ymin,ymax,ydiv,mydiv)
      IF( yr_m < 2 )THEN
        IF( ydiv < 8 )THEN
          ystep = 1
        ELSE IF( ydiv < 16 )THEN
          ystep = 2
        ELSE IF( ydiv < 25 )THEN
          ystep = 5
        ELSE
          ystep = 10
        END IF
      END IF
    END IF
  END IF
END IF
! bar
IF( ar_m /= 2 .AND. (p_mode == 2 .OR. p_mode == 3 .OR. p_mode == 5 .OR. p_mode==6 ) )THEN
  ! ========== arrange bar range
  CALL arrange_range(ar_m,2,amin,amax,ar_r,ierror,errmsg)
  IF( ierror /= 0 )THEN
    WRITE(*,'(A)') TRIM(errmsg)
    ierror = 0
    errmsg = ''
  END IF
  CALL auto_division(ar_m,amin,amax,adiv,madiv)
  IF( ar_m < 2 )THEN
    IF( adiv < 8 )THEN
      astep = 1
    ELSE IF( adiv < 16 )THEN
      astep = 2
    ELSE IF( adiv < 25 )THEN
      astep = 5
    ELSE
      astep = 10
    END IF
  END IF
END IF
! z
IF((p_mode == 4.OR.p_mode==5) .AND. zr_m /= 2 )THEN
  ! ========== arrange zrange
  CALL arrange_range(zr_m,3,zmin,zmax,zr_r,ierror,errmsg)
  IF( ierror /= 0 )THEN
    WRITE(*,'(A)') TRIM(errmsg)
    ierror = 0
    errmsg = ''
  END IF
  ! at this point, zmin and zmax is fixed for zr_rate
  ! 仮想的に値をいれておく
  zdiv = 4
  mzdiv = 2
END IF
RETURN
END SUBROUTINE
!%}}}
! SUB kplot_ratiokeep3d(debugmode,fnum) %{{{
SUBROUTINE kplot_ratiokeep3d(debugmode,fnum)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( IN ) :: fnum
DOUBLE PRECISION  :: rtma, rtmb, rtmc
IF( p_mode == 1 ) RETURN
IF( p_mode == 2 ) RETURN
IF( p_mode == 3 ) RETURN
IF( p_mode == 6 ) RETURN
IF( .NOT. realratio ) RETURN
! 3D plot で realratio なら平行移動した後でxyzを同じ縮尺のまま拡大縮小する
IF( debugmode == 1 ) WRITE (*,*) 'Begin: size arrangement for 3D realratio plot'
IF( fnum == 1 )THEN
  rtma = xmax(1)-xmin(1)
  rtmb = ymax-ymin
  rtmc = zmax-zmin
  IF((rtma >= rtmb).AND.(rtma >= rtmc))THEN
    ! x range is the biggest
    ! arrange y range
    rtmb = (ymax+ymin)/2.d0 + (xmax(1)-xmin(1))/2.d0
    rtmc = (ymax+ymin)/2.d0 - (xmax(1)-xmin(1))/2.d0
    ymax = rtmb
    ymin = rtmc
    ! arrange z range
    rtmb = (zmax+zmin)/2.d0 + (xmax(1)-xmin(1))/2.d0
    rtmc = (zmax+zmin)/2.d0 - (xmax(1)-xmin(1))/2.d0
    zmax = rtmb
    zmin = rtmc
  ELSE IF((rtmb >= rtma).AND.(rtmb >= rtmc))THEN
    ! y range is the biggest
    ! arrange x range
    rtmb = (xmax(1)+xmin(1))/2.d0 + (ymax-ymin)/2.d0
    rtmc = (xmax(1)+xmin(1))/2.d0 - (ymax-ymin)/2.d0
    xmax(1) = rtmb
    xmin(1) = rtmc
    ! arrange z range
    rtmb = (zmax+zmin)/2.d0 + (ymax-ymin)/2.d0
    rtmc = (zmax+zmin)/2.d0 - (ymax-ymin)/2.d0
    zmax = rtmb
    zmin = rtmc
  ELSE
    ! z range is the biggest
    ! arrange x range
    rtmb = (xmax(1)+xmin(1))/2.d0 + (zmax-zmin)/2.d0
    rtmc = (xmax(1)+xmin(1))/2.d0 - (zmax-zmin)/2.d0
    xmax(1) = rtmb
    xmin(1) = rtmc
    ! arrange y range
    rtmb = (ymax+ymin)/2.d0 + (zmax-zmin)/2.d0
    rtmc = (ymax+ymin)/2.d0 - (zmax-zmin)/2.d0
    ymax = rtmb
    ymin = rtmc
  END IF
END IF
RETURN
END SUBROUTINE
!%}}}
! SUB kplot_rotate3d(debugmode,fnum) %{{{
SUBROUTINE kplot_rotate3d(debugmode,fnum)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( IN ) :: fnum
INTEGER :: idoa
DOUBLE PRECISION  :: rtma, rtmb, rtmc
IF( p_mode == 1 ) RETURN
IF( p_mode == 2 ) RETURN
IF( p_mode == 3 ) RETURN
IF( p_mode == 6 ) RETURN
IF( debugmode == 1 ) WRITE (*,*) 'Begin: 3D rotation'
IF( fnum == 1 )THEN
  ! ただ回転するのではなく、データ中心を中心として回転する必要がある
  ! 式が複雑になるのでまず平行移動してから回転し、再びその位置へ戻す
  DO idoa = 1, rowarr(1)
    kpldata(1,1,idoa) = kpldata(1,1,idoa)-(xmax(1)+xmin(1))/2.d0
    kpldata(1,2,idoa) = kpldata(1,2,idoa)-(ymax   +ymin   )/2.d0
    kpldata(1,3,idoa) = kpldata(1,3,idoa)-(zmax   +zmin   )/2.d0
    rtma = DCOS(angph)*kpldata(1,1,idoa)+DSIN(angph)*kpldata(1,2,idoa)
    rtmb = -DSIN(angph)*kpldata(1,1,idoa)+DCOS(angph)*kpldata(1,2,idoa)
    rtmb = DCOS(angth)*rtmb+DSIN(angth)*kpldata(1,3,idoa)
    rtmc = -DSIN(angph)*kpldata(1,1,idoa)+DCOS(angph)*kpldata(1,2,idoa)
    rtmc = -DSIN(angth)*rtmc+DCOS(angth)*kpldata(1,3,idoa)
    kpldata(1,1,idoa) = rtma+(xmax(1)+xmin(1))/2.d0
    kpldata(1,2,idoa) = rtmb+(ymax   +ymin   )/2.d0
    kpldata(1,3,idoa) = rtmc+(zmax   +zmin   )/2.d0
  END DO
  DO idoa = 1, 8
    cubenode(idoa,1) = cubenode(idoa,1)-(xmax(1)+xmin(1))/2.d0
    cubenode(idoa,2) = cubenode(idoa,2)-(ymax   +ymin   )/2.d0
    cubenode(idoa,3) = cubenode(idoa,3)-(zmax   +zmin   )/2.d0

    rtma = DCOS(angph)*cubenode(idoa,1)+DSIN(angph)*cubenode(idoa,2)
    rtmb = -DSIN(angph)*cubenode(idoa,1)+DCOS(angph)*cubenode(idoa,2)
    rtmb = DCOS(angth)*rtmb+DSIN(angth)*cubenode(idoa,3)
    rtmc = -DSIN(angph)*cubenode(idoa,1)+DCOS(angph)*cubenode(idoa,2)
    rtmc = -DSIN(angth)*rtmc+DCOS(angth)*cubenode(idoa,3)

    cubenode(idoa,1) = rtma+(xmax(1)+xmin(1))/2.d0
    cubenode(idoa,2) = rtmb+(ymax   +ymin   )/2.d0
    cubenode(idoa,3) = rtmc+(zmax   +zmin   )/2.d0
  END DO
  colmax = 2
  repnum = 0
END IF
RETURN
END SUBROUTINE
!%}}}
! SUB kplot_bondreduce3d(debugmode,fnum) %{{{
SUBROUTINE kplot_bondreduce3d(debugmode,fnum)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( IN ) :: fnum
INTEGER :: ifli
INTEGER :: idoa, idob, idoc
INTEGER :: itma, itmb
IF( p_mode == 1 ) RETURN
IF( p_mode == 2 ) RETURN
IF( p_mode == 3 ) RETURN
IF( p_mode == 6 ) RETURN
IF( .NOT. showline ) RETURN
IF( .NOT. showbond ) RETURN
IF( debugmode == 1 ) WRITE (*,*) 'Begin: bond reduction'
! 各ボンドを、後ろ側の点に所属させる
! 後ろの点から順に描画し、かつ各点に属するボンドを
! その点描画の直後に描画することによって、ある程度正確な
! 前後関係が実現できる
!
! 処理の過程でボンドの数がibmaxを超えてしまう可能性がある
! ibmaxを超えない範囲で新しいものは追加する
! ibmaxを超える場合で
! 既存のボンドに長いものがあれば新しいボンドで置き換える
! 既存のボンドに同一のものがあればそのまま
! 新しいボンドが長いならばそのまま
DO ifli = 1, fnum
  DO idoa = 1, rowarr(ifli)
    idob = 0
    DO
      idob = idob + 1
      IF( idob > ibmax ) EXIT
      itma = bondgroup(ifli,idoa,idob)
      IF( itma == 0 ) EXIT
      IF(kpldata(ifli,3,idoa)<kpldata(ifli,3,itma)) CYCLE
      itmb = 0
      DO idoc = 1, ibmax
        IF( bondgroup(ifli,itma,idoc) == 0 )THEN
          ! 相手方の登録可能位置を負にして憶えておく
          itmb = - idoc
          EXIT
        END IF
        IF( idoa == bondgroup(ifli,itma,idoc) )THEN
          itmb = idoc
          EXIT
        END IF
      END DO
      IF(itmb==0)THEN
        ! 登録されていないが、相手方に登録する余裕もない
        ! あきらめてデータを破棄
        bondgroup(ifli,idoa,idob:ibmax-1) = bondgroup(ifli,idoa,idob+1:ibmax)
        bondgroup(ifli,idoa,ibmax) = 0
        idob = idob - 1
      ELSE IF(itmb<0)THEN
        ! 登録されていないので登録してからこのデータを消す
        bondgroup(ifli,itma,-itmb) = idoa
        bondgroup(ifli,idoa,idob:ibmax-1) = bondgroup(ifli,idoa,idob+1:ibmax)
        bondgroup(ifli,idoa,ibmax) = 0
        idob = idob - 1
        ! [TODO]
      ELSE
        ! 登録されているからこのデータを消す
        bondgroup(ifli,idoa,idob:ibmax-1) = bondgroup(ifli,idoa,idob+1:ibmax)
        bondgroup(ifli,idoa,ibmax) = 0
        idob = idob - 1
      END IF
    END DO
  END DO
END DO
RETURN
END SUBROUTINE
!%}}}
! SUB kplot_bondangle3d(debugmode,fnum) %{{{
SUBROUTINE kplot_bondangle3d(debugmode,fnum)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( IN ) :: fnum
INTEGER :: ifli
INTEGER :: idoa, idob
INTEGER :: itma
DOUBLE PRECISION  :: rtma
IF( p_mode == 1 ) RETURN
IF( p_mode == 2 ) RETURN
IF( p_mode == 3 ) RETURN
IF( p_mode == 6 ) RETURN
IF( .NOT. showline ) RETURN
IF( .NOT. showbond ) RETURN
IF( debugmode == 1 ) WRITE (*,*) 'Begin: decide bond angle'
DO ifli = 1, fnum
  DO idoa = 1, rowarr(ifli)
    DO idob = 1, ibmax
      itma = bondgroup(ifli,idoa,idob)
      IF( itma == 0 ) EXIT
      rtma = 0.d0
      rtma = rtma+((kpldata(ifli,1,idoa)-kpldata(ifli,1,itma))**2)
      rtma = rtma+((kpldata(ifli,2,idoa)-kpldata(ifli,2,itma))**2)
      rtma = rtma+((kpldata(ifli,3,idoa)-kpldata(ifli,3,itma))**2)
      bondangle(ifli,idoa,idob) = &
      & DACOS( DABS(kpldata(ifli,3,idoa)- kpldata(ifli,3,itma)) / DSQRT(rtma) )
      IF(kpldata(ifli,3,idoa)>kpldata(ifli,3,itma))THEN
        bondangle(ifli,idoa,idob) = -bondangle(ifli,idoa,idob)
      END IF
    END DO
  END DO
END DO
RETURN
END SUBROUTINE
!%}}}
! SUB kplot_normalizeinone(debugmode,fnum) %{{{
SUBROUTINE kplot_normalizeinone(debugmode,fnum)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( IN ) :: fnum
INTEGER :: ifli
INTEGER :: idoa, idob
DOUBLE PRECISION  :: rtma, rtmb
IF( debugmode == 1 ) WRITE (*,*) 'Begin: normalization in 1'
! arrange data from -1 to 1

! arrange x-data
DO ifli = 1, fnum
  rtma = 2.d0/(xmax(ifli)-xmin(ifli))
  rtmb = -1.d0-rtma*xmin(ifli)
  DO idoa = 1, rowarr(ifli)+1
    IF(idoa == rowarr(ifli)+1 .AND. p_mode /= 2)EXIT
    IF(empdata(ifli,1,idoa))CYCLE
    IF(empdata(ifli,2,idoa).AND.p_mode==6)CYCLE
    kpldata(ifli,1,idoa) = rtmb+rtma*kpldata(ifli,1,idoa)
  END DO
  DO idoa = 1, vbarnum(ifli)
    vbar(ifli,idoa) = rtmb+rtma*vbar(ifli,idoa)
  END DO
END DO
IF( (p_mode == 4) .AND. (fnum==1))THEN
  rtma = 2.d0/(xmax(1)-xmin(1))
  rtmb = -1.d0-rtma*xmin(1)
  DO idoa = 1, 8
    cubenode(idoa,1) = rtmb+rtma*cubenode(idoa,1)
  END DO
END IF

! arrange y-data
IF(p_mode/=2)THEN
  rtma = 2.0d0/(ymax-ymin)
  rtmb = -1.0d0-rtma*ymin
  DO ifli = 1, fnum
    DO idoa = 1, rowarr(ifli)
      IF(p_mode==1)THEN
        DO idob = 2, colmax
          kpldata(ifli,idob,idoa) = rtmb+rtma*kpldata(ifli,idob,idoa)
        END DO
      ELSE
        IF(p_mode==6.AND.empdata(ifli,2,idoa))CYCLE
        kpldata(ifli,2,idoa) = rtmb+rtma*kpldata(ifli,2,idoa)
      END IF
    END DO
  END DO
  IF(p_mode == 4 .OR. p_mode == 5)THEN
    DO idoa = 1, 8
      cubenode(idoa,2) = rtmb+rtma*cubenode(idoa,2)
    END DO
  END IF
END IF

! arrange z-data
IF(p_mode == 4 .OR. p_mode == 5)THEN
  rtma = 2.0d0/(zmax-zmin)
  rtmb = -1.0d0-rtma*zmin
  DO ifli = 1, fnum
    DO idoa = 1, rowarr(ifli)
      kpldata(ifli,3,idoa) = rtmb+rtma*kpldata(ifli,3,idoa)
    END DO
  END DO
  DO idoa = 1, 8
    cubenode(idoa,3) = rtmb+rtma*cubenode(idoa,3)
  END DO
END IF

! arrange bar-data
IF(p_mode == 2 .OR. p_mode == 3 .OR. p_mode == 5.OR.p_mode==6)THEN
  rtma = 2.0d0/(amax-amin)
  rtmb = -1.0d0-rtma*amin
  DO ifli = 1, fnum
    DO idoa = 1, rowarr(ifli)
      IF(p_mode==2)THEN
        DO idob = 2, colmax
          kpldata(ifli,idob,idoa) = rtmb+rtma*kpldata(ifli,idob,idoa)
        END DO
      ELSE IF(p_mode==3)THEN
        kpldata(ifli,3,idoa) = rtmb+rtma*kpldata(ifli,3,idoa)
      ELSE IF(p_mode==5)THEN
        kpldata(ifli,4,idoa) = rtmb+rtma*kpldata(ifli,4,idoa)
        ! この場合は色決定マクロに入力するために0~1の値にする
        kpldata(ifli,4,idoa) = 0.5d0 + kpldata(ifli,4,idoa)/2.d0
      ELSE IF(p_mode==6)THEN
        IF(empdata(ifli,2,idoa))THEN
          kpldata(ifli,1,idoa) = rtmb+rtma*kpldata(ifli,1,idoa)
          ! この場合は色決定マクロに入力するために0~1の値にする
          kpldata(ifli,1,idoa) = 0.5d0 + kpldata(ifli,1,idoa)/2.d0
        END IF
      END IF
    END DO
  END DO
END IF
RETURN
END SUBROUTINE
!%}}}
! SUB kplot_normalizeinthousand(debugmode,fnum) %{{{
SUBROUTINE kplot_normalizeinthousand(debugmode,fnum)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( IN ) :: fnum
INTEGER :: ifli
INTEGER :: idoa, idob
IF( debugmode == 1 ) WRITE (*,*) 'Begin: normalization in 1000'
! arrange data from -1000 to 1000
! EPS出力時に実数を毎回'0.147'のように出力すると、小数点や'0'を出力する
! ために余計に容量を占有してしまうので、あえて1000倍して出力する
DO ifli = 1, fnum
  DO idoa = 1, rowarr(ifli)+1
    IF(idoa == rowarr(ifli)+1.AND.p_mode /= 2)EXIT
    ! p_mode==6の色指定行を無視する
    IF(p_mode == 6 .AND. empdata(ifli,2,idoa) )CYCLE
    ! x軸データはほぼ常に1000倍にする
    kpldata(ifli,1,idoa) = 1000.d0*kpldata(ifli,1,idoa)
    IF(p_mode == 1)THEN
      DO idob = 2,colmax
        ! p_mode==1の場合にy軸データを1000倍にする
        kpldata(ifli,idob,idoa) = 1000.d0*kpldata(ifli,idob,idoa)
      END DO
    END IF
    ! IF plot-mode is 2, we don't need to transform
    ! p_mode=3,4,5,6の場合にはy軸データを1000倍にする
    IF(p_mode == 3)kpldata(ifli,2,idoa) = 1000.d0*kpldata(ifli,2,idoa)
    IF(p_mode == 4)kpldata(ifli,2,idoa) = 1000.d0*kpldata(ifli,2,idoa)
    IF(p_mode == 5)kpldata(ifli,2,idoa) = 1000.d0*kpldata(ifli,2,idoa)
    IF(p_mode == 6)kpldata(ifli,2,idoa) = 1000.d0*kpldata(ifli,2,idoa)
  END DO
  ! vbar は1000倍にしない
END DO
IF(p_mode == 4.OR.p_mode==5) cubenode(:,1) = 1000.d0*cubenode(:,1)
IF(p_mode == 4.OR.p_mode==5) cubenode(:,2) = 1000.d0*cubenode(:,2)
RETURN
END SUBROUTINE
!%}}}
! SUB kplot_perspective3d(debugmode,fnum) %{{{
SUBROUTINE kplot_perspective3d(debugmode,fnum)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( IN ) :: fnum
INTEGER :: ifli
INTEGER :: idoa
DOUBLE PRECISION  :: rtma, rtmb
IF( p_mode == 1 ) RETURN
IF( p_mode == 2 ) RETURN
IF( p_mode == 3 ) RETURN
IF( p_mode == 6 ) RETURN
IF( pprat == 0.d0 ) RETURN
IF( debugmode == 1 ) WRITE (*,*) 'Begin: perspective arrangement'
rtma = 1.d0+pprat/2.d0
DO ifli = 1, fnum
  DO idoa = 1, rowarr(1)
    IF( pprat * kpldata(ifli,3,idoa) < 1.d0 )THEN
      rtmb = 1.d0 / ( 1.d0 - pprat * kpldata(ifli,3,idoa) )
      kpldata(ifli,1,idoa) = kpldata(ifli,1,idoa)*rtmb
      kpldata(ifli,2,idoa) = kpldata(ifli,2,idoa)*rtmb
    END IF
  END DO
END DO
DO idoa = 1, 8
  IF( pprat * cubenode(idoa,3) < 1.d0 )THEN
    rtmb = 1.d0 / ( 1.d0 - pprat * cubenode(idoa,3) )
    cubenode(idoa,1) = cubenode(idoa,1)*rtmb
    cubenode(idoa,2) = cubenode(idoa,2)*rtmb
  END IF
END DO
RETURN
END SUBROUTINE
!%}}}
! SUB kplot_ratiofix3d(debugmode,fnum) %{{{
SUBROUTINE kplot_ratiofix3d(debugmode,fnum)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( IN ) :: fnum
INTEGER :: ifli
INTEGER :: idoa
IF( p_mode == 1 ) RETURN
IF( p_mode == 2 ) RETURN
IF( p_mode == 3 ) RETURN
IF( p_mode == 6 ) RETURN
IF( .NOT. realratio ) RETURN
IF( sizesqr ) RETURN
! 3D-plot でrealratio だがsizesqr でない場合の処理
IF( debugmode == 1 ) WRITE (*,*) 'Begin: realratio arrangement for 3D-plot'
IF(szx>szy)THEN
  DO ifli = 1, fnum
    DO idoa = 1, rowarr(ifli)+1
      kpldata(ifli,1,idoa) = kpldata(ifli,1,idoa)*(szy/szx)
    END DO
  END DO
  cubenode(:,1) = cubenode(:,1)*(szy/szx)
ELSE
  DO ifli = 1, fnum
    DO idoa = 1, rowarr(ifli)+1
      kpldata(ifli,2,idoa) = kpldata(ifli,2,idoa)*(szx/szy)
    END DO
  END DO
  cubenode(:,2) = cubenode(:,2)*(szx/szy)
END IF
RETURN
END SUBROUTINE
!%}}}
! SUB kplot_finaladjust(debugmode,fnum) %{{{
SUBROUTINE kplot_finaladjust(debugmode,fnum)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( IN ) :: fnum
INTEGER :: ifli
DOUBLE PRECISION  :: rtma, rtmb
IF( debugmode == 1 ) WRITE (*,*) 'Begin: final arrangement'
IF((realratio).AND.(fnum == 1))THEN
  ! [TODO] fnum>1への対応
  IF((p_mode == 1).OR.(p_mode == 3).OR.(p_mode==6))THEN
    rtma = (ymax-ymin)/(xmax(1)-xmin(1))
    rtmb = szy/szx
    IF(rtma>rtmb)THEN
      szx = szy/rtma
    ELSE
      szy = szx*rtma
    END IF
  END IF
END IF
IF(p_mode == 2)THEN
  ydiv = colmax-1
  DO ifli = 1, fnum
    IF( colmax-1 > ydiv ) ydiv = colmax-1
  END DO
END IF
dy = 2.0d0*szy/DBLE(ydiv)
IF(p_mode == 3) repnum = 0
RETURN
END SUBROUTINE
!%}}}
!-----------

!- epsgraph operation routine ----------------------
! SUB kplot_eps_initialdsc(debugmode,outputfile,ifgraph) %{{{
SUBROUTINE kplot_eps_initialdsc(debugmode,outputfile,ifgraph)
USE cs_psc, only: pscversion, pscupdate
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
CHARACTER( LEN = * ), INTENT( IN ) :: outputfile
INTEGER, INTENT( IN ) :: ifgraph
DOUBLE PRECISION  :: rtma
IF( debugmode == 1 ) WRITE (*,*) 'Begin: start making postscript'
WRITE(ifgraph,'(A)') '%!PS-Adobe-3.0 EPSF-3.0'
WRITE(ifgraph,'(A)') '%%Title: ' // TRIM(outputfile)
WRITE(ifgraph,'(A)') '%%Creator: kplot under PSC version ' // &
& TRIM(pscversion) // ' ' // TRIM(pscupdate)
WRITE(ifgraph,'(A)') '%%Orientation: Portrait'
WRITE(ifgraph,'(A$)') '%%BoundingBox:'
IF( implicitbb )THEN
  rtma = tcmg+tcfs
  IF(lymg+lbfs>rtma) rtma = lxmg+lbfs
  IF(tpst == 1)THEN
    WRITE(ifgraph,'(4I4)') &
    & NINT(trx+ctx-szx-lymg-tcfs), &
    & NINT(try+cty-szy-rtma-2),&
    & NINT(trx+ctx+szx+ly2mg+tcfs), &
    & NINT(try+cty+szy+ttmg+ttfs)
  ELSE IF(tpst == 0)THEN
    IF(ttmg+ttfs>rtma)rtma = ttmg+ttfs
    WRITE(ifgraph,'(4I4)') &
    & NINT(trx+ctx-szx-lymg-tcfs), &
    & NINT(try+cty-szy-rtma-ttmg-ttfs),&
    & NINT(trx+ctx+szx+ly2mg+tcfs), &
    & NINT(try+cty+szy+tcfs/2.0d0)
  END IF
ELSE
  WRITE(ifgraph,'(4I4)') &
  & NINT(ctx-szx-bbxmg), &
  & NINT(cty-szy-bbymg), &
  & NINT(ctx+szx+bbxmg), &
  & NINT(cty+szy+bbymg)
END IF
WRITE(ifgraph,'(A)') '%%Pages: 1'
WRITE(ifgraph,'(A)') '%%EndComments'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '%'
END IF
RETURN
END SUBROUTINE
!%}}}
! SUB kplot_eps_prolog(debugmode,fnum,ifgraph) %{{{
SUBROUTINE kplot_eps_prolog(debugmode,fnum,ifgraph)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( IN ) :: fnum
INTEGER, INTENT( IN ) :: ifgraph
LOGICAL, EXTERNAL :: isinrange
INTEGER :: ifli, iflj
INTEGER :: itma
DOUBLE PRECISION  :: rtma, rtmb, rtmc
WRITE(ifgraph,'(A)') '%%BeginProlog'
WRITE(ifgraph,'(A)') '/kpldict 258 dict def'
WRITE(ifgraph,'(A)') 'kpldict begin'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '%'
  !WRITE(ifgraph,'(A,4I4)') '%GraphRange: ', &
  !& NINT(trx+ctx-szx), NINT(try+cty-szy), &
  !& NINT(trx+ctx+szx), NINT(try+cty+szy)
  !WRITE(ifgraph,'(A,4I4)') '%PaperRange: ', &
  !& NINT(    ctx-ctx), NINT(    cty-cty), &
  !& NINT(    ctx+ctx), NINT(    cty+cty)
END IF
! postscript parameters %{{{
IF( debugmode == 1 ) WRITE (*,*) 'Begin: postscript parameters'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% Customizable Parameters %{{'//'{'
  WRITE(ifgraph,'(A)') '% graph size'
END IF
CALL kp_print_def_real(ifgraph, 'ppx', ppx, 'paper size x')
CALL kp_print_def_real(ifgraph, 'ppy', ppy, 'paper size y')
CALL kp_print_def_real(ifgraph, 'ctx', ctx+trx, 'x center')
CALL kp_print_def_real(ifgraph, 'cty', cty+try, 'y center')
CALL kp_print_def_real(ifgraph, 'szx', szx, 'half x width')
CALL kp_print_def_real(ifgraph, 'szy', szy, 'half y width')
CALL kp_print_def_real(ifgraph, 'mpmg', mpmg, 'margin between borders')
IF(fnum>1)THEN
  DO ifli = 1,fnum-1
    WRITE(ifgraph,'(A,F8.5,A)') '/pct'//TRIM(hint(ifli))//' ',pct(ifli), ' def'
  END DO
END IF
!----------------------------------
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% title'
END IF
CALL kp_print_def_str(ifgraph, 'title', title, 'title string')
CALL kp_print_def_real(ifgraph, 'ttmg', ttmg, 'title margin from border')
CALL kp_print_def_real(ifgraph, 'ttfs', ttfs, 'title font size')
CALL kp_print_def_int(ifgraph, 'ttft', ttft, 'title font type')
CALL kp_print_def_int(ifgraph, 'tpst', tpst, 'title position (0 or 1)')
!----------------------------------
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% border, key'
END IF
CALL kp_print_def_real(ifgraph, 'bdlw', bdlw, 'border line width')
CALL kp_print_def_int(ifgraph, 'bdlt', bdlt, 'border line type')
!CALL kp_print_def_str(ifgraph, 'key', key, 'key string')
CALL kp_print_def_real(ifgraph, 'kyll', kyll, 'key line length')
CALL kp_print_def_real(ifgraph, 'kymg', kymg, 'key margin from border')
CALL kp_print_def_real(ifgraph, 'kyfs', kyfs, 'key font size')
CALL kp_print_def_int(ifgraph, 'kyft', kyft, 'key font type')
CALL kp_print_def_real(ifgraph, 'kyfw', kyfw, 'key font width margin')
!----------------------------------
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% tics'
END IF
CALL kp_print_def_real(ifgraph, 'tcmg', tcmg, 'tics margin from border')
CALL kp_print_def_real(ifgraph, 'tcfs', tcfs, 'tics font size')
CALL kp_print_def_int(ifgraph, 'tcft', tcft, 'tics font type')
CALL kp_print_def_real(ifgraph, 'tcll', tcll, 'tics line length')
CALL kp_print_def_real(ifgraph, 'mtcll', mtcll, 'subtics line length')
CALL kp_print_def_real(ifgraph, 'tclw', tclw, 'tics line width')
CALL kp_print_def_real(ifgraph, 'mtclw', mtclw, 'subtics line width')
CALL kp_print_def_real(ifgraph, 'ctll', ctll, 'color bar tics line length')
CALL kp_print_def_real(ifgraph, 'cmtll', cmtll, 'color bar subtics line length')
!----------------------------------
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% label'
END IF
CALL kp_print_def_str(ifgraph, 'xlab', xlab, 'x axis label string')
CALL kp_print_def_str(ifgraph, 'ylab', ylab, 'y axis label string')
CALL kp_print_def_str(ifgraph, 'y2lb', y2lb, 'y2 axis label string')
CALL kp_print_def_real(ifgraph, 'lbfs', lbfs, 'label font size')
CALL kp_print_def_int(ifgraph, 'xlft', xlft, 'x axis label font type')
CALL kp_print_def_int(ifgraph, 'ylft', ylft, 'y axis label font type')
CALL kp_print_def_int(ifgraph, 'y2lft', y2lft, 'y2 axis label font type')
CALL kp_print_def_int(ifgraph, 'vbft', vbft, 'vertbar font type')
CALL kp_print_def_real(ifgraph, 'lxmg', lxmg, 'x axis label margin')
CALL kp_print_def_real(ifgraph, 'lymg', lymg, 'y axis label margin')
CALL kp_print_def_real(ifgraph, 'ly2mg', ly2mg, 'y2 axis label margin')
!----------------------------------
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% grid'
END IF
CALL kp_print_def_real(ifgraph, 'gxlw', gxlw, 'x grid line width')
CALL kp_print_def_real(ifgraph, 'gylw', gylw, 'y grid line width')
CALL kp_print_def_real(ifgraph, 'vblw', vblw, 'vertical bar line width')
!----------------------------------
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% 3D'
END IF
CALL kp_print_def_real(ifgraph, 'pprat', pprat, 'perspective parameter')
!----------------------------------
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% color-bar'
END IF
CALL kp_print_def_real(ifgraph, 'clmg', clmg, 'color bar margin from border')
CALL kp_print_def_real(ifgraph, 'clbx', clbx, 'color bar x size')
CALL kp_print_def_real(ifgraph, 'clby', clby, 'color bar y size')
CALL kp_print_def_int(ifgraph, 'clbn', clbn, 'color bar division number')
!----------------------------------
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% plot line,point,box'
END IF
CALL kp_print_def_real(ifgraph, 'linw', linw, 'line width')
CALL kp_print_def_real(ifgraph, 'pnts', pnts, 'point size')
CALL kp_print_def_real(ifgraph, 'ptlw', ptlw, 'point line width')
CALL kp_print_def_real(ifgraph, 'bxmg', bxmg, 'box x margin')
CALL kp_print_def_real(ifgraph, 'bymg', bymg, 'box y margin')
CALL kp_print_def_real(ifgraph, 'linh', linh, 'line hide width')
CALL kp_print_def_real(ifgraph, 'pnth', pnth, 'point hide width')
!----------------------------------
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% color'
END IF
CALL kp_print_def_color(ifgraph, 'linc', linc(:), 'line color')
CALL kp_print_def_color(ifgraph, 'pntc', pntc(:), 'point color')
CALL kp_print_def_color(ifgraph, 'glbc', glbc(:), 'global background color')
CALL kp_print_def_color(ifgraph, 'bakc', bakc(:), 'graph background color')
CALL kp_print_def_color(ifgraph, 'bdlc', bdlc(:), 'border line color')
CALL kp_print_def_color(ifgraph, 'kyfc', kyfc(:), 'key font color')
CALL kp_print_def_color(ifgraph, 'ttfc', ttfc(:), 'title font color')
CALL kp_print_def_color(ifgraph, 'xlfc', xlfc(:), 'x axis label font color')
CALL kp_print_def_color(ifgraph, 'ylfc', ylfc(:), 'y axis label font color')
CALL kp_print_def_color(ifgraph, 'y2lfc', y2lfc(:), 'y2 axis label font color')
CALL kp_print_def_color(ifgraph, 'tcfc', tcfc(:), 'tics label label font color')
CALL kp_print_def_color(ifgraph, 'tclc', tclc(:), 'tics line color')
CALL kp_print_def_color(ifgraph, 'mtclc', mtclc(:), 'subtics line color')
CALL kp_print_def_color(ifgraph, 'gxlc', gxlc(:), 'x grid line color')
CALL kp_print_def_color(ifgraph, 'gylc', gylc(:), 'y grid line color')
CALL kp_print_def_color(ifgraph, 'vblc', vblc(:), 'vertical bar line color')
CALL kp_print_def_color(ifgraph, 'edgc', edgc(:), 'edge color')
!----------------------------------
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% style number'
END IF
!WRITE(ifgraph,'(A,I3,A)')   '/cnodemax ', cnodemax, ' def'
CALL kp_print_def_int(ifgraph, 'ltnum', ltnum, 'line style number')
CALL kp_print_def_int(ifgraph, 'ptnum', ptnum, 'point style number')
CALL kp_print_def_int(ifgraph, 'clnum', clnum, 'color style number')
CALL kp_print_def_int(ifgraph, 'ftnum', ftnum, 'font style number')
CALL kp_print_def_int(ifgraph, 'sznum', sznum, 'size style number')
!----------------------------------
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% others'
END IF
CALL kp_print_def_real(ifgraph, 'ttkx', ttkx, 'title kern x')
CALL kp_print_def_real(ifgraph, 'ttky', ttky, 'title kern y')
CALL kp_print_def_real(ifgraph, 'kykx', kykx, 'key kern x')
CALL kp_print_def_real(ifgraph, 'kyky', kyky, 'key kern y')
CALL kp_print_def_real(ifgraph, 'xlkx', xlkx, 'x label kern x')
CALL kp_print_def_real(ifgraph, 'xlky', xlky, 'x label kern y')
CALL kp_print_def_real(ifgraph, 'ylkx', ylkx, 'y label kern x')
CALL kp_print_def_real(ifgraph, 'ylky', ylky, 'y label kern y')
CALL kp_print_def_real(ifgraph, 'y2lkx', y2lkx, 'y2 label kern x')
CALL kp_print_def_real(ifgraph, 'y2lky', y2lky, 'y2 label kern y')
CALL kp_print_def_real(ifgraph, 'tckx', tckx, 'tics label kern x')
CALL kp_print_def_real(ifgraph, 'tcky', tcky, 'tics label kern y')
IF( morecomment )THEN
  WRITE(ifgraph,'(A)')  '%}}'//'}'
END IF
!%}}}
IF( debugmode == 1 ) WRITE (*,*) 'Begin: postscript functions'
! Basic Macro %{{{
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% Basic Macros %{{'//'{'
END IF
WRITE(ifgraph,'(A)') '/MT  {  moveto } def'
WRITE(ifgraph,'(A)') '/LT  {  lineto } def'
WRITE(ifgraph,'(A)') '/RMT { rmoveto } def'
WRITE(ifgraph,'(A)') '/RLT { rlineto } def'
WRITE(ifgraph,'(A)') '/NEW { newpath } def'
WRITE(ifgraph,'(A)') '/ST  {  stroke } def'
WRITE(ifgraph,'(A)') '/RGB {  setrgbcolor } def'
WRITE(ifgraph,'(A)') '/LW  { setlinewidth } def'
WRITE(ifgraph,'(A)') '/minus   { -1 mul } def'
WRITE(ifgraph,'(A)') '/double  {  2 mul } def'
WRITE(ifgraph,'(A)') '/mdouble { -2 mul } def'
WRITE(ifgraph,'(A)') '/half    {  2 div } def'
WRITE(ifgraph,'(A)') '/mhalf   { -2 div } def'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '%}}'//'}'
END IF
!%}}}
! Constant Macro %{{{
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% Constant Macros %{{'//'{'
END IF
! pct
IF(fnum>1)THEN
  WRITE(ifgraph,'(A$)') '/pct'//TRIM(hint(fnum))//' 1.0'
  DO ifli = 1, fnum-1
    WRITE(ifgraph,'(A$)') ' pct'//TRIM(hint(ifli))//' sub'
  END DO
  WRITE(ifgraph,'(A)') ' def'
ELSE
  WRITE(ifgraph,'(A)') '/pct1 1.0 def'
END IF
! fnum
WRITE(ifgraph,'(A,I3,A)') '/fnum ', fnum,' def'
! xdiv, ydiv, zdiv
DO ifli = 1, fnum
  WRITE(ifgraph,'(A,I3,A)') '/xdiv'//TRIM(hint(ifli))//' ', xdiv(ifli),' def'
END DO
IF(ydiv<=0) ydiv=1 ! 緊急措置
IF(adiv<=0) adiv=1 ! 緊急措置
IF(zdiv<=0) zdiv=adiv ! 緊急措置
WRITE(ifgraph,'(A,I3,A)') '/ydiv ', ydiv,' def'
WRITE(ifgraph,'(A,I3,A)') '/adiv ', adiv,' def'
WRITE(ifgraph,'(A,I3,A)') '/zdiv ', zdiv,' def'
! mxdiv, mydiv, fxdiv, fmxdiv
DO ifli = 1, fnum
  WRITE(ifgraph,'(A,I3,A)') '/mxdiv'//TRIM(hint(ifli))//' ',mxdiv(ifli),' def'
END DO
WRITE(ifgraph,'(A,I3,A)') '/mydiv ',mydiv,' def'
WRITE(ifgraph,'(A,I3,A)') '/madiv ',madiv,' def'
WRITE(ifgraph,'(A)') '/fxdiv {'
WRITE(ifgraph,'(A)') '  1 dict begin'
WRITE(ifgraph,'(A)') '  /idox exch def'
DO ifli = 1, fnum
  WRITE(ifgraph,'(A)') '  idox '//TRIM(hint(ifli))//' eq { xdiv'//TRIM(hint(ifli))//' } if'
END DO
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
WRITE(ifgraph,'(A)') '/fmxdiv {'
WRITE(ifgraph,'(A)') '  1 dict begin'
WRITE(ifgraph,'(A)') '  /idox exch def'
DO ifli = 1, fnum
  WRITE(ifgraph,'(A)') '  idox '//TRIM(hint(ifli))//' eq { mxdiv'//TRIM(hint(ifli))//' } if'
END DO
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
! dx, dy, mdx, mdy, ...
WRITE(ifgraph,'(A)') '/dx { dup dup fxmax exch fxmin sub exch fxdiv div } def'
WRITE(ifgraph,'(A)') '/dy { szy 2 mul ydiv div } def'
WRITE(ifgraph,'(A)') '/da { szy 2 mul adiv div } def'
WRITE(ifgraph,'(A)') '/dz { szy double zdiv div } def'
WRITE(ifgraph,'(A)') '/ywidth { dy bymg sub } def'
WRITE(ifgraph,'(A)') '/hywidth { ywidth half } def'
WRITE(ifgraph,'(A)') '/hdy     { dy     half } def'
WRITE(ifgraph,'(A)') '/hbxmg   { bxmg   half } def'
WRITE(ifgraph,'(A)') '/htcfs   { tcfs   half } def'
WRITE(ifgraph,'(A)') '/fszxw { szx double mpmg fnum 1 sub mul sub} def'
WRITE(ifgraph,'(A)') '/fxmin {'
WRITE(ifgraph,'(A)') '  1 dict begin'
WRITE(ifgraph,'(A)') '  /idox exch def'
WRITE(ifgraph,'(A)') '  idox 1 eq { ctx szx sub } if'
IF(fnum>1)THEN
  DO ifli = 2, fnum
    WRITE(ifgraph,'(A)') '  idox '//TRIM(hint(ifli))//' eq { ctx szx sub'
    DO iflj = 1, ifli-1
      WRITE(ifgraph,'(A)') '    pct'//TRIM(hint(iflj))//' fszxw mul add mpmg add'
    END DO
    WRITE(ifgraph,'(A)') '  } if'
  END DO
END IF
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
WRITE(ifgraph,'(A)') '/fxmax {'
WRITE(ifgraph,'(A)') '  1 dict begin'
WRITE(ifgraph,'(A)') '  /idox exch def'
WRITE(ifgraph,'(A)') '  idox fnum eq {'
WRITE(ifgraph,'(A)') '    idox fxmin pct'//TRIM(hint(fnum))//' fszxw mul add'
WRITE(ifgraph,'(A)') '  }{'
WRITE(ifgraph,'(A)') '    idox 1 add fxmin mpmg sub'
WRITE(ifgraph,'(A)') '  } ifelse'
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
WRITE(ifgraph,'(A)') '/graphregion {'
! ボーダー線と重ならないように加味する
WRITE(ifgraph,'(A)') '  ctx szx sub bdlw 2 div add cty szy sub bdlw 2 div add MT'
WRITE(ifgraph,'(A)') '  ctx szx add bdlw 2 div sub cty szy sub bdlw 2 div add LT'
WRITE(ifgraph,'(A)') '  ctx szx add bdlw 2 div sub cty szy add bdlw 2 div sub LT'
WRITE(ifgraph,'(A)') '  ctx szx sub bdlw 2 div add cty szy add bdlw 2 div sub LT'
WRITE(ifgraph,'(A)') '} def'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '%}}'//'}'
END IF
!%}}}
! Style Definition %{{{
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% Style Definition %{{'//'{'
  WRITE(ifgraph,'(A)') '% ========== Colors ========='
END IF
WRITE(ifgraph,'(A)') '/STYLECOLORS {'
WRITE(ifgraph,'(A)') '  cvi'
WRITE(ifgraph,'(A)') '  1 sub clnum mod'
WRITE(ifgraph,'(A)') '  ['
DO itma = 1, clnum
  rtma = stylecolors(itma,1)
  rtmb = stylecolors(itma,2)
  rtmc = stylecolors(itma,3)
  IF( isinrange(rtma,0.d0,1.d0) .AND. &
  & isinrange(rtmb,0.d0,1.d0) .AND. &
  & isinrange(rtma,0.d0,1.d0) )THEN
    IF( morecomment )THEN
      WRITE(ifgraph,'(A,3F6.3,A,I2)') '    {',rtma,rtmb,rtmc,'}%',itma
    ELSE
      WRITE(ifgraph,'(A,3F6.3,A)') '    {',rtma,rtmb,rtmc,'}'
    END IF
  ELSE
    IF( morecomment )THEN
      WRITE(ifgraph,'(A,3F6.3,A,I2)') '    {',0.d0,0.d0,0.d0,'}%',itma
    ELSE
      WRITE(ifgraph,'(A,3F6.3,A)') '    {',0.d0,0.d0,0.d0,'}'
    END IF
  END IF
END DO
WRITE(ifgraph,'(A)') '  ]'
WRITE(ifgraph,'(A)') '  exch get exec'
WRITE(ifgraph,'(A)') '} def'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% ========== Lines =========='
END IF
! movetoは変更しない：文字列の描画等に影響するため
WRITE(ifgraph,'(A)') '/EMPTYLINES {'
WRITE(ifgraph,'(A)') '  /LT {pop pop} def'
WRITE(ifgraph,'(A)') '  /RLT {pop pop} def'
WRITE(ifgraph,'(A)') '} def'
WRITE(ifgraph,'(A)') '/NORMALLINES {'
WRITE(ifgraph,'(A)') '  /LT { lineto } def'
WRITE(ifgraph,'(A)') '  /RLT { rlineto } def'
WRITE(ifgraph,'(A)') '  0 setlinecap [] 0 setdash'
WRITE(ifgraph,'(A)') '} def'
WRITE(ifgraph,'(A)') '/STYLELINES {'
WRITE(ifgraph,'(A)') '  cvi'
WRITE(ifgraph,'(A)') '  dup 0 eq'
WRITE(ifgraph,'(A)') '  {'
WRITE(ifgraph,'(A)') '    pop'
WRITE(ifgraph,'(A)') '    EMPTYLINES'
WRITE(ifgraph,'(A)') '  }'
WRITE(ifgraph,'(A)') '  {'
WRITE(ifgraph,'(A)') '    NORMALLINES'
WRITE(ifgraph,'(A,I3,A)') '    1 sub ltnum mod'
WRITE(ifgraph,'(A)') '    ['
DO itma = 1, ltnum
  IF( morecomment )THEN
    WRITE(ifgraph,'(A,I2)') '      { ' // TRIM(stylelines(itma)) // ' }%',itma
  ELSE
    WRITE(ifgraph,'(A)') '      { ' // TRIM(stylelines(itma)) // ' }'
  END IF
END DO
WRITE(ifgraph,'(A)') '    ]'
WRITE(ifgraph,'(A)') '    exch get exec'
WRITE(ifgraph,'(A)') '  }'
WRITE(ifgraph,'(A)') '  ifelse'
WRITE(ifgraph,'(A)') '} def'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% ========== Points =========='
END IF
WRITE(ifgraph,'(A)') '/STYLEPOINTS {'
WRITE(ifgraph,'(A)') '  cvi'
WRITE(ifgraph,'(A)') '  dup 0 eq'
WRITE(ifgraph,'(A)') '  {'
WRITE(ifgraph,'(A)') '    pop'
WRITE(ifgraph,'(A)') '    /Plot {pop pop} def'
WRITE(ifgraph,'(A)') '    /Plot2 {pop pop} def'
WRITE(ifgraph,'(A)') '  }'
WRITE(ifgraph,'(A)') '  {'
WRITE(ifgraph,'(A)') '    /MT { moveto } def'
WRITE(ifgraph,'(A)') '    /LT { lineto } def'
WRITE(ifgraph,'(A)') '    /RMT { rmoveto } def'
WRITE(ifgraph,'(A)') '    /RLT { rlineto } def'
WRITE(ifgraph,'(A,I2,A)') '    1 sub ptnum mod'
WRITE(ifgraph,'(A)') '    ['
DO itma = 1, ptnum
  WRITE(ifgraph,'(A$)')    '      { /Plot {'//TRIM(stylepoints(itma))//'} def '
  WRITE(ifgraph,'(A$)')    '/Plot2 {'//TRIM(stylepoints2(itma))//'} def }'
  IF( morecomment )THEN
    WRITE(ifgraph,'(A,I3)') '%',itma
  ELSE
    WRITE(ifgraph,*)
  END IF
END DO
WRITE(ifgraph,'(A)') '    ]'
WRITE(ifgraph,'(A)') '    exch get exec'
WRITE(ifgraph,'(A)') '  }'
WRITE(ifgraph,'(A)') '  ifelse'
WRITE(ifgraph,'(A)') '} def'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% ========== Size ========='
END IF
WRITE(ifgraph,'(A)') '/STYLESIZE {'
IF( sznum > 0 )THEN
  WRITE(ifgraph,'(A)') '  cvi'
  WRITE(ifgraph,'(A,I2,A)') '  1 sub sznum mod'
  WRITE(ifgraph,'(A)') '  ['
  DO itma = 1, sznum
    rtma = stylesize(itma)
    IF( morecomment )THEN
      WRITE(ifgraph,'(A,F8.3,A,I2)') '    {',rtma,'}%',itma
    ELSE
      WRITE(ifgraph,'(A,F8.3,A)') '    {',rtma,'}'
    END IF
  END DO
  WRITE(ifgraph,'(A)') '  ]'
  WRITE(ifgraph,'(A)') '  exch get exec'
ELSE
  ! sznum==0ならデフォルトのpointsizeを使用する
  WRITE(ifgraph,'(A)') '  pop pnts'
END IF
WRITE(ifgraph,'(A)') '} def'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% ========== Fonts =========='
END IF
WRITE(ifgraph,'(A)') '/STYLEFONTS {'
WRITE(ifgraph,'(A)') '  cvi'
WRITE(ifgraph,'(A)') '  dup 0 eq'
WRITE(ifgraph,'(A)') '  {'
WRITE(ifgraph,'(A)') '    pop'
WRITE(ifgraph,'(A)') '    /strLT {pop pop pop} def'
WRITE(ifgraph,'(A)') '    /strCT {pop pop pop} def'
WRITE(ifgraph,'(A)') '    /strRT {pop pop pop} def'
WRITE(ifgraph,'(A)') '    /strLC {pop pop pop} def'
WRITE(ifgraph,'(A)') '    /strCC {pop pop pop} def'
WRITE(ifgraph,'(A)') '    /strRC {pop pop pop} def'
WRITE(ifgraph,'(A)') '    /strLB {pop pop pop} def'
WRITE(ifgraph,'(A)') '    /strCB {pop pop pop} def'
WRITE(ifgraph,'(A)') '    /strRB {pop pop pop} def'
WRITE(ifgraph,'(A)') '  }'
WRITE(ifgraph,'(A)') '  {'
WRITE(ifgraph,'(A)') '    /strLT {OstrLT} def'
WRITE(ifgraph,'(A)') '    /strCT {OstrCT} def'
WRITE(ifgraph,'(A)') '    /strRT {OstrRT} def'
WRITE(ifgraph,'(A)') '    /strLC {OstrLC} def'
WRITE(ifgraph,'(A)') '    /strCC {OstrCC} def'
WRITE(ifgraph,'(A)') '    /strRC {OstrRC} def'
WRITE(ifgraph,'(A)') '    /strLB {OstrLB} def'
WRITE(ifgraph,'(A)') '    /strCB {OstrCB} def'
WRITE(ifgraph,'(A)') '    /strRB {OstrRB} def'
WRITE(ifgraph,'(A,I3,A)') '    1 sub ftnum mod'
WRITE(ifgraph,'(A)') '    ['
DO itma = 1, ftnum
  IF( morecomment )THEN
    WRITE(ifgraph,'(A,I3)') '      { '//TRIM(stylefonts(itma))//' }%',itma
  ELSE
    WRITE(ifgraph,'(A)') '      { '//TRIM(stylefonts(itma))//' }'
  END IF
END DO
WRITE(ifgraph,'(A)') '    ]'
WRITE(ifgraph,'(A)') '    exch get exec'
WRITE(ifgraph,'(A)') '  }'
WRITE(ifgraph,'(A)') '  ifelse'
WRITE(ifgraph,'(A)') '} def'
WRITE(ifgraph,'(A)') '/SETFONT { findfont fsize scalefont setfont } def'
WRITE(ifgraph,'(A)') '/FS { /fsize exch def } def'
WRITE(ifgraph,'(A)') '/reencodeISO {'
WRITE(ifgraph,'(A)') '  dup dup findfont dup length dict begin'
WRITE(ifgraph,'(A)') '  { 1 index /FID ne { def }{ pop pop } ifelse } forall'
WRITE(ifgraph,'(A)') '  /Encoding ISOLatin1Encoding def'
WRITE(ifgraph,'(A)') '  currentdict end definefont'
WRITE(ifgraph,'(A)') '} def'
WRITE(ifgraph,'(A)') '/Angstrom <c5> def'
WRITE(ifgraph,'(A)') '/pinit (\050) def'
WRITE(ifgraph,'(A)') '/pfinal (\051) def'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '%}}'//'}'
END IF
!%}}}
! String Operations %{{{
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% String Operations %{{'//'{'
END IF
!
WRITE(ifgraph,'(A)') '/getstrheight {'
WRITE(ifgraph,'(A)') '  8 dict begin'
WRITE(ifgraph,'(A)') '  /mystring exch def'
WRITE(ifgraph,'(A)') '  /basefont currentfont def'
WRITE(ifgraph,'(A)') '  /loopcounter 0 def'
WRITE(ifgraph,'(A)') '  /readingspecialcommand false def'
WRITE(ifgraph,'(A)') '  /mychar 1 string def'
WRITE(ifgraph,'(A)') '  /loopcounter 0 def'
WRITE(ifgraph,'(A)') '  /readingspecialcommand false def'
WRITE(ifgraph,'(A)') '  /distance 0. def'
WRITE(ifgraph,'(A)') '  gsave'
WRITE(ifgraph,'(A)') '  newpath 0 0 moveto'
WRITE(ifgraph,'(A)') '  {'
WRITE(ifgraph,'(A)') '    readingspecialcommand'
WRITE(ifgraph,'(A)') '    {'
! reading special command
WRITE(ifgraph,'(A)') '      mystring loopcounter get'
WRITE(ifgraph,'(A)') '      125 eq'
WRITE(ifgraph,'(A)') '      {'
! end special command
WRITE(ifgraph,'(A)') '        /readingspecialcommand false def'
WRITE(ifgraph,'(A)') '      }{'
! continue reading special command
WRITE(ifgraph,'(A)') '        mystring loopcounter get 43 eq'!+ enlarge font
WRITE(ifgraph,'(A)') '        { currentfont 1.1 scalefont setfont } if'
WRITE(ifgraph,'(A)') '        mystring loopcounter get 45 eq'!- ensmall font
WRITE(ifgraph,'(A)') '        { currentfont 1 1.1 div scalefont setfont } if'
WRITE(ifgraph,'(A)') '        mystring loopcounter get 62 eq'!> kern 1pt to right
WRITE(ifgraph,'(A)') '        { 1 0 rmoveto } if'
WRITE(ifgraph,'(A)') '        mystring loopcounter get 60 eq'!< kern 1pt to left
WRITE(ifgraph,'(A)') '        { -1 0 rmoveto } if'
WRITE(ifgraph,'(A)') '        mystring loopcounter get 86 eq'!V kern 1pt to bottom
WRITE(ifgraph,'(A)') '        { 0 -1 rmoveto } if'
WRITE(ifgraph,'(A)') '        mystring loopcounter get 94 eq'!^ kern 1pt to top
WRITE(ifgraph,'(A)') '        { 0 1 rmoveto } if'
WRITE(ifgraph,'(A)') '        mystring loopcounter get dup 49 ge exch 57 le and'! 1-9 select font
WRITE(ifgraph,'(A)') '        { mystring loopcounter get 48 sub STYLEFONTS } if'
WRITE(ifgraph,'(A)') '        mystring loopcounter get 48 eq'! 0 reset font
WRITE(ifgraph,'(A)') '        { basefont setfont } if'
WRITE(ifgraph,'(A)') '      } ifelse'
WRITE(ifgraph,'(A)') '    }{'
! reading normal text
WRITE(ifgraph,'(A)') '      mystring loopcounter get'
WRITE(ifgraph,'(A)') '      123 eq'
WRITE(ifgraph,'(A)') '      {'
! start special command
WRITE(ifgraph,'(A)') '        /readingspecialcommand true def'
WRITE(ifgraph,'(A)') '      }{'
! continue normal text
WRITE(ifgraph,'(A)') '        mychar 0'
WRITE(ifgraph,'(A)') '        mystring loopcounter get put'
WRITE(ifgraph,'(A)') '        loopcounter 0 ne { 0 everykerny rmoveto } if'
WRITE(ifgraph,'(A)') '        mychar true charpath'
WRITE(ifgraph,'(A)') '      } ifelse'
WRITE(ifgraph,'(A)') '    } ifelse'
WRITE(ifgraph,'(A)') '    loopcounter 1 add /loopcounter exch def'
WRITE(ifgraph,'(A)') '    loopcounter mystring length ge {exit} if'
WRITE(ifgraph,'(A)') '  } loop'
WRITE(ifgraph,'(A)') '  pathbbox 4 1 roll pop 3 1 roll pop exch sub'
WRITE(ifgraph,'(A)') '  grestore'
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
WRITE(ifgraph,'(A)') '/getstrwidth {'
WRITE(ifgraph,'(A)') '  6 dict begin'
WRITE(ifgraph,'(A)') '  /mystring exch def'
WRITE(ifgraph,'(A)') '  /basefont currentfont def'
WRITE(ifgraph,'(A)') '  /loopcounter 0 def'
WRITE(ifgraph,'(A)') '  /readingspecialcommand false def'
WRITE(ifgraph,'(A)') '  /distance 0. def'
WRITE(ifgraph,'(A)') '  /mychar 1 string def'
WRITE(ifgraph,'(A)') '  gsave'
WRITE(ifgraph,'(A)') '  {'
WRITE(ifgraph,'(A)') '    readingspecialcommand'
WRITE(ifgraph,'(A)') '    {'
! reading special command
WRITE(ifgraph,'(A)') '      mystring loopcounter get'
WRITE(ifgraph,'(A)') '      125 eq'
WRITE(ifgraph,'(A)') '      {'
! end special command
WRITE(ifgraph,'(A)') '        /readingspecialcommand false def'
WRITE(ifgraph,'(A)') '      }{'
! continue reading special command
WRITE(ifgraph,'(A)') '        mystring loopcounter get 43 eq'!+ enlarge font
WRITE(ifgraph,'(A)') '        { currentfont 1.1 scalefont setfont } if'
WRITE(ifgraph,'(A)') '        mystring loopcounter get 45 eq'!- ensmall font
WRITE(ifgraph,'(A)') '        { currentfont 1 1.1 div scalefont setfont } if'
WRITE(ifgraph,'(A)') '        mystring loopcounter get 62 eq'!> kern 1pt to right
WRITE(ifgraph,'(A)') '        { distance 1 add /distance exch def } if'
WRITE(ifgraph,'(A)') '        mystring loopcounter get 60 eq'!< kern 1pt to left
WRITE(ifgraph,'(A)') '        { distance 1 sub /distance exch def } if'
WRITE(ifgraph,'(A)') '        mystring loopcounter get dup 49 ge exch 57 le and'! 1-9 select font
WRITE(ifgraph,'(A)') '        { mystring loopcounter get 48 sub STYLEFONTS } if'
WRITE(ifgraph,'(A)') '        mystring loopcounter get 48 eq'! 0 reset font
WRITE(ifgraph,'(A)') '        { basefont setfont } if'
WRITE(ifgraph,'(A)') '      } ifelse'
WRITE(ifgraph,'(A)') '    }{'
! reading normal text
WRITE(ifgraph,'(A)') '      mystring loopcounter get'
WRITE(ifgraph,'(A)') '      123 eq'
WRITE(ifgraph,'(A)') '      {'
! start special command
WRITE(ifgraph,'(A)') '        /readingspecialcommand true def'
WRITE(ifgraph,'(A)') '      }{'
! continue normal text
WRITE(ifgraph,'(A)') '        mychar 0'
WRITE(ifgraph,'(A)') '        mystring loopcounter get put'
WRITE(ifgraph,'(A)') '        loopcounter 0 ne { distance everykernx add /distance exch def } if'
WRITE(ifgraph,'(A)') '        mychar stringwidth pop distance add /distance exch def'
WRITE(ifgraph,'(A)') '      } ifelse'
WRITE(ifgraph,'(A)') '    } ifelse'
WRITE(ifgraph,'(A)') '    loopcounter 1 add /loopcounter exch def'
WRITE(ifgraph,'(A)') '    loopcounter mystring length ge {exit} if'
WRITE(ifgraph,'(A)') '  } loop'
WRITE(ifgraph,'(A)') '  distance'
WRITE(ifgraph,'(A)') '  grestore'
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
!-----------------------------
WRITE(ifgraph,'(A)') '/strshow {'
WRITE(ifgraph,'(A)') '  7 dict begin'
WRITE(ifgraph,'(A)') '  /mystring exch def'
WRITE(ifgraph,'(A)') '  /basefont currentfont def'
WRITE(ifgraph,'(A)') '  /loopcounter 0 def'
WRITE(ifgraph,'(A)') '  /readingspecialcommand false def'
WRITE(ifgraph,'(A)') '  /mychar 1 string def'
WRITE(ifgraph,'(A)') '  /readingspecialcommand false def'
WRITE(ifgraph,'(A)') '  gsave'
WRITE(ifgraph,'(A)') '  {'
WRITE(ifgraph,'(A)') '    readingspecialcommand'
WRITE(ifgraph,'(A)') '    {'
! reading special command
WRITE(ifgraph,'(A)') '      mystring loopcounter get'
WRITE(ifgraph,'(A)') '      125 eq'
WRITE(ifgraph,'(A)') '      {'
! end special command
WRITE(ifgraph,'(A)') '        /readingspecialcommand false def'
WRITE(ifgraph,'(A)') '      }{'
! continue reading special command
WRITE(ifgraph,'(A)') '        mystring loopcounter get 43 eq'!+ enlarge font
WRITE(ifgraph,'(A)') '        { currentfont 1.1 scalefont setfont } if'
WRITE(ifgraph,'(A)') '        mystring loopcounter get 45 eq'!- ensmall font
WRITE(ifgraph,'(A)') '        { currentfont 1 1.1 div scalefont setfont } if'
WRITE(ifgraph,'(A)') '        mystring loopcounter get 62 eq'!> kern 1pt to right
WRITE(ifgraph,'(A)') '        { 1 0 rmoveto } if'
WRITE(ifgraph,'(A)') '        mystring loopcounter get 60 eq'!< kern 1pt to left
WRITE(ifgraph,'(A)') '        { -1 0 rmoveto } if'
WRITE(ifgraph,'(A)') '        mystring loopcounter get 86 eq'!V kern 1pt to bottom
WRITE(ifgraph,'(A)') '        { 0 -1 rmoveto } if'
WRITE(ifgraph,'(A)') '        mystring loopcounter get 94 eq'!^ kern 1pt to top
WRITE(ifgraph,'(A)') '        { 0 1 rmoveto } if'
WRITE(ifgraph,'(A)') '        mystring loopcounter get dup 49 ge exch 57 le and'! 1-9 select font
WRITE(ifgraph,'(A)') '        { mystring loopcounter get 48 sub STYLEFONTS } if'
WRITE(ifgraph,'(A)') '        mystring loopcounter get 48 eq'! 0 reset font
WRITE(ifgraph,'(A)') '        { basefont setfont } if'
WRITE(ifgraph,'(A)') '      } ifelse'
WRITE(ifgraph,'(A)') '    }{'
! reading normal text
WRITE(ifgraph,'(A)') '      mystring loopcounter get'
WRITE(ifgraph,'(A)') '      123 eq'
WRITE(ifgraph,'(A)') '      {'
! start special command
WRITE(ifgraph,'(A)') '        /readingspecialcommand true def'
WRITE(ifgraph,'(A)') '      }{'
! continue normal text
WRITE(ifgraph,'(A)') '        mychar 0'
WRITE(ifgraph,'(A)') '        mystring loopcounter get put'
WRITE(ifgraph,'(A)') '        loopcounter 0 ne { everykernx everykerny rmoveto } if'
WRITE(ifgraph,'(A)') '        mychar show'
WRITE(ifgraph,'(A)') '      } ifelse'
WRITE(ifgraph,'(A)') '    } ifelse'
WRITE(ifgraph,'(A)') '    loopcounter 1 add /loopcounter exch def'
WRITE(ifgraph,'(A)') '    loopcounter mystring length ge {exit} if'
WRITE(ifgraph,'(A)') '  } loop'
WRITE(ifgraph,'(A)') '  grestore'
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
!
WRITE(ifgraph,'(A)') '/OstrLT {'
WRITE(ifgraph,'(A)') '  1 dict begin'
WRITE(ifgraph,'(A)') '  /mystring exch def'
WRITE(ifgraph,'(A)') '  MT mystring strshow'
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
WRITE(ifgraph,'(A)') '/OstrLC {'
WRITE(ifgraph,'(A)') '  1 dict begin'
WRITE(ifgraph,'(A)') '  /mystring exch def'
WRITE(ifgraph,'(A)') '  MT'
WRITE(ifgraph,'(A)') '  0 mystring getstrheight neg half RMT'
WRITE(ifgraph,'(A)') '  mystring strshow'
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
WRITE(ifgraph,'(A)') '/OstrLB {'
WRITE(ifgraph,'(A)') '  1 dict begin'
WRITE(ifgraph,'(A)') '  /mystring exch def'
WRITE(ifgraph,'(A)') '  MT'
WRITE(ifgraph,'(A)') '  0 mystring getstrheight neg RMT'
WRITE(ifgraph,'(A)') '  mystring strshow'
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
WRITE(ifgraph,'(A)') '/OstrCT {'
WRITE(ifgraph,'(A)') '  1 dict begin'
WRITE(ifgraph,'(A)') '  /mystring exch def'
WRITE(ifgraph,'(A)') '  MT'
WRITE(ifgraph,'(A)') '  mystring getstrwidth neg half 0 RMT'
WRITE(ifgraph,'(A)') '  mystring strshow'
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
WRITE(ifgraph,'(A)') '/OstrCC {'
WRITE(ifgraph,'(A)') '  1 dict begin'
WRITE(ifgraph,'(A)') '  /mystring exch def'
WRITE(ifgraph,'(A)') '  MT'
WRITE(ifgraph,'(A)') '  mystring getstrwidth neg half mystring getstrheight neg half RMT'
WRITE(ifgraph,'(A)') '  mystring strshow'
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
WRITE(ifgraph,'(A)') '/OstrCB {'
WRITE(ifgraph,'(A)') '  1 dict begin'
WRITE(ifgraph,'(A)') '  /mystring exch def'
WRITE(ifgraph,'(A)') '  MT'
WRITE(ifgraph,'(A)') '  mystring getstrwidth neg half mystring getstrheight neg RMT'
WRITE(ifgraph,'(A)') '  mystring strshow'
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
WRITE(ifgraph,'(A)') '/OstrRT {'
WRITE(ifgraph,'(A)') '  1 dict begin'
WRITE(ifgraph,'(A)') '  /mystring exch def'
WRITE(ifgraph,'(A)') '  MT'
WRITE(ifgraph,'(A)') '  mystring getstrwidth neg 0 RMT'
WRITE(ifgraph,'(A)') '  mystring strshow'
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
WRITE(ifgraph,'(A)') '/OstrRC {'
WRITE(ifgraph,'(A)') '  1 dict begin'
WRITE(ifgraph,'(A)') '  /mystring exch def'
WRITE(ifgraph,'(A)') '  MT'
WRITE(ifgraph,'(A)') '  mystring getstrwidth neg mystring getstrheight neg half RMT'
WRITE(ifgraph,'(A)') '  mystring strshow'
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
WRITE(ifgraph,'(A)') '/OstrRB {'
WRITE(ifgraph,'(A)') '  1 dict begin'
WRITE(ifgraph,'(A)') '  /mystring exch def'
WRITE(ifgraph,'(A)') '  MT'
WRITE(ifgraph,'(A)') '  mystring getstrwidth neg mystring getstrheight neg RMT'
WRITE(ifgraph,'(A)') '  mystring strshow'
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
WRITE(ifgraph,'(A)') '/SETKERN {'
WRITE(ifgraph,'(A)') '  /everykerny exch def'
WRITE(ifgraph,'(A)') '  /everykernx exch def'
WRITE(ifgraph,'(A)') '} def'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '%}}'//'}'
END IF
!%}}}
! Plot Points Definition %{{{
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% Plot Points Definitions %{{'//'{'
END IF
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% ========== Circle Function =========='
END IF
WRITE(ifgraph,'(A)') '/Ci {'
WRITE(ifgraph,'(A)') '  2 dict begin'
WRITE(ifgraph,'(A)') '  /y exch def'
WRITE(ifgraph,'(A)') '  /x exch def'
WRITE(ifgraph,'(A)') '  NEW'
WRITE(ifgraph,'(A)') '  x pnts add   y MT'
WRITE(ifgraph,'(A)') '  x y pnts 0 360 arc'
WRITE(ifgraph,'(A)') '  closepath'
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% ========== Square Function =========='
END IF
WRITE(ifgraph,'(A)') '/SqSize { 0.90 } def'
WRITE(ifgraph,'(A)') '/Sq {'
WRITE(ifgraph,'(A)') '  3 dict begin'
WRITE(ifgraph,'(A)') '  /y exch def'
WRITE(ifgraph,'(A)') '  /x exch def'
WRITE(ifgraph,'(A)') '  /tmpsq { SqSize pnts mul } def'
WRITE(ifgraph,'(A)') '  NEW'
WRITE(ifgraph,'(A)') '  x tmpsq sub y tmpsq sub MT'
WRITE(ifgraph,'(A)') '  tmpsq double  0             RLT'
WRITE(ifgraph,'(A)') '  0             tmpsq double  RLT'
WRITE(ifgraph,'(A)') '  tmpsq mdouble 0             RLT'
WRITE(ifgraph,'(A)') '  0             tmpsq mdouble RLT'
WRITE(ifgraph,'(A)') '  closepath'
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% ========== Dia Function ============='
END IF
WRITE(ifgraph,'(A)') '/DiSize { 1.30 } def'
WRITE(ifgraph,'(A)') '/Di {'
WRITE(ifgraph,'(A)') '  2 dict begin'
WRITE(ifgraph,'(A)') '  /y exch def'
WRITE(ifgraph,'(A)') '  /x exch def'
WRITE(ifgraph,'(A)') '  x y translate'
WRITE(ifgraph,'(A)') '  45 rotate'
WRITE(ifgraph,'(A)') '  0 0 Sq'
WRITE(ifgraph,'(A)') '  -45 rotate'
WRITE(ifgraph,'(A)') '  x minus y minus translate'
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% ========== Top Triangle Function =========='
END IF
WRITE(ifgraph,'(A)') '/TtSize { 1.30 } def'
WRITE(ifgraph,'(A)') '/Tt {'
WRITE(ifgraph,'(A)') '  4 dict begin'
WRITE(ifgraph,'(A)') '  /y exch def'
WRITE(ifgraph,'(A)') '  /x exch def'
WRITE(ifgraph,'(A)') '  /tmpttx TtSize pnts mul 2 div 3 sqrt mul def'
WRITE(ifgraph,'(A)') '  /tmptty TtSize pnts mul def'
WRITE(ifgraph,'(A)') '  x tmpttx sub y tmptty half sub MT'
WRITE(ifgraph,'(A)') '  tmpttx tmptty 3 mul 2 div RLT'
WRITE(ifgraph,'(A)') '  tmpttx tmptty 3 mul 2 div neg RLT'
WRITE(ifgraph,'(A)') '  closepath'
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% ========== Bottom Triangle Function =========='
END IF
WRITE(ifgraph,'(A)') '/Tb {'
WRITE(ifgraph,'(A)') '  2 dict begin'
WRITE(ifgraph,'(A)') '  /y exch def'
WRITE(ifgraph,'(A)') '  /x exch def'
WRITE(ifgraph,'(A)') '  x y translate'
WRITE(ifgraph,'(A)') '  180 rotate'
WRITE(ifgraph,'(A)') '  0 0 Tt'
WRITE(ifgraph,'(A)') '  -180 rotate'
WRITE(ifgraph,'(A)') '  x minus y minus translate'
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% ========== Left Triangle Function ============'
END IF
WRITE(ifgraph,'(A)') '/Tl {'
WRITE(ifgraph,'(A)') '  2 dict begin'
WRITE(ifgraph,'(A)') '  /y exch def'
WRITE(ifgraph,'(A)') '  /x exch def'
WRITE(ifgraph,'(A)') '  x y translate'
WRITE(ifgraph,'(A)') '  90 rotate'
WRITE(ifgraph,'(A)') '  0 0 Tt'
WRITE(ifgraph,'(A)') '  -90 rotate'
WRITE(ifgraph,'(A)') '  x minus y minus translate'
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% ========= Right Triangle Function ============'
END IF
WRITE(ifgraph,'(A)') '/Tr {'
WRITE(ifgraph,'(A)') '  2 dict begin'
WRITE(ifgraph,'(A)') '  /y exch def'
WRITE(ifgraph,'(A)') '  /x exch def'
WRITE(ifgraph,'(A)') '  x y translate'
WRITE(ifgraph,'(A)') '  270 rotate'
WRITE(ifgraph,'(A)') '  0 0 Tt'
WRITE(ifgraph,'(A)') '  -270 rotate'
WRITE(ifgraph,'(A)') '  x minus y minus translate'
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% ========= Penta Star Function ================'
END IF
WRITE(ifgraph,'(A)') '/PsSize { 1.80 } def'
WRITE(ifgraph,'(A)') '/Ps {'
WRITE(ifgraph,'(A)') '  3 dict begin'
WRITE(ifgraph,'(A)') '  /y exch def'
WRITE(ifgraph,'(A)') '  /x exch def'
WRITE(ifgraph,'(A)') '  /tmpps { PsSize pnts mul} def'
WRITE(ifgraph,'(A)') '  x y translate'
WRITE(ifgraph,'(A)') '  0 tmpps MT'
WRITE(ifgraph,'(A)') '  144 rotate 0 tmpps LT'
WRITE(ifgraph,'(A)') '  144 rotate 0 tmpps LT'
WRITE(ifgraph,'(A)') '  144 rotate 0 tmpps LT'
WRITE(ifgraph,'(A)') '  144 rotate 0 tmpps LT'
WRITE(ifgraph,'(A)') '  144 rotate'
WRITE(ifgraph,'(A)') '  closepath'
WRITE(ifgraph,'(A)') '  x minus y minus translate'
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% ========== Cross Function =========='
END IF
WRITE(ifgraph,'(A)') '/CrSize { 1.40 } def'
WRITE(ifgraph,'(A)') '/Cr {'
WRITE(ifgraph,'(A)') '  3 dict begin'
WRITE(ifgraph,'(A)') '  /y exch def'
WRITE(ifgraph,'(A)') '  /x exch def'
WRITE(ifgraph,'(A)') '  /tmpcr { CrSize pnts mul} def'
WRITE(ifgraph,'(A)') '  NEW'
WRITE(ifgraph,'(A)') '  x y MT'
WRITE(ifgraph,'(A)') '  tmpcr         0             RLT'
WRITE(ifgraph,'(A)') '  tmpcr mdouble 0             RLT'
WRITE(ifgraph,'(A)') '  tmpcr         0             RLT'
WRITE(ifgraph,'(A)') '  0             tmpcr         RLT'
WRITE(ifgraph,'(A)') '  0             tmpcr mdouble RLT'
WRITE(ifgraph,'(A)') '  closepath'
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% ========== Slant Cross Function =========='
END IF
WRITE(ifgraph,'(A)') '/ScSize { 1.00 } def'
WRITE(ifgraph,'(A)') '/Sc {'
WRITE(ifgraph,'(A)') '  3 dict begin'
WRITE(ifgraph,'(A)') '  /y exch def'
WRITE(ifgraph,'(A)') '  /x exch def'
WRITE(ifgraph,'(A)') '  /tmpscr { ScSize pnts mul } def'
WRITE(ifgraph,'(A)') '  NEW'
WRITE(ifgraph,'(A)') '  x y MT'
WRITE(ifgraph,'(A)') '  tmpscr         dup       RLT'
WRITE(ifgraph,'(A)') '  tmpscr mdouble dup       RLT'
WRITE(ifgraph,'(A)') '  tmpscr         dup       RLT'
WRITE(ifgraph,'(A)') '  tmpscr         dup minus RLT'
WRITE(ifgraph,'(A)') '  tmpscr mdouble dup minus RLT'
WRITE(ifgraph,'(A)') '  closepath'
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% ========== Asterisk Function =========='
END IF
WRITE(ifgraph,'(A)') '/AsSize { 1.40 } def'
WRITE(ifgraph,'(A)') '/AsRatio { 0.80 } def' ! 0.866=sqrt(3)/2
WRITE(ifgraph,'(A)') '/As {'
WRITE(ifgraph,'(A)') '  4 dict begin'
WRITE(ifgraph,'(A)') '  /y exch def'
WRITE(ifgraph,'(A)') '  /x exch def'
WRITE(ifgraph,'(A)') '  /tmpasi { AsSize pnts mul } def'
WRITE(ifgraph,'(A)') '  /astr { AsRatio tmpasi mul } def'
WRITE(ifgraph,'(A)') '  NEW'
WRITE(ifgraph,'(A)') '  x y MT'
WRITE(ifgraph,'(A)') '  tmpasi         0            RLT'
WRITE(ifgraph,'(A)') '  tmpasi mdouble 0            RLT'
WRITE(ifgraph,'(A)') '  tmpasi         0            RLT'
WRITE(ifgraph,'(A)') '  tmpasi half    astr         RLT'
WRITE(ifgraph,'(A)') '  tmpasi minus   astr mdouble RLT'
WRITE(ifgraph,'(A)') '  tmpasi half    astr         RLT'
WRITE(ifgraph,'(A)') '  tmpasi half    astr minus   RLT'
WRITE(ifgraph,'(A)') '  tmpasi minus   astr double  RLT'
WRITE(ifgraph,'(A)') '  tmpasi half    astr minus   RLT'
WRITE(ifgraph,'(A)') '  closepath'
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% ======= Vertical Hex Function ========'
END IF
WRITE(ifgraph,'(A)') '/VhSize { 1.30 } def'
WRITE(ifgraph,'(A)') '/Vh {'
WRITE(ifgraph,'(A)') '  4 dict begin'
WRITE(ifgraph,'(A)') '  /y exch def'
WRITE(ifgraph,'(A)') '  /x exch def'
WRITE(ifgraph,'(A)') '  /tmpvhy VhSize pnts mul def'
WRITE(ifgraph,'(A)') '  /tmpvhx tmpvhy 2 div 3 sqrt mul def'
WRITE(ifgraph,'(A)') '  NEW'
WRITE(ifgraph,'(A)') '  x y tmpvhy sub MT'
WRITE(ifgraph,'(A)') '  tmpvhx minus tmpvhy half       RLT'
WRITE(ifgraph,'(A)') '  0            tmpvhy            RLT'
WRITE(ifgraph,'(A)') '  tmpvhx       tmpvhy half       RLT'
WRITE(ifgraph,'(A)') '  tmpvhx       tmpvhy half minus RLT'
WRITE(ifgraph,'(A)') '  0            tmpvhy      minus RLT'
WRITE(ifgraph,'(A)') '  closepath'
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% ===== Horizontal Hex Function ========'
END IF
WRITE(ifgraph,'(A)') '/Hh {'
WRITE(ifgraph,'(A)') '  2 dict begin'
WRITE(ifgraph,'(A)') '  /y exch def'
WRITE(ifgraph,'(A)') '  /x exch def'
WRITE(ifgraph,'(A)') '  x y translate'
WRITE(ifgraph,'(A)') '  90 rotate'
WRITE(ifgraph,'(A)') '  0 0 Vh'
WRITE(ifgraph,'(A)') '  -90 rotate'
WRITE(ifgraph,'(A)') '  x minus y minus translate'
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% ========== The Witness Astarisk Function =========='
END IF
WRITE(ifgraph,'(A)') '/WaSize { 2.80 } def'
WRITE(ifgraph,'(A)') '/Wa {'
WRITE(ifgraph,'(A)') '  4 dict begin'
WRITE(ifgraph,'(A)') '  /y exch def'
WRITE(ifgraph,'(A)') '  /x exch def'
WRITE(ifgraph,'(A)') '  /t1 { 2 2 sqrt sub 4 div pnts mul WaSize mul } def'
WRITE(ifgraph,'(A)') '  /t2 { 1 2 sqrt 2 mul div pnts mul WaSize mul } def'
WRITE(ifgraph,'(A)') '  /t3 { 1 2 div pnts mul WaSize mul } def'
WRITE(ifgraph,'(A)') '  NEW'
WRITE(ifgraph,'(A)') '  x y t3 add MT'
WRITE(ifgraph,'(A)') '  x t1 add y t2 add LT'
WRITE(ifgraph,'(A)') '  x t2 add y t2 add LT'
WRITE(ifgraph,'(A)') '  x t2 add y t1 add LT'
WRITE(ifgraph,'(A)') '  x t3 add y LT'
WRITE(ifgraph,'(A)') '  x t2 add y t1 sub LT'
WRITE(ifgraph,'(A)') '  x t2 add y t2 sub LT'
WRITE(ifgraph,'(A)') '  x t1 add y t2 sub LT'
WRITE(ifgraph,'(A)') '  x y t3 sub LT'
WRITE(ifgraph,'(A)') '  x t1 sub y t2 sub LT'
WRITE(ifgraph,'(A)') '  x t2 sub y t2 sub LT'
WRITE(ifgraph,'(A)') '  x t2 sub y t1 sub LT'
WRITE(ifgraph,'(A)') '  x t3 sub y LT'
WRITE(ifgraph,'(A)') '  x t2 sub y t1 add LT'
WRITE(ifgraph,'(A)') '  x t2 sub y t2 add LT'
WRITE(ifgraph,'(A)') '  x t1 sub y t2 add LT'
WRITE(ifgraph,'(A)') '  closepath'
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
! definition of box shape
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% ========== Color Box Function =========='
END IF
WRITE(ifgraph,'(A)') '/BX {'
WRITE(ifgraph,'(A)') '  8 dict begin'
WRITE(ifgraph,'(A)') '  /coliii exch def'
WRITE(ifgraph,'(A)') '  /colii  exch def'
WRITE(ifgraph,'(A)') '  /coli   exch def'
WRITE(ifgraph,'(A)') '  /idoy exch def'
WRITE(ifgraph,'(A)') '  1000 div szx mul bxmg sub /xwidth exch def'
WRITE(ifgraph,'(A)') '  /xmin exch def'
WRITE(ifgraph,'(A)') '  /ifli exch def'
WRITE(ifgraph,'(A)') '  /ymin { cty szy sub hdy sub dy idoy mul add hywidth sub } def'
WRITE(ifgraph,'(A)') '  ifli xmin TransFormx hbxmg add /xmin exch def'
WRITE(ifgraph,'(A)') '  coli colii coliii RGB'
WRITE(ifgraph,'(A)') '  NEW'
WRITE(ifgraph,'(A)') '  xmin ymin MT'
WRITE(ifgraph,'(A)') '  0      ywidth       RLT'
WRITE(ifgraph,'(A)') '  xwidth 0            RLT'
WRITE(ifgraph,'(A)') '  0      ywidth minus RLT'
WRITE(ifgraph,'(A)') '  end fill'
WRITE(ifgraph,'(A)') '} def'
! definition of color bar box shape
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% ========== ColorBar Box Function =========='
END IF
WRITE(ifgraph,'(A)') '/BB {'
WRITE(ifgraph,'(A)') '  4 dict begin'
WRITE(ifgraph,'(A)') '  /idoy exch def'
WRITE(ifgraph,'(A)') '  /value idoy 0.5 add clbn div def'
WRITE(ifgraph,'(A)') '  szy double clbn div'
WRITE(ifgraph,'(A)') '  dup idoy mul cty add szy sub /tempmin exch def'
WRITE(ifgraph,'(A)') '  szy double clbn div'
WRITE(ifgraph,'(A)') '  tempmin add /tempmax exch def'
WRITE(ifgraph,'(A)') '  value MAKECOLOR RGB'
WRITE(ifgraph,'(A)') '  NEW'
WRITE(ifgraph,'(A)') '  ctx szx add clmg add tempmin MT'
WRITE(ifgraph,'(A)') '  clbx 0    RLT'
WRITE(ifgraph,'(A)') '  0    tempmax tempmin sub 1 add RLT'
! あえて少しだけ(1だけ)大き目に描画することで隙間ができるのを防ぐ
WRITE(ifgraph,'(A)') '  clbx minus 0    RLT'
WRITE(ifgraph,'(A)') '  closepath'
WRITE(ifgraph,'(A)') '  end fill'
WRITE(ifgraph,'(A)') '} def'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '%}}'//'}'
END IF
!%}}}
! Border Function %{{{
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% Border Functions %{{'//'{'
END IF
WRITE(ifgraph,'(A)') '/BordBottom {'
WRITE(ifgraph,'(A)') '  dup fxmin cty szy sub MT'
WRITE(ifgraph,'(A)') '      fxmax cty szy sub LT ST'
WRITE(ifgraph,'(A)') '} def'
WRITE(ifgraph,'(A)') '/BordLeft {'
WRITE(ifgraph,'(A)') '  fxmin cty szy sub MT'
WRITE(ifgraph,'(A)') '  0 szy double RLT ST'
WRITE(ifgraph,'(A)') '} def'
WRITE(ifgraph,'(A)') '/BordTop {'
WRITE(ifgraph,'(A)') '  dup fxmin cty szy add MT'
WRITE(ifgraph,'(A)') '      fxmax cty szy add LT ST'
WRITE(ifgraph,'(A)') '} def'
WRITE(ifgraph,'(A)') '/BordRight {'
WRITE(ifgraph,'(A)') '  fxmax cty szy sub MT'
WRITE(ifgraph,'(A)') '  0 szy double RLT ST'
WRITE(ifgraph,'(A)') '} def'
! definition of bar border
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% Colorbar Border Functions'
END IF
WRITE(ifgraph,'(A)') '/BarBordBottom {'
WRITE(ifgraph,'(A)') '  ctx szx add clmg add dup cty szy sub MT'
WRITE(ifgraph,'(A)') '  clbx 0 RLT ST'
WRITE(ifgraph,'(A)') '} def'
WRITE(ifgraph,'(A)') '/BarBordLeft {'
WRITE(ifgraph,'(A)') '  ctx szx add clmg add cty szy sub MT'
WRITE(ifgraph,'(A)') '  0 szy double RLT ST'
WRITE(ifgraph,'(A)') '} def'
WRITE(ifgraph,'(A)') '/BarBordTop {'
WRITE(ifgraph,'(A)') '  ctx szx add clmg add dup cty szy add MT'
WRITE(ifgraph,'(A)') '  clbx 0 RLT ST'
WRITE(ifgraph,'(A)') '} def'
WRITE(ifgraph,'(A)') '/BarBordRight {'
WRITE(ifgraph,'(A)') '  ctx szx add clmg add clbx add cty szy sub MT'
WRITE(ifgraph,'(A)') '  0 szy double RLT ST'
WRITE(ifgraph,'(A)') '} def'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '%}}'//'}'
END IF
!%}}}
! Grid Function %{{{
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% Grid Functions %{{'//'{'
END IF
! definition of xgrid
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% ========== xgrid Function =========='
END IF
WRITE(ifgraph,'(A)') '/XG {'
WRITE(ifgraph,'(A)') '  3 dict begin'
WRITE(ifgraph,'(A)') '  /idox exch def'
WRITE(ifgraph,'(A)') '  /ifli exch def'
WRITE(ifgraph,'(A)') '  /x { ifli fxmin idox ifli dx mul add } def'
WRITE(ifgraph,'(A)') '  NEW'
WRITE(ifgraph,'(A)') '  x cty szy sub MT'
WRITE(ifgraph,'(A)') '  x cty szy add LT'
WRITE(ifgraph,'(A)') '  end ST'
WRITE(ifgraph,'(A)') '} def'
! definition of ygrid
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% ========== ygrid Function =========='
END IF
WRITE(ifgraph,'(A)') '/YG {'
WRITE(ifgraph,'(A)') '  2 dict begin'
WRITE(ifgraph,'(A)') '  /idoy exch def'
WRITE(ifgraph,'(A)') '  /y { cty szy sub idoy dy mul add } def'
WRITE(ifgraph,'(A)') '  NEW'
WRITE(ifgraph,'(A)') '  ctx szx sub y MT'
WRITE(ifgraph,'(A)') '  ctx szx add y LT'
WRITE(ifgraph,'(A)') '  end ST'
WRITE(ifgraph,'(A)') '} def'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '%}}'//'}'
END IF
!%}}}
! Label Function %{{{
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% Label Functions %{{'//'{'
END IF
! definition of xtics label
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% ========== xtics label Function =========='
END IF
WRITE(ifgraph,'(A)') '/XTL {'
WRITE(ifgraph,'(A)') '  3 dict begin'
WRITE(ifgraph,'(A)') '  gsave'
WRITE(ifgraph,'(A)') '  /str exch def'
WRITE(ifgraph,'(A)') '  /idox exch def'
WRITE(ifgraph,'(A)') '  /ifli exch def'
WRITE(ifgraph,'(A)') '  ifli fxmin ifli dx idox mul add'
WRITE(ifgraph,'(A)') '  cty szy sub tcmg sub'
IF( rotxtlabs )THEN
  WRITE(ifgraph,'(A)') '  translate -90 rotate'
  WRITE(ifgraph,'(A)') '  0 0 str strLC'
ELSE
  WRITE(ifgraph,'(A)') '  str strCB'
END IF
WRITE(ifgraph,'(A)') '  grestore'
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
! definition of ytics label
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% ========== ytics label Function =========='
END IF
WRITE(ifgraph,'(A)') '/YTL {'
WRITE(ifgraph,'(A)') '  2 dict begin'
WRITE(ifgraph,'(A)') '  gsave'
WRITE(ifgraph,'(A)') '  /str exch def'
WRITE(ifgraph,'(A)') '  /idoy exch def'
WRITE(ifgraph,'(A)') '  ctx szx sub tcmg sub'
WRITE(ifgraph,'(A)') '  cty szy sub idoy dy mul add'
IF(p_mode == 2)THEN
  WRITE(ifgraph,'(A)') '  hdy sub'
END IF
IF( rotytlabs )THEN
  WRITE(ifgraph,'(A)') '  translate 90 rotate'
  WRITE(ifgraph,'(A)') '  0 0 str strCT'
ELSE
  WRITE(ifgraph,'(A)') '  str strRC'
END IF
WRITE(ifgraph,'(A)') '  grestore'
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
! definition of bartics label
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% ========== bartics label Function =========='
END IF
WRITE(ifgraph,'(A)') '/BTL {'
WRITE(ifgraph,'(A)') '  2 dict begin'
WRITE(ifgraph,'(A)') '  /str exch def'
WRITE(ifgraph,'(A)') '  /idoy exch def'
WRITE(ifgraph,'(A)') '  ctx szx add clmg add clbx add tcmg add'
WRITE(ifgraph,'(A)') '  cty szy sub idoy da mul add'
WRITE(ifgraph,'(A)') '  str strLC'
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '%}}'//'}'
END IF
!%}}}
! Tics Function %{{{
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% Tics Functions %{{'//'{'
END IF
! definition of xtics
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% ========== xtics Function =========='
END IF
WRITE(ifgraph,'(A)') '/xticsbase {'
WRITE(ifgraph,'(A)') '  5 dict begin'
WRITE(ifgraph,'(A)') '  /length exch def'
WRITE(ifgraph,'(A)') '  /xtmp exch def'
WRITE(ifgraph,'(A)') '  /ifli exch def'
WRITE(ifgraph,'(A)') '  /idox exch def'
WRITE(ifgraph,'(A)') '  /x { ifli fxmin idox xtmp mul add} def'
WRITE(ifgraph,'(A)') '  NEW'
WRITE(ifgraph,'(A)') '  x cty szy sub MT'
WRITE(ifgraph,'(A)') '  0 length RLT'
WRITE(ifgraph,'(A)') '  ST'
WRITE(ifgraph,'(A)') '  x cty szy add MT'
WRITE(ifgraph,'(A)') '  0 0 length sub RLT'
WRITE(ifgraph,'(A)') '  ST'
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
WRITE(ifgraph,'(A)') '/XT { exch dup dx tcll xticsbase } def'
WRITE(ifgraph,'(A)') '/MXT {'
WRITE(ifgraph,'(A)') '  exch dup dx mtcll xticsbase'
WRITE(ifgraph,'(A)') '} def'
! definition of ytics
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% ========== ytics Function =========='
END IF
WRITE(ifgraph,'(A)') '/yticsbase {'
WRITE(ifgraph,'(A)') '  4 dict begin'
WRITE(ifgraph,'(A)') '  /length exch def'
WRITE(ifgraph,'(A)') '  /ytmp exch def'
WRITE(ifgraph,'(A)') '  /idoy exch def'
IF((p_mode == 1).OR.(p_mode == 3).OR.(p_mode==6))THEN
  WRITE(ifgraph,'(A)') '  /y { cty szy sub idoy ytmp mul add } def'
ELSE IF(p_mode == 2)THEN
  WRITE(ifgraph,'(A)') '  /y { cty szy sub hdy sub idoy ytmp mul add } def'
END IF
WRITE(ifgraph,'(A)') '  NEW'
WRITE(ifgraph,'(A)') '  ctx szx sub y MT'
WRITE(ifgraph,'(A)') '  length 0 RLT'
WRITE(ifgraph,'(A)') '  ST'
WRITE(ifgraph,'(A)') '  ctx szx add y MT'
WRITE(ifgraph,'(A)') '  0 length sub 0 RLT'
WRITE(ifgraph,'(A)') '  end ST'
WRITE(ifgraph,'(A)') '} def'
!
WRITE(ifgraph,'(A)') '/aticsbase {'
WRITE(ifgraph,'(A)') '  4 dict begin'
WRITE(ifgraph,'(A)') '  /length exch def'
WRITE(ifgraph,'(A)') '  /ytmp exch def'
WRITE(ifgraph,'(A)') '  /idoy exch def'
WRITE(ifgraph,'(A)') '  /y { cty szy sub idoy ytmp mul add } def'
WRITE(ifgraph,'(A)') '  NEW'
WRITE(ifgraph,'(A)') '  ctx szx add clmg add y MT'
WRITE(ifgraph,'(A)') '  length 0 RLT'
WRITE(ifgraph,'(A)') '  ctx szx add clmg add clbx add y MT'
WRITE(ifgraph,'(A)') '  0 length sub 0 RLT'
WRITE(ifgraph,'(A)') '  end ST'
WRITE(ifgraph,'(A)') '} def'
!
WRITE(ifgraph,'(A)') '/YT { dy tcll yticsbase } def'
WRITE(ifgraph,'(A)') '/AT { da ctll aticsbase } def'
WRITE(ifgraph,'(A)') '/MYT { dy mtcll yticsbase } def'
WRITE(ifgraph,'(A)') '/MAT { da cmtll aticsbase } def'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '%}}'//'}'
END IF
!%}}}
! Key Function %{{{
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% Key Functions %{{'//'{'
END IF
! definition of xtics
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% ========== key Function =========='
END IF
WRITE(ifgraph,'(A)') '/KEY {'
WRITE(ifgraph,'(A)') '  4 dict begin'
WRITE(ifgraph,'(A)') '  /str exch def'
WRITE(ifgraph,'(A)') '  /idox exch def'
WRITE(ifgraph,'(A)') '  /basex ctx szx add kymg add def'
WRITE(ifgraph,'(A)') '  /basey cty szy add kyfs idox mul sub def'
IF( stcl == 0 )THEN
  WRITE(ifgraph,'(A)') '  idox STYLECOLORS RGB'
ELSE
  WRITE(ifgraph,'(A,I3,A)') '  ',stcl,' STYLECOLORS RGB'
END IF
IF( stlt == 0 )THEN
  WRITE(ifgraph,'(A)') '  idox STYLELINES'
ELSE
  WRITE(ifgraph,'(A,I3,A)') '  ',stlt,' STYLELINES'
END IF
WRITE(ifgraph,'(A)') '  linw LW'
WRITE(ifgraph,'(A)') '  NEW'
WRITE(ifgraph,'(A)') '  basex basey moveto kyll 0 rlineto stroke'
WRITE(ifgraph,'(A)') '  NORMALLINES'
IF( stpt == 0 )THEN
  WRITE(ifgraph,'(A)') '  idox STYLEPOINTS'
ELSE
  WRITE(ifgraph,'(A,I3,A)') '  ',stpt,' STYLEPOINTS'
END IF
WRITE(ifgraph,'(A)') '  ptlw LW'
WRITE(ifgraph,'(A)') '  gsave'
WRITE(ifgraph,'(A)') '  bakc RGB'
WRITE(ifgraph,'(A)') '  basex kyll 2 div add basey Plot2'
WRITE(ifgraph,'(A)') '  grestore'
WRITE(ifgraph,'(A)') '  basex kyll 2 div add basey Plot'
WRITE(ifgraph,'(A)') '  basex kyll add basey str strLC'
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '%}}'//'}'
END IF
!%}}}
! Transform Function %{{{
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% Transform Functions %{{'//'{'
END IF
WRITE(ifgraph,'(A)') '/TransFormx {'
WRITE(ifgraph,'(A)') '  2 dict begin'
WRITE(ifgraph,'(A)') '  /val exch def'
WRITE(ifgraph,'(A)') '  /ifli exch def'
WRITE(ifgraph,'(A)') '  ifli fxmax ifli fxmin add 2 div'
WRITE(ifgraph,'(A)') '  ifli fxmax ifli fxmin sub 2 div'
WRITE(ifgraph,'(A)') '  val 1000 div mul add'
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
WRITE(ifgraph,'(A)') '/TransFormy { 1000 div szy mul cty add } def'
WRITE(ifgraph,'(A)') '/TransForm2 {'
WRITE(ifgraph,'(A)') '  1 dict begin'
WRITE(ifgraph,'(A)') '  TransFormy /tmpy exch def'
WRITE(ifgraph,'(A)') '  TransFormx tmpy'
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '  } def'
WRITE(ifgraph,'(A)') '/TRM { TransForm2 MT } def'
WRITE(ifgraph,'(A)') '/TRL { TransForm2 LT } def'
WRITE(ifgraph,'(A)') '/TRP { TransForm2 Plot } def'
WRITE(ifgraph,'(A)') '/TRP2 { TransForm2 Plot2 } def'
WRITE(ifgraph,'(A)') '/M {'
WRITE(ifgraph,'(A)') '  2 dict begin'
WRITE(ifgraph,'(A)') '  /y exch def'
WRITE(ifgraph,'(A)') '  /x exch def'
WRITE(ifgraph,'(A)') '  ifile x y TRM'
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
WRITE(ifgraph,'(A)') '/L {'
WRITE(ifgraph,'(A)') '  2 dict begin'
WRITE(ifgraph,'(A)') '  /y exch def'
WRITE(ifgraph,'(A)') '  /x exch def'
WRITE(ifgraph,'(A)') '  ifile x y TRL'
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
WRITE(ifgraph,'(A)') '/BOND {'
WRITE(ifgraph,'(A)') '  4 dict begin'
WRITE(ifgraph,'(A)') '  /yb exch def'
WRITE(ifgraph,'(A)') '  /xb exch def'
WRITE(ifgraph,'(A)') '  /ya exch def'
WRITE(ifgraph,'(A)') '  /xa exch def'
WRITE(ifgraph,'(A)') '  ifile xa ya M ifile xb yb L'
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '%}}'//'}'
END IF
!%}}}
! Color Decision Function %{{{
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% Color Decision Function %{{'//'{'
END IF
WRITE(ifgraph,'(A)' ) '/ColorMerge {'
WRITE(ifgraph,'(A)' ) '  7 dict begin'
WRITE(ifgraph,'(A)' ) '  /ratio exch def'
WRITE(ifgraph,'(A)' ) '  /b2 exch def'
WRITE(ifgraph,'(A)' ) '  /g2 exch def'
WRITE(ifgraph,'(A)' ) '  /r2 exch def'
WRITE(ifgraph,'(A)' ) '  /b1 exch def'
WRITE(ifgraph,'(A)' ) '  /g1 exch def'
WRITE(ifgraph,'(A)' ) '  /r1 exch def'
WRITE(ifgraph,'(A)' ) '  r1 ratio mul r2 1 ratio sub mul add'
WRITE(ifgraph,'(A)' ) '  g1 ratio mul g2 1 ratio sub mul add'
WRITE(ifgraph,'(A)' ) '  b1 ratio mul b2 1 ratio sub mul add'
WRITE(ifgraph,'(A)' ) '  end'
WRITE(ifgraph,'(A)' ) '} def'
WRITE(ifgraph,'(A)' ) '/SmoothColorMergePostFlat {'
WRITE(ifgraph,'(A)' ) '  % flat near post-data'
WRITE(ifgraph,'(A)' ) '  7 dict begin'
WRITE(ifgraph,'(A)' ) '  90 mul sin /ratio exch def'
WRITE(ifgraph,'(A)' ) '  /b2 exch def'
WRITE(ifgraph,'(A)' ) '  /g2 exch def'
WRITE(ifgraph,'(A)' ) '  /r2 exch def'
WRITE(ifgraph,'(A)' ) '  /b1 exch def'
WRITE(ifgraph,'(A)' ) '  /g1 exch def'
WRITE(ifgraph,'(A)' ) '  /r1 exch def'
WRITE(ifgraph,'(A)' ) '  r1 ratio mul r2 1 ratio sub mul add'
WRITE(ifgraph,'(A)' ) '  g1 ratio mul g2 1 ratio sub mul add'
WRITE(ifgraph,'(A)' ) '  b1 ratio mul b2 1 ratio sub mul add'
WRITE(ifgraph,'(A)' ) '  end'
WRITE(ifgraph,'(A)' ) '} def'
WRITE(ifgraph,'(A)' ) '/SmoothColorMerge {'
WRITE(ifgraph,'(A)' ) '  7 dict begin'
WRITE(ifgraph,'(A)' ) '  180 mul 90 sub sin 2 div 0.5 add /ratio exch def'
WRITE(ifgraph,'(A)' ) '  /b2 exch def'
WRITE(ifgraph,'(A)' ) '  /g2 exch def'
WRITE(ifgraph,'(A)' ) '  /r2 exch def'
WRITE(ifgraph,'(A)' ) '  /b1 exch def'
WRITE(ifgraph,'(A)' ) '  /g1 exch def'
WRITE(ifgraph,'(A)' ) '  /r1 exch def'
WRITE(ifgraph,'(A)' ) '  r1 ratio mul r2 1 ratio sub mul add'
WRITE(ifgraph,'(A)' ) '  g1 ratio mul g2 1 ratio sub mul add'
WRITE(ifgraph,'(A)' ) '  b1 ratio mul b2 1 ratio sub mul add'
WRITE(ifgraph,'(A)' ) '  end'
WRITE(ifgraph,'(A)' ) '} def'
WRITE(ifgraph,'(A)') '/MAKECOLOR {'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '  % input: 0~1'
  WRITE(ifgraph,'(A)') '  % output: 1~clnum'
END IF
WRITE(ifgraph,'(A)') '  13 dict begin'
WRITE(ifgraph,'(A)') '  /val exch def'
WRITE(ifgraph,'(A)') '  val 0 lt { 0 0 0 }{'
WRITE(ifgraph,'(A)') '    val 1 gt { 1 1 1 }{'
WRITE(ifgraph,'(A)') '      val clnum 1 sub mul 1 add /val exch def'
WRITE(ifgraph,'(A)') '      val floor       cvi STYLECOLORS /cminb exch def /cming exch def /cminr exch def'
WRITE(ifgraph,'(A)') '      val floor 1 add cvi STYLECOLORS /cmaxb exch def /cmaxg exch def /cmaxr exch def'
WRITE(ifgraph,'(A)') '      val val floor sub /rest exch def'
WRITE(ifgraph,'(A)') '      /ratio1 1 rest sub def'
WRITE(ifgraph,'(A)') '      /ratio2 rest def'
WRITE(ifgraph,'(A)') '      cminr cming cminb cmaxr cmaxg cmaxb ratio1 ColorMerge'
WRITE(ifgraph,'(A)') '    } ifelse'
WRITE(ifgraph,'(A)') '  } ifelse'
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '%}}'//'}'
END IF
!%}}}
! Size Decision Function %{{{
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% Size Decision Function %{{'//'{'
END IF
WRITE(ifgraph,'(A)' ) '/SizeMerge {'
WRITE(ifgraph,'(A)' ) '  3 dict begin'
WRITE(ifgraph,'(A)' ) '  /ratio exch def'
WRITE(ifgraph,'(A)' ) '  /r2 exch def'
WRITE(ifgraph,'(A)' ) '  /r1 exch def'
WRITE(ifgraph,'(A)' ) '  r1 ratio mul r2 1 ratio sub mul add'
WRITE(ifgraph,'(A)' ) '  end'
WRITE(ifgraph,'(A)' ) '} def'
WRITE(ifgraph,'(A)' ) '/SmoothSizeMergePostFlat {'
WRITE(ifgraph,'(A)' ) '  % flat near post-data'
WRITE(ifgraph,'(A)' ) '  3 dict begin'
WRITE(ifgraph,'(A)' ) '  90 mul sin /ratio exch def'
WRITE(ifgraph,'(A)' ) '  /r2 exch def'
WRITE(ifgraph,'(A)' ) '  /r1 exch def'
WRITE(ifgraph,'(A)' ) '  r1 ratio mul r2 1 ratio sub mul add'
WRITE(ifgraph,'(A)' ) '  end'
WRITE(ifgraph,'(A)' ) '} def'
WRITE(ifgraph,'(A)' ) '/SmoothSizeMerge {'
WRITE(ifgraph,'(A)' ) '  3 dict begin'
WRITE(ifgraph,'(A)' ) '  180 mul 90 sub sin 2 div 0.5 add /ratio exch def'
WRITE(ifgraph,'(A)' ) '  /r2 exch def'
WRITE(ifgraph,'(A)' ) '  /r1 exch def'
WRITE(ifgraph,'(A)' ) '  r1 ratio mul r2 1 ratio sub mul add'
WRITE(ifgraph,'(A)' ) '  end'
WRITE(ifgraph,'(A)' ) '} def'
WRITE(ifgraph,'(A)') '/MAKESIZE {'
IF( sznum > 0 )THEN
  IF( morecomment )THEN
    WRITE(ifgraph,'(A)') '  % input: 0~1'
    WRITE(ifgraph,'(A)') '  % output: 1~sznum'
  END IF
  WRITE(ifgraph,'(A)') '  13 dict begin'
  WRITE(ifgraph,'(A)') '  /val exch def'
  WRITE(ifgraph,'(A)') '  val 0 lt { 0 }{'
  WRITE(ifgraph,'(A)') '    val 1 gt { 1 }{'
  WRITE(ifgraph,'(A)') '      val sznum 1 sub mul 1 add /val exch def'
  WRITE(ifgraph,'(A)') '      val floor       cvi STYLESIZE /cminr exch def'
  WRITE(ifgraph,'(A)') '      val floor 1 add cvi STYLESIZE /cmaxr exch def'
  WRITE(ifgraph,'(A)') '      val val floor sub /rest exch def'
  WRITE(ifgraph,'(A)') '      /ratio1 1 rest sub def'
  WRITE(ifgraph,'(A)') '      /ratio2 rest def'
  WRITE(ifgraph,'(A)') '      cminr cmaxr ratio1 SizeMerge'
  WRITE(ifgraph,'(A)') '    } ifelse'
  WRITE(ifgraph,'(A)') '  } ifelse'
  WRITE(ifgraph,'(A)') '  end'
ELSE
  WRITE(ifgraph,'(A)') '  pop pnts'
END IF
WRITE(ifgraph,'(A)') '} def'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '%}}'//'}'
END IF
!%}}}
WRITE(ifgraph,'(A)') 'end'
WRITE(ifgraph,'(A)') '%%EndProlog'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '%'
END IF
RETURN
END SUBROUTINE
!%}}}
! SUB kplot_eps_draw(debugmode,fnum,ifgraph) %{{{
SUBROUTINE kplot_eps_draw(debugmode,fnum,ifgraph)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( IN ) :: fnum
INTEGER, INTENT( IN ) :: ifgraph
INTEGER :: ifli ! integer for files
WRITE(ifgraph,'(A)') '%%Page: 1'
WRITE(ifgraph,'(A)') '%%BeginPageSetup'
WRITE(ifgraph,'(A)') '/kplsave save def'
WRITE(ifgraph,'(A)') 'gsave'
WRITE(ifgraph,'(A)') 'clipsave'
WRITE(ifgraph,'(A)') 'kpldict begin'
WRITE(ifgraph,'(A)') '%%EndPageSetup'
! [debuger] %{{{
!WRITE(ifgraph,'(A)') 'end grestore'
!WRITE(ifgraph,'(A)') 'showpage'
!CLOSE(ifgraph)
!RETURN
!%}}}

! postscript contents
CALL kplot_eps_background(debugmode,ifgraph)
!CALL kplot_eps_border(debugmode,fnum,ifgraph)
CALL kplot_eps_gridline(debugmode,fnum,ifgraph)
CALL kplot_eps_vbarline(debugmode,fnum,ifgraph)
CALL kplot_eps_ticsline(debugmode,fnum,ifgraph)
CaLL kplot_eps_ticslabel(debugmode,fnum,ifgraph)
CALL kplot_eps_axislabel(debugmode,ifgraph)
CALL kplot_eps_title(debugmode,ifgraph)
! [debuger] %{{{
!WRITE(ifgraph,'(A)') 'end grestore showpage'
!CLOSE(ifgraph)
!RETURN
!%}}}
CALL kplot_eps_colorbar(debugmode,ifgraph)
!clip
WRITE(ifgraph,'(A)') 'graphregion clip'
CALL kplot_eps_boxspectrum(debugmode,fnum,ifgraph)
! backward cube border %{{{
IF( debugmode == 1 ) WRITE (*,*) 'Begin: backward cube border'
IF((showcube).AND.(p_mode == 4.OR.p_mode==5))THEN
  ifli = 1
  WRITE(ifgraph,'(A)') 'gsave'
  WRITE(ifgraph,'(A)') '/ifile '//TRIM(hint(ifli))//' def'
  CALL cube_border(ifgraph,cubenode(:,:),angth,angph,.TRUE.)
  WRITE(ifgraph,'(A)') 'grestore'
END IF
!%}}}
CALL kplot_eps_face(debugmode,fnum,ifgraph)
CALL kplot_eps_bond(debugmode,fnum,ifgraph)
CALL kplot_eps_line(debugmode,fnum,ifgraph)
CALL kplot_eps_point(debugmode,fnum,ifgraph)
CALL kplot_eps_hiddenlinepoint(debugmode,fnum,ifgraph)
! forward cube border %{{{
IF( debugmode == 1 ) WRITE (*,*) 'Begin: forward cube border'
IF((showcube).AND.(p_mode == 4.OR.p_mode==5))THEN
  ifli = 1
  WRITE(ifgraph,'(A)') 'gsave'
  WRITE(ifgraph,'(A)') '/ifile '//TRIM(hint(ifli))//' def'
  CALL cube_border(ifgraph,cubenode(:,:),angth,angph,.FALSE.)
  WRITE(ifgraph,'(A)') 'grestore'
END IF
!%}}}
WRITE(ifgraph,'(A)') 'cliprestore'
CALL kplot_eps_key(debugmode,ifgraph)
CALL kplot_eps_border(debugmode,fnum,ifgraph)
! end plot
WRITE(ifgraph,'(A)') 'end'
WRITE(ifgraph,'(A)') 'grestore'
WRITE(ifgraph,'(A)') 'kplsave restore'
WRITE(ifgraph,'(A)') 'showpage'
WRITE(ifgraph,'(A)') '%%EOF'
RETURN
END SUBROUTINE
!%}}}

! SUB kplot_eps_background(debugmode,ifgraph) %{{{
SUBROUTINE kplot_eps_background(debugmode,ifgraph)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( IN ) :: ifgraph
IF( debugmode == 1 ) WRITE (*,*) 'Begin: background'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% Background %{{'//'{'
END IF
WRITE(ifgraph,'(A)') 'gsave'
! global background color
IF( morecomment )THEN
  WRITE(ifgraph,'(A)')'% global background color'
END IF
WRITE(ifgraph,'(A)') 'glbc RGB clippath fill'
! background color
IF( morecomment )THEN
  WRITE(ifgraph,'(A)')'% background color'
END IF
WRITE(ifgraph,'(A)') 'bakc RGB graphregion fill'
WRITE(ifgraph,'(A)') 'grestore'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)')  '%}}'//'}'
END IF
RETURN
END SUBROUTINE
!%}}}
! SUB kplot_eps_border(debugmode,fnum,ifgraph) %{{{
SUBROUTINE kplot_eps_border(debugmode,fnum,ifgraph)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( IN ) :: fnum
INTEGER, INTENT( IN ) :: ifgraph
INTEGER :: ifli
IF( debugmode == 1 ) WRITE (*,*) 'Begin: border'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% Border %{{'//'{'
END IF
WRITE(ifgraph,'(A)') 'gsave'
IF((brdt /= 0).AND.(bdlt /= 0))THEN
  IF( morecomment )THEN
    WRITE(ifgraph,'(A)') '% border'
  END IF
  WRITE(ifgraph,'(A)') 'bdlw LW'
  WRITE(ifgraph,'(A)') 'bdlt STYLELINES'
  WRITE(ifgraph,'(A)') '2 setlinecap'
  WRITE(ifgraph,'(A)') 'bdlc RGB'
  DO ifli = 1, fnum
    IF( morecomment )THEN
      IF(fnum>1)WRITE(ifgraph,'(A)') '% ===== file-'//TRIM(hint(ifli))
    END IF
    WRITE(ifgraph,'(A)') 'NEW'
    IF(MOD(brdt,2) >= 1)THEN
      WRITE(ifgraph,'(A)') TRIM(hint(ifli))//' BordBottom'
    END IF
    IF(MOD(brdt,4) >= 2)THEN
      WRITE(ifgraph,'(A)') TRIM(hint(ifli))//' BordLeft'
    END IF
    IF(MOD(brdt,8) >= 4)THEN
      WRITE(ifgraph,'(A)') TRIM(hint(ifli))//' BordTop'
    END IF
    IF(MOD(brdt,16) >= 8)THEN
      WRITE(ifgraph,'(A)') TRIM(hint(ifli))//' BordRight'
    END IF
  END DO
END IF
IF((p_mode == 2).OR.(p_mode == 3).OR.(p_mode==6))THEN
  IF( morecomment )THEN
    WRITE(ifgraph,'(A)') '% color-bar border'
  END IF
  WRITE(ifgraph,'(A)') 'BarBordBottom'
  WRITE(ifgraph,'(A)') 'BarBordLeft'
  WRITE(ifgraph,'(A)') 'BarBordTop'
  WRITE(ifgraph,'(A)') 'BarBordRight'
END IF
WRITE(ifgraph,'(A)') 'grestore'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)')  '%}}'//'}'
END IF
RETURN
END SUBROUTINE
!%}}}
! SUB kplot_eps_key(debugmode,ifgraph) %{{{
SUBROUTINE kplot_eps_key(debugmode,ifgraph)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( IN ) :: ifgraph
INTEGER :: idob
IF( .NOT. showkey ) RETURN
IF( debugmode == 1 ) WRITE (*,*) 'Begin: key'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% Key %{{'//'{'
END IF
WRITE(ifgraph,'(A)') 'gsave'
! key box
IF( showkeybox )THEN
  WRITE(ifgraph,'(A)') 'newpath'
  WRITE(ifgraph,'(A)') 'ctx szx add kymg add cty szy add moveto'
  WRITE(ifgraph,'(A,F6.1,A)') '0 kyfs ', -(repnum+2.0d0), ' mul rlineto'
  WRITE(ifgraph,'(A)') 'kyll kyfw 0 rlineto'
  WRITE(ifgraph,'(A,F6.1,A)') '0 kyfs ', (repnum+2.0d0), ' mul rlineto'
  WRITE(ifgraph,'(A)') 'closepath'
  WRITE(ifgraph,'(A)') 'gsave'
  WRITE(ifgraph,'(A)') '1 1 1 RGB'
  WRITE(ifgraph,'(A)') 'fill'
  WRITE(ifgraph,'(A)') 'grestore'
  WRITE(ifgraph,'(A)') '0.2 LW'
  WRITE(ifgraph,'(A)') '0 0 0 RGB'
  WRITE(ifgraph,'(A)') 'stroke'
END IF
! key
!WRITE(ifgraph,'(A)') 'kyfc RGB'
WRITE(ifgraph,'(A)') 'kyfs FS'
WRITE(ifgraph,'(A)') 'kyft STYLEFONTS'
WRITE(ifgraph,'(A)') 'kykx kyky SETKERN'
DO idob = 1, 1+repnum
  IF( idob > 100 )THEN
    ! key配列オーバーフローのため描画しない
    CONTINUE
  ELSE IF( key(idob) == '' )THEN
    WRITE(ifgraph,'(I3,A,I3,A)') idob,'(',idob,')KEY'
  ELSE
    WRITE(ifgraph,'(I3,A)') idob,'(' // TRIM(key(idob)) // ')KEY'
  END IF
END DO
WRITE(ifgraph,'(A)') 'grestore'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)')  '%}}'//'}'
END IF
RETURN
END SUBROUTINE
!%}}}
! SUB kplot_eps_title(debugmode,ifgraph) %{{{
SUBROUTINE kplot_eps_title(debugmode,ifgraph)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( IN ) :: ifgraph
IF( title == '' ) RETURN
IF( debugmode == 1 ) WRITE (*,*) 'Begin: title'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% Title %{{'//'{'
END IF
WRITE(ifgraph,'(A)') 'gsave'
WRITE(ifgraph,'(A)') 'ttfc RGB'
WRITE(ifgraph,'(A)') 'ttfs FS'
WRITE(ifgraph,'(A)') 'ttft STYLEFONTS'
WRITE(ifgraph,'(A)') 'ttkx ttky SETKERN'
! tpst; 1 > top, 0> bottom
WRITE(ifgraph,'(A)') 'tpst 1 eq { % top'
WRITE(ifgraph,'(A)') '  ctx cty szy ttmg add add title strCT'
WRITE(ifgraph,'(A)') '}{ % bottom'
WRITE(ifgraph,'(A)') '  ctx'
WRITE(ifgraph,'(A)') '  cty'
WRITE(ifgraph,'(A)') '  szy tcmg tcfs ttmg add add add'
WRITE(ifgraph,'(A)') '  sub'
WRITE(ifgraph,'(A)') '  title strCB'
WRITE(ifgraph,'(A)') '} ifelse'
WRITE(ifgraph,'(A)') 'grestore'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)')  '%}}'//'}'
END IF
RETURN
END SUBROUTINE
!%}}}
! SUB kplot_eps_axislabel(debugmode,ifgraph) %{{{
SUBROUTINE kplot_eps_axislabel(debugmode,ifgraph)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( IN ) :: ifgraph
IF( p_mode == 4 .OR. p_mode == 5 ) RETURN
IF(xlab == '' .AND. ylab == '' .AND. (p_mode /= 1 .OR. y2lb == '') ) RETURN
IF( debugmode == 1 ) WRITE (*,*) 'Begin: axis label'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% Axis Labels %{{'//'{'
END IF
WRITE(ifgraph,'(A)') 'gsave'
WRITE(ifgraph,'(A)') 'lbfs FS'
! x label
IF(xlab /= '')THEN
  IF( morecomment )THEN
    WRITE(ifgraph,'(A)') '% x label'
  END IF
  WRITE(ifgraph,'(A)') 'xlft STYLEFONTS'
  WRITE(ifgraph,'(A)') 'xlfc RGB'
  WRITE(ifgraph,'(A)') 'xlkx xlky SETKERN'
  WRITE(ifgraph,'(A)') 'ctx'
  WRITE(ifgraph,'(A)') 'cty szy sub lxmg sub'
  WRITE(ifgraph,'(A)') 'xlab strCB'
END IF
! y label
IF(ylab /= '')THEN
  IF( morecomment )THEN
    WRITE(ifgraph,'(A)') '% y label'
  END IF
  WRITE(ifgraph,'(A)') 'gsave'
  WRITE(ifgraph,'(A)') 'ylft STYLEFONTS'
  WRITE(ifgraph,'(A)') 'ylfc RGB'
  WRITE(ifgraph,'(A)') 'ylkx ylky SETKERN'
  WRITE(ifgraph,'(A)') 'ctx szx sub lymg sub'
  WRITE(ifgraph,'(A)') 'cty'
  WRITE(ifgraph,'(A)') 'translate 90 rotate'
  WRITE(ifgraph,'(A)') '0 0 ylab strCT'
  !WRITE(ifgraph,'(A)') '-90 rotate'
  !WRITE(ifgraph,'(A)') 'ctx szx sub lymg sub -1 mul'
  !WRITE(ifgraph,'(A)') 'cty -1 mul'
  !WRITE(ifgraph,'(A)') 'translate'
  WRITE(ifgraph,'(A)') 'grestore'
END IF
! y2 label
IF(y2lb /= '')THEN
  IF( morecomment )THEN
    WRITE(ifgraph,'(A)') '% y2 label'
  END IF
  WRITE(ifgraph,'(A)') 'gsave'
  WRITE(ifgraph,'(A)') 'y2lft STYLEFONTS'
  WRITE(ifgraph,'(A)') 'y2lfc RGB'
  WRITE(ifgraph,'(A)') 'y2lkx y2lky SETKERN'
  WRITE(ifgraph,'(A)') 'ctx szx add ly2mg add'
  WRITE(ifgraph,'(A)') 'cty'
  WRITE(ifgraph,'(A)') 'translate -90 rotate'
  WRITE(ifgraph,'(A)') '0 0 y2lb strCT'
  !WRITE(ifgraph,'(A)') '90 rotate'
  !WRITE(ifgraph,'(A)') 'ctx szx add ly2mg add -1 mul'
  !WRITE(ifgraph,'(A)') 'cty -1 mul'
  !WRITE(ifgraph,'(A)') 'translate'
  WRITE(ifgraph,'(A)') 'grestore'
END IF
WRITE(ifgraph,'(A)') 'grestore'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '%}}'//'}'
END IF
RETURN
END SUBROUTINE
!%}}}
! SUB kplot_eps_ticslabel(debugmode,fnum,ifgraph) %{{{
SUBROUTINE kplot_eps_ticslabel(debugmode,fnum,ifgraph)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( IN ) :: fnum
INTEGER, INTENT( IN ) :: ifgraph
INTEGER :: idoa, idob
INTEGER :: ifli
INTEGER :: itma
DOUBLE PRECISION :: rtma, rtmb
CHARACTER(LEN=500):: stma
IF( p_mode == 4 .OR. p_mode == 5 ) RETURN
IF( .NOT. ( showxtlabs .OR. showytlabs .OR. showztlabs ) ) RETURN
IF( debugmode == 1 ) WRITE (*,*) 'Begin: tics label'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% Tics Labels %{{'//'{'
END IF
WRITE(ifgraph,'(A)') 'gsave'
! xtics labels %{{{
IF(showxtlabs)THEN
  IF( morecomment )THEN
    WRITE(ifgraph,'(A)') '% xtics labels'
  END IF
  WRITE(ifgraph,'(A)') 'tcfc RGB'
  WRITE(ifgraph,'(A)') 'tcfs FS'
  WRITE(ifgraph,'(A)') 'tcft STYLEFONTS'
  WRITE(ifgraph,'(A)') 'tckx tcky SETKERN'
  DO ifli = 1, fnum
    IF( morecomment )THEN
      IF(fnum>1)WRITE(ifgraph,'(A)') '% ===== file-'//TRIM(hint(ifli))
    END IF
    rtmb = xmax(ifli)-xmin(ifli)
    DO idoa = mxdiv(ifli), xdiv(ifli), xstep(ifli)
      ! setting of output format
      rtma = xmin(ifli)+(xmax(ifli)-xmin(ifli))*DBLE(idoa)/DBLE(xdiv(ifli))
      CALL kp_print_real(rtmb,rtma,xtlf,stma,cutextrazero)
      IF( stma == '-0' ) stma = '0'
      WRITE(ifgraph,'(2I3,A)') ifli,idoa,' ('//TRIM(stma)//') XTL'
    END DO
  END DO
END IF
!%}}}
! vert bar labels %{{{
IF(vbarlab(1,1) /= '')THEN
  IF( morecomment )THEN
    WRITE(ifgraph,'(A)') '% vbar label'
  END IF
  WRITE(ifgraph,'(A)') 'tcfc RGB'
  WRITE(ifgraph,'(A)') 'tcfs FS'
  WRITE(ifgraph,'(A)') 'vbft STYLEFONTS'
  WRITE(ifgraph,'(A)') 'tckx tcky SETKERN'
  DO ifli = 1, fnum
    IF( morecomment )THEN
      IF(fnum>1)WRITE(ifgraph,'(A)') '% ===== file-'//TRIM(hint(ifli))
    END IF
    DO idoa = 1, vbarnum(ifli)
      WRITE(ifgraph,'(I3,F9.2,A)') ifli, DBLE(xdiv(ifli))*(vbar(ifli,idoa)+1.d0)/2.d0, &
      & ' (' // TRIM(vbarlab(ifli,idoa)) // ') XTL'
    END DO
  END DO
END IF
!%}}}
! ytics labels %{{{
IF(showytlabs)THEN
  IF( morecomment )THEN
    WRITE(ifgraph,'(A)') '% ytics labels'
  END IF
  WRITE(ifgraph,'(A)') 'tcfc RGB'
  WRITE(ifgraph,'(A)') 'tcfs FS'
  WRITE(ifgraph,'(A)') 'tcft STYLEFONTS'
  WRITE(ifgraph,'(A)') 'tckx tcky SETKERN'
  rtmb = ymax-ymin
  IF((p_mode == 1).OR.(p_mode == 3).OR.(p_mode==6))THEN
    DO idob = mydiv, ydiv, ystep
      ! setting of output format
      rtma = ymin+(ymax-ymin)*DBLE(idob)/DBLE(ydiv)
      CALL kp_print_real(rtmb,rtma,ytlf,stma,cutextrazero)
      IF( stma == '-0' ) stma = '0'
      WRITE(ifgraph,'(I3,A)') idob,' ('//TRIM(stma)//') YTL'
    END DO
  ELSE IF(p_mode == 2)THEN
    itma = 1+NINT((tcfs+ytcmg)/dy)
    DO idob = 1, repnum+1
      IF(MOD(idob,itma) /= 0)CYCLE
      WRITE(stma,'(I3)') idob
      stma = ADJUSTL(stma)
      IF( stma == '-0' ) stma = '0'
      WRITE(ifgraph,'(I3,A)') idob,' ('//TRIM(stma)//') YTL'
    END DO
  END IF
END IF
!%}}}
! bartics labels %{{{
IF(showztlabs)THEN
  IF( p_mode == 2 .OR. p_mode == 3 .OR. p_mode == 6 )THEN
    IF( morecomment )THEN
      WRITE(ifgraph,'(A)') '% bartics labels'
    END IF
    WRITE(ifgraph,'(A)') 'tcfc RGB'
    WRITE(ifgraph,'(A)') 'tcfs FS'
    WRITE(ifgraph,'(A)') 'tcft STYLEFONTS'
    WRITE(ifgraph,'(A)') 'tckx tcky SETKERN'
    rtmb = amax - amin
    DO idoa = madiv, adiv, astep
      rtma = amin+(amax-amin)*DBLE(idoa)/DBLE(adiv)
      CALL kp_print_real(rtmb,rtma,atlf,stma,cutextrazero)
      IF( stma == '-0' ) stma = '0'
      WRITE(ifgraph,'(I3,A)') idoa,' ('//TRIM(stma)//') BTL'
    END DO
  END IF
END IF
!%}}}
WRITE(ifgraph,'(A)') 'grestore'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '%}}'//'}'
END IF
RETURN
END SUBROUTINE
!%}}}
! SUB kplot_eps_ticsline(debugmode,fnum,ifgraph) %{{{
SUBROUTINE kplot_eps_ticsline(debugmode,fnum,ifgraph)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( IN ) :: fnum
INTEGER, INTENT( IN ) :: ifgraph
INTEGER :: idoa
INTEGER :: ifli
INTEGER :: itma
IF( .NOT. ( showxtics .OR. showytics ) ) RETURN
IF( p_mode == 4 .OR. p_mode == 5 ) RETURN
IF( debugmode == 1 ) WRITE (*,*) 'Begin: tics line'
! [TODO] ここの処理はp_mode==5の場合にも必要になるかも
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% Tics Lines %{{'//'{'
END IF
WRITE(ifgraph,'(A)') 'gsave'
! xtics, mxtics %{{{
IF(showxtics)THEN
  IF(tclt /= 0)THEN
    ! xtics
    IF( morecomment )THEN
      WRITE(ifgraph,'(A)') '% xtics'
    END IF
    WRITE(ifgraph,'(A)') 'tclw LW'
    WRITE(ifgraph,'(I3,A)') tclt,' STYLELINES'
    WRITE(ifgraph,'(A)') 'tclc RGB'
    DO ifli = 1, fnum
      IF( morecomment )THEN
        IF(fnum>1)WRITE(ifgraph,'(A)') '% ===== file-'//TRIM(hint(ifli))
      END IF
      DO idoa = mxdiv(ifli), xdiv(ifli), xstep(ifli)
        WRITE(ifgraph,'(2I3,A)') ifli,idoa,' XT'
      END DO
    END DO
  END IF
  IF(mtclt /= 0)THEN
    ! mxtics
    IF( morecomment )THEN
      WRITE(ifgraph,'(A)') '% mxtics'
    END IF
    WRITE(ifgraph,'(A)') 'mtclw LW'
    WRITE(ifgraph,'(I3,A)') mtclt,' STYLELINES'
    WRITE(ifgraph,'(A)') 'mtclc RGB'
    DO ifli = 1, fnum
      IF( morecomment )THEN
        IF(fnum > 1)WRITE(ifgraph,'(A)') '% ===== file-'//TRIM(hint(ifli))
      END IF
      DO idoa = 0, xdiv(ifli)
        IF( MOD(idoa,xstep(ifli)) == mxdiv(ifli) ) CYCLE
        WRITE(ifgraph,'(2I3,A)') ifli,idoa,' MXT'
      END DO
    END DO
  END IF
END IF
!%}}}
! ytics, mytics %{{{
! ytics
IF(showytics)THEN
  IF(tclt /= 0)THEN
    IF( morecomment )THEN
      WRITE(ifgraph,'(A)') '% ytics'
    END IF
    WRITE(ifgraph,'(A)') 'tclw LW'
    WRITE(ifgraph,'(I3,A)') tclt,' STYLELINES'
    WRITE(ifgraph,'(A)') 'tclc RGB'
    IF(p_mode == 1 .OR. p_mode == 3 .OR. p_mode == 6 )THEN
      DO idoa = mydiv, ydiv, ystep
        WRITE(ifgraph,'(I3,A)') idoa,' YT'
      END DO
    ELSE IF(p_mode == 2)THEN
      itma = 1+NINT((tcfs+ytcmg)/dy)
      DO idoa = 1,repnum+1
        IF(MOD(idoa,itma) /= 0)CYCLE
        WRITE(ifgraph,'(I3,A)') idoa,' YT'
      END DO
    END IF
    IF(p_mode == 2 .OR. p_mode == 3 .OR. p_mode == 5 .OR. p_mode==6 )THEN
      DO idoa = madiv, adiv, astep
        WRITE(ifgraph,'(I3,A)') idoa,' AT'
      END DO
    END IF
  END IF
  ! mytics
  IF(mtclt /= 0)THEN
    IF( morecomment )THEN
      WRITE(ifgraph,'(A)') '% mytics'
    END IF
    WRITE(ifgraph,'(A)') 'mtclw LW'
    WRITE(ifgraph,'(I3,A)') mtclt,' STYLELINES'
    WRITE(ifgraph,'(A)') 'mtclc RGB'
    IF(p_mode == 1 .OR. p_mode == 3 .OR. p_mode == 6 )THEN
      DO idoa = 0, ydiv
        IF( MOD(idoa,ystep) == mydiv ) CYCLE
        WRITE(ifgraph,'(I3,A)') idoa,' MYT'
      END DO
    ELSE IF(p_mode == 2)THEN
      itma = 1+NINT((tcfs+ytcmg)/dy)
      DO idoa = 1,repnum+1
        IF(MOD(idoa,itma) == 0)CYCLE
        WRITE(ifgraph,'(I3,A)') idoa,' MYT'
      END DO
    END IF
    IF(p_mode == 2 .OR. p_mode == 3 .OR. p_mode==6 )THEN
      WRITE(ifgraph,'(A)') '% matics'
      WRITE(ifgraph,'(A)') 'mtclw LW'
      WRITE(ifgraph,'(I3,A)') mtclt,' STYLELINES'
      WRITE(ifgraph,'(A)') 'mtclc RGB'
      !DO idoa = madiv, adiv, astep
      DO idoa = madiv, adiv
        IF( MOD(idoa,astep) == madiv ) CYCLE
        WRITE(ifgraph,'(I3,A)') idoa,' MAT'
      END DO
    END IF
  END IF
END IF
!%}}}
WRITE(ifgraph,'(A)') 'grestore'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '%}}'//'}'
END IF
RETURN
END SUBROUTINE
!%}}}
! SUB kplot_eps_gridline(debugmode,fnum,ifgraph) %{{{
SUBROUTINE kplot_eps_gridline(debugmode,fnum,ifgraph)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( IN ) :: fnum
INTEGER, INTENT( IN ) :: ifgraph
INTEGER :: idoa, idob
INTEGER :: ifli
IF( p_mode == 2 .OR. p_mode == 4 .OR. p_mode == 5 ) RETURN
IF( gxlt == 0 .AND. gylt == 0 ) RETURN
IF( debugmode == 1 ) WRITE (*,*) 'Begin: grid'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% Grids %{{'//'{'
END IF
WRITE(ifgraph,'(A)') 'gsave'
WRITE(ifgraph,'(A)') 'graphregion clip'
! xgrid
IF(gxlt /= 0)THEN
  IF( morecomment )THEN
    WRITE(ifgraph,'(A)') '% xgrid'
  END IF
  WRITE(ifgraph,'(I3,A)') gxlt,' STYLELINES'
  WRITE(ifgraph,'(A)') 'gxlw LW'
  WRITE(ifgraph,'(A)') 'gxlc RGB'
  DO ifli = 1, fnum
    IF(fnum>1)WRITE(ifgraph,'(A)') '% ===== file-'//TRIM(hint(ifli))
    DO idoa = 1,xdiv(ifli)-1
      WRITE(ifgraph,'(2I3,A)') ifli,idoa,' XG'
    END DO
  END DO
END IF
! ygrid
IF(gylt /= 0)THEN
  IF( morecomment )THEN
    WRITE(ifgraph,'(A)') '% ygrid'
  END IF
  WRITE(ifgraph,'(I3,A)') gylt,' STYLELINES'
  WRITE(ifgraph,'(A)') 'gylw LW'
  WRITE(ifgraph,'(A)') 'gylc RGB'
  DO idob = 1, ydiv-1
    WRITE(ifgraph,'(I3,A)') idob,' YG'
  END DO
END IF
WRITE(ifgraph,'(A)') 'cliprestore'
WRITE(ifgraph,'(A)') 'grestore'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '%}}'//'}'
END IF
RETURN
END SUBROUTINE
!%}}}
! SUB kplot_eps_vbarline(debugmode,fnum,ifgraph) %{{{
SUBROUTINE kplot_eps_vbarline(debugmode,fnum,ifgraph)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( IN ) :: fnum
INTEGER, INTENT( IN ) :: ifgraph
INTEGER :: idoa
INTEGER :: ifli
IF(vblt == 0) RETURN
IF( p_mode == 2 .OR. p_mode == 4 .OR. p_mode == 5 ) RETURN
IF( debugmode == 1 ) WRITE (*,*) 'Begin: vbar'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% Vbar %{{'//'{'
END IF
WRITE(ifgraph,'(A)') 'gsave'
WRITE(ifgraph,'(A)') 'graphregion clip'
! [TODO] fnum>1 ではgraphregionでクリップするのは正確ではない
WRITE(ifgraph,'(I3,A)') vblt, ' STYLELINES'
WRITE(ifgraph,'(A)') 'vblw LW'
WRITE(ifgraph,'(A)') 'vblc RGB'
DO ifli = 1, fnum
  IF( morecomment )THEN
    IF(fnum>1)WRITE(ifgraph,'(A)') '% ===== file-'//TRIM(hint(ifli))
  END IF
  DO idoa = 1, vbarnum(ifli)
    WRITE(ifgraph,'(I3,F8.3,A)') ifli,DBLE(xdiv(ifli))*(vbar(ifli,idoa)+1.d0)/2.d0,' XG'
  END DO
END DO
WRITE(ifgraph,'(A)') 'cliprestore'
WRITE(ifgraph,'(A)') 'grestore'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '%}}'//'}'
END IF
RETURN
END SUBROUTINE
!%}}}
! SUB kplot_eps_colorbar(debugmode,ifgraph) %{{{
SUBROUTINE kplot_eps_colorbar(debugmode,ifgraph)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( IN ) :: ifgraph
IF( .NOT. showcolorbar ) RETURN
IF( p_mode == 1 .OR. p_mode == 4 ) RETURN
IF( debugmode == 1 ) WRITE (*,*) 'Begin: color bar'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% Color Bars %{{'//'{'
END IF
WRITE(ifgraph,'(A)') 'gsave'
WRITE(ifgraph,'(A)') 'ctx szx add clmg add          bdlw 2 div add cty szy sub bdlw 2 div add MT'
WRITE(ifgraph,'(A)') 'ctx szx add clmg add clbx add bdlw 2 div sub cty szy sub bdlw 2 div add LT'
WRITE(ifgraph,'(A)') 'ctx szx add clmg add clbx add bdlw 2 div sub cty szy add bdlw 2 div sub LT'
WRITE(ifgraph,'(A)') 'ctx szx add clmg add          bdlw 2 div add cty szy add bdlw 2 div sub LT'
WRITE(ifgraph,'(A)') 'closepath'
WRITE(ifgraph,'(A)') 'clip'
WRITE(ifgraph,'(A)') '0 1 clbn 1 sub { BB } for'
WRITE(ifgraph,'(A)') 'cliprestore'
WRITE(ifgraph,'(A)') 'grestore'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '%}}'//'}'
END IF
RETURN
END SUBROUTINE
!%}}}
! SUB kplot_eps_boxspectrum(debugmode,fnum,ifgraph) %{{{
SUBROUTINE kplot_eps_boxspectrum(debugmode,fnum,ifgraph)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( IN ) :: fnum
INTEGER, INTENT( IN ) :: ifgraph
INTEGER :: idoa, idob
INTEGER :: ifli
DOUBLE PRECISION :: rtma
IF(p_mode /= 2) RETURN
IF( debugmode == 1 ) WRITE (*,*) 'Begin: plot boxspectrum'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% Plot BoxSpectrum %{{'//'{'
  WRITE(ifgraph,'(A)') '% ========== Preparation'
END IF
WRITE(ifgraph,'(A)') 'gsave'
WRITE(ifgraph,'(A)') '/B {'
WRITE(ifgraph,'(A)') '  4 dict begin'
WRITE(ifgraph,'(A)') '  /value exch def'
WRITE(ifgraph,'(A)') '  /xwidth exch def'
WRITE(ifgraph,'(A)') '  /xmin exch def'
WRITE(ifgraph,'(A)') '  ifile xmin xwidth icolumn value MAKECOLOR BX'
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% ========== Start'
END IF
IF(bymg >= dy)bymg = 0.0d0
DO ifli = 1, fnum
  IF( morecomment )THEN
    IF(fnum>1)WRITE(ifgraph,'(A)') '% ===== file-'//TRIM(hint(ifli))
  END IF
  WRITE(ifgraph,'(A)') '/ifile '//TRIM(hint(ifli))//' def'
  DO idob = 2, colmax
    WRITE(ifgraph,'(A,I8,A)') '/icolumn ',idob-1,' def'
    DO idoa = 1, rowarr(ifli)
      IF(empdata(ifli,idob,idoa))CYCLE
      IF( cutzerobox )THEN
        IF( kpldata(ifli,idob,idoa) < -0.999999d0 ) CYCLE
      END IF
      rtma = (kpldata(ifli,1,idoa)+kpldata(ifli,1,idoa+1))/2.d0
      IF( rtma < -1000.d0 .OR. rtma > 1000.d0 ) CYCLE
      WRITE(ifgraph,'(2I6,F7.3,A)') NINT(kpldata(ifli,1,idoa)), &
      & NINT(kpldata(ifli,1,idoa+1)-kpldata(ifli,1,idoa)), &
      & (kpldata(ifli,idob,idoa)+1.d0)/2.d0,' B'
    END DO
  END DO
END DO
WRITE(ifgraph,'(A)') 'grestore'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '%}}'//'}'
END IF
RETURN
END SUBROUTINE
!%}}}
! SUB kplot_eps_face(debugmode,fnum,ifgraph) %{{{
SUBROUTINE kplot_eps_face(debugmode,fnum,ifgraph)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( IN ) :: fnum
INTEGER, INTENT( IN ) :: ifgraph
INTEGER :: idoa
INTEGER :: ifli
DOUBLE PRECISION :: rtma
DOUBLE PRECISION :: prex, prey, postx, posty
INTEGER :: pointcounter ! counter for face-plot mode
LOGICAL :: prepointexist
IF( p_mode /= 6 ) RETURN
IF( debugmode == 1 ) WRITE (*,*) 'Begin: plot faces'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% Plot Faces %{{'//'{'
END IF
WRITE(ifgraph,'(A)') 'gsave'
WRITE(ifgraph,'(A)') 'linw LW'
WRITE(ifgraph,'(A)') 'linc RGB'
WRITE(ifgraph,'(A)') '1 setlinejoin'
! [TODO] 描画面が枠外にはみ出す場合の処理は後回し
! 追記：おそらくclipで処理できる
pointcounter = 0
DO ifli = 1, fnum
  IF( morecomment )THEN
    IF(fnum>1)WRITE(ifgraph,'(A)') '% ===== file-'//TRIM(hint(ifli))
  END IF
  WRITE(ifgraph,'(A)') '/ifile '//TRIM(hint(ifli))//' def'
  prex = kpldata(ifli,1,1)
  prey = kpldata(ifli,2,1)
  prepointexist = .FALSE.
  DO idoa = 1,rowarr(ifli)
    postx = kpldata(ifli,1,idoa)
    posty = kpldata(ifli,2,idoa)
    IF(empdata(ifli,1,idoa).AND.empdata(ifli,2,idoa))THEN
      ! ignore empty line
      cycle
    ELSE IF((.NOT.empdata(ifli,1,idoa)).AND.(.NOT.empdata(ifli,2,idoa)))THEN
      IF(prepointexist) THEN
        WRITE(ifgraph,'(2I6,A)') NINT(postx),NINT(posty),' L'
        pointcounter = pointcounter + 1
      ELSE
        ! 直前がNODATAの場合
        WRITE(ifgraph,'(A)') 'NEW'
        WRITE(ifgraph,'(2I6,A)') NINT(postx),NINT(posty),' M'
        prex = postx
        prey = posty
        pointcounter = 1
      END IF
      prex = postx
      prey = posty
      prepointexist = .TRUE.
    ELSE IF(empdata(ifli,2,idoa))THEN
      ! p_mode==6では面に色を付ける
      rtma = kpldata(ifli,1,idoa)
      WRITE(ifgraph,'(F8.4,A)') rtma,' MAKECOLOR RGB'
      IF(prepointexist)THEN
        IF(pointcounter==1)THEN
        ELSE IF(pointcounter==2)THEN
          WRITE(ifgraph,'(A)') 'ST'
        ELSE
          WRITE(ifgraph,'(A)') 'closepath fill'
        END IF
      END IF
      prepointexist = .FALSE.
      pointcounter = 0
    END IF
  END DO
  IF(prepointexist) WRITE(ifgraph,'(A)') 'ST'
END DO
WRITE(ifgraph,'(A)') 'grestore'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '%}}'//'}'
END IF
RETURN
END SUBROUTINE
!%}}}
! SUB kplot_eps_line(debugmode,fnum,ifgraph) %{{{
SUBROUTINE kplot_eps_line(debugmode,fnum,ifgraph)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( IN ) :: fnum
INTEGER, INTENT( IN ) :: ifgraph
INTEGER :: idoa, idob
INTEGER :: ifli
DOUBLE PRECISION :: rtma
DOUBLE PRECISION :: prex, prey, postx, posty
LOGICAL :: prepointexist
LOGICAL :: prepointinrange
LOGICAL :: data_found
IF( .NOT. showline ) RETURN
IF( showbond ) RETURN
IF( p_mode == 2 .OR. p_mode == 6 ) RETURN
IF( p_mode == 4 .AND. hidden3d ) RETURN
IF( p_mode == 5 .AND. hidden3d ) RETURN
IF( debugmode == 1 ) WRITE (*,*) 'Begin: plot lines'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% Plot Lines %{{'//'{'
END IF
WRITE(ifgraph,'(A)') 'gsave'
WRITE(ifgraph,'(A)') 'linw LW'
WRITE(ifgraph,'(A)') 'linc RGB'
WRITE(ifgraph,'(A)') '1 setlinejoin'
WRITE(ifgraph,'(A)') '/B { BOND ST } def'
DO ifli = 1, fnum
  IF( morecomment )THEN
    IF(fnum>1)WRITE(ifgraph,'(A)') '% ===== file-'//TRIM(hint(ifli))
  END IF
  WRITE(ifgraph,'(A)') '/ifile '//TRIM(hint(ifli))//' def'
  ! data plot
  DO idob = 1, 1+repnum
    ! フラグdata_foundはその列のデータに、枠内データが見つかったか
    ! どうかを記憶する
    ! これによって、枠内データを一切含まない列がたくさんある場合に
    ! RGB指定や点種類指定が余計に出力されるのを防ぐ
    data_found = .FALSE.
    prepointexist = .FALSE.
    prepointinrange = .FALSE.
    rtma = lineacceptrange * 1000.d0
    DO idoa = 1,rowarr(ifli)
      postx = kpldata(ifli,1,idoa)
      posty = kpldata(ifli,using(idob),idoa)
      IF((.NOT.empdata(ifli,1,idoa)).AND.(.NOT.empdata(ifli,using(idob),idoa)))THEN
        IF((DABS(postx) <= rtma).AND.(DABS(posty) <= rtma))THEN
          IF(.NOT.data_found)THEN
            ! 初めての枠内データ
            IF( stcl == 0 )THEN
              !itma = MOD(idob,clnum)+1
              WRITE(ifgraph,'(I3,A)') idob,' STYLECOLORS RGB'
            ELSE
              WRITE(ifgraph,'(I3,A)') stcl,' STYLECOLORS RGB'
            END IF
            IF( stlt == 0 )THEN
              WRITE(ifgraph,'(I3,A)') idob,' STYLELINES'
            ELSE
              WRITE(ifgraph,'(I3,A)') stlt,' STYLELINES'
            END IF
            IF( prepointexist )THEN
              WRITE(ifgraph,'(A)') 'NEW'
              WRITE(ifgraph,'(2I6,A)') NINT(prex),NINT(prey),' M'
              WRITE(ifgraph,'(2I6,A)') NINT(postx),NINT(posty),' L'
            ELSE
              WRITE(ifgraph,'(A)') 'NEW'
              WRITE(ifgraph,'(2I6,A)') NINT(postx),NINT(posty),' M'
            END IF
            prex = postx
            prey = posty
            prepointexist = .TRUE.
            prepointinrange = .TRUE.
            data_found = .TRUE.
          ELSE
            ! 初めてではない枠内データ
            IF(prepointexist) THEN
              IF(prepointinrange)THEN
                ! 直前のデータが存在し、かつ枠内
                WRITE(ifgraph,'(2I6,A)') NINT(postx),NINT(posty),' L'
              ELSE
                ! 直前のデータは存在するが枠外
                WRITE(ifgraph,'(A)') 'NEW'
                WRITE(ifgraph,'(2I6,A)') NINT(prex),NINT(prey),' M'
                WRITE(ifgraph,'(2I6,A)') NINT(postx),NINT(posty),' L'
              END IF
            ELSE
              ! 直前がNODATAの場合
              WRITE(ifgraph,'(A)') 'NEW'
              WRITE(ifgraph,'(2I6,A)') NINT(postx),NINT(posty),' M'
              prex = postx
              prey = posty
            END IF
            prex = postx
            prey = posty
            prepointinrange = .TRUE.
            prepointexist = .TRUE.
          END IF
        ELSE
          IF(.NOT.data_found)THEN
            ! まだ枠内データが見つかっていないうえに、今回も枠外
            prepointexist = .TRUE.
            prepointinrange = .FALSE.
            prex = postx
            prey = posty
          ELSE
            IF(prepointinrange)THEN
              ! 前の点が枠内で、新しい点が枠外の場合の処理
              WRITE(ifgraph,'(2I6,A)') NINT(postx),NINT(posty),' L ST'
            ELSE
              ! 既に枠内データはあったが直前の点は枠外で、今回も枠外
            END IF
            prex = postx
            prey = posty
            prepointexist = .TRUE.
            prepointinrange = .FALSE.
          END IF
        END IF
      ELSE
        IF(prepointinrange) WRITE(ifgraph,'(A)') 'ST'
        prepointexist = .FALSE.
        prepointinrange = .FALSE.
      END IF
    END DO
    IF(prepointinrange) WRITE(ifgraph,'(A)') 'ST'
  END DO
END DO
WRITE(ifgraph,'(A)') 'grestore'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '%}}'//'}'
END IF
RETURN
END SUBROUTINE
!%}}}
! SUB kplot_eps_bond(debugmode,fnum,ifgraph) %{{{
SUBROUTINE kplot_eps_bond(debugmode,fnum,ifgraph)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( IN ) :: fnum
INTEGER, INTENT( IN ) :: ifgraph
INTEGER :: idoa, idob
INTEGER :: itma
INTEGER :: ifli
DOUBLE PRECISION :: rtma
DOUBLE PRECISION :: prex, prey, postx, posty
LOGICAL :: prepointexist
LOGICAL :: prepointinrange
LOGICAL :: data_found
IF( .NOT. showline ) RETURN
IF( .NOT. showbond ) RETURN
IF( p_mode == 2 .OR. p_mode == 6 ) RETURN
IF( p_mode == 4 .AND. hidden3d ) RETURN
IF( p_mode == 5 .AND. hidden3d ) RETURN
IF( debugmode == 1 ) WRITE (*,*) 'Begin: plot bonds'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% Plot Bonds %{{'//'{'
END IF
WRITE(ifgraph,'(A)') 'gsave'
WRITE(ifgraph,'(A)') 'linw LW'
WRITE(ifgraph,'(A)') 'linc RGB'
WRITE(ifgraph,'(A)') '1 setlinejoin'
WRITE(ifgraph,'(A)') '/B { BOND ST } def'
DO ifli = 1, fnum
  IF( morecomment )THEN
    IF(fnum>1)WRITE(ifgraph,'(A)') '% ===== file-'//TRIM(hint(ifli))
  END IF
  WRITE(ifgraph,'(A)') '/ifile '//TRIM(hint(ifli))//' def'
  ! data plot
  DO idob = 1, 1+repnum
    ! フラグdata_foundはその列のデータに、枠内データが見つかったか
    ! どうかを記憶する
    ! これによって、枠内データを一切含まない列がたくさんある場合に
    ! RGB指定や点種類指定が余計に出力されるのを防ぐ
    data_found = .FALSE.
    prepointexist = .FALSE.
    prepointinrange = .FALSE.
    ! bonds %{{{
    IF( stcl == 0 )THEN
      !itma = MOD(idob,clnum)+1
      WRITE(ifgraph,'(I3,A)') idob,' STYLECOLORS RGB'
    ELSE
      WRITE(ifgraph,'(I3,A)') stcl,' STYLECOLORS RGB'
    END IF
    rtma = lineacceptrange * 1000.d0
    DO idoa = 1, rowarr(ifli)
      prex = kpldata(ifli,1,idoa)
      prey = kpldata(ifli,using(idob),idoa)
      IF(DABS(prex)>rtma.OR.DABS(prey)>rtma)THEN
        prepointinrange = .FALSE.
      ELSE
        prepointinrange = .TRUE.
      END IF
      itma = 1
      DO
        IF(itma>ibmax)EXIT
        IF(bondgroup(ifli,idoa,itma) == 0)EXIT
        postx = kpldata(ifli,1,bondgroup(ifli,idoa,itma))
        posty = kpldata(ifli,using(idob),bondgroup(ifli,idoa,itma))
        itma = itma+1
        ! 少なくとも一方の端点が枠内にあるならば描画
        IF(prepointinrange.OR.&
        &(DABS(postx)<=rtma.AND.DABS(posty)<=rtma))THEN
          WRITE(ifgraph,'(4I6,A)') NINT(prex),NINT(prey),&
          & NINT(postx),NINT(posty),' B'
        END IF
      END DO
    END DO
    !%}}}
  END DO
END DO
WRITE(ifgraph,'(A)') 'grestore'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '%}}'//'}'
END IF
RETURN
END SUBROUTINE
!%}}}
! SUB kplot_eps_point(debugmode,fnum,ifgraph) %{{{
SUBROUTINE kplot_eps_point(debugmode,fnum,ifgraph)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( IN ) :: fnum
INTEGER, INTENT( IN ) :: ifgraph
INTEGER :: idoa, idob
INTEGER :: ifli
DOUBLE PRECISION :: rtma, rtmb
LOGICAL :: data_found
IF( .NOT. showpoint ) RETURN
IF( p_mode == 2 .OR. p_mode == 6 ) RETURN
IF( p_mode == 4 .AND. hidden3d ) RETURN
IF( p_mode == 5 .AND. hidden3d ) RETURN
IF( debugmode == 1 ) WRITE (*,*) 'Begin: plot points'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% Plot Points %{{'//'{'
END IF
WRITE(ifgraph,'(A)') 'gsave'
WRITE(ifgraph,'(A)') '/P {'
WRITE(ifgraph,'(A)') '  4 dict begin'
WRITE(ifgraph,'(A)') '  /y exch def'
WRITE(ifgraph,'(A)') '  /x exch def'
IF(p_mode == 3 .OR. p_mode == 5)THEN
  WRITE(ifgraph,'(A)') '  /w exch def'
  WRITE(ifgraph,'(A)') '  w MAKECOLOR RGB'
  WRITE(ifgraph,'(A)') '  /origpnts pnts def'
  WRITE(ifgraph,'(A)') '  /pnts w MAKESIZE def'
END IF
WRITE(ifgraph,'(A)') '  gsave'
WRITE(ifgraph,'(A)') '  bakc RGB'
WRITE(ifgraph,'(A)') '  ifile x y TRP2'
WRITE(ifgraph,'(A)') '  grestore'
WRITE(ifgraph,'(A)') '  ifile x y TRP'
IF(p_mode == 3 .OR. p_mode == 5)THEN
  WRITE(ifgraph,'(A)') '  /pnts origpnts def'
END IF
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
WRITE(ifgraph,'(A)') '/FP {'
WRITE(ifgraph,'(A)') '  4 dict begin'
WRITE(ifgraph,'(A)') '  /y exch def'
WRITE(ifgraph,'(A)') '  /x exch def'
IF(p_mode == 3 .OR. p_mode == 5)THEN
  WRITE(ifgraph,'(A)') '  /w exch def'
  WRITE(ifgraph,'(A)') '  w MAKECOLOR RGB'
  WRITE(ifgraph,'(A)') '  /origpnts pnts def'
  WRITE(ifgraph,'(A)') '  /pnts w MAKESIZE def'
END IF
WRITE(ifgraph,'(A)') '  ifile x y TRP2'
WRITE(ifgraph,'(A)') '  ifile x y TRP'
WRITE(ifgraph,'(A)') '  end'
WRITE(ifgraph,'(A)') '} def'
WRITE(ifgraph,'(A)') 'pntc RGB'
WRITE(ifgraph,'(A)') 'ptlw LW'
WRITE(ifgraph,'(A)') '[] 0 setdash'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '% ========== Start'
END IF
DO ifli = 1, fnum
  IF( morecomment )THEN
    IF(fnum>1)WRITE(ifgraph,'(A)') '% ===== file-'//TRIM(hint(ifli))
  END IF
  WRITE(ifgraph,'(A)') '/ifile '//TRIM(hint(ifli))//' def'
  DO idob = 1, 1+repnum
    ! フラグdata_foundはその列のデータに、枠内データが見つかったか
    ! どうかを記憶する
    ! これによって、枠内データを一切含まない列がたくさんある場合に
    ! RGB指定や点種類指定が余計に出力されるのを防ぐ
    data_found = .FALSE.
    rtma = pointacceptrange * 1000.d0
    DO idoa = 1, rowarr(ifli)
      ! 従来の実装は枠内のみ描画したが、clipを用いることにより部分的に枠に重なるデータも
      ! 描画できるようになった
      ! これに合わせて描画上限をゆるめに設定する
      ! (ゆるくしすぎると点が大量にある場合のファイルサイズが過剰になるので注意)
      ! この比率をpointacceptrangeとする
      IF(DABS(kpldata(ifli,1,idoa))>rtma)CYCLE
      IF(DABS(kpldata(ifli,using(idob),idoa))>rtma)CYCLE
      IF(empdata(ifli,1,idoa))CYCLE
      IF(empdata(ifli,using(idob),idoa))CYCLE
      IF( .NOT. data_found )THEN
        IF( stcl == 0 )THEN
          !itma = MOD(idob,clnum)+1
          WRITE(ifgraph,'(I3,A)') idob,' STYLECOLORS RGB'
        ELSE
          WRITE(ifgraph,'(I3,A)') stcl,' STYLECOLORS RGB'
        END IF
        IF( stpt == 0 )THEN
          WRITE(ifgraph,'(I3,A)') idob,' STYLEPOINTS'
        ELSE
          WRITE(ifgraph,'(I3,A)') stpt,' STYLEPOINTS'
        END IF
        data_found = .TRUE.
      END IF
      IF(p_mode == 3)THEN
        ! p_mode=3なら点に色を付ける
        rtmb = kpldata(ifli,3,idoa)
        WRITE(ifgraph,'(F8.4,2I6,A)') &
        & (rtmb+1.d0)/2.d0, &
        & NINT(kpldata(ifli,1,idoa)), &
        & NINT(kpldata(ifli,2,idoa)), ' P'
      ELSE IF(p_mode == 5)THEN
        ! p_mode=5でも点に色を付ける
        rtmb = kpldata(ifli,4,idoa)
        WRITE(ifgraph,'(F8.4,2I6,A)') &
        & (rtmb+1.d0)/2.d0, &
        & NINT(kpldata(ifli,1,idoa)), &
        & NINT(kpldata(ifli,2,idoa)),' P'
      ELSE
        ! 通常プロットモード
        ! 同一列データについて、いくつかの点だけ限定的にstrokeとfillを切り替えられるようにしたい
        ! fildata()でこれを実現する
        IF( fildata(ifli,using(idob),idoa) )THEN
          WRITE(ifgraph,'(2I6,A)') &
          & NINT(kpldata(ifli,1,idoa)), &
          & NINT(kpldata(ifli,using(idob),idoa)),' FP'
        ELSE
          WRITE(ifgraph,'(2I6,A)') &
          & NINT(kpldata(ifli,1,idoa)), &
          & NINT(kpldata(ifli,using(idob),idoa)),' P'
        END IF
      END IF
    END DO
  END DO
END DO
!WRITE(ifgraph,'(A)') 'NORMALLINES'
WRITE(ifgraph,'(A)') 'grestore'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '%}}'//'}'
END IF
RETURN
END SUBROUTINE
!%}}}
! SUB kplot_eps_hiddenlinepoint(debugmode,fnum,ifgraph) %{{{
SUBROUTINE kplot_eps_hiddenlinepoint(debugmode,fnum,ifgraph)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT( IN ) :: debugmode
INTEGER, INTENT( IN ) :: fnum
INTEGER, INTENT( IN ) :: ifgraph
INTEGER :: idoa, idob
INTEGER :: itma
INTEGER :: ifli
DOUBLE PRECISION :: rtma, rtmc, rtmd, rtme
DOUBLE PRECISION :: prex, prey, postx, posty
IF( .NOT. hidden3d ) RETURN
IF( p_mode == 1 ) RETURN
IF( p_mode == 2 ) RETURN
IF( p_mode == 3 ) RETURN
IF( p_mode == 6 ) RETURN
IF( debugmode == 1 ) WRITE (*,*) 'Begin: plot hidden-linepoints'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)' ) '% Plot HiddenLinepoints %{{'//'{'
  WRITE(ifgraph,'(A)' ) '% ========== Preparation'
END IF
WRITE(ifgraph,'(A)') 'gsave'
! macro definitions %{{{
WRITE(ifgraph,'(A)' ) '/LINW linw def'
WRITE(ifgraph,'(A)' ) '/LINWH linw linh 2 mul sub def'
WRITE(ifgraph,'(A)' ) '/LINC {linc} def'
WRITE(ifgraph,'(A)' ) '/L {'
WRITE(ifgraph,'(A)' ) '  5 dict begin'
WRITE(ifgraph,'(A)' ) '  /yb exch def'
WRITE(ifgraph,'(A)' ) '  /xb exch def'
WRITE(ifgraph,'(A)' ) '  /ya exch def'
WRITE(ifgraph,'(A)' ) '  /xa exch def'
IF(pprat>0.d0)THEN
  WRITE(ifgraph,'(A)' ) '  1000 div /lwadjust exch def'
  WRITE(ifgraph,'(A)' ) '  /LINW linw lwadjust mul def'
  WRITE(ifgraph,'(A)' ) '  /LINWH linw linh 2 mul sub lwadjust mul def'
END IF
IF(mist>0.d0)THEN
  WRITE(ifgraph,'(A)' ) '  1000 div /mistratio exch def'
  WRITE(ifgraph,'(A)' ) '  /LINC {linc bakc mistratio ColorMerge} def'
  WRITE(ifgraph,'(A)' ) '  /EDGC {edgc bakc mistratio ColorMerge} def'
END IF
WRITE(ifgraph,'(A)' ) '  1 STYLELINES'
WRITE(ifgraph,'(A)' ) '  1 setlinejoin'
WRITE(ifgraph,'(A)' ) '  1 setlinecap'
IF(pseudoshade)THEN
  WRITE(ifgraph,'(A)' ) '  0 0.1 1 {'
  WRITE(ifgraph,'(A)' ) '    1 dict begin'
  WRITE(ifgraph,'(A)' ) '    /rcnt exch def'
  WRITE(ifgraph,'(A)' ) '    LINW 1 1.0 rcnt mul sub mul LW'
  WRITE(ifgraph,'(A)' ) '    LINC'
  WRITE(ifgraph,'(A)' ) '    EDGC rcnt SmoothColorMergePostFlat RGB'
  WRITE(ifgraph,'(A)' ) '    ifile xa ya TRM ifile xb yb TRL ST'
  WRITE(ifgraph,'(A)' ) '    end'
  WRITE(ifgraph,'(A)' ) '  } for'
ELSE
  WRITE(ifgraph,'(A)' ) '  LINW LW'
  WRITE(ifgraph,'(A)' ) '  EDGC RGB'
  WRITE(ifgraph,'(A)' ) '  ifile xa ya TRM ifile xb yb TRL ST'
  WRITE(ifgraph,'(A)' ) '  LINWH LW'
  WRITE(ifgraph,'(A)' ) '  LINC RGB'
  WRITE(ifgraph,'(A)' ) '  ifile xa ya TRM ifile xb yb TRL ST'
END IF
WRITE(ifgraph,'(A)' ) '  end'
WRITE(ifgraph,'(A)' ) '} def'
!-------------------------------------------
WRITE(ifgraph,'(A)' ) '/HL {'
WRITE(ifgraph,'(A)' ) '  13 dict begin'
WRITE(ifgraph,'(A)' ) '  /yb exch def'
WRITE(ifgraph,'(A)' ) '  /xb exch def'
WRITE(ifgraph,'(A)' ) '  /ya exch def'
WRITE(ifgraph,'(A)' ) '  /xa exch def'
WRITE(ifgraph,'(A)' ) '  1000 div /sinth exch def'
IF(pprat>0.d0)THEN
  WRITE(ifgraph,'(A)' ) '  1000 div /lwadjust exch def'
  WRITE(ifgraph,'(A)' ) '  /PNTS pnts lwadjust mul def'
END IF
IF(mist>0.d0)THEN
  WRITE(ifgraph,'(A)' ) '  1000 div /mistratio exch def'
END IF
WRITE(ifgraph,'(A)' ) '  /dx xb xa sub def'
WRITE(ifgraph,'(A)' ) '  /dy yb ya sub def'
WRITE(ifgraph,'(A)' ) '  /dxreal ifile xb TransFormx ifile xa TransFormx sub def'
WRITE(ifgraph,'(A)' ) '  /dyreal yb TransFormy ya TransFormy sub def'
WRITE(ifgraph,'(A)' ) '  /l dxreal dxreal mul dyreal dyreal mul add sqrt def'
! lがゼロの場合には線を描画しない（こうしないと直後のdivでゼロ割が起こる）
! それだけでなく、前方の点に完全に隠れてしまうことがわかっている場合は描画をしない
IF(pprat>0.d0)THEN
  WRITE(ifgraph,'(A)' ) '  l PNTS gt {'
ELSE
  WRITE(ifgraph,'(A)' ) '  l pnts gt {'
END IF
! [FIXME] この、点のサイズに応じた調整は、点のサイズとしてスタイルサイズを使用した場合に厳密でない
WRITE(ifgraph,'(A)' ) '    /ratio PNTS sinth mul l div def'
WRITE(ifgraph,'(A)' ) '    ratio 0.5 lt {'
WRITE(ifgraph,'(A)' ) '      xa dx ratio mul add /xa exch def'
WRITE(ifgraph,'(A)' ) '      ya dy ratio mul add /ya exch def'
WRITE(ifgraph,'(A)' ) '      xb dx ratio mul sub /xb exch def'
WRITE(ifgraph,'(A)' ) '      yb dy ratio mul sub /yb exch def'
IF(mist>0.d0)THEN
  ! mistratioの処理はそのままLに渡して任せる
  WRITE(ifgraph,'(A)' ) '      mistratio 1000 mul'
END IF
IF(pprat>0.d0)THEN
  ! lwadjustの処理はそのままLに渡して任せる
  WRITE(ifgraph,'(A)' ) '      lwadjust 1000 mul'
END IF
WRITE(ifgraph,'(A)' ) '      xa ya xb yb L'
WRITE(ifgraph,'(A)' ) '    } if'
WRITE(ifgraph,'(A)' ) '  } if'
WRITE(ifgraph,'(A)' ) '  end'
WRITE(ifgraph,'(A)' ) '} def'
WRITE(ifgraph,'(I3,A)' ) stpt,' STYLEPOINTS'
WRITE(ifgraph,'(A)' ) '/PNTS pnts def'
WRITE(ifgraph,'(A)' ) '/PNTSH pnts pnth sub def'
WRITE(ifgraph,'(A)' ) '/PNTC {pntc} def'
WRITE(ifgraph,'(A)' ) '/EDGC {edgc} def'
WRITE(ifgraph,'(A)' ) '/P {'
WRITE(ifgraph,'(A)' ) '  7 dict begin'
WRITE(ifgraph,'(A)' ) '  /y exch def'
WRITE(ifgraph,'(A)' ) '  /x exch def'
IF(p_mode==5)THEN
  WRITE(ifgraph,'(A)' ) '  1000 div /v exch def'
  WRITE(ifgraph,'(A)' ) '  /pntc { v MAKECOLOR } def'
  WRITE(ifgraph,'(A)' ) '  /pnts v MAKESIZE def'
  WRITE(ifgraph,'(A)' ) '  /PNTS v MAKESIZE def'
  ! この場合点のサイズは常にスタイルサイズからのみ決定されるため、
  ! もとのpntsのデータを残しておく必要がない
END IF
IF(pprat>0.d0)THEN
  ! psadjustの値も1000倍して渡されることに注意
  WRITE(ifgraph,'(A)' ) '  1000 div /psadjust exch def'
  WRITE(ifgraph,'(A)' ) '  /PNTS pnts psadjust mul def'
  WRITE(ifgraph,'(A)' ) '  /PNTSH pnts pnth sub psadjust mul def'
END IF
IF(mist>0.d0)THEN
  WRITE(ifgraph,'(A)' ) '  1000 div /mistratio exch def'
  WRITE(ifgraph,'(A)' ) '  /PNTC {pntc bakc mistratio ColorMerge} def'
  WRITE(ifgraph,'(A)' ) '  /EDGC {edgc bakc mistratio ColorMerge} def'
END IF
WRITE(ifgraph,'(A)' ) '  ptlw LW'
WRITE(ifgraph,'(A)' ) '  [] 0 setdash'
IF(pseudoshade)THEN
  WRITE(ifgraph,'(A)' ) '  0 0.05 1 {'
  WRITE(ifgraph,'(A)' ) '    1 dict begin'
  WRITE(ifgraph,'(A)' ) '    /rcnt exch def'
  WRITE(ifgraph,'(A)' ) '    /pnts  PNTS 1 0.6 rcnt mul sub mul def'
  WRITE(ifgraph,'(A)' ) '    PNTC EDGC rcnt SmoothColorMergePostFlat RGB'
  WRITE(ifgraph,'(A)' ) '    ifile x y TRP2'
  WRITE(ifgraph,'(A)' ) '    end'
  WRITE(ifgraph,'(A)' ) '  } for'
  WRITE(ifgraph,'(A)' ) '  /pnts  PNTS 0.4 mul def'
ELSE
  WRITE(ifgraph,'(A)' ) '  /pnts  PNTS def'
  WRITE(ifgraph,'(A)' ) '  EDGC RGB'
  WRITE(ifgraph,'(A)' ) '  ifile x y TRP2'
  WRITE(ifgraph,'(A)' ) '  /pnts  PNTSH def'
END IF
WRITE(ifgraph,'(A)' ) '  PNTC RGB'
WRITE(ifgraph,'(A)' ) '  ifile x y TRP2'
IF(pseudoshade)THEN
  WRITE(ifgraph,'(A)' ) '  /x ifile x TransFormx pnts 0.5 mul sub def'
  WRITE(ifgraph,'(A)' ) '  /y y TransFormy pnts 0.5 mul add def'
  WRITE(ifgraph,'(A)' ) '  0 0.1 1 {'
  WRITE(ifgraph,'(A)' ) '    1 dict begin'
  WRITE(ifgraph,'(A)' ) '    /rcnt exch def'
  WRITE(ifgraph,'(A)' ) '    /pnts  PNTS 0.2 0.2 rcnt mul sub mul def'
  IF(mist>0.d0)THEN
    WRITE(ifgraph,'(A)' ) '    1 1 1 bakc mistratio ColorMerge'
  ELSE
    WRITE(ifgraph,'(A)' ) '    1 1 1'
  END IF
  WRITE(ifgraph,'(A)' ) '    PNTC 0.5 SmoothColorMergePostFlat PNTC rcnt SmoothColorMergePostFlat RGB'
  WRITE(ifgraph,'(A)' ) '    ifile x y Plot2'
  WRITE(ifgraph,'(A)' ) '    end'
  WRITE(ifgraph,'(A)' ) '  } for'
END IF
WRITE(ifgraph,'(A)' ) '  end'
WRITE(ifgraph,'(A)' ) '} def'
!%}}}
DO ifli = 1, fnum
  IF( morecomment )THEN
    IF(fnum>1)WRITE(ifgraph,'(A)') '% ===== file-'//TRIM(hint(ifli))
  END IF
  WRITE(ifgraph,'(A)') '/ifile '//TRIM(hint(ifli))//' def'
  data_for_scan(:) = kpldata(ifli,3,1:rowarr(ifli))
  WHERE( empdata(ifli,1,:) )
    data_for_scan(:) = 1.d100
    ! これ以降そのデータを描画させないために大きい値を入れる
    ! 十分大きい値でないとlineacceptrangeまたはpointacceptrangeに
    ! ひっかかってしまう可能性がある
  END WHERE
  hlp:DO
    call getarrpos(rowarr(ifli),data_for_scan,'min',itma)
    IF( data_for_scan(itma) < -1000.d0 )THEN
      data_for_scan(itma) = 1.d100
      CYCLE hlp
    ELSE IF( data_for_scan(itma) <= 1000.d0 )THEN
      idoa = itma
      prex = kpldata(ifli,1,idoa)
      prey = kpldata(ifli,2,idoa)
      IF(showpoint)THEN
        ! points %{{{
        ! 従来の実装は-1000~1000の範囲のみ描画したが、clipを用いることにより部分的に枠に重なるデータも
        ! 描画できるようになった
        ! これに合わせて描画上限をゆるめに設定する
        ! (ゆるくしすぎると点が大量にある場合のファイルサイズが過剰になるので注意)
        rtma = pointacceptrange*1000.d0
        ! 視点よりも手前のオブジェクトは描画しない
        IF( pprat*kpldata(ifli,3,idoa) < 1.d0 )THEN
          rtmc = 1.d0 / (1.d0 - pprat * kpldata(ifli,3,idoa))
          IF( mist > 0.d0 )THEN
            ! ppratがゼロでmistが正だと理論上は全てのオブジェクトが完全に見えなくなることに注意
            IF( pprat > 0.d0 )THEN
              rtmd = DEXP( mist * ( kpldata(ifli,3,idoa) - 1.d0 / pprat ) )
            ELSE
              rtmd = 0.d0
            END IF
          END IF
          IF(DABS(prex)<=rtma.AND.DABS(prey)<=rtma)THEN
            ! 遠方の点を小さくする場合はrtmcをPNTSにかける
            ! 空気遠近法に対応するため、ppratの演算はポストスクリプト側に回した方が良い
            IF(mist>0.d0)THEN
              WRITE(ifgraph,'(I5$)') NINT(rtmd*1000.d0)
            END IF
            IF(pprat>0.d0)THEN
              IF(rtmc>8.d0)THEN
                WRITE(ifgraph,'(I10$)') NINT(rtmc*1000.d0)
              ELSE
                WRITE(ifgraph,'(I5$)') NINT(rtmc*1000.d0)
              END IF
            END IF
            IF(p_mode==5)THEN
              WRITE(ifgraph,'(I5$)') NINT(kpldata(ifli,4,idoa)*1000.d0)
            END IF
            WRITE(ifgraph,'(2I6,A)') NINT(prex),NINT(prey),' P'
          END IF
        END IF
        !%}}}
      END IF
      IF(showline.AND.showbond)THEN
        ! bonds %{{{
        rtma = lineacceptrange*1000.d0
        DO idob = 1, ibmax
          IF(bondgroup(ifli,idoa,idob) == 0)EXIT
          postx = kpldata(ifli,1,bondgroup(ifli,idoa,idob))
          posty = kpldata(ifli,2,bondgroup(ifli,idoa,idob))
          ! bondangleは正常処理なら正の値
          rtmd = DSIN( bondangle(ifli,idoa,idob) )
          ! ボンドの端点の内手前の方が視点より手前なら描画しない
          rtmc = kpldata(ifli,3,(bondgroup(ifli,idoa,idob)))
          IF( pprat * rtmc < 1.d0 )THEN
            rtmc = (kpldata(ifli,3,idoa) + kpldata(ifli,3,(bondgroup(ifli,idoa,idob)))) / 2.d0
            IF( mist > 0.d0 )THEN
              ! ppratがゼロでmistが正だと理論上は全てのオブジェクトが完全に見えなくなることに注意
              IF( pprat > 0.d0 )THEN
                rtme = DEXP( mist * ( rtmc - 1.d0 / pprat ) )
              ELSE
                rtme = 0.d0
              END IF
            END IF
            rtmc = 1.d0 / (1.d0 - pprat * rtmc )
            ! 少なくとも一方の端点が枠内にあるならば描画
            ! 両端点が枠外にある場合でも許容範囲なら描画処理を行う
            ! こうしないとクリップ境界付近で不自然になることがある
            IF((DABS(prex)<=rtma.AND.DABS(prey)<=rtma) &
            & .OR. (DABS(postx)<=rtma.AND.DABS(posty)<=rtma))THEN
              IF(mist>0.d0)THEN
                WRITE(ifgraph,'(I5$)') NINT(rtme*1000.d0)
              END IF
              IF(pprat>0.d0)THEN
                IF(rtmc>8.d0)THEN
                  WRITE(ifgraph,'(I10$)') NINT(rtmc*1000.d0)
                ELSE
                  WRITE(ifgraph,'(I5$)') NINT(rtmc*1000.d0)
                END IF
              END IF
              IF(hiddenbond)THEN
                WRITE(ifgraph,'(I5$)') NINT(rtmd*1000.d0)
              END IF
              WRITE(ifgraph,'(4I6$)') NINT(prex),NINT(prey),&
              & NINT(postx),NINT(posty)
              IF(hiddenbond)THEN
                WRITE(ifgraph,'(A)') ' HL'
              ELSE
                WRITE(ifgraph,'(A)') ' L'
              END IF
            END IF
          END IF
        END DO
        !%}}}
      END IF
      data_for_scan(itma) = 1.d100
    ELSE
      EXIT hlp
    END IF
  END DO hlp
END DO
WRITE(ifgraph,'(A)') 'grestore'
IF( morecomment )THEN
  WRITE(ifgraph,'(A)') '%}}'//'}'
END IF
RETURN
END SUBROUTINE
!%}}}
!-----------

! SUB print_error(ierror,errmsg) %{{{
SUBROUTINE print_error(ierror,errmsg)
IMPLICIT NONE
INTRINSIC:: TRIM
INTEGER,INTENT(IN):: ierror
CHARACTER(LEN= *),INTENT(IN):: errmsg
WRITE(0,'(A,I4,A)') 'E',ierror,': '//TRIM(errmsg)
RETURN
END SUBROUTINE
!%}}}
! SUB dealloc_kp() %{{{
SUBROUTINE dealloc_kp()
USE cv_kplot
IMPLICIT NONE
IF(ALLOCATED(using    )) DEALLOCATE(using    )
IF(ALLOCATED(kpldata  )) DEALLOCATE(kpldata  )
IF(ALLOCATED(empdata  )) DEALLOCATE(empdata  )
IF(ALLOCATED(fildata  )) DEALLOCATE(fildata  )
IF(ALLOCATED(data_for_scan )) DEALLOCATE(data_for_scan )
IF(ALLOCATED(bondgroup)) DEALLOCATE(bondgroup)
IF(ALLOCATED(bondangle)) DEALLOCATE(bondangle)
IF(ALLOCATED(distance )) DEALLOCATE(distance )
RETURN
END SUBROUTINE
!%}}}
! SUB alloc_kp(fnum) %{{{
SUBROUTINE alloc_kp(fnum)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT (IN) :: fnum
INTEGER :: itma
! Automatic Allocation
CALL dealloc_kp()
itma = MAXVAL(rowarr(1:fnum))
ALLOCATE(using(1:1+repnum))
ALLOCATE( kpldata(fnum,colmax,itma+2) )
ALLOCATE( empdata(fnum,colmax,itma+2) )
ALLOCATE( fildata(fnum,colmax,itma+2) )
ALLOCATE( data_for_scan(itma+2) )
ALLOCATE(bondgroup(fnum,itma,ibmax))
ALLOCATE(bondangle(fnum,itma,ibmax))
ALLOCATE(distance(itma))
RETURN
END SUBROUTINE
!%}}}

! SUB dcheckrange(var,varname,varmin,varmax,errcode,ierr,msg) %{{{
SUBROUTINE dcheckrange(var,varname,varmin,varmax,errcode,ierr,msg)
USE cv_kplot, ONLY : autofix, printerror
IMPLICIT NONE
DOUBLE PRECISION, INTENT(INOUT) :: var
CHARACTER( LEN= * ), INTENT(IN) :: varname
DOUBLE PRECISION, INTENT(IN) :: varmin, varmax
INTEGER, INTENT(IN) :: errcode
INTEGER, INTENT(INOUT) :: ierr
CHARACTER( LEN= * ), INTENT(OUT) :: msg
IF(ierr/=0) RETURN
IF(autofix)THEN
  IF( var<varmin ) var=varmin
  IF( var>varmax ) var=varmax
ELSE
  IF( var<varmin .OR. var>varmax )THEN
    ierr = errcode
    WRITE(msg,*) 'Error '// TRIM(varname) // &
    '. Input real between ', varmin, 'and', varmax
    IF(printerror) CALL print_error(ierr,msg)
    CALL dealloc_kp()
    RETURN
  END IF
END IF
ierr = 0
RETURN
END SUBROUTINE
!%}}}
! SUB icheckrange(var,varname,varmin,varmax,errcode,ierr,msg) %{{{
SUBROUTINE icheckrange(var,varname,varmin,varmax,errcode,ierr,msg)
USE cv_kplot, ONLY : autofix, printerror
IMPLICIT NONE
INTEGER, INTENT(INOUT) :: var
CHARACTER( LEN= * ), INTENT(IN) :: varname
INTEGER, INTENT(IN) :: varmin, varmax
INTEGER, INTENT(IN) :: errcode
INTEGER, INTENT(INOUT) :: ierr
CHARACTER( LEN= * ), INTENT(OUT) :: msg
IF(ierr/=0) RETURN
IF(autofix)THEN
  IF( var<varmin ) var=varmin
  IF( var>varmax ) var=varmax
ELSE
  IF( var<varmin .OR. var>varmax )THEN
    ierr = errcode
    WRITE(msg,*) 'Error '// TRIM(varname) // &
    '. Input integer between ', varmin, 'and', varmax
    IF(printerror) CALL print_error(ierr,msg)
    CALL dealloc_kp()
    RETURN
  END IF
END IF
ierr = 0
RETURN
END SUBROUTINE
!%}}}

! SUB cutoff_extrazero(strinout) %{{{
SUBROUTINE cutoff_extrazero(strinout)
IMPLICIT NONE
CHARACTER(LEN= *),INTENT(INOUT):: strinout
INTEGER:: idoa,leng
leng = LEN_TRIM(strinout)
DO idoa = leng, 1, -1
  IF(strinout(idoa:idoa) == '.')THEN
    strinout(idoa:idoa) = ' '
  ELSE IF((INDEX(strinout,'.') /= 0).AND.(strinout(idoa:idoa) == '0'))THEN
    strinout(idoa:idoa) = ' '
  ELSE
    RETURN
  END IF
END DO
RETURN
END SUBROUTINE
!%}}}
! SUB getarrpos(arrsize,arrin,obj,iresult) %{{{
SUBROUTINE getarrpos(arrsize,arrin,obj,iresult)
IMPLICIT NONE
INTEGER,INTENT(IN):: arrsize
DOUBLE PRECISION,INTENT(IN):: arrin(arrsize)
CHARACTER(LEN= 3),INTENT(IN):: obj
INTEGER,INTENT(OUT):: iresult
!           IF obj='max' > RETURN max position of array arrin(:)
!           IF obj='min' > RETURN min position of array arrin(:)
!           IF otherwise > RETURN -1
INTRINSIC:: MAXLOC, MINLOC
INTEGER:: zeroarray(1)

IF(obj == 'max')THEN
  zeroarray(:) = MAXLOC(arrin)
  iresult = zeroarray(1)
  RETURN
ELSE IF(obj == 'min')THEN
  zeroarray(:) = MINLOC(arrin)
  iresult = zeroarray(1)
  RETURN
ELSE
  iresult = -1
  RETURN
END IF
RETURN
END SUBROUTINE
!%}}}

! SUB decide_range(the_colmax,the_fnum,the_min,the_max) %{{{
SUBROUTINE decide_range(the_colmax,the_fnum,the_min,the_max)
USE cv_kplot
IMPLICIT NONE
INTEGER,INTENT(IN):: the_colmax,the_fnum
DOUBLE PRECISION,INTENT(OUT):: the_min,the_max
DOUBLE PRECISION:: the_current
INTEGER:: idoa,idob

the_min = kpl_large
the_max = kpl_small
DO idoa = 1,the_fnum
DO idob = 1,rowarr(the_fnum)
  IF(empdata(idoa,the_colmax,idob))CYCLE
  IF(empdata(idoa,1,idob))CYCLE
  the_current = kpldata(idoa,the_colmax,idob)
  IF(the_current<the_min)the_min = the_current
  IF(the_current>the_max)the_max = the_current
END DO
END DO
RETURN
END SUBROUTINE
!%}}}
! SUB arrange_range(the_mode,the_colmax,the_min,the_max,the_rate,ierror,errmsg) %{{{
SUBROUTINE arrange_range(the_mode,the_colmax,the_min,the_max,the_rate,ierror,errmsg)
USE cv_kplot
IMPLICIT NONE
INTEGER,INTENT(IN):: the_mode,the_colmax
INTEGER,INTENT(OUT):: ierror
DOUBLE PRECISION,INTENT(INOUT):: the_min,the_max
DOUBLE PRECISION,INTENT(IN):: the_rate
CHARACTER(LEN=*), INTENT( OUT ) :: errmsg
DOUBLE PRECISION:: rtma
ierror = 0
rtma = dmax1(DABS(the_max),DABS(the_min))
IF(dlog10(rtma)<DBLE(kpl_logmin))THEN
  the_max = 1.d0
  the_min = -1.d0
  WRITE(errmsg,'(A,I2)') 'Caution: too small data on colmax ', the_colmax
  ierror = -4
  RETURN
END IF
IF(DLOG(the_max-the_min)-DLOG(rtma)<DLOG(2.0d0)*DBLE(kpl_rndmin))THEN
  the_max = the_max+1.d0
  the_min = the_min-1.d0
  WRITE(errmsg,'(A,I2)') 'Caution: too small range on colmax ', the_colmax
  ierror = -5
  RETURN
END IF
IF(MOD(the_mode,2) == 1)THEN
  the_min = (the_max+the_min)/2.0d0
  the_max = (the_max-the_min)/the_rate
  the_min = the_min-the_max
  the_max = the_min+the_max*2.0d0
END IF
RETURN
END SUBROUTINE
!%}}}
! SUB auto_division(the_mode,the_min,the_max,the_div,the_mdiv) %{{{
SUBROUTINE auto_division(the_mode,the_min,the_max,the_div,the_mdiv)
USE cv_kplot
IMPLICIT NONE
INTEGER, INTENT(IN) :: the_mode
DOUBLE PRECISION, INTENT(INOUT) :: the_min
DOUBLE PRECISION, INTENT(INOUT) :: the_max
INTEGER, INTENT(INOUT) :: the_div
INTEGER, INTENT(INOUT) :: the_mdiv
DOUBLE PRECISION :: rtma
DOUBLE PRECISION :: rtmb
INTEGER :: itma
INTEGER :: intmin
INTEGER :: intmax
IF( the_mode > 1 ) RETURN
rtma = 10.d0 ** FLOOR( DLOG10( ( the_max - the_min ) / 5.d0 ) )
! [NOTE] atp, 5*rtma<= the_max-the_min < 50*rtma
!------------
rtmb = the_min / rtma
intmin = FLOOR( rtmb )
the_min = rtma * DBLE( intmin )
rtmb = the_max / rtma
intmax = CEILING( rtmb )
the_max = rtma * DBLE( intmax )
! この時点でthe_minとthe_maxは整数かける10のべきになっている
itma = NINT( ( the_max - the_min ) / rtma )
!
IF( the_div == 0 )THEN
  the_div = itma
  IF( the_div < 8 )THEN
    the_mdiv = 0
  ELSE IF( the_div < 16 )THEN
    the_mdiv = 2 * CEILING( DBLE(intmin) / 2.d0 ) - intmin
  ELSE IF( the_div < 25 )THEN
    the_mdiv = 5 * CEILING( DBLE(intmin) / 5.d0 ) - intmin
  ELSE
    ! 一番最小のきりの良い数字までの区切りの個数をmdivとして返す
    the_mdiv = 10 * CEILING( DBLE(intmin) / 10.d0 ) - intmin
  END IF
END IF
RETURN
END SUBROUTINE
!%}}}
! SUB cube_border(ifile,arr,th,ph,flagin) %{{{
SUBROUTINE cube_border(ifile,arr,th,ph,flagin)
IMPLICIT NONE
INTEGER,INTENT(IN):: ifile
DOUBLE PRECISION,INTENT(IN):: arr(8,3), th, ph
LOGICAL,INTENT(IN):: flagin
INTRINSIC :: DATAN, MOD, FLOOR, DABS
INTEGER:: idoa, idob
INTEGER:: itma, itmb, itmc
DOUBLE PRECISION:: prex, prey, posx, posy
DOUBLE PRECISION:: rtma, rtmb
DOUBLE PRECISION,PARAMETER :: pi = 4.d0* DATAN(1.d0)
! ここで使用する"L"の定義はプロット線描画時に変更されている可能性がある
! 特にhidden-line-pointsモードでボンド描画後に手前側のキューブボーダーを表示する場合
! 万全を期して再定義しておく

WRITE(ifile,'(A)') '% Cube Border %{{'//'{'
WRITE(ifile,'(A)') 'bdlt STYLELINES'
WRITE(ifile,'(A)') '/M {'
WRITE(ifile,'(A)') '  2 dict begin'
WRITE(ifile,'(A)') '  /y exch def'
WRITE(ifile,'(A)') '  /x exch def'
WRITE(ifile,'(A)') '  ifile x y TRM'
WRITE(ifile,'(A)') '  end'
WRITE(ifile,'(A)') '} def'
WRITE(ifile,'(A)') '/L {'
WRITE(ifile,'(A)') '  2 dict begin'
WRITE(ifile,'(A)') '  /y exch def'
WRITE(ifile,'(A)') '  /x exch def'
WRITE(ifile,'(A)') '  ifile x y TRL'
WRITE(ifile,'(A)') '  end'
WRITE(ifile,'(A)') '} def'
WRITE(ifile,'(A)') '/B {'
WRITE(ifile,'(A)') '  4 dict begin'
WRITE(ifile,'(A)') '  /yb exch def'
WRITE(ifile,'(A)') '  /xb exch def'
WRITE(ifile,'(A)') '  /ya exch def'
WRITE(ifile,'(A)') '  /xa exch def'
WRITE(ifile,'(A)') '  bdlw linh mul LW'
WRITE(ifile,'(A)') '  bakc RGB'
WRITE(ifile,'(A)') '  ifile xa ya M ifile xb yb L ST'
WRITE(ifile,'(A)') '  bdlw LW'
WRITE(ifile,'(A)') '  bdlc RGB'
WRITE(ifile,'(A)') '  ifile xa ya M ifile xb yb L ST'
WRITE(ifile,'(A)') '  end'
WRITE(ifile,'(A)') '} def'
! 3d border
DO idob = 1, 3
DO idoa = 1, 4
  SELECT CASE(idob)
  CASE(1)
    itma = idoa   ! (1,2,3,4)
    itmb = idoa+4 ! (5,6,7,8)
  CASE(2)
    itma = idoa*2-1  ! (1,3,5,7)
    itmb = idoa*2    ! (2,4,6,8)
  CASE(3)
    itma = 2*idoa-MOD(idoa-1,2)-1 ! (1,2,5,6)
    itmb = itma+2                 ! (3,4,7,8)
  END SELECT
  !
  rtma = th * 2.d0 / pi
  rtmb = ph * 2.d0 / pi
  IF(rtma<1.d0)THEN
    SELECT CASE(FLOOR(rtmb))
      CASE(0); itmc = 6
      CASE(1); itmc = 8
      CASE(2); itmc = 4
      CASE(3); itmc = 2
    END SELECT
  ELSE
    SELECT CASE(FLOOR(rtmb))
      CASE(0); itmc = 5
      CASE(1); itmc = 7
      CASE(2); itmc = 3
      CASE(3); itmc = 1
    END SELECT
  END IF
  IF(flagin)THEN
    IF((itma == itmc).OR.(itmb == itmc))CYCLE
  ELSE
    IF(.NOT.((itma == itmc).OR.(itmb == itmc)))CYCLE
  END IF
  prex = arr(itma,1)
  prey = arr(itma,2)
  posx = arr(itmb,1)
  posy = arr(itmb,2)
  WRITE(ifile,'(4I8,A)') NINT(prex),NINT(prey),&
  & NINT(posx),NINT(posy),' B'
END DO
END DO
WRITE(ifile,'(A)') '%}}'//'}'
RETURN
END SUBROUTINE
!%}}}
! SUB kp_print_real(realscale,realin,fmtstr,strout,cutexzero) %{{{
SUBROUTINE kp_print_real(realscale,realin,fmtstr,strout,cutexzero)
IMPLICIT NONE
! arguments
DOUBLE PRECISION, INTENT( IN ) :: realscale
DOUBLE PRECISION, INTENT( IN ) :: realin
CHARACTER( LEN= 20 ), INTENT( IN ) :: fmtstr
CHARACTER( LEN= 500 ), INTENT( OUT ) :: strout
LOGICAL,INTENT(IN):: cutexzero
! local variables
CHARACTER( LEN= 100 ) :: fmtbase, width, fmtfull
CHARACTER( LEN= 100 ) :: fmtdefault
CHARACTER( LEN= 100 ) :: stmpa, stmpb
INTEGER( KIND= 4 ) :: iround
INTEGER( KIND= 4 ) :: itmpa, itmpb
INTEGER( KIND= 4 ) :: idoa
INTEGER( KIND= 4 ) :: icap

fmtdefault = 'es'
width = ''
iround = NINT( realin )
icap = 0
! prepare for 'auto' format
IF(DABS(realin)>0.049990d0 .AND. DABS(realin)<10000)THEN
  fmtdefault = 'f'
ELSE IF(DABS(realin)<realscale*1.d-12)THEN
  fmtdefault = 'f'
  ! 桁落ちで0.00がずれる対策
END IF
! detect width info %{{{
itmpa = SCAN( fmtstr, '0123456789.' )
IF( itmpa > 0 )THEN
  ! 注意: 指数部の桁数の指定に'Ew.dEe'のようにEを使用することがある
  itmpb = itmpa - 1 + VERIFY( fmtstr(itmpa:), '0123456789.E' )
  width = fmtstr(itmpa:itmpb-1)
END IF
!%}}}
! detect output format %{{{
IF     ( INDEX( fmtstr, 'auto' ) > 0 )THEN ! auto
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
! fool proof %{{{
IF( fmtbase == 'logic' )THEN
  width = ''
END IF
IF( fmtbase == 'roman' )THEN
  width = ''
END IF
!%}}}
fmtfull = '(' // TRIM( fmtbase ) // TRIM( width ) // ')'
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
    WRITE(strout,'(A'//TRIM(width)//'$)') TRIM(stmpa)
  ELSE
    WRITE(strout,'(A$)') TRIM(stmpa)
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
      ELSE IF( iround == 4 )THEN
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
    WRITE(strout,'(A'//TRIM(width)//'$)') TRIM(stmpa)
  ELSE
    WRITE(strout,'(A$)') TRIM(stmpa)
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
    WRITE(strout,'(A'//TRIM(width)//'$)') TRIM(stmpa)
  ELSE
    WRITE(strout,'(A$)') TRIM(stmpa)
  END IF
!%}}}
! f %{{{
ELSE IF( fmtbase == 'f' )THEN
  ! fortran 'f' format
  IF( width /= '' )THEN
    WRITE(strout,fmtfull) realin
  ELSE
    WRITE(stmpa,'(F32.12)') realin
    WRITE(strout,'(A$)') TRIM(ADJUSTL(stmpa))
    IF(cutexzero) CALL kp_cut_extra_zero(strout)
  END IF
!%}}}
! g %{{{
ELSE IF( fmtbase == 'g' )THEN
  ! fortran 'g' format
  IF( width /= '' )THEN
    WRITE(strout,fmtfull) realin
  ELSE
    WRITE(stmpa,'(G32.12)') realin
    WRITE(strout,'(A$)') TRIM(ADJUSTL(stmpa))
    IF(cutexzero) CALL kp_cut_extra_zero(strout)
  END IF
!%}}}
! en %{{{
ELSE IF( fmtbase == 'en' )THEN
  ! fortran 'en' format
  IF( width /= '' )THEN
    WRITE(strout,fmtfull) realin
  ELSE
    WRITE(stmpa,'(EN32.12)') realin
    WRITE(strout,'(A$)') TRIM(ADJUSTL(stmpa))
    IF(cutexzero) CALL kp_cut_extra_zero(strout)
  END IF
!%}}}
! es %{{{
ELSE IF( fmtbase == 'es' )THEN
  ! fortran 'es' format
  IF( width /= '' )THEN
    WRITE(strout,fmtfull) realin
  ELSE
    WRITE(stmpa,'(ES32.12)') realin
    WRITE(strout,'(A$)') TRIM(ADJUSTL(stmpa))
    IF(cutexzero) CALL kp_cut_extra_zero(strout)
  END IF
!%}}}
! e %{{{
ELSE IF( fmtbase == 'e' )THEN
  ! fortran 'e' format
  IF( width /= '' )THEN
    WRITE(strout,fmtfull) realin
  ELSE
    WRITE(stmpa,'(E32.12)') realin
    WRITE(strout,'(A$)') TRIM(ADJUSTL(stmpa))
    IF(cutexzero) CALL kp_cut_extra_zero(strout)
  END IF
!%}}}
! d %{{{
ELSE IF( fmtbase == 'd' )THEN
  ! fortran 'd' format
  IF( width /= '' )THEN
    WRITE(strout,fmtfull) realin
  ELSE
    WRITE(stmpa,'(D32.12)') realin
    WRITE(strout,'(A$)') TRIM(ADJUSTL(stmpa))
    IF(cutexzero) CALL kp_cut_extra_zero(strout)
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
    WRITE(strout,'(A$)') TRIM(ADJUSTL(stmpa))
  END IF
!%}}}
! z %{{{
ELSE IF( fmtbase == 'z' )THEN
  ! fortran 'z' format
  ! Note:  -(8*16^(7)) ~ (8*16^(7)-1) -> need only 8 digits
  IF( width /= '' )THEN
    WRITE(strout,fmtfull) iround
  ELSE
    WRITE(stmpa,'(Z10)') iround
    WRITE(strout,'(A$)') TRIM(ADJUSTL(stmpa))
    IF(cutexzero) CALL kp_cut_extra_zero(strout)
  END IF
!%}}}
! b %{{{
ELSE IF( fmtbase == 'b' )THEN
  ! fortran 'b' format
  ! Note:  -(2^(31)) ~ (2^(31)-1) -> need only 32 digits
  IF( width /= '' )THEN
    WRITE(strout,fmtfull) iround
  ELSE
    WRITE(stmpa,'(B40)') iround
    WRITE(strout,'(A$)') TRIM(ADJUSTL(stmpa))
    IF(cutexzero) CALL kp_cut_extra_zero(strout)
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
    WRITE(strout,'(A$)') TRIM(ADJUSTL(stmpa))
    IF(cutexzero) CALL kp_cut_extra_zero(strout)
  END IF
!%}}}
END IF
RETURN
END SUBROUTINE
!%}}}
! SUB kp_cut_extra_zero(strinout) %{{{
SUBROUTINE kp_cut_extra_zero(strinout)
IMPLICIT NONE
CHARACTER(LEN= 500),INTENT(INOUT) :: strinout
INTRINSIC:: INDEX, SCAN, VERIFY, LEN_TRIM, TRIM
INTEGER:: itmpa, itmpb
CHARACTER(LEN= 500):: stmpa

IF(INDEX(strinout,'.') == 0)RETURN
IF(SCAN(strinout,'eEdD')>0)THEN
  ! with-index-representation
  itmpa = SCAN(strinout,'eEdD')
  stmpa = strinout(1:itmpa-1)
  stmpa(VERIFY(stmpa,'0 ',.TRUE.)+1:) = ''
  itmpb = LEN_TRIM(stmpa)
  IF(stmpa(itmpb:itmpb) == '.')stmpa(itmpb:itmpb) = ''
  strinout = TRIM(stmpa)//TRIM(strinout(itmpa:))
ELSE
  ! without-index-representation
  strinout(VERIFY(strinout,'0 ',.TRUE.)+1:) = ''
  strinout(VERIFY(strinout,'. ',.TRUE.)+1:) = ''
END IF
RETURN
END SUBROUTINE ! cut extra zero routine
!%}}}

! SUB kp_print_def_int %{{{
SUBROUTINE kp_print_def_int(ifgraph_in,tag_in,int_in,msg_in)
USE cv_kplot, ONLY: morecomment
IMPLICIT NONE
INTEGER, INTENT(IN) :: ifgraph_in
CHARACTER(LEN=*), INTENT(IN) :: tag_in
INTEGER, INTENT(IN) :: int_in
CHARACTER(LEN=*), INTENT(IN) :: msg_in
WRITE(ifgraph_in,'(A,I8,A$)') '/' // TRIM(tag_in) // ' ', int_in, ' def'
IF( morecomment )THEN
  WRITE(ifgraph_in,'(A$)') ' % ' // TRIM(msg_in)
END IF
WRITE(ifgraph_in,*)
RETURN
END SUBROUTINE
!%}}}
! SUB kp_print_def_real %{{{
SUBROUTINE kp_print_def_real(ifgraph_in,tag_in,real_in,msg_in)
USE cv_kplot, ONLY: morecomment
IMPLICIT NONE
INTEGER, INTENT(IN) :: ifgraph_in
CHARACTER(LEN=*), INTENT(IN)  :: tag_in
DOUBLE PRECISION, INTENT(IN)  :: real_in
CHARACTER(LEN=*), INTENT(IN)  :: msg_in
WRITE(ifgraph_in,'(A,F8.2,A$)') '/' // TRIM(tag_in) // ' ', real_in, ' def'
IF( morecomment )THEN
  WRITE(ifgraph_in,'(A$)') ' % ' // TRIM(msg_in)
END IF
WRITE(ifgraph_in,*)
RETURN
END SUBROUTINE
!%}}}
! SUB kp_print_def_color %{{{
SUBROUTINE kp_print_def_color(ifgraph_in,tag_in,color_in,msg_in)
USE cv_kplot, ONLY: morecomment
IMPLICIT NONE
INTEGER, INTENT(IN) :: ifgraph_in
CHARACTER(LEN=*), INTENT(IN) :: tag_in
DOUBLE PRECISION, INTENT(IN) :: color_in(3)
CHARACTER(LEN=*), INTENT(IN) :: msg_in
INTEGER :: idoa
WRITE(ifgraph_in,'(A,3F6.3,A$)') '/' // TRIM(tag_in) // ' {', &
& ( color_in(idoa), idoa = 1, 3 ), '} def'
IF( morecomment )THEN
  WRITE(ifgraph_in,'(A$)') ' % ' // TRIM(msg_in)
END IF
WRITE(ifgraph_in,*)
RETURN
END SUBROUTINE
!%}}}
! SUB kp_print_def_str %{{{
SUBROUTINE kp_print_def_str(ifgraph_in,tag_in,str_in,msg_in)
USE cv_kplot, ONLY: morecomment
IMPLICIT NONE
INTEGER, INTENT(IN) :: ifgraph_in
CHARACTER(LEN=*), INTENT(IN) :: tag_in
CHARACTER(LEN=*), INTENT(IN) :: str_in
CHARACTER(LEN=*), INTENT(IN) :: msg_in
WRITE(ifgraph_in,'(A$)') '/' // TRIM(tag_in) // ' (' // &
& TRIM(str_in) // ') def'
IF( morecomment )THEN
  WRITE(ifgraph_in,'(A$)') ' % ' // TRIM(msg_in)
END IF
WRITE(ifgraph_in,*)
RETURN
END SUBROUTINE
!%}}}

!= Functions ============================

! INTEGER kplot_new_devnum() %{{{
INTEGER FUNCTION kplot_new_devnum() RESULT ( res )
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
  ENDIF
END DO
res = 0
RETURN
END FUNCTION
!%}}}
! INTEGER realtype(real_input) %{{{
!INTEGER FUNCTION realtype(real_input)
!IMPLICIT NONE
!DOUBLE PRECISION:: real_input
!DOUBLE PRECISION:: rtmpa
!INTEGER:: inta
!inta = 0 ! RETURN 0 IF normal number
!IF(real_input /= real_input)THEN
!  inta = 1 ! RETURN 1 IF NaN
!ELSE
!  rtmpa = real_input*0.d0
!  IF(rtmpa /= rtmpa)THEN
!    inta = 2                  ! RETURN 2 IF +Infinity
!    IF(real_input<0)inta = 3  ! RETURN 3 IF -Infinity
!  END IF
!END IF
!realtype = inta
!END FUNCTION
!%}}}
! LOGICAL isreal(str_input) %{{{
LOGICAL FUNCTION isreal(str_input)
IMPLICIT NONE
CHARACTER*500:: str_input
INTRINSIC:: SCAN,LEN_TRIM
INTEGER:: level,icount
isreal = .TRUE.
level = 1
icount = 1
DO
  SELECT CASE(level)
  CASE(1) ! 符号または数字または小数点を求めている段階
    IF(SCAN(str_input(icount:icount),'+-') /= 0)THEN
      level = 2
    ELSE IF(SCAN(str_input(icount:icount),'0123456789') /= 0)THEN
      level = 3
    ELSE IF(SCAN(str_input(icount:icount),'.') /= 0)THEN
      level = 4
    ELSE
      isreal = .FALSE.
      RETURN
    END IF
  CASE(2) ! 符号を観測し、数字または小数点を求めている段階
    IF(SCAN(str_input(icount:icount),'0123456789') /= 0)THEN
      level = 3
    ELSE IF(SCAN(str_input(icount:icount),'.') /= 0)THEN
      level = 4
    ELSE
      isreal = .FALSE.
      RETURN
    END IF
  CASE(3) ! 小数点を含まない数字を観測し、数字または小数点または指数記号または数字の終了を求めている段階
    IF(SCAN(str_input(icount:icount),'0123456789') /= 0)THEN
      CONTINUE
    ELSE IF(SCAN(str_input(icount:icount),'.') /= 0)THEN
      level = 4
    ELSE IF(SCAN(str_input(icount:icount),'dDeE') /= 0)THEN
      level = 5
    ELSE
      isreal = .FALSE.
      RETURN
    END IF
  CASE(4) ! 小数点を含む数字を観測し数字または指数記号または数字の終了を求めている段階
    IF(SCAN(str_input(icount:icount),'0123456789') /= 0)THEN
      CONTINUE
    ELSE IF(SCAN(str_input(icount:icount),'dDeE') /= 0)THEN
      level = 5
    ELSE
      isreal = .FALSE.
      RETURN
    END IF
  CASE(5) ! 指数記号を観測し、指数符号または数字を求めている段階
    IF(SCAN(str_input(icount:icount),'+-') /= 0)THEN
      level = 6
    ELSE IF(SCAN(str_input(icount:icount),'0123456789') /= 0)THEN
      level = 7
    ELSE
      isreal = .FALSE.
      RETURN
    END IF
  CASE(6) ! 指数符号を観測し、数字を求めている段階
    IF(SCAN(str_input(icount:icount),'0123456789') /= 0)THEN
      level = 7
    ELSE
      isreal = .FALSE.
      RETURN
    END IF
  CASE(7) ! 指数の数字を観測し、数字または数字の終了を求めている段階
    IF(SCAN(str_input(icount:icount),'0123456789') /= 0)THEN
      CONTINUE
    ELSE
      isreal = .FALSE.
      RETURN
    END IF
  END SELECT
  icount = icount+1
  IF(icount>LEN_TRIM(str_input))EXIT
END DO
isreal = .TRUE.
RETURN
END FUNCTION
!%}}}
! LOGICAL isspace(char_input) %{{{
LOGICAL FUNCTION isspace(char_input)
IMPLICIT NONE
CHARACTER*1:: char_input
INTRINSIC:: IACHAR
INTEGER:: icc
LOGICAL:: logica
icc = IACHAR(char_input)
logica = .FALSE.
IF(icc == 32) logica = .TRUE. ! normal space
IF(icc ==  9) logica = .TRUE.  ! horizontal tab
IF(icc == 11) logica = .TRUE. ! vertical tab
IF(icc == 12) logica = .TRUE. ! form feed
! Caution!! This does not contain newline CHARACTER
isspace = logica
RETURN
END FUNCTION
!%}}}
! LOGICAL isinrange(real_input,the_min,the_max) %{{{
LOGICAL FUNCTION isinrange(real_input,the_min,the_max)
IMPLICIT NONE
DOUBLE PRECISION:: real_input, the_min, the_max
IF(real_input<the_min)THEN
  isinrange = .FALSE.
  RETURN
ELSE IF(real_input>the_max)THEN
  isinrange = .FALSE.
  RETURN
ELSE
  isinrange = .TRUE.
  RETURN
END IF
RETURN
END FUNCTION
!%}}}
! LOGICAL isinrangei(int_input,the_min,the_max) %{{{
LOGICAL FUNCTION isinrangei(int_input,the_min,the_max)
IMPLICIT NONE
INTEGER:: int_input, the_min, the_max
IF(int_input<the_min)THEN
  isinrangei = .FALSE.
  RETURN
ELSE IF(int_input>the_max)THEN
  isinrangei = .FALSE.
  RETURN
ELSE
  isinrangei = .TRUE.
  RETURN
END IF
RETURN
END FUNCTION
!%}}}

! EOF
