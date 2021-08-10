! Language    : Fortran90
! Description : source code for PSC
! Lastchange  : 2019, Jan. 22, 14:11
! Note: # of commands = 274
! =================================================================
! command naming conventions %{{{
!
! このフィールドでは各種コマンドの命名規則を記述する
!
! [1] プッシュコマンドの命名規則
!
! 実数のプッシュは標準的な実数または整数またはフラグの記述方法を用いる
!   ex.:  -3.14, 57, 1.e-10, true
! 文字列のプッシュは文字列を丸括弧で囲む記法を用いる
!   ex.:  (Hello, World!)
! 丸括弧はペアの数があっている限り多重化して記述できる
!   ex.:  (This (certainly) works well.)
! 片方の括弧のみを入れたい場合やその他の特殊文字を入れたい場合は
! アスキーコードを'/x'の後に指定する
!   ex.:  (New/x0aLine), (/x28/x28/x28/x28/x28)
! ３つの実数を同時にプッシュするHTML記法もある
!   ex.:  #00ff00
! いくつかの通常コマンドもプッシュコマンドとして動作する
!   ex.:  zero, one, white, magenta, true, pi
!
!----------------------------------------------------------
! [2] 通常コマンドの命名規則
!
! 通常コマンドの名称は小文字アルファベットのみから構成する
!
! 実数スタック操作 : 可能な限りポストスクリプトに合わせる
!   ex.:  dup, exch, pop, clear, roll, reverse
! 文字列スタック操作 : ポストスクリプトの名称の末尾にsを不可
!   ex.:  dups, exchs, pops, clears, rolls, reverses
!
! 実数関数 : 標準的な名称
!   ex.:  add, mul, modulo, pow, fact, gt, sqrt, sin, floor, not
! 文字列関数 : 標準的な名称
!   ex.:  char, ichar, length, , concat, cut, searchword
!
! 値(フラグ、文字列等を含む)を設定 : setから開始
!   ex.:  setvalue, setflag, setunit, setdigit, setfmt
! 値(フラグ、文字列等を含む)を参照 : getから開始
!   ex.:  getvalue, getflag, getunit, getdigit, getfmt
!
! 一覧の取得・表示コマンドは、要素が数値型の場合はスペース区切り、
! 文字列型の場合は丸括弧で各要素を囲む
!
! 一覧を取得 : listから開始(対象が文字列型の場合は末尾にsをつける)
!   ex.:  list, lists, listvar, listvars, listmaps, listcommands, listdirs
! 一覧を表示 : dispから開始
!   ex.:  disp, disps, dispvar, dispvars, dispmaps, dispcommands, dispdirs
!
! ループ、マップ系のコマンド : 直観的な名称を採用
!   ex.:  do, ifelse, def, exec, builtin, cycle, break
!
!----------------------------------------------------------
! [3] 記号コマンドの命名規則(例のコマンドは[2]と対応)
!
! 記号コマンドの名称は数字、アルファベット、空白を除くアスキー記号文字
! のみから構成する
!
! 実数スタック操作 : <, >, [や]を使用して非常に個性的なコマンドを構成する
!   ex.: \>, \<->, =, \><, \[], \<=>
! 文字列スタック操作 : 実数に対するコマンドの記号を重複させる
!   ex.: \>>, \<<->>, ==, \>><<, \[[]], \<<=>>
!
! 実数関数 : 基本的なものはそのまま、特殊な数学関数は\$から開始、
!            いくつかの関数は~で否定(または逆関数)
!            いくつかの関数では0を.,1を-,2を=,10を+で表す
!            三角関数は%の後に分子の辺と分母の辺のイメージを記号的に表現
!            整数化は$>#をベースにして表す
!   ex.:  +, *, %, ^, !, >, \$~=^, \$%/|, \$>_#, \~
! 文字列関数 : 文字列を表すのに"を用いる
!   ex.:  
! 値(フラグ、文字列等を含む)を設定 : \!で開始
!   ex.:  \!@>$, \!&, \!+, \!., \!_
! 値(フラグ、文字列等を含む)を参照 : \?で開始
!   ex.:  \?@>$, \?&, \?+, \?., \?_
!
! 一覧を取得 : \で開始、>>"で終了、としようかと思ったが長いので@で終了とする
!   次のように考えて統一する
!        変数               : 文字列から実数へのマップなので">$
!        文字列変数         : 文字列から文字列へのマップなので">"
!        エイリアス         : STDINからSTDINへのマップなので'>'
!        ループ、文字列実行 : 文字列からSTDINへのマップなので">'
!   これでは長くなりすぎるので間の>も可能な限り省略することにする
!
!   ex.:  \$@, \"@, \"$@, \""@, \''@, \_@, \@@, \*@
! 一覧を表示 : \で開始、>>'で終了、としようかと思ったが長いので_で終了とする
!   ex.:  \$_, \"_, \"$_, \""_, \''_, \__, \@_, \*_
!
! ループ、マップ系のコマンド : 他のコマンドとかぶらない範囲で個性的な
!                              ものを採用
!   ex.:  \?, \?:, \->, \., \.., \!, \!!
!
!%}}}
! coding conventions %{{{
!
! このプロジェクトではソースコードのほとんど全てをfortran90で記述する
! コンパイラはgfortran version 5 を想定する
! コーディングの際には可能な限りフォーマットを以下の形式に揃える
!
! fortranのキーワードは全て大文字にする
! 例：INTEGER,.TRUE.,SIN(),CALL,IF,DO,SUBROUTINE,SAVE
!
! ２つ以上の単語からなるキーワードの場合、間に１つ以上の半角スペースを挟む
! 例：END IF,ELSE IF,DOUBLE PRECISION
!
! セパレータ文字や演算子記号の前後には半角スペースを０個以上入れる
! 例：x = 8,x=8,MAXLOC( x( : ) ),MAXLOC(x(:)),CALL mysub(x,y),CALL mysub( x , y )
!
! 半角スペース（インデント以外）を一箇所に２つ以上入れるかどうかは自由とする
!
! 変数の型は基本的に以下の形式で記述する
! LOGICAL
! CHARACTER(LEN=<数>)
! INTEGER(KIND=<数>)
! DOUBLE PRECISION
! TYPE(型名)
!
! 以下は単精度整数定数の例(状況によってはこの方法は推奨されないことに注意)
! INTEGER( KIND= 4 ), PARAMETER :: psc_maxtag = 10
!
! 動的配列は可能な限り避け、代わりに配列を大きめに宣言する
! これはエラー発生時の安全性を高めると考える
!
! 変数宣言時に初期化指定を記述しても良いが、安全のため実際のコード中でも忘れずに初期化を行う
!
! 変数名等の識別子名は全て小文字アルファベットで記述して、必要の場合はアンダースコア文字で単語間を区切る
! 変数名はローカル・グローバルにかかわらず、短すぎず長すぎない程度に（３文字以上１５文字以下が理想）
! 例：xvalue,myarr,loop_counter_a
! プログラムやサブルーチン・関数等のプログラム単位に割り当てる識別子は多少長めでも良い（例えば２０文字）
! 例：myproject_stack_beautify()
!
! 基本的にこれらの識別子は意図を反映するようにする
! グローバルにする必要のない変数はローカルなプログラム単位に局在させる
! グローバルにする場合はモジュールを使用して共有するものとする(初期化に注意する)
!
! 実数の記述で精度指定子を後置する場合、小文字アルファベットを使用する。
! 例：5.4d0, 3.e0, -27.483d0
!
! 文字列は基本的にシングルクオートで囲む
! 文字列中にシングルクオートが含まれていて、それをだぶらせて記述するのが紛らわしいと考えられる場合は例外的にダブルクオートで囲んでも良いものとする
!
! インデントには半角スペースのみを使用し、基本的に一つの構造あたり空白２文字を使用する
! 基本的に１行に１つ以下の文を記述する。
! すなわちセミコロンを使用して１行に複数の文を記述することは推奨されない
! 空き行があっても良い
!
! １行が異様に長い場合（70文字程度が目安）は、継続行を使用することが推奨される
! その際、行末と次の行の行頭にアンパサンド文字を配置する
! 例：
!     current_line = very_long_line + should_be * &
!     & on_multiple_lines
!
! 基本的に演算子の直後で継続行にする
! 文字列の途中で継続行にはしない。次の例のようにする
! 例：
!     current_line = 'very_long_line' // ' should_be' // &
!     & ' on_multiple_lines'
!
! 継続行のインデント合わせは、その文の開始部分の高さに合わせる
! 例：
!     IF( ( x == 5 ) .AND. ( y == 8 ) .AND. &
!     & ( z == 7 ) )THEN
!       CALL mysub( myinput_a, myinput_b, &
!       & myinput_c )
!       WRITE(*,*) 'Hello, ' // &
!       & 'World!'
!     END IF
!
! コメントはエクスクラメーションマークから開始する
! コメントのみの行の場合、コメントのエクスクラメーション文字は周囲のインデント高さに合わせる
! 例：
!    ! initialization of x
!    x = 0
!    ! start loop from here
!    DO i = 1, 8
!      ! initialization of y
!      y = 0
!      ...
!    END DO
!
! 出力にPRINT文は使用しない：常にWRITE文を使用する
! WRITE文の書式指定と変数リストの間にはカンマを入れない
! 例: WRITE(*,'(A,I5)') 'Hello', 2018
!
!%}}}
! development history %{{{
!d(  1)= d(150208,1,'開発記録をつけ始める')
!d(  2)= d(150208,2,'history表示とundo,redo機能をつける')
!d(  3)= d(150209,1,'16色アプリケーションにする')
!d(  4)= d(150210,1,'文字列型スタックを作り入力できるようにする')
!d(  5)= d(150211,1,'ファイルに履歴を保存(ロードはまだできない)')
!d(  6)= d(150212,1,'do文による繰り返しの機能を追加(historyがおかしくなるバグあり)')
!d(  7)= d(150213,1,'doでhistoryがおかしくなるバグを修正')
!d(  8)= d(150215,1,'多重do文の実行を実装(historyがおかしくなるバグあり)')
!d(  9)= d(150220,1,'文字列型スタック操作関数の追加')
!d( 10)= d(150220,2,'多重doでhistoryのバグを修正したが、今度はdo時のundoがうまくいかない')
!d( 11)= d(150221,1,'do,undo,history関係のバグを全て修正')
!d( 12)= d(150222,1,'ファイル入出力(open,close,eol)の機能を追加')
!d( 13)= d(150225,1,'プロット機能(plot)を追加')
!d( 14)= d(150226,1,'簡易データ作成機能(makedata)を追加')
!d( 15)= d(150226,2,'このあたりでマニュアルを作成し始める')
!d( 16)= d(150308,1,'変数をモジュール化して整理')
!d( 17)= d(150308,2,'コマンドのマップ(noremap)機能を追加')
!d( 18)= d(150309,1,'256色に拡張して表示の色を調節')
!d( 19)= d(150309,2,'プログラム開始時の自動ロード機能を自分用に追加')
!d( 20)= d(150310,1,'コメントアウトの機能を追加、ファイルロード時の文字数制限の問題を発見')
!d( 21)= d(150310,2,'ファイルロードの文字数制限問題を解決')
!d( 22)= d(150311,1,'履歴移動のコマンドを増強(undoall,redoall)')
!d( 23)= d(150312,1,'open,closeをopenout,closeoutに変更、ファイルからの入力機能を追加(openin,closein)')
!d( 24)= d(150312,2,'実数の読み込み機能を修正(指数形式のEやDを認める)')
!d( 25)= d(150313,1,'文字列カウント機能の追加')
!d( 26)= d(150313,2,'stackmaxを変更可能にする')
!d( 27)= d(150313,3,'subroutineの整理と未定義コマンド判定の修正')
!d( 28)= d(150314,1,'エラー用のsubroutineを追加')
!d( 29)= d(150314,2,'倍精度ベッセル関数(besselj)を追加')
!d( 30)= d(150314,3,'オーバーフロー、アンダーフローの評価の追加')
!d( 31)= d(150325,1,'mapリストを構造型にする')
!d( 32)= d(150325,2,'noremap(VIM方式)とdef(TeX方式)を実装(edef(TeX方式)はまだ)')
!d( 33)= d(150325,3,'エラー発生時の強制終了フラグを設定、エラー管理をerrorサブルーチンにまとめる')
!d( 34)= d(150325,4,'エラー発生時の表示抑制フラグも追加')
!d( 35)= d(150326,1,'try & catchに備えてエラー番号を整理')
!d( 36)= d(150328,1,'エラーメッセージをerrmsg(:)にまとめて整理')
!d( 37)= d(150410,1,'pstartとpendによって括弧の開始符と終了符を文字列型に入れられるようにする')
!d( 38)= d(150410,2,'wformコマンドにより直接出力形式を指定できるようにする')
!d( 39)= d(150410,3,'文字列操作の実装を改良し、c1line,c1wordなどの変数をなくす')
!d( 40)= d(150411,1,'変数の宣言と代入(rem)と値の呼び出し(rec)を実装')
!d( 41)= d(150412,1,'deallocateとallocateに起因するremのバグを修正')
!d( 42)= d(150412,2,'the,showもwformによる書式指定を可能にする')
!d( 43)= d(150413,1,'ローカルレベル変数を追加し変数をレベルローカルにする')
!d( 44)= d(150415,1,'inc,dec,incunitを実装')
!d( 45)= d(150415,2,'advance = 'no'のほとんどを$に修正')
!d( 46)= d(150420,1,'do文中のundo文でexitできるようにする')
!d( 47)= d(150420,2,'ローカル変数のグルーピング挙動を修正')
!d( 48)= d(150430,1,'無限ループを実装')
!d( 49)= d(150430,2,'四捨五入による整数化を追加')
!d( 50)= d(150430,3,'index関数を追加')
!d( 51)= d(150501,1,'肥大化したコマンドリファレンスを分割して整理')
!d( 52)= d(150507,1,'pwd,home,lsの機能をシステムコールで実現')
!d( 53)= d(150509,1,'コマンドライン変数を受け取りフラグを設定できるようにする')
!d( 54)= d(150514,1,'quiet_loadフラグとsuper_quietフラグを追加')
!d( 55)= d(150514,2,'ファイル指定最速実行を可能にする')
!d( 56)= d(150524,1,'プロットサブルーチンを更新してエラー処理を連携')
!d( 57)= d(150524,2,'大量のサブルーチンを別ファイルに分離')
!d( 58)= d(150526,1,'折り畳みを少し整理')
!d( 59)= d(150603,1,'プロットルーチンのパラメータ初期化の整理に伴いプロットコマンドの増強')
!d( 60)= d(150628,1,'コマンドライン引数で直接ヘルプを参照できるように改良')
!d( 61)= d(150722,1,'バイナリ読み込み形式に変更した新しいkplot.f90と結合')
!d( 62)= d(150915,1,'組み込み関数と組み込みルーチンのintrinsic宣言を記述')
!d( 63)= d(150915,2,'組み込み関数nintとacharを導入して効率化')
!d( 64)= d(151006,1,'.eqv. を排除')
!d( 65)= d(151027,1,'想定しないコマンドラインオプションの対策')
!d( 66)= d(151027,2,'real*8からdouble precisionに修正')
!d( 67)= d(151027,3,'kplotを一回のセッションで複数回呼び出せるように改良')
!d( 68)= d(151027,4,'kplotのオプションリセットをするかどうかを制御するべき')
!d( 69)= d(151122,1,'コマンドラインに直接PSC命令を記述して実行するオプションを追加')
!d( 70)= d(151122,2,'fileexistやclear_screenなど雑多なコマンドいくつかを追加')
!d( 71)= d(151123,1,'表示色の管理を変数でまとめる')
!d( 72)= d(151123,2,'文字列をcutするコマンドを追加')
!d( 73)= d(151129,1,'プロットルーチンの軸ラベル書式変数追加に伴うコマンド追加')
!d( 74)= d(151129,2,'プロット時の色のオプションに対応するコマンドを追加')
!d( 75)= d(151223,1,'マイナス演算子として記号"_"を導入')
!d( 76)= d(151223,2,'true,false定数,neq(xor),floor,ceil,&,|の実装')
!d( 77)= d(151223,3,'現段階の不使用アスキー文字は$`\[]の５個のみ')
!d( 78)= d(151223,4,'論理演算の細かい処理修正')
!d( 79)= d(151225,1,'rgb値の色名による参照')
!d( 80)= d(151225,2,'kplotオプション変更コマンドの統合')
!d( 81)= d(151226,1,'コメントとヘルプメッセージの整理')
!d( 82)= d(151226,2,'コマンド名重複のためにexitで終了しないバグ修正')
!d( 83)= d(151226,3,'clineの宣言長さに依存する部分を修正')
!d( 84)= d(151226,4,'基本数学演算を記号コーダブルにする')
!d( 85)= d(151226,5,'コマンドとヘルプの分類を整理')
!d( 86)= d(151227,1,'積分と平均計算の実装')
!d( 87)= d(151227,2,'postscriptライクなrollとindexの実装')
!d( 88)= d(151227,3,'countコマンドをpostscriptに合わせて命名を修正し、本来のcountを実装')
!d( 89)= d(151227,4,'indexの逆にスタックを送るコマンド(send)を実装')
!d( 90)= d(151230,1,'平均計算コマンドのバグ修正')
!d( 91)= d(151230,2,'微分計算の実装')
!d( 92)= d(160101,1,'ファイル実行時に文字列中のコメント時に発生するバグを発見、解決')
!d( 93)= d(160101,2,'kplot 3D plot が実現')
!d( 94)= d(160101,3,'kplot オプションを追加')
!d( 95)= d(160101,4,'debug plot のコマンドとしての実装')
!d( 96)= d(160102,1,'文字列中の改行で文字数をとりすぎないように改良')
!d( 97)= d(160102,2,'デバグ機能の増強')
!d( 98)= d(160106,1,'ローカルな整数変数の整理')
!d( 99)= d(160108,1,'ファイル実行とスクリプト実行として実行後終了しないものを作成しそれぞれの今までのモードと両立')
!d(100)= d(160108,2,'ifelse の実装')
!d(101)= d(160116,1,'トークン抽出とセパレータ除去にscanとverifyを使用し効率化')
!d(102)= d(160116,2,'文字列のトークン処理をルーチン化')
!d(103)= d(160116,3,'文字列開始行にコメントやエスケープトークンがある場合のバグを修正')
!d(104)= d(160116,4,'HTML形式の色指定を実装')
!d(105)= d(160117,1,'アスキーコードを文字列中に直接指定できる機能を実装')
!d(106)= d(160118,1,'実数の出力を外部ルーチンに分離しlogic形式を実装')
!d(107)= d(160118,2,'varexist, mapexistの実装')
!d(108)= d(160118,3,'コマンド数200!!')
!d(109)= d(160118,4,'実数出力後に改行するかどうかをフラグ化')
!d(110)= d(160118,5,'フラグ変数名の整理')
!d(111)= d(160119,1,'一般システムコールの実装')
!d(112)= d(160119,2,'トークン処理コマンドの実装')
!d(113)= d(160120,1,'builtinコマンドの実装')
!d(114)= d(160120,2,'map系コマンドのremapに関するバグ修正と改良')
!d(115)= d(160121,1,'ループ実装を再構成しcycle,breakを一般化')
!d(116)= d(160121,2,'履歴の凍結をフラグ化しexecとsetを分離')
!d(117)= d(160121,3,'nowを文字列取得するように改良')
!d(118)= d(160121,4,'stopの実装')
!d(119)= d(160121,5,'psc_printnumに指数形式にも対応したcutoff_extra_zeroを実装')
!d(120)= d(160121,6,'psc_printnumの出力形式を科学、工学、hex, bin, oct, e, dに対応するように拡張')
!d(121)= d(160126,1,'ガンマ関数')
!d(122)= d(160206,2,'findzeropointマクロの実装')
!d(123)= d(160206,3,'文字列比較コマンドの実装・postscript get, putの実装')
!d(124)= d(160206,4,'intとnint, modとmoduloを分離')
!d(125)= d(160208,1,'mapとvarのリスト取得コマンド、削除コマンドの実装')
!d(126)= d(160314,1,'アプリ終了時にdeallocateするように修正')
!d(127)= d(160314,2,'素数計算コマンド(primelist, prime)の実装')
!d(128)= d(160314,3,'装置番号の取得部分を一般化し複数ファイルを扱えるように改良')
!d(129)= d(160314,4,'ソースコードのincludeを多重化できるように改良')
!d(130)= d(160314,5,'kplot連携時にfort.11などが生成されるバグ発生、修正')
!d(131)= d(160315,1,'sepstrを設定できるコマンドを追加')
!d(132)= d(160316,1,'コマンドライン引数受け取り処理部分のソース重複を改良')
!d(133)= d(160326,1,'kplotルーチンの改良にともないmultiplotコマンドを実装')
!d(134)= d(160326,2,'kplotバグ修正に伴う修正')
!d(125)= d(160327,1,'isrealコマンドの実装')
!d(126)= d(160328,1,'スタック操作ルーチンを配列演算を使用した実装に改良')
!d(127)= d(160328,2,'基本文字列長をparameterにする')
!d(128)= d(160329,1,'エラーメッセージの参照・変更機能の追加')
!d(129)= d(160329,2,'文字列の色付けおよびxxdライクな変換機能の追加')
!d(130)= d(160401,1,'gethistコマンドの実装')
!d(131)= d(160419,1,'exinclude, returnの実装')
!d(132)= d(160419,2,'include,exinclude,returnを使用したループの実装に成功')
!d(133)= d(160425,1,'サブルーチン名の整理')
!d(134)= d(160425,2,'ローマ数字出力に対応')
!d(135)= d(160425,3,'kplotのstylecolor変更コマンドを改良')
!d(136)= d(160425,4,'cline変数名をbufinに変更し関連部も修正')
!d(137)= d(160425,5,'モジュールと本体を別ファイルに分離')
!d(138)= d(160425,6,'kplot, kdrawも全く同様に分離し整理')
!d(139)= d(160427,1,'holdbuffer(:)を実装しロード機能をより正確にする')
!d(140)= d(160428,1,'多重オープンにlinesコマンドを対応')
!d(141)= d(160428,2,'文字列系マクロコマンドの改良')
!d(142)= d(160428,3,'エラー発生時にファイル名と行番号を表示する機能の実装')
!d(143)= d(160428,4,'数学マクロ関数の整理')
!d(144)= d(160428,5,'debug_bufferモードを細かい個別モードに分割')
!d(145)= d(160503,1,'従来のlist系コマンド名称をdispに統一し対応文字列化コマンドをlistに統一')
!d(146)= d(160503,2,'set系コマンドとget系コマンドで統一し記号コマンドの記法も!と?に統一')
!d(147)= d(160503,3,'記号コマンドの強化')
!d(148)= d(160504,1,'大規模version upの開発計画を立て始める')
!d(149)= d(160504,2,'kplotオプション参照機能の実装')
!d(150)= d(160505,1,'プロンプト文字列を書式付変数にしてset,get可能にする')
!d(151)= d(160505,2,'ANSI色指定を整数配列に変更し色設定をルーチン化')
!d(152)= d(160506,1,'afterwriteフラグをstackchangedに改名しhist_appendルーチン内の処理に変更')
!d(153)= d(160506,2,'use_newlineフラグとuse_historyフラグを改名し対応するコマンドも修正')
!d(154)= d(160506,3,'フラグをfalseにするコマンドライン引数をoff_~の形式に統一')
!d(155)= d(160507,1,'コメント、文字列、記号コマンド内の改行文字の取り扱いを変更')
!d(156)= d(160507,2,'ループにおける-365の無限ループ機能を中止して新たに無限ループを実装しなおす')
!d(157)= d(160511,1,'変数を$で参照できるようにする')
!d(158)= d(160513,1,'load、input、outputファイルリストの参照取得コマンドの実装')
!d(159)= d(160513,2,'36以下の任意の基数で整数を出力できるように改良')
!d(160)= d(160514,1,'空白文字を中心に文字列の扱いを改善')
!d(161)= d(160517,1,'mdとfzpのバグ修正')
!d(162)= d(160518,1,'version1.0.3開発開始')
!d(163)= d(160519,1,'long-ifをサブルーチンとして分離')
!d(165)= d(160523,1,'スタック・コマンド履歴・マップ・変数をallocatableでなく形状固定に変更')
!d(164)= d(160531,1,'その他コマンド群、スタック操作コマンド群、数学コマンド群、ディスプレイコマンド群、ヘルプコマンド群、ファイル操作コマンド群、文字列操作コマンド群のID登録完了')
!d(165)= d(170120,1,'コマンド判定のLONG-IFの工事完了')
!d(166)= d(170123,1,'kplotのバーのticsをyticsとは別に指定できるように修正')
!d(167)= d(170126,1,'kplotのlabel関係オプションの設定コマンドを追加')
!d(168)= d(170202,1,'erf関数を実装')
!d(169)= d(170204,1,'kplot関係の更新')
!d(170)= d(170205,1,'0を含む乗除の判定を修正')
!d(171)= d(170206,1,'バグ修正')
!d(172)= d(170211,1,'kplot関係の更新')
!d(173)= d(170301,1,'cmdhelp(コマンドの個別ヘルプ)の実装')
!d(174)= d(170303,1,'個別ヘルプ文字列の整理')
!d(175)= d(170304,1,'個別ヘルプ文字列の整理')
!d(176)= d(170308,1,'個別ヘルプ文字列の整理')
!d(177)= d(170308,2,'個別ヘルプ文字列の整理')
!d(178)= d(170310,1,'version1.5開発開始')
!d(179)= d(170310,2,'履歴機能の廃止')
!d(180)= d(170310,2,'kdraw修正')
!d(181)= d(170312,1,'kplot関係のヘルプメッセージの修正')
!d(182)= d(170312,2,'kplot、p_mode==5実装')
!d(183)= d(170312,3,'kplot関係の修正')
!d(184)= d(170322,1,'version1.6開発開始')
!d(185)= d(170323,1,'stack(:)配列の格納順序を逆向きに変更')
!d(186)= d(170323,2,'文字列の逆向き検索の代わりにreverseletterコマンドを実装')
!d(187)= d(170324,1,'mergeコマンドの実装')
!d(188)= d(170326,1,'kplot修正')
!d(189)= d(170406,1,'リファレンス更新に伴う修正')
!d(190)= d(170413,1,'mergesの実装')
!d(191)= d(170424,1,'kplot修正')
!d(192)= d(170509,1,'kplot修正')
!d(193)= d(170511,1,'kplot修正')
!d(194)= d(170516,1,'saveコマンドの修正')
!d(195)= d(170517,1,'kplot修正')
!d(196)= d(170517,2,'kplot修正')
!d(197)= d(170520,1,'kplot修正')
!d(198)= d(170523,1,'version1.7開発開始')
!d(199)= d(170531,1,'kplot修正')
!d(200)= d(170601,1,'kplot修正')
!d(201)= d(170605,1,'kplot修正')
!d(202)= d(170608,1,'グラフ関係のヘルプメッセージの書式変更')
!d(203)= d(170608,2,'kplot修正')
!d(204)= d(170609,1,'setkp,getkp系コマンドのヘルプメッセージの実装')
!d(205)= d(170609,2,'debug関係のヘルプの整備')
!d(206)= d(170610,1,'コーディングフォーマットの整備と明文化')
!d(207)= d(170612,1,'kplot修正')
!d(208)= d(170614,1,'kplot修正')
!d(209)= d(170616,1,'kplot修正')
!d(210)= d(170619,1,'kplot修正')
!d(211)= d(170620,1,'kplot修正')
!d(212)= d(170623,1,'kplot修正')
!d(213)= d(170626,1,'kplot修正')
!d(214)= d(170628,1,'kplot修正')
!d(215)= d(170629,1,'サブルーチンの整備')
!d(216)= d(170629,2,'version1.8開発開始')
!d(217)= d(170702,1,'kdraw大規模修正')
!d(218)= d(170705,1,'kdraw修正')
!d(219)= d(170705,2,'kplot修正')
!d(220)= d(170801,1,'kdraw修正')
!d(221)= d(170801,2,'スタック割り当てメモリサイズ調整')
!d(222)= d(170802,1,'kdraw修正')
!d(223)= d(170807,1,'kdraw修正')
!d(224)= d(170828,1,'kdraw修正')
!d(225)= d(170904,1,'kdraw修正')
!d(226)= d(170907,1,'kdraw修正')
!d(227)= d(171113,1,'kplot修正')
!d(228)= d(171121,1,'グラフ関係のヘルプを細分化')
!d(229)= d(171122,1,'kplot修正')
!d(230)= d(171124,1,'kplot修正')
!d(231)= d(171128,1,'kplot修正')
!d(232)= d(171128,2,'version1.9開発開始')
!d(233)= d(171130,1,'kplot修正')
!d(234)= d(171212,1,'kplot修正')
!d(235)= d(171215,1,'kplot修正')
!d(236)= d(171218,1,'kplot修正')
!d(237)= d(171220,1,'コーディング規定を少し変更')
!d(238)= d(180116,1,'kplot修正')
!d(239)= d(180213,1,'kplot修正')
!d(240)= d(180309,1,'version1.a開発開始')
!d(241)= d(180309,2,'ルーチン構造の整理')
!d(242)= d(180309,3,'ハイフン文字から始まるファイル名およびスクリプトを指定可能にする')
!d(243)= d(180310,1,"kdraw修正")
!d(243)= d(180311,1,"kdraw修正")
!d(244)= d(180314,1,"kplot修正")
!d(245)= d(180315,1,"kdraw修正")
!d(246)= d(180316,1,"kplot修正")
!d(247)= d(180321,1,"kplot修正")
!d(248)= d(180404,1,"cmdhelp機能の強化：usage_commによってusageの説明を簡明化")
!d(249)= d(180409,1,"setkpoptの改修工事開始")
!d(250)= d(180517,1,"kplot更新")
!d(251)= d(180517,2,'version1.b開発開始')
!d(252)= d(180529,1,'setkpmultioptの実装')
!d(253)= d(180601,1,'文字列コマンドのバックスラッシュ記法を条件付きで導入')
!d(254)= d(180604,1,'version1.c開発開始')
!d(255)= d(180604,2,'kplotのルーチン整理開始')
!d(256)= d(180629,1,'kplot修正')
!d(254)= d(180629,2,'version1.d開発開始')
!d(255)= d(180704,1,'kplot修正')
!d(256)= d(180710,1,'kplot修正')
!d(257)= d(180717,1,'kplot修正')
!d(258)= d(180718,1,'kplot修正')
!d(259)= d(180720,1,'kplot修正')
!d(254)= d(180720,2,'version1.e開発開始')
!d(255)= d(180723,1,'kplot修正')
!d(256)= d(180731,1,'kplot修正')
!d(257)= d(180803,1,'kplot修正')
!d(258)= d(180816,1,'kplot修正')
!d(259)= d(180823,1,'kplot修正')
!d(260)= d(181116,1,'kplotインターフェースにstylepoint2オプションを追加')
!d(261)= d(181225,1,'ヘルプの調整')
!%}}}
! ideas %{{{
!
! 素数判定
!
!文字列スタックの格納向きも実数型同様逆向きにすべき
!
!residue, uglifyなど
!
!単位変換
!
!文字列操作コマンドの増強
!  -> srcw, srcc, cntw, cntc, cntr, getc, setc, getw, setw
!  backward_searchも
!
!スタックに「キープ領域」を用意してローカルにスタック操作がしやすいようにしては？
!
!文字集合コマンド
!  -> digit, alnum, symbol (or mark), hex, caphex, nocaphep, letter, capletter, nocapletter, remainder
!  [0-9a-zA-Z_]の形式をサポートしたほうが良いかも
!
!debug_includeモードの実装
!
!dupやpopはpostscript固有のものでわかりにくい
!copyやprint,writeを使って他のコマンド群と統一する
!
!ヘルプを文字列化してdispコマンドとget、setコマンドに分ける
!
!ループを文字列処理による実装でなくして効率化(整数変数でループ回数を記憶する)
!
!SYSTEM_~系のシステムコマンド文字列を変数に入れて管理
!
!関数最小化マクロ(mmf, minimizefunc)
!
!楕円積分
!
!スプライン積分マクロ
!
!文字列を記憶する変数の実装 -> version 2 で実装予定
!
!文字列の置換機能
!
!コマンド名に一貫性が弱いのでは? ;
!  例えば'string ls'や'(n) real var get'のように複数のトークンでコマンドを構成するのはどうか?
!  そうすれば「フラグスタック」や「整数スタック」のようなものも気軽に実装できる
!  さらには「ユーザー定義構造型スタック」のような実装の可能性もある
!
!try & catch による制御機能 -> version 2 に盛り込むか検討中
!「成功フラグ」を実装すべきか? -> version 2 に盛り込むか検討中
!
!正規表現によるパターンマッチング -> 難しい
!
!スタックの実装は新しい方を配列末尾方向に置いた方が効率が良いのでは？ -> version 2 で実装予定
!
!Nan, Infを許した計算の実装 -> どうするか・・・
!
!複素数計算 -> version 2で実装予定
!
!%}}}

!----------------------------------------------------------------
!-- MAIN --------------------------------------------------------
!----------------------------------------------------------------
! psc %{{{
PROGRAM psc
! local variables %{{{
USE cv_psc
IMPLICIT NONE
LOGICAL, EXTERNAL :: psc_isreal
INTEGER( KIND = 4 ), EXTERNAL :: psc_free_unit
INTEGER( KIND = 4 ) :: idoa
INTEGER( KIND = 4 ) :: itmpa
CHARACTER( LEN = psc_strlen ) :: stmpa
LOGICAL              :: ltmpa
!%}}}
CALL psc_init
CALL psc_read_cmdline
CALL psc_set_load
DO
  ! read current buffer (=bufin) %{{{
  IF( debug_loop .OR. ( INDEX( bufin, 'SYSTEM_' ) == 0 )  )THEN
    IF( debug_buffer_start) WRITE(*,*) 'bufin:start:=' // TRIM(bufin)
  END IF
  IF( ( LEN_TRIM( bufin ) == 0 )  )THEN
    IF( script_exec.AND.(.NOT.file_pexe))STOP
    IF( script_pexe.AND.file_exec )THEN
      ! script_pexe と file_exec 同時指定の対策
      script_pexe = .FALSE.
      INQUIRE(FILE=TRIM(dev_ld(1)%tag), EXIST = ltmpa)
      IF( ltmpa )THEN
        ! Setting File Found
        IF( (.NOT.quiet_load).AND.(.NOT.super_quiet) )THEN
          WRITE(*,*) 'Setting file '''//TRIM(dev_ld(1)%tag)//''' found'
          WRITE(*,*) 'Loading '''//TRIM(dev_ld(1)%tag)//''''
        END IF
      ELSE
        ! Setting File Not Found --> don't load
        ildnow = 0
      END IF
    END IF
    CALL psc_buf_readnext
    IF( debug_buffer_read) WRITE(*,*) 'bufin:read :=' // TRIM( bufin )
  END IF
  !%}}}
  ! determine token length (=wlen) %{{{
  ! remove separators at bufin beginning
  CALL psc_buf_rmtopsep

  IF( debug_loop .OR. ( INDEX( bufin, 'SYSTEM_' ) == 0 )  )THEN
    IF( debug_buffer_clean ) WRITE(*,*) 'bufin:clean:=' // TRIM( bufin )
  END IF

  IF( bufin == '' ) CYCLE
  IF( bufin(1:1) == ')'  )THEN
    WRITE(0,*) 'Error: too many end-parenthesis'
    bufin = ''
    CYCLE
  END IF

  CALL psc_buf_gettoklen
  !%}}}
  ! string operation (read and append to stack) %{{{
  IF( bufin(1:1) == '('  )THEN
    DO
      CALL psc_buf_getstrlen
      IF( wlen>0 )THEN
        ! push macro(:) and save new macro(1)
        CALL psc_strst_append
        macro(1) = bufin(2:wlen)
        bufin = bufin(wlen+1:)
        CALL psc_buf_rmtopsep
        !
        CALL transform_hex_chars(macro(1), wlen)
        IF( INDEX( bufin, 'SYSTEM_' ) == 0 .OR. debug_loop )THEN
          IF( debug_commd ) WRITE(*,'(A)') ' commd: string: ('//TRIM(macro(1))
          IF( debug_stack )THEN
            DO idoa = 1,  macrosize
              WRITE(*,'(A$)') ' (' // TRIM(macro(idoa))
            END DO
            WRITE(*,*)
          END IF
        END IF
        EXIT
      ELSE IF( wlen < 0 )THEN
        ! string dose not end
        IF( INDEX( bufin, 'SYSTEM_' ) == 0 .OR. debug_loop )THEN
          IF( debug_level ) WRITE(*,'(A,I5)') ' level:eol  :=', -wlen
        END IF
        stmpa = TRIM(bufin)
        CALL psc_buf_readnext
        bufin = TRIM(stmpa) // ACHAR(10) // TRIM(ADJUSTL(bufin))
        CYCLE
      END IF
    END DO
    CYCLE
  END IF
  !%}}}
  ! some rest operation (cmd and bufin) %{{{

  ! restore first word into cmd
  cmd = bufin(1:wlen) // ')'

  ! delete first word
  bufin(1:wlen) = ''
  bufin = ADJUSTL(bufin)
  IF( debug_loop.OR.(INDEX(bufin, 'SYSTEM_')==0) )THEN
    IF( debug_buffer_trim) WRITE(*,*) 'bufin:trim :='//TRIM(bufin)
  END IF

  ! delete all separators at line-beginning
  CALL psc_buf_rmtopsep
  IF( debug_loop.OR.(INDEX(bufin, 'SYSTEM_')==0) )THEN
    IF( debug_buffer_clean) WRITE(*,*) 'bufin:clean:='//TRIM(bufin)
  END IF
  !%}}}
  ! mapping %{{{
  IF( INDEX(bufin, 'SYSTEM_END_MAP')==0 )THEN
    ltmpa = .FALSE.
    DO idoa = 1,  mapsize
      IF( cmd == map(idoa)%lhs )THEN
        cmd = TRIM(map(idoa)%rhs)
        IF( debug_buffer_map) WRITE(*,*) 'bufin:map  :='//TRIM(bufin)

        stmpa = cmd
        itmpa = LEN_TRIM( stmpa )
        IF( map(idoa)%no )THEN
          bufin = stmpa(1:itmpa-1) // '\\SYSTEM_END_MAP\\' // TRIM(bufin)
        ELSE
          bufin = stmpa(1:itmpa-1) // '\\' // TRIM(bufin)
        END IF
        ltmpa = .TRUE.
        EXIT    ! 一つでも該当したらただちにexit (remapしないため)
      END IF
    END DO
    IF( ltmpa ) CYCLE   ! 再マップしないために必要
  END IF
  !%}}}
  cword = TRIM(cmd)
  cword = cword(1:LEN_TRIM(cword)-1)
  IF( debug_loop.OR.(INDEX(bufin, 'SYSTEM_')==0) )THEN
    IF( debug_commd)WRITE(*,'(A)') ' commd: command: '//TRIM(cword)
  END IF
  !---------------------------------------------------------
  CALL psc_main()
  ! display stack-list %{{{
  IF( .NOT. stackchanged )THEN
    ! スタックに変化がないならば、絶対に出力しない
    CYCLE
  END IF
  IF( (.NOT. debug_loop) .AND. (INDEX( bufin, 'SYSTEM_') > 0 .OR. ifloop) )THEN
    ! ループ(またはマクロ)の内部ならば、debug_loopでない限りは絶対に出力しない
    ! この条件はquietフラグやquiet_loadフラグの指定よりも強いことに注意
    CYCLE
  END IF
  IF( debug_stack )THEN
    CALL psc_dispstacklist
    CYCLE
    ! ここのCYCLEに注意：これがないと次のIFと両方満たされた時に二重に出力されてしまう
  END IF
  IF( (file_exec).OR.(file_pexe).OR.(script_exec).OR.(script_pexe) )THEN
    IF( .NOT. quiet_load )THEN
      CALL psc_dispstacklist
    END IF
  ELSE
    IF( .NOT. quiet )THEN
      CALL psc_dispstacklist
    END IF
  END IF
  !%}}}
END DO
END PROGRAM
!%}}}

! psc_init %{{{
SUBROUTINE psc_init
USE cv_psc
IMPLICIT NONE
INTEGER( KIND = 4 ), EXTERNAL :: psc_free_unit
CHARACTER (LEN=16), EXTERNAL :: psc_getintstr
INTEGER( KIND = 4 ) :: idoa
quiet   = .TRUE.   ! q, Q
color   = .FALSE.  ! c, C
autonl  = .TRUE.  ! n, N
!
quiet_load  = .TRUE.
error_cont  = .FALSE.
error_quiet = .FALSE.
super_quiet = .FALSE. ! s, S
!
debug_buffer_start = .FALSE. ! d, D
debug_buffer_read  = .FALSE. ! d, D
debug_buffer_clean = .FALSE. ! d, D
debug_buffer_map   = .FALSE. ! d, D
debug_buffer_trim  = .FALSE. ! d, D
debug_buffer_last  = .FALSE. ! d, D
debug_buffer_do    = .FALSE. ! d, D
debug_loop         = .FALSE.  ! d, D
debug_level        = .FALSE.  ! d, D
debug_commd        = .FALSE.  ! d, D
debug_stack        = .FALSE.  ! d, D
!
file_exec   = .FALSE.  ! f
script_exec = .FALSE.  ! e
file_pexe   = .FALSE.  ! F
script_pexe = .FALSE.  ! E
!
decimals = 0
fmt_a = '(f)'
sepstr = ' )'
prompt_normal  = 'PSC</r;/s>'
prompt_special = '>'
cl_reset        = (/  -1,  -1,  -1,  -1,  -1,  -1 /)
cl_prompt       = (/   0, 255,   0,  -1,  -1,  -1 /) ! green
cl_histnum      = (/ 150, 150, 255,  -1,  -1,  -1 /) ! blue
cl_histcom      = (/   0, 255, 255,  -1,  -1,  -1 /) ! cyan
cl_histstack    = (/  -1,  -1,  -1,  -1,  -1,  -1 /)
cl_loadcom      = (/ 150, 150, 255,  -1,  -1,  -1 /) ! blue
cl_listcom      = (/   0, 255, 255,  -1,  -1,  -1 /) ! cyan
cl_liststack    = (/  -1,  -1,  -1,  -1,  -1,  -1 /)
cl_helptitle    = (/ 255, 255,   0,  -1,  -1,  -1 /) ! yellow
cl_helpcontents = (/ 255, 150,  50,  -1,  -1,  -1 /) ! orange
cl_error        = (/ 255,  80,  50,  -1,  -1,  -1 /) ! red
cl_fatalerror   = (/  -1,  -1,  -1, 200,   0,   0 /) ! red bg
! ===============================================
!   default ANSI color in mintty on cygwin-64 (My rough guess)
! red    : 31;1     =~ 38;2;255; 80; 50
! green  : 32;1     =~ 38;2;  0;255;  0
! yellow : 33;1     =~ 38;2;255;255;  0
! blue   : 34;1     =~ 38;2;100;100;255
! purple : 35;1     =~ 38;2;255;110;255  =~ 38;5;207
! cyan   : 36;1     =~ 38;2;  0;255;255
! orange :             38;2;200;150;  0  =~ 38;5;172 
! ===============================================
incunit = 1.d0
IF( decimals > 0 )THEN
  fmt_a = '(G' // &
  & TRIM(ADJUSTL(psc_getintstr(10+decimals))) // '.' // &
  & TRIM(ADJUSTL(psc_getintstr(decimals))) // ')'
END IF
stack(:)  = 0.d0
stz = 0
macro(:)  = ')'
cmd    = ')'
macrosize = 0
mapsize = 0
varsize = 0
loclev = 0
iinnow = 0
iounow = 0
dev_in(:) = (/ (dev_stdin, idoa = 0,100) /)
dev_ou(:) = (/ (dev_stdou, idoa = 0,100) /)
dev_ld(:) = (/ (dev_stdin, idoa = 0,100) /)
dev_ld(1) = device( psc_free_unit(), '.pscrc' )
ildnow = 1
bufin = ''
holdbuffer(:) = ''
codepoint(:) = 0
exec_mode = 0
ifloop = .FALSE.
errmsg(:) = ''
errmsg( 1) = 'Unknown command'
errmsg( 2) = 'Empty stack'
errmsg( 3) = 'Insufficient number of stack'
errmsg( 4) = 'Empty string-stack'
errmsg( 5) = 'Insufficient number of string-stack'
errmsg( 6) = 'Insufficient stack allocation (Need to be reallocated)'
errmsg( 7) = 'Insufficient string-stack allocation (Need to be reallocated)'
errmsg( 8) = 'Stack has some problem (Maybe in its range)'
errmsg( 9) = 'String-stack has some problem (Maybe has wrong syntax)'
errmsg(10) = 'Requested file not found'
errmsg(11) = 'Filenames for input and output must differ'
errmsg(12) = 'End of file'
errmsg(13) = 'No file currently opened'
errmsg(14) = 'Overflow'
errmsg(15) = 'Wrong syntax'
errmsg(16) = 'Unknown variable'
errmsg(17) = 'Unknown map'
errmsg(18) = 'History is not available here'
errmsg(19) = 'Prime-numbers-list is not allocated yet'
errmsg(20) = 'Allocation of prime-numbers-list is too small'
errmsg(21) = 'No device opened for load yet'
errmsg(22) = 'Requested variable not initialized yet'
errmsg(23) = 'No device opened for input yet'
errmsg(24) = 'No device opened for output yet'
CALL kplot_initializer
CALL random_seed_initializer
RETURN
END SUBROUTINE
!%}}}
! psc_read_cmdline %{{{
SUBROUTINE psc_read_cmdline
USE cv_psc
IMPLICIT NONE
INTEGER( KIND = 4 ), EXTERNAL :: psc_free_unit
INTEGER( KIND = 4 ) :: numarg
INTEGER( KIND = 4 ) :: iarg
INTEGER( KIND = 4 ) :: itmpc
CHARACTER( LEN = psc_strlen ) :: argstr
CHARACTER( LEN = 1 ) :: cchar
numarg = IARGC()
IF( numarg == 0 ) RETURN
iarg = 1
itmpc = 0
DO
  IF( iarg > numarg ) EXIT
  CALL GETARG(iarg, argstr)
  argstr = ADJUSTL(argstr)
  IF( argstr(1:1)=='-' .AND. itmpc == 0 )THEN
    argstr = argstr(2:)
    DO
      IF( LEN_TRIM(argstr)==0 )EXIT
      cchar = argstr(1:1)
      itmpc = 0
      IF( (argstr == '-help').OR.(cchar == 'h') )THEN
        super_quiet = .TRUE.
        bufin = 'help,quit'
      ELSE IF( (argstr == '-version').OR.(cchar == 'v') )THEN
        super_quiet = .TRUE.
        bufin = 'version,quit'
      ! ------------------------------------------------
      ELSE IF( (argstr == '-quiet').OR.(cchar == 'q') )THEN
        quiet      = .TRUE.
        quiet_load = .TRUE.
      ELSE IF( (argstr == '-off_quiet').OR.(cchar == 'Q') )THEN
        quiet      = .FALSE.
        quiet_load = .FALSE.
      ELSE IF( (argstr == '-superquiet').OR.(cchar == 's') )THEN
        super_quiet = .TRUE.
      ELSE IF( (argstr == '-off_superquiet').OR.(cchar == 'S') )THEN
        super_quiet = .FALSE.
      ELSE IF( (argstr == '-color').OR.(cchar == 'c') )THEN
        color = .TRUE.
      ELSE IF( (argstr == '-off_color').OR.(cchar == 'C') )THEN
        color = .FALSE.
      ELSE IF( (argstr == '-autonl').OR.(cchar == 'n') )THEN
        autonl = .TRUE.
      ELSE IF( (argstr == '-off_autonl').OR.(cchar == 'N') )THEN
        autonl = .FALSE.
      ! ------------------------------------------------
      ELSE IF( (argstr == '-debug').OR.(cchar == 'd') )THEN
        debug_buffer_start = .TRUE.
        debug_buffer_read  = .TRUE.
        debug_buffer_clean = .TRUE.
        debug_buffer_map   = .TRUE.
        debug_buffer_trim  = .TRUE.
        debug_buffer_last  = .TRUE.
        debug_buffer_do    = .TRUE.
        debug_loop = .TRUE.
        debug_level = .TRUE.
        debug_commd = .TRUE.
        debug_stack = .TRUE.
      ! ------------------------------------------------
      ELSE IF( argstr == '-debug_buffer' )THEN
        debug_buffer_start = .TRUE.
        debug_buffer_read  = .TRUE.
        debug_buffer_clean = .TRUE.
        debug_buffer_map   = .TRUE.
        debug_buffer_trim  = .TRUE.
        debug_buffer_last  = .TRUE.
        debug_buffer_do    = .TRUE.
      ELSE IF( argstr == '-debug_buffer_start' )THEN
        debug_buffer_start = .TRUE.
      ELSE IF( argstr == '-debug_buffer_read' )THEN
        debug_buffer_read = .TRUE.
      ELSE IF( argstr == '-debug_buffer_clean' )THEN
        debug_buffer_clean = .TRUE.
      ELSE IF( argstr == '-debug_buffer_map' )THEN
        debug_buffer_map = .TRUE.
      ELSE IF( argstr == '-debug_buffer_trim' )THEN
        debug_buffer_trim = .TRUE.
      ELSE IF( argstr == '-debug_buffer_last' )THEN
        debug_buffer_last = .TRUE.
      ELSE IF( argstr == '-debug_buffer_do' )THEN
        debug_buffer_do = .TRUE.
      ! ------------------------------------------------
      ELSE IF( argstr == '-debug_loop' )THEN
        debug_loop = .TRUE.
      ELSE IF( argstr == '-debug_level' )THEN
        debug_level = .TRUE.
      ELSE IF( argstr == '-debug_commd' )THEN
        debug_commd = .TRUE.
      ELSE IF( argstr == '-debug_stack' )THEN
        debug_stack = .TRUE.
      ELSE IF( (argstr == '-off_debug').OR.(cchar == 'D') )THEN
        debug_buffer_start = .FALSE.
        debug_buffer_read = .FALSE.
        debug_buffer_clean = .FALSE.
        debug_buffer_map = .FALSE.
        debug_buffer_trim = .FALSE.
        debug_buffer_last = .FALSE.
        debug_buffer_do = .FALSE.
        debug_level = .FALSE.
        debug_commd = .FALSE.
        debug_stack = .FALSE.
        super_quiet = .FALSE.
      ! ------------------------------------------------
      ELSE IF( argstr == '-off_debug_buffer' )THEN
        debug_buffer_start = .FALSE.
        debug_buffer_read = .FALSE.
        debug_buffer_clean = .FALSE.
        debug_buffer_map = .FALSE.
        debug_buffer_trim = .FALSE.
        debug_buffer_last = .FALSE.
        debug_buffer_do = .FALSE.
      ELSE IF( argstr == '-off_debug_buffer_start' )THEN
        debug_buffer_start = .FALSE.
      ELSE IF( argstr == '-off_debug_buffer_read' )THEN
        debug_buffer_read = .FALSE.
      ELSE IF( argstr == '-off_debug_buffer_clean' )THEN
        debug_buffer_clean = .FALSE.
      ELSE IF( argstr == '-off_debug_buffer_map' )THEN
        debug_buffer_map = .FALSE.
      ELSE IF( argstr == '-off_debug_buffer_trim' )THEN
        debug_buffer_trim = .FALSE.
      ELSE IF( argstr == '-off_debug_buffer_last' )THEN
        debug_buffer_last = .FALSE.
      ELSE IF( argstr == '-off_debug_buffer_do' )THEN
        debug_buffer_do = .FALSE.
      ! ------------------------------------------------
      ELSE IF( argstr == '-off_debug_loop' )THEN
        debug_loop = .FALSE.
      ELSE IF( argstr == '-off_debug_level' )THEN
        debug_level = .FALSE.
      ELSE IF( argstr == '-off_debug_commd' )THEN
        debug_commd = .FALSE.
      ELSE IF( argstr == '-off_debug_stack' )THEN
        debug_stack = .FALSE.
      ! ------------------------------------------------
      ELSE IF( (argstr == '-file').OR.(argstr == 'f') )THEN
        ! file実行とscript実行は両立しないようにする
        ! file と fileprev は両立しない
        ! script と scriptprev も両立しない
        ! ただし(file, scriptprev) や (script, fileprev) は両立する
        itmpc = 1
        script_exec = .FALSE.
        file_pexe = .FALSE.
        file_exec = .TRUE.
      ELSE IF( (argstr == '-script').OR.(cchar == 'e') )THEN
        itmpc = 2
        file_exec = .FALSE.
        script_pexe = .FALSE.
        script_exec = .TRUE.
      ELSE IF( (argstr == '-FILE').OR.(cchar == 'F') )THEN
        itmpc = 1
        script_pexe = .FALSE.
        file_exec = .FALSE.
        file_pexe = .TRUE.
      ELSE IF( (argstr == '-SCRIPT').OR.(cchar == 'E') )THEN
        itmpc = 2
        file_pexe = .FALSE.
        script_exec = .FALSE.
        script_pexe = .TRUE.
      ELSE
        WRITE(*,'(A)') 'Unknown PSC option ''-'//TRIM(argstr)//''''
        WRITE(*,'(A)') 'Use ''-h'' or ''--help'' option for more help'
        STOP
      END IF
      IF( argstr(1:1)=='-') EXIT
      argstr = argstr(2:)
    END DO
  ELSE IF( itmpc == 2 )THEN
    script = TRIM(ADJUSTL(argstr))
    itmpc = 0
  ELSE IF( itmpc == 1 .OR. itmpc == 0 )THEN
    ! file specification
    ! this will stop searching for .pscrc file
    dev_ld(1) = device(psc_free_unit(), TRIM(ADJUSTL(argstr)))
    ildnow = 1
    itmpc = 0
  END IF
  iarg = iarg + 1
END DO
RETURN
END SUBROUTINE
!%}}}
! psc_set_load %{{{
SUBROUTINE psc_set_load
USE cv_psc
IMPLICIT NONE
LOGICAL :: ltmpa
IF( .NOT.file_pexe )THEN
  IF( script_exec.OR.script_pexe )THEN
    bufin = script
  END IF
END IF
IF( .NOT.script_pexe )THEN
  INQUIRE(FILE=TRIM(dev_ld(1)%tag), EXIST = ltmpa)
  IF( ltmpa )THEN
    ! Setting File Found
    OPEN(dev_ld(1)%devnum, FILE=TRIM(dev_ld(1)%tag), STATUS= 'old')
  ELSE
    ! Setting File Not Found -> don't load
    dev_ld(1)=device(0, '')
    ildnow = 0
  END IF
END IF
RETURN
END SUBROUTINE
!%}}}

!----------------------------------------------------------------
!-- SUBROUTINES -------------------------------------------------
!----------------------------------------------------------------
SUBROUTINE psc_main()
! declare %{{{
USE cv_psc
IMPLICIT NONE
LOGICAL, EXTERNAL :: psc_isreal
INTEGER( KIND= 4 ), EXTERNAL :: get_commandmode
INTEGER( KIND = 4 ), EXTERNAL :: psc_free_unit
INTEGER( KIND = 4 ) :: commandmode
INTEGER( KIND = 4 ) :: ierror
INTEGER( KIND = 4 ) :: idoa, idob, idoc
INTEGER( KIND = 4 ) :: itmpa, itmpb, itmpc, itmpd, itmpe
DOUBLE PRECISION    :: rtmpa, rtmpb, rtmpc
CHARACTER( LEN = psc_strlen ) :: stmpa, stmpb, stmpc, stmpd, stmpe
CHARACTER( LEN = 1 ) :: ctmpa
LOGICAL              :: ltmpa, ltmpb
!%}}}
  stackchanged = .FALSE.
  id_comm = 0
  ! General(13) %{{{
  ! quit, q, endapp, bye, \// %{{{
  CALL next_command()
  maintag_comm   = 'quit'
  subtag_comm(1) = 'q'
  subtag_comm(2) = 'endapp'
  subtag_comm(3) = 'bye'
  subtag_comm(4) = '\//'
  help_comm = 'end this application'
  helpja_comm = 'このアプリケーションを終了する'
  usage_comm = 'quit'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_clean()
    IF( cword =='bye')WRITE(*,'(A)') 'bye!'
    IF( cword =='\//')WRITE(*,'(A)') '\//!'
    STOP
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! stop, \%% %{{{
  CALL next_command()
  maintag_comm   = 'stop'
  subtag_comm(1) = '\%%'
  help_comm = 'end this application with error-code 1'
  helpja_comm = 'エラーコード1でこのアプリケーションを終了する'
  usage_comm = 'stop'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_clean()
    STOP 1
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! msg, print, === %{{{
  CALL next_command()
  maintag_comm   = 'msg'
  subtag_comm(1) = 'print'
  subtag_comm(2) = '==='
  help_comm = 'print <value> to STDOUT'
  helpja_comm = '標準出力に<value>を出力する'
  usage_comm = '(<value>)msg'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize < 1 )THEN
      CALL psc_error( cword, 4 )
      RETURN
    END IF
    stmpa = macro(1)
    itmpa = LEN_TRIM( stmpa )
    WRITE(*,'(A$)') stmpa(1:itmpa-1)
    IF( autonl ) WRITE(*,*)
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! nop, relax, do_nothing \\ %{{{
  CALL next_command()
  maintag_comm   = 'nop'
  subtag_comm(1) = 'relax'
  subtag_comm(2) = 'do_nothing'
  subtag_comm(3) = '\\'
  help_comm = 'do nothing'
  helpja_comm = '何もしない'
  usage_comm = 'nop'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! sleep, pause, zzz, \... %{{{
  CALL next_command()
  maintag_comm   = 'sleep'
  subtag_comm(1) = 'pause'
  subtag_comm(2) = 'zzz'
  subtag_comm(3) = '\...'
  help_comm = 'pause <i> seconds'
  helpja_comm = '<i>秒間停止する'
  usage_comm = '<i> sleep'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error(cword, 2)
      RETURN
    END IF
    IF( stack(stz) < 0.d0 )THEN
      CALL psc_error(cword, 8)
      RETURN
    END IF
    CALL sleep( NINT( stack(stz) ) )
    CALL psc_stack_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! cls, wipe, \^ %{{{
  CALL next_command()
  maintag_comm   = 'cls'
  subtag_comm(1) = 'wipe'
  subtag_comm(2) = '\^'
  help_comm = 'clear screen'
  helpja_comm = '画面を消去する'
  usage_comm = 'cls'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    WRITE(*,'(A$)') ACHAR(27) // 'c' // ACHAR(27) // '[3J'
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! debug, \(;_;) %{{{
  CALL next_command()
  maintag_comm   = 'debug'
  subtag_comm(1) = '\(;_;)'
  help_comm = 'make all debug flag TRUE'
  helpja_comm = '全てのdebugフラグをTRUEにする'
  usage_comm = 'debug'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    WRITE(*,*) 'debug-flag set true'
    debug_buffer_start = .TRUE.
    debug_buffer_read  = .TRUE.
    debug_buffer_clean = .TRUE.
    debug_buffer_map   = .TRUE.
    debug_buffer_trim  = .TRUE.
    debug_buffer_last  = .TRUE.
    debug_buffer_do    = .TRUE.
    debug_level        = .TRUE.
    debug_commd        = .TRUE.
    debug_stack        = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! offdebug, \(^_^) %{{{
  CALL next_command()
  maintag_comm   = 'offdebug'
  subtag_comm(1) = '\(^_^)'
  help_comm = 'make all debug flag FALSE'
  helpja_comm = '全てのdebugフラグをFALSEにする'
  usage_comm = 'offdebug'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    debug_buffer_start = .FALSE.
    debug_buffer_read  = .FALSE.
    debug_buffer_clean = .FALSE.
    debug_buffer_map   = .FALSE.
    debug_buffer_trim  = .FALSE.
    debug_buffer_last  = .FALSE.
    debug_buffer_do    = .FALSE.
    debug_level        = .FALSE.
    debug_commd        = .FALSE.
    debug_stack        = .FALSE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! version, ver, author, \('-')? %{{{
  CALL next_command()
  maintag_comm   = 'version'
  subtag_comm(1) = 'ver'
  subtag_comm(2) = 'author'
  subtag_comm(3) = '\(''-'')?'
  help_comm = 'print version information'
  helpja_comm = 'バージョン情報を表示する'
  usage_comm = 'version'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    stmpa = 'version'
    CALL psc_comm_comm(stmpa)
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! \!? %{{{
  CALL next_command()
  maintag_comm   = 'v^' // &
  & 'v^' // '<><>ba'
  subtag_comm(1) = '\!?'
  help_comm = 'something happens'
  helpja_comm = '何かが起こる'
  usage_comm = 'v^' // &
  & 'v^' // '<><>ba'
  commandmode = get_commandmode()
  itmpa=76
  IF( commandmode == 1 )THEN
    stmpa = 'v^v^' // &
    & '<><>' // ACHAR(22+itmpa) // &
    & ACHAR(23+itmpa)
    CALL psc_comm_comm(stmpa)
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! banner, \&& %{{{
  CALL next_command()
  maintag_comm   = 'banner'
  subtag_comm(1) = '\&&'
  help_comm = 'print banner'
  helpja_comm = 'バナーを表示する'
  usage_comm = 'banner'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    stmpa = 'banner'
    CALL psc_comm_comm(stmpa)
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! flow, walk, \^^ %{{{
  CALL next_command()
  maintag_comm   = 'flow'
  subtag_comm(1) = 'walk'
  subtag_comm(2) = '\^^'
  help_comm = 'print random walking points'
  helpja_comm = 'ランダムウォークの点を出力する'
  usage_comm = 'flow'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    stmpa = 'flow'
    CALL psc_comm_comm(stmpa)
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! stars, numbers, snow, \## %{{{
  CALL next_command()
  maintag_comm   = 'stars'
  subtag_comm(1) = 'numbers'
  subtag_comm(2) = 'snow'
  subtag_comm(3) = '\##'
  help_comm = 'print random positioned random numbers'
  helpja_comm = 'ランダムな位置にランダムな数を出力する'
  usage_comm = 'stars'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    stmpa = 'stars'
    CALL psc_comm_comm(stmpa)
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  !%}}}
  ! Stack Operation(28) %{{{
  ! count, size , # %{{{
  CALL next_command()
  maintag_comm   = 'size'
  subtag_comm(1) = 'count'
  subtag_comm(2) = '#'
  help_comm = 'push the number of elements in real-number stack'
  helpja_comm = '実数型スタックの要素数をプッシュする'
  usage_comm = 'size'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_stack_append
    IF( iferror ) RETURN
    stack(stz) = DBLE(stz-1)
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! counts, sizes, ## %{{{
  CALL next_command()
  maintag_comm   = 'sizes'
  subtag_comm(1) = 'counts'
  subtag_comm(2) = '##'
  help_comm = 'push the number of elements in string stack'
  helpja_comm = '文字列型スタックの要素数をプッシュする'
  usage_comm = 'sizes'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_stack_append
    IF( iferror ) RETURN
    stack(stz) = DBLE( macrosize )
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! pop  , = %{{{
  CALL next_command()
  maintag_comm   = 'pop'
  subtag_comm(1) = '='
  help_comm = 'print <value>'
  helpja_comm = '<value>を出力する'
  usage_comm = '<value> pop'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error(cword, 2)
      RETURN
    END IF
    CALL psc_printnum(stack(stz), fmt_a, stmpa, dev_ou(iounow)%devnum, autonl, sepstr)
    CALL psc_stack_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! pops , == %{{{
  CALL next_command()
  maintag_comm   = 'pops'
  subtag_comm(1) = '=='
  help_comm = 'print <string>'
  helpja_comm = '<string>を出力する'
  usage_comm = '(<string>) pops'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize < 1 )THEN
      CALL psc_error(cword, 4)
      RETURN
    END IF
    stmpa = macro(1)
    itmpa = LEN_TRIM( stmpa )
    IF( dev_ou(iounow) % devnum == 0 )THEN
      WRITE(*,'(A$)') stmpa(1:itmpa-1)
      IF( autonl) WRITE(*,*)
    ELSE
      WRITE(dev_ou(iounow)%devnum, '(A$)') stmpa(1:itmpa-1)
      IF( autonl ) WRITE(dev_ou(iounow)%devnum, *)
    END IF
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! dump , kill , \< %{{{
  CALL next_command()
  maintag_comm   = 'kill'
  subtag_comm(1) = 'dump'
  subtag_comm(2) = '\<'
  help_comm = 'kill <value>'
  helpja_comm = '<value>を破棄する'
  usage_comm = '<value> kill'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error(cword, 2)
      RETURN
    END IF
    CALL psc_stack_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! dumps, kills, \<< %{{{
  CALL next_command()
  maintag_comm   = 'kills'
  subtag_comm(1) = 'dumps'
  subtag_comm(2) = '\<<'
  help_comm = 'kill <string>'
  helpja_comm = '<string>を破棄する'
  usage_comm = '(<string>) kills'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize < 1 )THEN
      CALL psc_error(cword, 4)
      RETURN
    END IF
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! dup  , \> %{{{
  CALL next_command()
  maintag_comm   = 'dup'
  subtag_comm(1) = '\>'
  help_comm = 'push <value> twice'
  helpja_comm = '<value>を２つプッシュする'
  usage_comm = '<value> dup'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error(cword, 2)
      RETURN
    END IF
    CALL psc_stack_append
    stack(stz) = stack(stz-1)
    IF( iferror ) RETURN
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! dups , \>> %{{{
  CALL next_command()
  maintag_comm   = 'dups'
  subtag_comm(1) = '\>>'
  help_comm = 'push <string> twice'
  helpja_comm = '<string>を２つプッシュする'
  usage_comm = '(<string>) dups'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize < 1 )THEN
      CALL psc_error(cword, 4)
      RETURN
    END IF
    CALL psc_strst_append
    IF( iferror ) RETURN
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! exch , swap , \<-> %{{{
  CALL next_command()
  maintag_comm   = 'exch'
  subtag_comm(1) = 'swap'
  subtag_comm(2) = '\<->'
  help_comm = 'exchange <value1> and <value2>'
  helpja_comm = '<value1>と<value2>を交換する'
  usage_comm = '<value1> <value2> exch'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz<2 )THEN
      CALL psc_error(cword, 3)
      RETURN
    END IF
    rtmpa = stack(stz)
    stack(stz) = stack(stz-1)
    stack(stz-1) = rtmpa
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! exchs, swaps, \<<->> %{{{
  CALL next_command()
  maintag_comm   = 'exchs'
  subtag_comm(1) = 'swaps'
  subtag_comm(2) = '\<<->>'
  help_comm = 'exchange (##1) and (##2)'
  helpja_comm = '(##1)と(##2)を交換する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize < 2 )THEN
      CALL psc_error(cword, 5)
      RETURN
    END IF
    stmpa = macro(1)
    macro(1) = macro(2)
    macro(2) = stmpa
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! clear , clr , \>< %{{{
  CALL next_command()
  maintag_comm   = 'clear'
  subtag_comm(1) = 'clr'
  subtag_comm(2) = '\><'
  help_comm = 'kill all elements in real-number stack'
  helpja_comm = '実数型スタックの全要素を破棄する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error(cword, 2)
      RETURN
    END IF
    stz = 0
    stackchanged = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! clears, clrs, \>><< %{{{
  CALL next_command()
  maintag_comm   = 'clears'
  subtag_comm(1) = 'clrs'
  subtag_comm(2) = '\>><<'
  help_comm = 'kill all elements in string stack'
  helpja_comm = '文字列型スタックの全要素を破棄する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize < 1 )THEN
      CALL psc_error(cword, 4)
      RETURN
    END IF
    macrosize = 0
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! pull , [ %{{{
  CALL next_command()
  maintag_comm   = 'pull'
  subtag_comm(1) = '['
  help_comm = 'roll all elements in real-number stack with one element backward'
  helpja_comm = '実数型スタックの全要素を一要素分だけ後ろ向きに回転する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 2 )THEN
      CALL psc_error(cword, 3)
      RETURN
    END IF
    stack(2:stz+1) = stack(1:stz)
    stack(1) = stack(stz+1)
    stackchanged = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! pulls, [[ %{{{
  CALL next_command()
  maintag_comm   = 'pulls'
  subtag_comm(1) = '[['
  help_comm = 'roll all elements in string stack with one element backward'
  helpja_comm = '文字列型スタックの全要素を一要素分だけ後ろ向きに回転する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize < 2 )THEN
      CALL psc_error(cword, 5)
      RETURN
    END IF
    stmpa = macro(1)
    CALL psc_strst_remove
    macrosize = macrosize + 1
    macro(macrosize) = stmpa
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! push , ] %{{{
  CALL next_command()
  maintag_comm   = 'push'
  subtag_comm(1) = ']'
  help_comm = 'roll all elements in real-number stack with one element forward'
  helpja_comm = '実数型スタックの全要素を一要素分だけ前向きに回転する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 2 )THEN
      CALL psc_error(cword, 3)
      RETURN
    END IF
    rtmpa = stack(1)
    stack(1:stz-1) = stack(2:stz)
    stack(stz) = rtmpa
    stackchanged = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! pushs, ]] %{{{
  CALL next_command()
  maintag_comm   = 'pushs'
  subtag_comm(1) = ']]'
  help_comm = 'roll all elements in string stack with one element forward'
  helpja_comm = '文字列型スタックの全要素を一要素分だけ前向きに回転する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize < 2 )THEN
      CALL psc_error(cword, 5)
      RETURN
    END IF
    stmpa = macro(macrosize)
    CALL psc_strst_append
    macrosize = macrosize - 1
    macro(1) = stmpa
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! roll , \[] %{{{
  CALL next_command()
  maintag_comm   = 'roll'
  subtag_comm(1) = '\[]'
  help_comm = 'roll (#3) toward #(2+int(#1)) (#2) times'
  helpja_comm = '(#3)から(#(2+int(#1)))に向かって(#2)回回転する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    ! this command works like postscript-roll
    IF( stz < 3 )THEN
      CALL psc_error(cword, 3)
      RETURN
    END IF
    IF( NINT( stack(stz-1) ) > stz )THEN
      CALL psc_error(cword, 3)
      RETURN
    END IF
    IF( NINT( stack(stz-1) ) < 2 )THEN
      CALL psc_error(cword, 8)
      RETURN
    END IF
    itmpa = NINT(stack(stz-1)) ! number of stacks to roll
    itmpb = NINT(stack(stz)) ! roll for (itmpb)-times
    CALL psc_stack_remove
    CALL psc_stack_remove
    ! be careful: itmpb may positive or negative
    !IF( itmpb<0 ) itmpb = itmpa + itmpb
    itmpb = mod(itmpb,itmpa+1)
    DO
      IF( itmpb ==0)EXIT
      rtmpa = stack(stz+1-itmpa)
      ! [TODO] 配列代入のほうが速いかも
      DO itmpc =  itmpa-1, 1, -1
        stack(stz-itmpc) = stack(stz+1-itmpc)
      END DO
      stack(stz) = rtmpa
      itmpb = itmpb-1
    END DO
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! rolls, \[[]] %{{{
  CALL next_command()
  maintag_comm   = 'rolls'
  subtag_comm(1) = '\[[]]'
  help_comm = 'roll all elements in string stack like in postscript'
  helpja_comm = '文字列型スタックの全要素をpostscriptライクに回転する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    ! this command works like postscript-roll
    IF( stz < 2 )THEN
      CALL psc_error(cword, 3)
      RETURN
    END IF
    IF( NINT( stack(stz-1) ) > macrosize )THEN
      CALL psc_error(cword, 3)
      RETURN
    END IF
    IF( NINT( stack(stz-1) ) < 2 )THEN
      CALL psc_error(cword, 8)
      RETURN
    END IF
    IF( IABS( NINT( stack(stz) ) ) > stack(stz-1) )THEN
      CALL psc_error(cword, 8)
      RETURN
    END IF
    itmpa = NINT(stack(stz-1)) ! number of stacks to roll
    itmpb = NINT(stack(stz)) ! roll for (itmpb)-times
    CALL psc_stack_remove
    CALL psc_stack_remove
    ! be careful: itmpb may positive or negative
    IF( itmpb<0)itmpb = itmpa+itmpb
    DO
      IF( itmpb ==0)EXIT
      stmpa = macro(itmpa)
      DO itmpc =  itmpa-1, 1, -1
        macro(itmpc+1) = macro(itmpc)
      END DO
      macro(1) = stmpa
      itmpb = itmpb-1
    END DO
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! merge, \[]<->[] %{{{
  CALL next_command()
  maintag_comm   = 'merge'
  subtag_comm(1) = '\[]<->[]'
  help_comm = 'merge real-number stack: (number of blocks)=(#2), (block size)=(#1)'
  helpja_comm = '実数型スタックをマージする: (ブロックの数)=(#2)、(ブロックサイズ)=(#1)'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 3 )THEN
      CALL psc_error(cword, 3)
      RETURN
    END IF
    itmpa = NINT(stack(stz-1)) ! number of merge blocks
    itmpb = NINT(stack(stz)) ! block size
    IF( itmpa < 1 .OR. itmpb < 1 )THEN
      CALL psc_error(cword, 8)
      RETURN
    END IF
    IF( stz - 2 < itmpa * itmpb )THEN
      CALL psc_error(cword, 3)
      RETURN
    END IF
    IF( stz + itmpa * itmpb >= stackmax )THEN
      CALL psc_error(cword, 14) ! 計算に使用するために必要な領域の条件
      RETURN
    END IF
    CALL psc_stack_remove
    CALL psc_stack_remove
    ! stack(:) の空き領域を利用する
    stack(stz+1:stz+itmpa*itmpb) = stack(stz+1-itmpa*itmpb:stz)
    DO idoa = 0, itmpa-1
      DO idob = 0, itmpb-1
        stack(stz+1-itmpa*itmpb+idob*itmpa+idoa) = &
        & stack(stz+1+idoa*itmpb+idob)
      END DO
    END DO
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! merges, \[[]]<<->>[[]] %{{{
  CALL next_command()
  maintag_comm   = 'merges'
  subtag_comm(1) = '\[[]]<<->>[[]]'
  help_comm = 'merge string stack: (number of blocks)=(#2), (block size)=(#1)'
  helpja_comm = '文字列型スタックをマージする: (ブロックの数)=(#2)、(ブロックサイズ)=(#1)'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 2 )THEN
      CALL psc_error(cword, 3)
      RETURN
    END IF
    itmpa = NINT(stack(stz-1)) ! number of merge blocks
    itmpb = NINT(stack(stz)) ! block size
    IF( itmpa < 1 .OR. itmpb < 1 )THEN
      CALL psc_error(cword, 8)
      RETURN
    END IF
    IF( macrosize < itmpa * itmpb )THEN
      CALL psc_error(cword, 5)
      RETURN
    END IF
    IF( macrosize + itmpa * itmpb >= macromax )THEN
      CALL psc_error(cword, 14) ! 計算に使用するために必要な領域の条件
      RETURN
    END IF
    CALL psc_stack_remove
    CALL psc_stack_remove
    ! macro(:) の空き領域を利用する
    macro(macrosize+1:macrosize+itmpa*itmpb) = macro(1:itmpa*itmpb)
    DO idoa = 0, itmpa-1
      DO idob = 0, itmpb-1
        macro(itmpa*itmpb-idob*itmpa-idoa) = &
        & macro(macrosize+itmpa*itmpb-idoa*itmpb-idob)
      END DO
    END DO
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! index , \<> %{{{
  CALL next_command()
  maintag_comm   = 'index'
  subtag_comm(1) = '\<>'
  help_comm = 'copy (#{(#1)+1})'
  helpja_comm = '(#{(#1)+1})を複製する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    ! [Caution]: this command work like postscript-index-command, but
    !            not exactly the same!
    ! PSC treat stack-top as '1st' element while postscript treat it as '0th'
    ! in addition, PSC allow negative positioning
    IF( stz < 1 )THEN
      CALL psc_error(cword, 3)
      RETURN
    END IF
    IF( NINT( stack(stz) ) == 0 )THEN
      CALL psc_error(cword, 8)
      RETURN
    END IF
    IF( IABS( NINT( stack(stz) ) ) > stz-1 )THEN
      CALL psc_error(cword, 8)
      RETURN
    END IF
    itmpa = NINT(stack(stz))
    CALL psc_stack_remove
    IF( itmpa < 0 ) itmpa = stz+1+itmpa
    rtmpa = stack(stz+1-itmpa)
    CALL psc_stack_append
    stack(stz) = rtmpa
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! indexs, \<<>> %{{{
  CALL next_command()
  maintag_comm   = 'indexs'
  subtag_comm(1) = '\<<>>'
  help_comm = 'copy (##(#1))'
  helpja_comm = '(##(#1))を複製する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error(cword, 2)
      RETURN
    END IF
    IF( NINT( stack(stz) )==0 )THEN
      CALL psc_error(cword, 8)
      RETURN
    END IF
    IF( NINT( stack(stz) ) > macrosize )THEN
      CALL psc_error(cword, 8)
      RETURN
    END IF
    itmpa = NINT(stack(stz))
    CALL psc_stack_remove
    IF( itmpa<0) itmpa = macrosize+1+itmpa
    CALL psc_strst_append
    IF( iferror ) RETURN
    macro(1) = macro(1+itmpa)
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! rev , \<=> %{{{
  CALL next_command()
  maintag_comm   = 'rev'
  subtag_comm(1) = '\<=>'
  help_comm = 'reverse from (#2) to (#{(#1)+1})'
  helpja_comm = '(#2)から(#{(#1)+1})までを反転する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz<3 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    IF( NINT(stack(stz))>stz )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    IF( NINT(stack(stz))<2 )THEN
      CALL psc_error( cword, 8 )
      RETURN
    END IF
    itmpa = NINT(stack(stz)) ! number of stacks to reverse
    CALL psc_stack_remove
    DO idoa = 1, itmpa/2
      rtmpa = stack(stz+1-idoa)
      stack(stz+1-idoa)=stack(stz-itmpa+idoa)
      stack(stz-itmpa+idoa)=rtmpa
    END DO
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! revs, \<<=>> %{{{
  CALL next_command()
  maintag_comm   = 'revs'
  subtag_comm(1) = '\<<=>>'
  help_comm = 'reverse from (##1) to (##{(#1)})'
  helpja_comm = '(##1)から(##{(#1)})までを反転する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    IF( macrosize < 2 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    IF( NINT(stack(stz)) > macrosize )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    IF( NINT(stack(stz)) < 2 )THEN
      CALL psc_error( cword, 8 )
      RETURN
    END IF
    itmpa = NINT(stack(stz)) ! number of stacks to reverse
    CALL psc_stack_remove
    DO idoa = 1, itmpa/2
      stmpa = macro(idoa)
      macro(idoa) = macro(itmpa+1-idoa)
      macro(itmpa+1-idoa) = stmpa
    END DO
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! dupop,  \>;= %{{{
  CALL next_command()
  maintag_comm   = 'dupop'
  subtag_comm(1) = '\>;='
  help_comm = 'dup then pop'
  helpja_comm = 'dupとpop'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error(cword, 2)
      RETURN
    END IF
    CALL psc_printnum(stack(stz), fmt_a, stmpa, &
    & dev_ou(iounow)%devnum, autonl, sepstr)
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! dupops, \>>;== %{{{
  CALL next_command()
  maintag_comm   = 'dupops'
  subtag_comm(1) = '\>>;=='
  help_comm = 'dups then pops'
  helpja_comm = 'dupsとpops'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize < 1 )THEN
      CALL psc_error(cword, 4)
      RETURN
    END IF
    stmpa = macro(1)
    itmpa = LEN_TRIM( stmpa )
    IF( iounow ==0 )THEN
      WRITE(*,'(A$)') stmpa(1:itmpa-1)
      IF( autonl) WRITE(*,*)
    ELSE
      WRITE(dev_ou(iounow)%devnum, '(A$)') stmpa(1:itmpa-1)
      IF( autonl) WRITE(dev_ou(iounow)%devnum, *)
    END IF
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! indexpop,  \<>;= %{{{
  CALL next_command()
  maintag_comm   = 'indexpop'
  subtag_comm(1) = '\<>;='
  help_comm = 'index then pop'
  helpja_comm = 'indexとpop'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz<1 )THEN
      CALL psc_error(cword, 3)
      RETURN
    END IF
    IF( NINT(stack(stz)) == 0 )THEN
      CALL psc_error(cword, 8)
      RETURN
    END IF
    IF( IABS(NINT(stack(stz))) > stz-1 )THEN
      CALL psc_error(cword, 8)
      RETURN
    END IF
    itmpa = NINT(stack(stz))
    CALL psc_stack_remove
    IF( itmpa<0 ) itmpa = stz+1+itmpa
    rtmpa = stack(stz+1-itmpa)
    CALL psc_printnum(rtmpa, fmt_a, stmpa, &
    & dev_ou(iounow)%devnum, autonl, sepstr)
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! indexpops, \<<>>;== %{{{
  CALL next_command()
  maintag_comm   = 'indexpops'
  subtag_comm(1) = '\<<>>;=='
  help_comm = 'indexs then pops'
  helpja_comm = 'indexsとpops'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz<1 )THEN
      CALL psc_error(cword, 2)
      RETURN
    END IF
    IF( NINT( stack(stz) ) == 0 )THEN
      CALL psc_error(cword, 8)
      RETURN
    END IF
    IF( NINT( stack(stz) ) > macrosize )THEN
      CALL psc_error(cword, 8)
      RETURN
    END IF
    itmpa = NINT(stack(stz))
    CALL psc_stack_remove
    IF( itmpa<0)itmpa = macrosize+1+itmpa
    stmpa = macro(itmpa)
    itmpa = LEN_TRIM( stmpa )
    IF( iounow ==0 )THEN
      WRITE(*,'(A$)') stmpa(1:itmpa-1)
      IF( autonl) WRITE(*,*)
    ELSE
      WRITE(dev_ou(iounow)%devnum, '(A$)') stmpa(1:itmpa-1)
      IF( autonl) WRITE(dev_ou(iounow)%devnum, *)
    END IF
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! End Stack Operation %}}}
  ! Math Basic(27) %{{{
  ! add, +, \~- %{{{
  CALL next_command()
  maintag_comm   = 'add'
  subtag_comm(1) = '+'
  subtag_comm(2) = '\~-'
  help_comm = 'push (#1)+(#2)'
  helpja_comm = '(#1)+(#2)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 2 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    stack(stz-1) = stack(stz-1) + stack(stz)
    CALL psc_stack_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! sub, -, \~+ %{{{
  CALL next_command()
  maintag_comm   = 'sub'
  subtag_comm(1) = '-'
  subtag_comm(2) = '\~+'
  help_comm = 'push (#1)-(#2)'
  helpja_comm = '(#1)-(#2)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 2 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    stack(stz-1) = stack(stz-1) - stack(stz)
    CALL psc_stack_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! mul, *, \~/ %{{{
  CALL next_command()
  maintag_comm   = 'mul'
  subtag_comm(1) = '*'
  subtag_comm(2) = '\~/'
  help_comm = 'push (#1)*(#2)'
  helpja_comm = '(#1)*(#2)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 2 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    IF( stack(stz-1) == 0.d0 .OR. &
    & stack(stz) == 0.d0 )THEN
    ELSE
      rtmpa = DLOG10(DABS(stack(stz-1)))
      rtmpb = DLOG10(DABS(stack(stz)))
      IF( rtmpa+rtmpb > log_overf )THEN
        CALL psc_error( cword, 14 )
        RETURN
      END IF
    END IF
    stack(stz-1) = stack(stz-1) * stack(stz)
    CALL psc_stack_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! div, /, \~* %{{{
  CALL next_command()
  maintag_comm   = 'div'
  subtag_comm(1) = '/'
  subtag_comm(2) = '\~/'
  help_comm = 'push (#1)/(#2)'
  helpja_comm = '(#1)/(#2)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 2 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    IF( stack(stz) == 0.d0 )THEN
      CALL psc_error( cword, 14 )
      RETURN
    END IF
    IF( stack(stz-1) /= 0.d0 )THEN
      rtmpa = DLOG10(DABS(stack(stz-1)))
      rtmpb = DLOG10(DABS(stack(stz)))
      IF( rtmpa-rtmpb > log_overf )THEN
        CALL psc_error( cword, 14 )
        RETURN
      END IF
    END IF
    stack(stz-1) = stack(stz-1) / stack(stz)
    CALL psc_stack_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! mod, \% %{{{
  CALL next_command()
  maintag_comm   = 'mod'
  subtag_comm(1) = '\%'
  help_comm = 'push mod(#1,#2)=#1-#2*int(#1/#2)'
  helpja_comm = 'mod(#1,#2)=#1-#2*int(#1/#2)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 2 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    stack(stz-1) = stack(stz-1) - (stack(stz) * &
    & DBLE(INT(stack(stz-1)/stack(stz))))
    CALL psc_stack_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! modulo, % %{{{
  CALL next_command()
  maintag_comm   = 'modulo'
  subtag_comm(1) = '%'
  help_comm = 'push modulo(#1,#2)=#1-#2*floor(#1/#2)'
  helpja_comm = 'modulo(#1,#2)=#1-#2*floor(#1/#2)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 2 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    stack(stz-1) = stack(stz-1) - (stack(stz) * &
    & DBLE(FLOOR(stack(stz-1)/stack(stz))))
    CALL psc_stack_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! pow, ^, ** %{{{
  CALL next_command()
  maintag_comm   = 'pow'
  subtag_comm(1) = '^'
  subtag_comm(2) = '**'
  help_comm = 'push (#1)^(#2)'
  helpja_comm = '(#1)^(#2)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 2 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    IF( stack(stz-1) <= 0.d0 )THEN
      CALL psc_error( cword, 8 )
      RETURN
    END IF
    rtmpa = stack(stz) * DLOG10(DABS(stack(stz-1)))
    IF( rtmpa > log_overf )THEN
      CALL psc_error( cword, 14 )
      RETURN
    END IF
    stack(stz-1) = stack(stz-1) ** stack(stz)
    CALL psc_stack_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! fact, ! %{{{
  CALL next_command()
  maintag_comm   = 'fact'
  subtag_comm(1) = '!'
  help_comm = 'push (round(#1))!'
  helpja_comm = '(round(#1))!をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    IF( stack(stz) < 0.d0 )THEN
      CALL psc_error( cword, 8 )
      RETURN
    END IF
    ! detect overflow
    itmpa = NINT(stack(stz))
    rtmpa = 0.d0
    DO idoa = 1, itmpa
      rtmpa = rtmpa + DLOG10(DBLE(idoa))
    END DO
    IF( rtmpa > log_overf )THEN
      CALL psc_error( cword, 14 )
      RETURN
    END IF
    ! calc
    rtmpa = 1.d0
    DO idoa = 1, itmpa
      rtmpa = rtmpa * DBLE(idoa)
    END DO
    stack(stz) = rtmpa
    stackchanged = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! gt, > %{{{
  CALL next_command()
  maintag_comm   = 'gt'
  subtag_comm(1) = '>'
  help_comm = 'push 1 if #1>#2, otherwise push 0'
  helpja_comm = '#1>#2なら1を、そうでないなら0をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 2 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    IF( stack(stz-1) > stack(stz) )THEN
      stack(stz-1) = 1.d0
    ELSE
      stack(stz-1) = 0.d0
    END IF
    CALL psc_stack_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! lt, < %{{{
  CALL next_command()
  maintag_comm   = 'lt'
  subtag_comm(1) = '<'
  help_comm = 'push 1 if #1<#2, otherwise push 0'
  helpja_comm = '#1<#2なら1を、そうでないなら0をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 2 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    IF( stack(stz-1) < stack(stz) )THEN
      stack(stz-1) = 1.d0
    ELSE
      stack(stz-1) = 0.d0
    END IF
    CALL psc_stack_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! ge, geq, \>=, \~< %{{{
  CALL next_command()
  maintag_comm   = 'ge'
  subtag_comm(1) = 'geq'
  subtag_comm(2) = '\>='
  subtag_comm(3) = '\~<'
  help_comm = 'push 1 if round(#1)>=round(#2), otherwise push 0'
  helpja_comm = 'round(#1)>=round(#2)なら1を、そうでないなら0をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 2 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    IF( NINT(stack(stz-1)) >= NINT(stack(stz)) )THEN
      stack(stz-1) = 1.d0
    ELSE
      stack(stz-1) = 0.d0
    END IF
    CALL psc_stack_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! le, leq, \<=, \~> %{{{
  CALL next_command()
  maintag_comm   = 'le'
  subtag_comm(1) = 'leq'
  subtag_comm(2) = '\<='
  subtag_comm(3) = '\~>'
  help_comm = 'push 1 if round(#1)<=round(#2), otherwise push 0'
  helpja_comm = 'round(#1)<=round(#2)なら1を、そうでないなら0をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 2 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    IF( NINT(stack(stz-1)) <= NINT(stack(stz)) )THEN
      stack(stz-1) = 1.d0
    ELSE
      stack(stz-1) = 0.d0
    END IF
    CALL psc_stack_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! eq, \==, \~<> %{{{
  CALL next_command()
  maintag_comm   = 'eq'
  subtag_comm(1) = '\=='
  subtag_comm(2) = '\~<>'
  help_comm = 'push 1 if round(#1)==round(#2), otherwise push 0'
  helpja_comm = 'round(#1)==round(#2)なら1を、そうでないなら0をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 2 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    IF( NINT(stack(stz-1)) == NINT(stack(stz)) )THEN
      stack(stz-1) = 1.d0
    ELSE
      stack(stz-1) = 0.d0
    END IF
    CALL psc_stack_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! neq, xor, \~=, \<> %{{{
  CALL next_command()
  maintag_comm   = 'neq'
  subtag_comm(1) = 'xor'
  subtag_comm(2) = '\~='
  subtag_comm(3) = '\<>'
  help_comm = 'push 1 if round(#1)/=round(#2), otherwise push 0'
  helpja_comm = 'round(#1)/=round(#2)なら1を、そうでないなら0をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 2 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    IF( NINT(stack(stz-1)) /= NINT(stack(stz)) )THEN
      stack(stz-1) = 1.d0
    ELSE
      stack(stz-1) = 0.d0
    END IF
    CALL psc_stack_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! not, neg, \~ %{{{
  CALL next_command()
  maintag_comm   = 'not'
  subtag_comm(1) = 'neg'
  subtag_comm(2) = '\~'
  help_comm = 'push 1 if round(#1)==0, otherwise push 0'
  helpja_comm = 'round(#1)==0ならば1を、そうでないなら0をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz == 0 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    IF( NINT(stack(stz)) == 0 )THEN
      stack(stz) = 1.d0
    ELSE
      stack(stz) = 0.d0
    END IF
    stackchanged = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! min %{{{
  CALL next_command()
  maintag_comm   = 'min'
  help_comm = 'push min(#1,#2)'
  helpja_comm = 'min(#1,#2)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 2 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    stack(stz-1) = MIN(stack(stz-1),stack(stz))
    CALL psc_stack_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! max %{{{
  CALL next_command()
  maintag_comm   = 'max'
  help_comm = 'push max(#1,#2)'
  helpja_comm = 'max(#1,#2)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 2 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    stack(stz-1) = MAX(stack(stz-1),stack(stz))
    CALL psc_stack_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! and, both, & %{{{
  CALL next_command()
  maintag_comm   = 'and'
  subtag_comm(1) = 'both'
  subtag_comm(2) = '&'
  help_comm = 'push 1 if both(round(#1)/=0,round(#2)/=0), otherwise push 0'
  helpja_comm = 'both(round(#1)/=0,round(#2)/=0)ならば1を、そうでないなら0をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 2 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    IF( NINT(stack(stz)) /= 0 .AND. NINT(stack(stz-1)) /= 0 )THEN
      stack(stz-1) = 1.d0
    ELSE
      stack(stz-1) = 0.d0
    END IF
    CALL psc_stack_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! or, either, | %{{{
  CALL next_command()
  maintag_comm   = 'or'
  subtag_comm(1) = 'either'
  subtag_comm(2) = '|'
  help_comm = 'push 1 if either(round(#1)/=0,round(#2)/=0), otherwise push 0'
  helpja_comm = 'either(round(#1)/=0,round(#2)/=0)ならば1を、そうでないなら0をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 2 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    IF( NINT(stack(stz)) /= 0 .OR. NINT(stack(stz-1)) /= 0 )THEN
      stack(stz-1) = 1.d0
    ELSE
      stack(stz-1) = 0.d0
    END IF
    CALL psc_stack_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! nand, \~& %{{{
  CALL next_command()
  maintag_comm   = 'nand'
  subtag_comm(1) = '\~&'
  help_comm = 'push 1 if not(both(round(#1)/=0,round(#2)/=0)), otherwise push 0'
  helpja_comm = 'not(both(round(#1)/=0,round(#2)/=0))ならば1を、そうでないなら0をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 2 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    IF( NINT(stack(stz))/=0 .AND. NINT(stack(stz-1))/=0 )THEN
      stack(stz-1) = 0.d0
    ELSE
      stack(stz-1) = 1.d0
    END IF
    CALL psc_stack_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! nor, \~| %{{{
  CALL next_command()
  maintag_comm   = 'nor'
  subtag_comm(1) = '\~|'
  help_comm = 'push 1 if not(either(round(#1)/=0,round(#2)/=0)), otherwise push 0'
  helpja_comm = 'not(either(round(#1)/=0,round(#2)/=0))ならば1を、そうでないなら0をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 2 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    IF( NINT(stack(stz))/=0 .OR. NINT(stack(stz-1))/=0 )THEN
      stack(stz-1) = 0.d0
    ELSE
      stack(stz-1) = 1.d0
    END IF
    CALL psc_stack_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! inc, \++ %{{{
  CALL next_command()
  maintag_comm   = 'inc'
  subtag_comm(1) = '\++'
  help_comm = 'push #1+incunit'
  helpja_comm = '#1+incunitをプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz == 0 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    stack(stz) = stack(stz) + incunit
    stackchanged = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! dec, \-- %{{{
  CALL next_command()
  maintag_comm   = 'dec'
  subtag_comm(1) = '\--'
  help_comm = 'push #1-incunit'
  helpja_comm = '#1-incunitをプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz == 0 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    stack(stz) = stack(stz) - incunit
    stackchanged = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! even, \$=%~ %{{{
  CALL next_command()
  maintag_comm   = 'even'
  subtag_comm(1) = '\$=%~'
  help_comm = 'push 1 if even(round(#1)), otherwise push 0'
  helpja_comm = 'even(round(#1))ならば1を、そうでないなら0をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz == 0 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    IF( MOD(NINT(stack(stz)),2) == 0 )THEN
      stack(stz) = 1.d0
    ELSE
      stack(stz) = 0.d0
    END IF
    stackchanged = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! odd, \$=% %{{{
  CALL next_command()
  maintag_comm   = 'odd'
  subtag_comm(1) = '\$=%'
  help_comm = 'push 1 if odd(round(#1)), otherwise push 0'
  helpja_comm = 'odd(round(#1))ならば1を、そうでないなら0をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz == 0 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    IF( MOD(NINT(stack(stz)),2) == 1 )THEN
      stack(stz) = 1.d0
    ELSE
      stack(stz) = 0.d0
    END IF
    stackchanged = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! positive, \+ %{{{
  CALL next_command()
  maintag_comm   = 'positive'
  subtag_comm(1) = '\+'
  help_comm = 'push 1 if #1>0, otherwise push 0'
  helpja_comm = '#1>0ならば1を、そうでないなら0をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz == 0 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    IF( stack(stz) > 0.d0 )THEN
      stack(stz) = 1.d0
    ELSE
      stack(stz) = 0.d0
    END IF
    stackchanged = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! negative, \- %{{{
  CALL next_command()
  maintag_comm   = 'negative'
  subtag_comm(1) = '\-'
  help_comm = 'push 1 if #1<0, otherwise push 0'
  helpja_comm = '#1<0ならば1を、そうでないなら0をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz == 0 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    IF( stack(stz) < 0.d0 )THEN
      stack(stz) = 1.d0
    ELSE
      stack(stz) = 0.d0
    END IF
    stackchanged = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! End Basic Math %}}}
  ! Math Other Function(35) %{{{
  ! int %{{{
  CALL next_command()
  maintag_comm   = 'int'
  help_comm = 'push int(#1)'
  helpja_comm = 'int(#1)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    stack(stz) = DBLE( INT( stack(stz) ) )
    stackchanged = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! nint, round, \$># %{{{
  CALL next_command()
  maintag_comm   = 'nint'
  subtag_comm(1) = 'round'
  subtag_comm(2) = '\$>#'
  help_comm = 'push nint(#1)'
  helpja_comm = 'nint(#1)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    stack(stz) = DBLE( NINT( stack(stz) ) )
    stackchanged = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! floor, \$>_# %{{{
  CALL next_command()
  maintag_comm   = 'floor'
  subtag_comm(1) = '\$>_#'
  help_comm = 'push floor(#1)'
  helpja_comm = 'floor(#1)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    stack(stz) = DBLE( FLOOR( stack(stz) ) )
    stackchanged = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! ceil, ceiling, \$>^# %{{{
  CALL next_command()
  maintag_comm   = 'ceil'
  subtag_comm(1) = 'ceiling'
  subtag_comm(2) = '\$>^#'
  help_comm = 'push ceil(#1)'
  helpja_comm = 'ceil(#1)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    stack(stz) = DBLE( CEILING( stack(stz) ) )
    stackchanged = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! abs, \$| %{{{
  CALL next_command()
  maintag_comm   = 'abs'
  subtag_comm(1) = '\$|'
  help_comm = 'push abs(#1)'
  helpja_comm = 'abs(#1)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    stack(stz) = DABS( stack(stz) )
    stackchanged = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! minus, _ %{{{
  CALL next_command()
  maintag_comm   = 'minus'
  subtag_comm(1) = '_'
  help_comm = 'push -(#1)'
  helpja_comm = '-(#1)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    stack(stz) = - stack(stz)
    stackchanged = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! double, \$=* %{{{
  CALL next_command()
  maintag_comm   = 'double'
  subtag_comm(1) = '\$=*'
  help_comm = 'push 2*(#1)'
  helpja_comm = '2*(#1)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    stack(stz) = stack(stz) * 2.d0
    stackchanged = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! half, \$=/ %{{{
  CALL next_command()
  maintag_comm   = 'half'
  subtag_comm(1) = '\$=/'
  help_comm = 'push (#1)/2'
  helpja_comm = '(#1)/2をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    stack(stz) = stack(stz) / 2.d0
    stackchanged = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! exp, \$^ %{{{
  CALL next_command()
  maintag_comm   = 'exp'
  subtag_comm(1) = '\$^'
  help_comm = 'push exp(#1)'
  helpja_comm = 'exp(#1)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    IF( stack(stz) / DLOG(10.d0) > log_overf )THEN
      CALL psc_error( cword, 14 )
      RETURN
    END IF
    stack(stz) = DEXP( stack(stz) )
    stackchanged = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! log, \$~^, %{{{
  CALL next_command()
  maintag_comm   = 'log'
  subtag_comm(1) = '\$~^,'
  help_comm = 'push log_(#2)(#1)'
  helpja_comm = 'log_(#2)(#1)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 2 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    IF( (stack(stz) < 0.d0).OR.(stack(stz-1) < 0.d0) )THEN
      CALL psc_error( cword, 8 )
      RETURN
    END IF
    IF( stack(stz-1) == 1.d0 )THEN
      CALL psc_error( cword, 8 )
      RETURN
    END IF
    stack(stz-1) = DLOG(stack(stz)) / DLOG(stack(stz-1))
    CALL psc_stack_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! logten, \$~^,+ %{{{
  CALL next_command()
  maintag_comm   = 'logten'
  subtag_comm(1) = '\$~^,+'
  help_comm = 'push log_10(#1)'
  helpja_comm = 'log_10(#1)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    IF( stack(stz) <= 0.d0 )THEN
      CALL psc_error( cword, 8 )
      RETURN
    END IF
    stack(stz) = DLOG10( stack(stz) )
    stackchanged = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! ln, \$~^ %{{{
  CALL next_command()
  maintag_comm   = 'ln'
  subtag_comm(1) = '\$~^'
  help_comm = 'push ln(#1)'
  helpja_comm = 'ln(#1)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    IF( stack(stz) <= 0.d0 )THEN
      CALL psc_error( cword, 8 )
      RETURN
    END IF
    stack(stz) = DLOG( stack(stz) )
    stackchanged = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! inv, \$~ %{{{
  CALL next_command()
  maintag_comm   = 'inv'
  subtag_comm(1) = '\$~'
  help_comm = 'push 1/(#1)'
  helpja_comm = '1/(#1)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    IF( DLOG10( DABS( stack(stz) ) ) < log_underf )THEN
      CALL psc_error( cword, 14 )
      RETURN
    END IF
    stack(stz) = 1.d0 / stack(stz)
    stackchanged = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! square, squared, \$=^ %{{{
  CALL next_command()
  maintag_comm   = 'square'
  subtag_comm(1) = 'squared'
  subtag_comm(2) = '\$=^'
  help_comm = 'push (#1)^2'
  helpja_comm = '(#1)^2をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    IF( stack(stz) < 0.d0 )THEN
      CALL psc_error( cword, 8 )
      RETURN
    END IF
    stack(stz) = (stack(stz)) ** 2
    stackchanged = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! sqr, sqrt, \$~=^ %{{{
  CALL next_command()
  maintag_comm   = 'sqr'
  subtag_comm(1) = 'sqrt'
  subtag_comm(2) = '\$~=^'
  help_comm = 'push sqrt(#1)'
  helpja_comm = 'sqrt(#1)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    IF( stack(stz) < 0.d0 )THEN
      CALL psc_error( cword, 8 )
      RETURN
    END IF
    stack(stz) = DSQRT( stack(stz) )
    stackchanged = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! random, rand, rnd, \$? %{{{
  CALL next_command()
  maintag_comm   = 'random'
  subtag_comm(1) = 'rand'
  subtag_comm(2) = 'rnd'
  subtag_comm(3) = '\$?'
  help_comm = 'push random number from 0 to 1'
  helpja_comm = '0以上1以下の乱数をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_stack_append
    IF( iferror ) RETURN
    CALL random_number( stack(stz) )
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! primelist %{{{
  CALL next_command()
  maintag_comm   = 'primelist'
  help_comm = 'make list of prime numbers up to (#1)''th'
  helpja_comm = '(#1)番目までの素数リストを作成する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    IF( NINT(stack(stz)) < 1 )THEN
      CALL psc_error( cword, 8 )
      RETURN
    END IF
    itmpa = NINT( stack(stz) )
    CALL psc_stack_remove
    IF( ALLOCATED(prime) ) DEALLOCATE(prime)
    ALLOCATE( prime(itmpa) )
    CALL makeprimelist(itmpa, prime, itmpb)
    IF( itmpb ==2 )THEN
      CALL psc_error( cword, 14 )
      RETURN
    END IF
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! prime %{{{
  CALL next_command()
  maintag_comm   = 'prime'
  help_comm = 'push (#1)''th prime number'
  helpja_comm = '(#1)番目の素数をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    IF( NINT(stack(stz)) < 1 )THEN
      CALL psc_error( cword, 8 )
      RETURN
    END IF
    IF( .NOT. ALLOCATED(prime) )THEN
      CALL psc_error( cword, 19 )
      RETURN
    END IF
    IF( NINT(stack(stz)) > UBOUND(prime,1) )THEN
      CALL psc_error( cword, 20 )
      RETURN
    END IF
    itmpa = NINT( stack(stz) )
    stack(stz) = DBLE( prime(itmpa) )
    stackchanged = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  !
  ! sin, \$%/| %{{{
  CALL next_command()
  maintag_comm   = 'sin'
  subtag_comm(1) = '\$%/|'
  help_comm = 'push sin(#1)'
  helpja_comm = 'sin(#1)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    stack(stz) = DSIN( stack(stz) )
    stackchanged = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! cos, \$%/_ %{{{
  CALL next_command()
  maintag_comm   = 'cos'
  subtag_comm(1) = '\$%/_'
  help_comm = 'push cos(#1)'
  helpja_comm = 'cos(#1)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    stack(stz) = DCOS( stack(stz) )
    stackchanged = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! tan, \$%_| %{{{
  CALL next_command()
  maintag_comm   = 'tan'
  subtag_comm(1) = '\$%_|'
  help_comm = 'push tan(#1)'
  helpja_comm = 'tan(#1)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    stack(stz) = DTAN( stack(stz) )
    stackchanged = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! asin, \$~%/| %{{{
  CALL next_command()
  maintag_comm   = 'asin'
  subtag_comm(1) = '\$~%/|'
  help_comm = 'push asin(#1)'
  helpja_comm = 'asin(#1)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    IF( DABS(stack(stz)) > 1.d0 )THEN
      CALL psc_error( cword, 8 )
      RETURN
    END IF
    stack(stz) = DASIN( stack(stz) )
    stackchanged = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! acos, \$~%/_ %{{{
  CALL next_command()
  maintag_comm   = 'acos'
  subtag_comm(1) = '\$~%/_'
  help_comm = 'push acos(#1)'
  helpja_comm = 'acos(#1)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    IF( DABS(stack(stz)) > 1.d0 )THEN
      CALL psc_error( cword, 8 )
      RETURN
    END IF
    stack(stz) = DACOS( stack(stz) )
    stackchanged = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! atan, \$~%_| %{{{
  CALL next_command()
  maintag_comm   = 'atan'
  subtag_comm(1) = '\$~%_|'
  help_comm = 'push atan(#1)'
  helpja_comm = 'atan(#1)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    stack(stz) = DATAN( stack(stz) )
    stackchanged = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! atantwo, \$~%_|& %{{{
  CALL next_command()
  maintag_comm   = 'atantwo'
  subtag_comm(1) = '\$~%_|&'
  help_comm = 'push atan2(#2,#1)'
  helpja_comm = 'atan2(#2,#1)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 0 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    stack(stz) = DATAN2( stack(stz),stack(stz) )
    CALL psc_stack_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! sinh, \$%/|! %{{{
  CALL next_command()
  maintag_comm   = 'sinh'
  subtag_comm(1) = '\$%/|!'
  help_comm = 'push sinh(#1)'
  helpja_comm = 'sinh(#1)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    IF( stack(stz) / DLOG(10.d0) > log_overf )THEN
      CALL psc_error( cword, 14 )
      RETURN
    END IF
    stack(stz) = DSINH( stack(stz) )
    stackchanged = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! cosh, \$%/_! %{{{
  CALL next_command()
  maintag_comm   = 'cosh'
  subtag_comm(1) = '\$%/_!'
  help_comm = 'push cosh(#1)'
  helpja_comm = 'cosh(#1)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    IF( stack(stz) / DLOG(10.d0) > log_overf )THEN
      CALL psc_error( cword, 14 )
      RETURN
    END IF
    stack(stz) = DCOSH( stack(stz) )
    stackchanged = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! tanh, \$%_|! %{{{
  CALL next_command()
  maintag_comm   = 'tanh'
  subtag_comm(1) = '\$%_|!'
  help_comm = 'push tanh(#1)'
  helpja_comm = 'tanh(#1)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    stack(stz) = DTANH( stack(stz) )
    stackchanged = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! besselj, besj %{{{
  CALL next_command()
  maintag_comm   = 'besselj'
  subtag_comm(1) = 'besj'
  help_comm = 'push besselj_(#2)(#1)'
  helpja_comm = 'besselj_(#2)(#1)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 2 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    itmpa = NINT( stack(stz-1) )
    CALL besjno(itmpa, stack(stz), rtmpa)
    CALL psc_stack_remove
    stack(stz) = rtmpa
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! gamma %{{{
  CALL next_command()
  maintag_comm   = 'gamma'
  help_comm = 'push gamma(#1)'
  helpja_comm = 'gamma(#1)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    stack(stz) = DGAMMA( stack(stz) )
    stackchanged = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! erf %{{{
  CALL next_command()
  maintag_comm   = 'erf'
  help_comm = 'push erf(#1)'
  helpja_comm = 'erf(#1)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    stack(stz) = DERF( stack(stz) )
    stackchanged = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! integratefunc, itf %{{{
  CALL next_command()
  maintag_comm   = 'integratefunc'
  subtag_comm(1) = 'itf'
  help_comm = 'integrate function (##1) over range (#3)~(#2) with division (#1)'
  helpja_comm = '関数(##1)を範囲(#3)~(#2)にわたって分割数(#1)で積分する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    ! function:(##1) range:(#3)~(#2) div:(#1)
    IF( macrosize < 1 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    IF( stz < 3 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    IF( NINT(stack(stz)) < 3 )THEN
      CALL psc_error( cword, 8 )
      RETURN
    END IF
    rtmpa = stack(stz-2) ! xmin
    rtmpb = stack(stz-1) ! xmax
    itmpc = NINT( stack(stz) ) ! divnum
    WRITE(stmpa,*) rtmpa
    WRITE(stmpb,*) rtmpb
    WRITE(stmpc,*) itmpc + 1
    WRITE(stmpd,*) (rtmpb-rtmpa) / DBLE(itmpc)
    stmpa = ADJUSTL( stmpa )
    stmpb = ADJUSTL( stmpb )
    stmpc = ADJUSTL( stmpc )
    stmpd = ADJUSTL( stmpd )
    CALL psc_cut_extra_zero( stmpa )
    CALL psc_cut_extra_zero( stmpb )
    CALL psc_cut_extra_zero( stmpc )
    CALL psc_cut_extra_zero( stmpd )

    stmpa = TRIM(stmpd) // ',' // TRIM(stmpa) // ',0,pull' // &
    & '(' // TRIM(macro(1)) // &
    & '(dup,dups,exec' // &
    & ',push,+,pull,pull,dup,push,+)' // TRIM(stmpc) // &
    & ',do,kill,push,2,*,' // TRIM(stmpa) // &
    & ',dups,exec,-,' // TRIM(stmpb) // &
    & ',dups,exec,-,2,/,*,kills,'
    bufin = TRIM(stmpa) // TRIM(bufin)
    !
    CALL psc_strst_remove
    CALL psc_stack_remove
    CALL psc_stack_remove
    CALL psc_stack_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! averagefunc, avf %{{{
  CALL next_command()
  maintag_comm   = 'averagefunc'
  subtag_comm(1) = 'avf'
  help_comm = 'average function (##1) over range (#3)~(#2) with division (#1)'
  helpja_comm = '関数(##1)を範囲(#3)~(#2)にわたって分割数(#1)で平均する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    ! function:(##1) range:(#3)~(#2) div:(#1)
    IF( macrosize < 1 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    IF( stz < 3 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    IF( NINT(stack(stz)) < 3 )THEN
      CALL psc_error( cword, 8 )
      RETURN
    END IF
    rtmpa = stack(stz-2) ! xmin
    rtmpb = stack(stz-1) ! xmax
    itmpc = NINT(stack(stz)) ! divnum
    WRITE(stmpa,*) rtmpa
    WRITE(stmpb,*) rtmpb
    WRITE(stmpc,*) itmpc
    WRITE(stmpd,*) (rtmpb-rtmpa) / DBLE(itmpc)
    WRITE(stmpe,*) itmpc + 1
    stmpa = ADJUSTL( stmpa )
    stmpb = ADJUSTL( stmpb )
    stmpc = ADJUSTL( stmpc )
    stmpd = ADJUSTL( stmpd )
    stmpe = ADJUSTL( stmpe )
    CALL psc_cut_extra_zero( stmpa )
    CALL psc_cut_extra_zero( stmpb )
    CALL psc_cut_extra_zero( stmpc )
    CALL psc_cut_extra_zero( stmpd )
    CALL psc_cut_extra_zero( stmpe )
    stmpa = TRIM(stmpd) // ',' // TRIM(stmpa) // ',0,pull' // &
    & '(' // TRIM(macro(1)) // &
    & '(dup,dups,exec' // &
    & ',push,+,pull,pull,dup,push,+)' // TRIM(stmpe) // &
    & ',do,kill,kill,size,1.5,gt(push)if,2,*,' // &
    & TRIM(stmpa) // ',dups,exec' // &
    & ',-,' // TRIM(stmpb) // ',dups,exec' // &
    & ',-,2,/,' // TRIM(stmpc) // ',/,kills,'
    bufin = TRIM(stmpa) // TRIM(bufin)
    !
    CALL psc_strst_remove
    CALL psc_stack_remove
    CALL psc_stack_remove
    CALL psc_stack_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! difffunc, dff %{{{
  CALL next_command()
  maintag_comm   = 'difffunc'
  subtag_comm(1) = 'dff'
  help_comm = 'differentiate function (##1) at position (#1)'
  helpja_comm = '関数(##1)を位置(#1)で微分する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    ! function:(##1) at (#1)
    IF( macrosize < 1 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    IF( stz < 1 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    !
    ! Note: 関数のスケールに依存しないように改良が必要
    stmpa = 'dup,dups,exec,exch,' // &
    & '1.d-10,+,dups,exec' // &
    & ',exch,-,1.d-10,/,kills,'
    bufin = TRIM(stmpa) // TRIM(bufin)
    !
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! findzeropoint, fzp %{{{
  CALL next_command()
  maintag_comm   = 'findzeropoint'
  subtag_comm(1) = 'fzp'
  help_comm = 'find zero-point of function (##1) between (#2)~(#1)'
  helpja_comm = '関数(##1)のゼロ点を(#2)~(#1)の間で求める'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    ! function:(##1) range:(#2)~(#1)
    IF( macrosize < 1 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    IF( stz < 2 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    stmpa = macro(1)
    itmpa = LEN_TRIM( stmpa )
    stmpa = stmpa(1:itmpa-1)
    CALL psc_strst_remove
    ! Note: この実装はmacro(1) が長いとバッファが長くなりすぎる
    ! 文字列型スタックを使うように改良が必要
    stmpa = '{(XB)let(XA)let' // &
    & '(XA)ref,' // TRIM(stmpa) // '(YA)let' // &
    & '(XB)ref,' // TRIM(stmpa) // '(YB)let' // &
    & '(XB)ref,' // TRIM(stmpa) // '(YB)let' // &
    & '(YA)ref(YB)ref,*,0,gt((Error/x0a)==,2,break)if' // &
    & '(I)0,let((I)ref,1,+(I)let' // &
    & '(XA)ref(XB)ref,+,2,/(XC)let' // &
    & '(XC)ref,' // TRIM(stmpa) // '(YC)let' // &
    & '(YA)ref(YC)ref,*,0,gt' // &
    & '((XC)ref(XA)let(YC)ref(YA)let)' // &
    & '((XC)ref(XB)let(YC)ref(YB)let)ifelse' // &
    & '(XB)ref,dup(XA)ref,-,abs,/,1.d-12,lt' // &
    & '(2,break)if' // &
    & ')50,do' // &
    & '(f)setfmt(XC)ref' // &
    & '}'
    bufin = TRIM(stmpa) // TRIM(bufin)
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! End Other Math Function %}}}
  ! Math Constant(23) %{{{
  ! false, zero, \$. %{{{
  CALL next_command()
  maintag_comm   = 'false'
  subtag_comm(1) = 'zero'
  subtag_comm(2) = '\$.'
  help_comm = 'push 0'
  helpja_comm = '0をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_stack_append
    IF( iferror ) RETURN
    stack(stz) = 0.d0
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! true, one, nonzero, \$-, \$~. %{{{
  CALL next_command()
  maintag_comm   = 'true'
  subtag_comm(1) = 'one'
  subtag_comm(2) = 'nonzero'
  subtag_comm(3) = '\$-'
  subtag_comm(4) = '\$~.'
  help_comm = 'push 1'
  helpja_comm = '1をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_stack_append
    IF( iferror ) RETURN
    stack(stz) = 1.d0
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! two, \$= %{{{
  CALL next_command()
  maintag_comm   = 'two'
  subtag_comm(1) = '\$='
  help_comm = 'push 2'
  helpja_comm = '2をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_stack_append
    IF( iferror ) RETURN
    stack(stz) = 2.d0
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! ten, \$+ %{{{
  CALL next_command()
  maintag_comm   = 'ten'
  subtag_comm(1) = '\$+'
  help_comm = 'push 10'
  helpja_comm = '10をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_stack_append
    IF( iferror ) RETURN
    stack(stz) = 10.d0
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! pi, \$() %{{{
  CALL next_command()
  maintag_comm   = 'pi'
  subtag_comm(1) = '\$()'
  help_comm = 'push pi(=3.141592653589793)'
  helpja_comm = 'pi(=3.141592653589793)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_stack_append
    IF( iferror ) RETURN
    stack(stz) = 4.d0 * DATAN(1.d0)
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! napier, \$-^ %{{{
  CALL next_command()
  maintag_comm   = 'napier'
  subtag_comm(1) = '\$-^'
  help_comm = 'push napier(=2.718281828459045)'
  helpja_comm = 'napier(=2.718281828459045)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_stack_append
    IF( iferror ) RETURN
    stack(stz) = DEXP( 1.d0 )
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! egamma %{{{
  CALL next_command()
  maintag_comm   = 'egamma'
  help_comm = 'push egamma(=0.577215664901533)'
  helpja_comm = 'egamma(=0.577215664901533)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_stack_append
    IF( iferror ) RETURN
    stack(stz) = 0.57721566490153286d0
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! gold %{{{
  CALL next_command()
  maintag_comm   = 'gold'
  help_comm = 'push gold(=1.6180339887498948)'
  helpja_comm = 'gold(=1.6180339887498948)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_stack_append
    IF( iferror ) RETURN
    stack(stz) = 1.6180339887498948d0
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! catalan %{{{
  CALL next_command()
  maintag_comm   = 'catalan'
  help_comm = 'push catalan(=9.159655941772190)'
  helpja_comm = 'catalan(=9.159655941772190)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_stack_append
    IF( iferror ) RETURN
    stack(stz) = 9.159655941772190d0
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! glasier %{{{
  CALL next_command()
  maintag_comm   = 'glasier'
  help_comm = 'push glasier(=1.2824271291006226)'
  helpja_comm = 'glasier(=1.2824271291006226)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_stack_append
    IF( iferror ) RETURN
    stack(stz) = 1.2824271291006226d0
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! black, color_k, \&... %{{{
  CALL next_command()
  maintag_comm   = 'black'
  subtag_comm(1) = 'color_k'
  subtag_comm(2) = '\&...'
  help_comm = 'push black(=0 0 0)'
  helpja_comm = '黒(=0 0 0)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_stack_append
    CALL psc_stack_append
    CALL psc_stack_append
    IF( iferror ) RETURN
    stack(stz-2:stz) = [ 0.d0, 0.d0, 0.d0 ]
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! white, color_w, \&--- %{{{
  CALL next_command()
  maintag_comm   = 'white'
  subtag_comm(1) = 'color_w'
  subtag_comm(2) = '\&---'
  help_comm = 'push white(=1 1 1)'
  helpja_comm = '白(=1 1 1)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_stack_append
    CALL psc_stack_append
    CALL psc_stack_append
    IF( iferror ) RETURN
    stack(stz-2:stz) = [ 1.d0, 1.d0, 1.d0 ]
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! red, color_r, \&-.. %{{{
  CALL next_command()
  maintag_comm   = 'red'
  subtag_comm(1) = 'color_r'
  subtag_comm(2) = '\&-..'
  help_comm = 'push red(=1 0 0)'
  helpja_comm = '赤(=1 0 0)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_stack_append
    CALL psc_stack_append
    CALL psc_stack_append
    IF( iferror ) RETURN
    stack(stz-2:stz) = [ 1.d0, 0.d0, 0.d0 ]
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! green, color_g, \&.-. %{{{
  CALL next_command()
  maintag_comm   = 'green'
  subtag_comm(1) = 'color_g'
  subtag_comm(2) = '\&.-.'
  help_comm = 'push green(=0 1 0)'
  helpja_comm = '緑(=0 1 0)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_stack_append
    CALL psc_stack_append
    CALL psc_stack_append
    IF( iferror ) RETURN
    stack(stz-2:stz) = [ 0.d0, 1.d0, 0.d0 ]
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! blue, color_b, \&..- %{{{
  CALL next_command()
  maintag_comm   = 'blue'
  subtag_comm(1) = 'color_b'
  subtag_comm(2) = '\&..-'
  help_comm = 'push blue(=0 0 1)'
  helpja_comm = '青(=0 0 1)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_stack_append
    CALL psc_stack_append
    CALL psc_stack_append
    IF( iferror ) RETURN
    stack(stz-2:stz) = [ 0.d0, 0.d0, 1.d0 ]
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! magenta, color_m, \&-.- %{{{
  CALL next_command()
  maintag_comm   = 'magenta'
  subtag_comm(1) = 'color_m'
  subtag_comm(2) = '\&-.-'
  help_comm = 'push magenta(=1 0 1)'
  helpja_comm = 'マゼンタ(=1 0 1)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_stack_append
    CALL psc_stack_append
    CALL psc_stack_append
    IF( iferror ) RETURN
    stack(stz-2:stz) = [ 1.d0, 0.d0, 1.d0 ]
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! cyan, color_c, \&.-- %{{{
  CALL next_command()
  maintag_comm   = 'cyan'
  subtag_comm(1) = 'color_c'
  subtag_comm(2) = '\&.--'
  help_comm = 'push cyan(=0 1 1)'
  helpja_comm = 'シアン(=0 1 1)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_stack_append
    CALL psc_stack_append
    CALL psc_stack_append
    IF( iferror ) RETURN
    stack(stz-2:stz) = [ 0.d0, 1.d0, 1.d0 ]
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! yellow, color_y, \&--. %{{{
  CALL next_command()
  maintag_comm   = 'yellow'
  subtag_comm(1) = 'color_y'
  subtag_comm(2) = '\&--.'
  help_comm = 'push yellow(=1 1 0)'
  helpja_comm = '黄色(=1 1 0)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_stack_append
    CALL psc_stack_append
    CALL psc_stack_append
    IF( iferror ) RETURN
    stack(stz-2:stz) = [ 1.d0, 1.d0, 0.d0 ]
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! gray, grey %{{{
  CALL next_command()
  maintag_comm   = 'gray'
  subtag_comm(1) = 'grey'
  help_comm = 'push gray(=0.5 0.5 0.5)'
  helpja_comm = 'グレー(=0.5 0.5 0.5)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_stack_append
    CALL psc_stack_append
    CALL psc_stack_append
    IF( iferror ) RETURN
    stack(stz-2:stz) = [ .5d0, .5d0, .5d0 ]
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! dark %{{{
  CALL next_command()
  maintag_comm   = 'dark'
  help_comm = 'push dark gray(=0.25 0.25 0.25)'
  helpja_comm = '暗いグレー(=0.25 0.25 0.25)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_stack_append
    CALL psc_stack_append
    CALL psc_stack_append
    IF( iferror ) RETURN
    stack(stz-2:stz) = [ .25d0, .25d0, .25d0 ]
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! light %{{{
  CALL next_command()
  maintag_comm   = 'light'
  help_comm = 'push light gray(=0.75 0.75 0.75)'
  helpja_comm = '明るいグレー(=0.75 0.75 0.75)をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_stack_append
    CALL psc_stack_append
    CALL psc_stack_append
    IF( iferror ) RETURN
    stack(stz-2:stz) = [ .75d0, .75d0, .75d0 ]
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! darker %{{{
  CALL next_command()
  maintag_comm   = 'darker'
  help_comm = 'make color on stack top more dark'
  helpja_comm = 'スタックトップの色をより暗くする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 3 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    IF( iferror ) RETURN
    stack(stz-2:stz) = stack(stz-2:stz) - 0.1d0
    stackchanged = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! lighter %{{{
  CALL next_command()
  maintag_comm   = 'lighter'
  help_comm = 'make color on stack top more light'
  helpja_comm = 'スタックトップの色をより明るくする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 3 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    IF( iferror ) RETURN
    stack(stz-2:stz) = stack(stz-2:stz) + 0.1d0
    stackchanged = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! End Math Constant %}}}
  ! Display(20) %{{{
  ! disp     ,dp    , \$_ %{{{
  CALL next_command()
  maintag_comm   = 'disp'
  subtag_comm(1) = 'dp'
  subtag_comm(2) = '\$_'
  help_comm = 'display list of real-number stack'
  helpja_comm = '実数型スタックのリストを表示する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_dispstacklist
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! disps    ,dps   , \"_ %{{{
  CALL next_command()
  maintag_comm   = 'disps'
  subtag_comm(1) = 'dps'
  subtag_comm(2) = '\"_'
  help_comm = 'display list of string stack'
  helpja_comm = '文字列型スタックのリストを表示する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize < 1 )THEN
      RETURN
    END IF
    DO idoa = 1, macrosize
      WRITE(*,'(A$)') '(' // TRIM(macro(idoa))
    END DO
    WRITE(*,*)
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! dispvar  ,dpvar , \"$_ %{{{
  CALL next_command()
  maintag_comm   = 'dispvar'
  subtag_comm(1) = 'dpvar'
  subtag_comm(2) = '\"$_'
  help_comm = 'display list of real-number variables'
  helpja_comm = '実数型変数のリストを表示する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    DO idoa = 1, varsize
      WRITE(*,'(I3$)') var(idoa) % loc
      WRITE(*,'(A$)') '(' // TRIM( var(idoa) % tag ) // ' = '
      CALL psc_printnum(var(idoa)%val, fmt_a, stmpa, &
      & dev_stdou%devnum, .TRUE., sepstr)
    END DO
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! dispvars ,dpvars, \""_ %{{{
  CALL next_command()
  maintag_comm   = 'dispvars'
  subtag_comm(1) = 'dpvars'
  subtag_comm(2) = '\""_'
  help_comm = 'display list of string variables (not implemented)'
  helpja_comm = '文字列型変数のリストを表示する(未実装)'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    ! version 2 で実装予定
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! dispmap  ,dpmap , \''_ %{{{
  CALL next_command()
  maintag_comm   = 'dispmap'
  subtag_comm(1) = 'dpmap'
  subtag_comm(2) = "\''_"
  help_comm = 'display list of map'
  helpja_comm = 'マップのリストを表示する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    DO idoa = 1, mapsize
      WRITE(*,'(A$)') ' '
      WRITE(*,'(A$)') '(' // TRIM( map(idoa) % lhs )
      IF( map(idoa) % no )THEN
        WRITE(*,'(A$)') ' -> '
      ELSE
        WRITE(*,'(A$)') ' => '
      END IF
      WRITE(*,'(A$)') '(' // TRIM( map(idoa) % rhs )
    END DO
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! dispcom  ,dpcom %{{{
  CALL next_command()
  maintag_comm   = 'dispcom'
  subtag_comm(1) = 'dpcom'
  help_comm = 'display list of command (not implemented)'
  helpja_comm = 'コマンドのリストを表示する(未実装)'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    ! version 2 で実装予定
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! dispfile ,dpfile %{{{
  CALL next_command()
  maintag_comm   = 'dispfile'
  subtag_comm(1) = 'dpfile'
  help_comm = 'display list of file in current directory'
  helpja_comm = 'カレントディレクトリにおけるファイルのリストを表示する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL SYSTEM('ls>.psc_system_getlsresult')
    itmpa = psc_free_unit()
    OPEN(itmpa, FILE='.psc_system_getlsresult', STATUS= 'old')
    itmpb = 0
    DO
      READ(itmpa, '(1000A)', IOSTAT= ierror) stmpa
      IF( ierror/=0)EXIT
      itmpb = itmpb + 1
    END DO
    CLOSE(itmpa)
    IF( iferror ) RETURN
    stmpa = ''
    itmpa = psc_free_unit()
    OPEN( itmpa, FILE= '.psc_system_getlsresult', STATUS= 'old' )
    DO idoa = 1, itmpb
      READ(itmpa,*) stmpb
      stmpa = TRIM(stmpa) // '(' // TRIM(stmpb) // ')'
    END DO
    CLOSE(itmpa)
    CALL SYSTEM('rm .psc_system_getlsresult')
    stmpa = ADJUSTL( stmpa )
    WRITE(*,'(A)') ' ' // TRIM( stmpa )
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! dispload ,dpload %{{{
  CALL next_command()
  maintag_comm   = 'dispload'
  subtag_comm(1) = 'dpload'
  help_comm = 'display list of loaded files'
  helpja_comm = 'ロードされているファイルのリストを表示する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    DO idoa = 1, ildnow
      WRITE(*,'(A)') '(' // TRIM( dev_ld(idoa) % tag )
    END DO
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! dispin   ,dpin %{{{
  CALL next_command()
  maintag_comm   = 'dispin'
  subtag_comm(1) = 'dpin'
  help_comm = 'display list of files opened for read'
  helpja_comm = '読み込み用に開かれているファイルのリストを表示する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    DO idoa = 1, iinnow
      WRITE(*,'(A)') '(' // TRIM( dev_in(idoa) % tag )
    END DO
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! dispout  ,dpout %{{{
  CALL next_command()
  maintag_comm   = 'dispout'
  subtag_comm(1) = 'dpout'
  help_comm = 'display list of files opened for write'
  helpja_comm = '書き込み用に開かれているファイルのリストを表示する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    DO idoa = 1, iounow
      WRITE(*,'(A)') '(' // TRIM( dev_ou(idoa) % tag )
    END DO
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  !
  ! list     ,ls    , \$@ %{{{
  CALL next_command()
  maintag_comm   = 'list'
  subtag_comm(1) = 'ls'
  subtag_comm(2) = '\$@'
  help_comm = 'push list of real-number stack as string'
  helpja_comm = '実数型スタックのリストを文字列としてプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    stmpa = ')'
    DO idoa = 1, stz
      CALL psc_printnum(stack(idoa), fmt_a, stmpb, &
      & -1, .FALSE., sepstr)
      itmpa = LEN_TRIM(stmpa)
      stmpa = stmpa(1:itmpa-1) // TRIM(stmpb)
    END DO
    CALL psc_strst_append
    IF( iferror ) RETURN
    macro(1) = stmpa
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! lists    ,lss   , \"@ %{{{
  CALL next_command()
  maintag_comm   = 'lists'
  subtag_comm(1) = 'lss'
  subtag_comm(2) = '\"@'
  help_comm = 'push list of string stack as string'
  helpja_comm = '文字列型スタックのリストを文字列としてプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    stmpa = ''
    DO idoa = 1, macrosize
      stmpa = TRIM(stmpa) // '(' // TRIM( macro(idoa) )
    END DO
    CALL psc_strst_append
    IF( iferror ) RETURN
    macro(1) = TRIM( stmpa ) // ')'
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! listvar  ,lsvar , \"$@ %{{{
  CALL next_command()
  maintag_comm   = 'listvar'
  subtag_comm(1) = 'lsvar'
  subtag_comm(2) = '\"$@'
  help_comm = 'push list of real-number variables as string'
  helpja_comm = '実数型変数のリストを文字列としてプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    stmpa = ''
    DO idoa = 1, varsize
      WRITE(stmpb,'(I3)') var(idoa) % loc
      stmpb = ADJUSTL( stmpb )
      stmpa = TRIM( stmpa ) // TRIM( stmpb )
      stmpa = TRIM( stmpa ) // '(' // TRIM(var(idoa)%tag)
    END DO
    CALL psc_strst_append
    IF( iferror ) RETURN
    macro(1) = TRIM( stmpa ) // ')'
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! listvars ,lsvars, \""@ %{{{
  CALL next_command()
  maintag_comm   = 'listvars'
  subtag_comm(1) = 'lsvars'
  subtag_comm(2) = '\""@'
  help_comm = 'push list of string variables as string (not implemented)'
  helpja_comm = '文字列型変数のリストを文字列としてプッシュする(未実装)'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    ! version 2 で実装予定
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! listmap  ,lsmap , \''@ %{{{
  CALL next_command()
  maintag_comm   = 'lsitmap'
  subtag_comm(1) = 'lsmap'
  subtag_comm(2) = "\''@"
  help_comm = 'push list of map as string'
  helpja_comm = 'マップのリストを文字列としてプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    stmpa = ''
    DO idoa = 1, mapsize
      stmpa = TRIM(stmpa) // '(' // TRIM(map(idoa)%lhs)
    END DO
    CALL psc_strst_append
    IF( iferror ) RETURN
    macro(1) = TRIM( stmpa ) // ')'
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! listcom  ,lscom %{{{
  CALL next_command()
  maintag_comm   = 'lsitcom'
  subtag_comm(1) = 'lscom'
  help_comm = 'push list of command as string (not implemented)'
  helpja_comm = 'コマンドのリストを文字列としてプッシュする(未実装)'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    ! version 2 で実装予定
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! listfile ,lsfile %{{{
  CALL next_command()
  maintag_comm   = 'listfile'
  subtag_comm(1) = 'lsfile'
  help_comm = 'push list of file in current directory as string'
  helpja_comm = 'カレントディレクトリにおけるファイルのリストを' // &
  & '文字列としてプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL SYSTEM('ls>.psc_system_getlsresult')
    itmpa = psc_free_unit()
    OPEN(itmpa, FILE='.psc_system_getlsresult', STATUS= 'old' )
    itmpb = 0
    DO
      READ(itmpa, '(1000A)', IOSTAT = ierror) stmpa
      IF( ierror /= 0 ) EXIT
      itmpb = itmpb+1
    END DO
    CLOSE(itmpa)
    CALL psc_strst_append
    IF( iferror ) RETURN
    stmpa = ''
    itmpa = psc_free_unit()
    OPEN(itmpa, FILE='.psc_system_getlsresult', STATUS= 'old' )
    DO idoa = 1, itmpb
      READ(itmpa,*) stmpb
      stmpa = TRIM(stmpa) // '(' // TRIM(stmpb) // ')'
    END DO
    CLOSE(itmpa)
    macro(1) = TRIM(stmpa) // ')'
    CALL SYSTEM('rm .psc_system_getlsresult')
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! listload ,lsload %{{{
  CALL next_command()
  maintag_comm   = 'listload'
  subtag_comm(1) = 'lsload'
  help_comm = 'push list of loaded files as string'
  helpja_comm = 'ロードされているファイルのリストを' // &
  & '文字列としてプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    stmpa = ''
    DO idoa = 1, ildnow
      stmpa = TRIM(stmpa) // '(' // TRIM( dev_ld(idoa) % tag ) // ')'
    END DO
    CALL psc_strst_append
    IF( iferror ) RETURN
    macro(1) = TRIM( stmpa ) // ')'
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! listin   ,lsin %{{{
  CALL next_command()
  maintag_comm   = 'listin'
  subtag_comm(1) = 'lsin'
  help_comm = 'push list of files opened for read as string'
  helpja_comm = '読み込み用に開かれているファイルのリストを' // &
  & '文字列としてプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    stmpa = ''
    DO idoa = 1, iinnow
      stmpa = TRIM(stmpa) // '(' // TRIM( dev_in(idoa) % tag ) // ')'
    END DO
    CALL psc_strst_append
    IF( iferror ) RETURN
    macro(1) = TRIM( stmpa ) // ')'
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! listout  ,lsout %{{{
  CALL next_command()
  maintag_comm   = 'listout'
  subtag_comm(1) = 'lsout'
  help_comm = 'push list of files opened for write as string'
  helpja_comm = '書き込み用に開かれているファイルのリストを' // &
  & '文字列としてプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    stmpa = ''
    DO idoa = 1, iounow
      stmpa = TRIM(stmpa) // '(' // TRIM( dev_ou(idoa) % tag ) // ')'
    END DO
    CALL psc_strst_append
    IF( iferror ) RETURN
    macro(1) = TRIM( stmpa ) // ')'
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! End Show %}}}
  ! Help(14) %{{{
  ! help, h, man, ? %{{{
  CALL next_command()
  maintag_comm   = 'help'
  subtag_comm(1) = 'h'
  subtag_comm(2) = 'man'
  subtag_comm(3) = '?'
  help_comm = 'display help'
  helpja_comm = 'ヘルプを表示する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_printcolor( cl_helptitle )
    WRITE(*,'(A)')  ' PSC version ' // TRIM( pscversion ) // ', updated ' // TRIM( pscupdate )
    WRITE(*,'(A)')  ' PSC is a stack-oriented script language contrived by ' // TRIM( pscauthor )
    WRITE(*,'(A)')  ' Type "q" to terminate PSC                                            '
    CALL psc_printcolor( cl_helpcontents )
    WRITE(*,'(A)')  ' Command Line Options                                                 '
    WRITE(*,'(A)')  '   -h, --help           : display this help message and quit          '
    WRITE(*,'(A)')  '   -v, --version        : dispaly version info and quit               '
    WRITE(*,'(A)')  '   -q, --quiet          : invoke PSC with quiet-flag ON               ' 
    WRITE(*,'(A)')  '   -Q, --off_quiet      : invoke PSC with quiet-flag OFF              '
    WRITE(*,'(A)')  '   -s, --superquiet     : invoke PSC with superquiet-flag ON          '
    WRITE(*,'(A)')  '   -S, --off_superquiet : invoke PSC with superquiet-flag OFF         '
    WRITE(*,'(A)')  '   -c, --color          : invoke PSC with color-flag ON               '
    WRITE(*,'(A)')  '   -C, --off_color      : invoke PSC with color-flag OFF              '
    WRITE(*,'(A)')  '   -n, --autonl         : invoke PSC with autonl-flag ON              '
    WRITE(*,'(A)')  '   -N, --off_autonl     : invoke PSC with autonl-flag OFF             '
    WRITE(*,'(A)')  '   -f <file>, --file <file>    : execute <file> and quit              '
    WRITE(*,'(A)')  '   -F <file>, --FILE <file>    : execute <file>                       '
    WRITE(*,'(A)')  '   -e ''<script>'', --script ''<script>''  : execute <script> and quit    '
    WRITE(*,'(A)')  '   -E ''<script>'', --SCRIPT ''<script>''  : execute <script>             '
    WRITE(*,'(A)' ) 'for debug options, use ''hdb'' command                                  '
    WRITE(*,'(A)')
    CALL psc_printcolor( cl_helptitle )
    WRITE(*,'(A)')  ' use following commands to see brief descriptions of commands'
    CALL psc_printcolor( cl_helpcontents )
    WRITE(*,'(A)')  '   hdi      :  Display and Miscellaneous '
    WRITE(*,'(A)')  '   hst      :  Stack Operations          '
    WRITE(*,'(A)')  '   hmb      :  Math Basic                '
    WRITE(*,'(A)')  '   hmf      :  Math Functions            '
    WRITE(*,'(A)')  '   hmc      :  Math Constants            '
    WRITE(*,'(A)')  '   hfi      :  File Operation            '
    WRITE(*,'(A)')  '   hsr      :  String Operation          '
    WRITE(*,'(A)')  '   hmv      :  Macro and Variables       '
    WRITE(*,'(A)')  '   hop      :  Options                   '
    WRITE(*,'(A)')  '   hdr      :  Draw Script               '
    WRITE(*,'(A)')  '   hgr      :  Graph Command             '
    WRITE(*,'(A)' ) '   hgrint   :  Integer Graph-variables   '
    WRITE(*,'(A)' ) '   hgrreal  :  Real Graph-variables      '
    WRITE(*,'(A)' ) '   hgrstr   :  String Graph-variables    '
    WRITE(*,'(A)' ) '   hgrbool  :  Bool Graph-variables      '
    WRITE(*,'(A)' ) '   hgrcolor :  Color Graph-variables     '
    WRITE(*,'(A)')  '   hall     :  Full Output               '
    WRITE(*,'(A)')  '   hdb      :  Debug (for developers)    '
    WRITE(*,'(A)')  '   cmdhelp  :  help for command (##1)    '
    CALL psc_printcolor( cl_reset )
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! hdi %{{{
  CALL next_command()
  maintag_comm   = 'hdi'
  help_comm = 'display help on display commands'
  helpja_comm = '表示コマンドに関するヘルプを表示する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_printcolor( cl_helptitle )
    WRITE(*,'(A)')  '======================================================================'
    WRITE(*,'(A)' ) '[Help-DI]. Display and Miscelaneous                                   '
    WRITE(*,'(A)' ) '             Command        Operation                                 '
    CALL psc_printcolor( cl_helpcontents )
    WRITE(*,'(A)' ) '   quit,q,endapp,bye,\//   exit this App                              '
    WRITE(*,'(A)' ) '               stop,\//!   stop this App with error-code 1            '
    WRITE(*,'(A)' ) '                help,h,?   display help                               '
    WRITE(*,'(A)' ) '           msg,print,===   pop (##1) to stdout                        '
    WRITE(*,'(A)' ) ' relax,do_nothing,nop,\\   do nothing                                 '
    WRITE(*,'(A)' ) '    sleep,pause,zzz,\...   sleep for (#1) seconds                     '
    WRITE(*,'(A)' ) '             cls,wipe,\^   clear screen                               '
    WRITE(*,'(A)' ) '    disp    ,dp    , \$_   display list of stacks                     '
    WRITE(*,'(A)' ) '    disps   ,dps   , \"_   display list of string-stacks              '
    WRITE(*,'(A)' ) '    dispvar ,dpvar ,\"$_   display list of variables                  '
    WRITE(*,'(A)' ) '    dispvars,dpvars,\""_   display list of string-variables           '
    WRITE(*,'(A)' ) '    dispmap ,dpmap ,\''''_   display list of maps                       '
    WRITE(*,'(A)' ) '    dispcom ,dpcom         display list of available commands         '
    WRITE(*,'(A)' ) '    dispfile,dpfile        display list of files                      '
    WRITE(*,'(A)' ) '    dispload,dpload        display list of load files                 '
    WRITE(*,'(A)' ) '    dispin  ,dpin          display list of input files                '
    WRITE(*,'(A)' ) '    dispout ,dpout         display list of output files               '
    WRITE(*,'(A)' ) '    list    ,ls    , \$@   get list of stacks                         '
    WRITE(*,'(A)' ) '    lists   ,lss   , \"@   get list of string-stacks                  '
    WRITE(*,'(A)' ) '    listvar ,lsvar ,\"$@   get list of variables                      '
    WRITE(*,'(A)' ) '    listvars,lsvars,\""@   get list of string-variables               '
    WRITE(*,'(A)' ) '    listmap ,lsmap ,\''''@   get list of maps                           '
    WRITE(*,'(A)' ) '    listcom ,lscom         get list of available commands             '
    WRITE(*,'(A)' ) '    listfile,lsfile        get list of files                          '
    WRITE(*,'(A)' ) '    listload,lsload        get list of load files                     '
    WRITE(*,'(A)' ) '    listin  ,lsin          get list of input files                    '
    WRITE(*,'(A)' ) '    listout ,lsout         get list of output files                   '
    CALL psc_printcolor( cl_helptitle )
    WRITE(*,'(A)')  '======================================================================'
    CALL psc_printcolor( cl_reset )
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! hst %{{{
  CALL next_command()
  maintag_comm   = 'hst'
  help_comm = 'display help on basic stack operations'
  helpja_comm = '基本的なスタック操作に関するヘルプを表示する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_printcolor( cl_helptitle )
    WRITE(*,'(A)')  '======================================================================'
    WRITE(*,'(A)' ) '[Help-ST]. Basic Stack Operations                                     '
    WRITE(*,'(A)' ) '         Command          Operation                                   '
    CALL psc_printcolor( cl_helpcontents )
    WRITE(*,'(A)' ) '      size ,count ,#    get number of stacks                          '
    WRITE(*,'(A)' ) '      sizes,counts,##   get number of string-stacks                   '
    WRITE(*,'(A)' ) '              pop ,=    print and kill (#1)                           '
    WRITE(*,'(A)' ) '              pops,==   print and kill (##1)                          '
    WRITE(*,'(A)' ) '      kill ,dump ,\<    kill (#1)                                     '
    WRITE(*,'(A)' ) '      kills,dumps,\<<   kill (##1)                                    '
    WRITE(*,'(A)' ) '             dup ,\>    duplicate (#1)                                '
    WRITE(*,'(A)' ) '             dups,\>>   duplicate (##1)                               '
    WRITE(*,'(A)' ) '   exch ,swap ,\<->     exchange (#2) and (#1)                        '
    WRITE(*,'(A)' ) '   exchs,swaps,\<<->>   exchange (##2) and (##1)                      '
    WRITE(*,'(A)' ) '    clear ,clr ,\><     clear all stacks                              '
    WRITE(*,'(A)' ) '    clears,clrs,\>><<   clear all string-stacks                       '
    WRITE(*,'(A)' ) '             pull ,[    rotate all stacks                             '
    WRITE(*,'(A)' ) '             pulls,[[   rotate all string-stacks                      '
    WRITE(*,'(A)' ) '             push ,]    inverse rotate stacks                         '
    WRITE(*,'(A)' ) '             pushs,]]   inverse rotate string-stacks                  '
    WRITE(*,'(A)' ) '          roll ,\[]     postscript-like roll command                  '
    WRITE(*,'(A)' ) '          rolls,\[[]]   postscript-like roll command                  '
    WRITE(*,'(A)' ) '       merge,\[]<->[]   merge stacks                                  '
    WRITE(*,'(A)' ) 'merges,\[[]]<<->>[[]]   merge string-stacks                           '
    WRITE(*,'(A)' ) '         index ,\<>     duplicate (#(int(#1)+1))                      '
    WRITE(*,'(A)' ) '         indexs,\<<>>   duplicate (##(int(#1)))                       '
    WRITE(*,'(A)' ) '          rev ,\<=>     reverse (#1) to (#(int(#1)))                  '
    WRITE(*,'(A)' ) '          revs,\<<=>>   reverse (##1) to (##(int(#1)))                '
    WRITE(*,'(A)' ) '        dupop ,\>;=     print (#1) without dump                       '
    WRITE(*,'(A)' ) '        dupops,\>>;==   print (##1) without dump                      '
    WRITE(*,'(A)' ) '   indexpop ,\<>;=      index then pop                                '
    WRITE(*,'(A)' ) '   indexpops,\<<>>;==   indexs then pops                              '
    CALL psc_printcolor( cl_helptitle )
    WRITE(*,'(A)')  '======================================================================'
    CALL psc_printcolor( cl_reset )
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! hmb %{{{
  CALL next_command()
  maintag_comm   = 'hmb'
  help_comm = 'display help on basic arithmatics'
  helpja_comm = '基本的な数学演算に関するヘルプを表示する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_printcolor( cl_helptitle )
    WRITE(*,'(A)')  '======================================================================'
    WRITE(*,'(A)' ) '[Help-MB]. Math Basic                                                 '
    WRITE(*,'(A)' ) '           Command        Operation                                   '
    CALL psc_printcolor( cl_helpcontents )
    WRITE(*,'(A)' ) '            add,+,\~-   add       ((#2)+(#1))                         '
    WRITE(*,'(A)' ) '            sub,-,\~+   subtract  ((#2)-(#1))                         '
    WRITE(*,'(A)' ) '            mul,*,\~/   multiply  ((#2)*(#1))                         '
    WRITE(*,'(A)' ) '            div,/,\~*   divide    ((#2)/(#1))                         '
    WRITE(*,'(A)' ) '               mod,\%   mod       ((#2)%(#1))                         '
    WRITE(*,'(A)' ) '             modulo,%   modulo    ((#2)%(#1))                         '
    WRITE(*,'(A)' ) '                  min   min((#1),(#2))                                '
    WRITE(*,'(A)' ) '                  max   max((#1),(#2))                                '
    WRITE(*,'(A)' ) '             pow,^,**   power     ((#2)^(#1))                         '
    WRITE(*,'(A)' ) '               fact,!   factorial (int(#1)!)                          '
    WRITE(*,'(A)' ) '                 gt,>   1.0 if (#2) > (#1)                            '
    WRITE(*,'(A)' ) '                 lt,<   1.0 if (#2) < (#1)                            '
    WRITE(*,'(A)' ) '           ge,\>=,\~<   1.0 if (int(#2)) >= (int(#1))                 '
    WRITE(*,'(A)' ) '           le,\<=,\~>   1.0 if (int(#2)) <= (int(#1))                 '
    WRITE(*,'(A)' ) '          eq,\==,\~<>   1.0 if (int(#2)) == (int(#1))                 '
    WRITE(*,'(A)' ) '      neq,xor,\~=,\<>   1.0 if (int(#2)) /= (int(#1))                 '
    WRITE(*,'(A)' ) '           not,neg,\~   1.0 if (int(#1)) == 0                         '
    WRITE(*,'(A)' ) '                and,&   1.0 if (int(#1)) /= 0 and (int(#2)) /= 0      '
    WRITE(*,'(A)' ) '             nand,\~&   1.0 if (int(#1)) == 0 or (int(#2)) == 0       '
    WRITE(*,'(A)' ) '                 or,|   1.0 if (int(#1)) /= 0 or (int(#2)) /= 0       '
    WRITE(*,'(A)' ) '              nor,\~|   1.0 if (int(#1)) == 0 and (int(#2)) == 0      '
    WRITE(*,'(A)' ) '              inc,\++   increase (#1) by incunit                      '
    WRITE(*,'(A)' ) '              dec,\--   decrease (#1) by incunit                      '
    WRITE(*,'(A)' ) '           even,\$=%~   1.0 if (int(#1)) is even number               '
    WRITE(*,'(A)' ) '            odd,\$=%    1.0 if (int(#1)) is odd number                 '
    WRITE(*,'(A)' ) '          positive,\+   1.0 if (#1) is positive number                '
    WRITE(*,'(A)' ) '          negative,\-   1.0 if (#1) is negative number                '
    CALL psc_printcolor( cl_helptitle )
    WRITE(*,'(A)')  '======================================================================'
    CALL psc_printcolor( cl_reset )
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! hmf %{{{
  CALL next_command()
  maintag_comm   = 'hmf'
  help_comm = 'display help on math functions'
  helpja_comm = '各種数学関数に関するヘルプを表示する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_printcolor( cl_helptitle )
    WRITE(*,'(A)')  '======================================================================'
    WRITE(*,'(A)' ) '[Help-MF]. Math Functions                                             '
    WRITE(*,'(A)' ) '           Command        Operation                                   '
    CALL psc_printcolor( cl_helpcontents )
    WRITE(*,'(A)' ) '                  int   int (#1)                                      '
    WRITE(*,'(A)' ) '      nint,round,\$>#   nearest integer for (#1)                      '
    WRITE(*,'(A)' ) '          floor,\$>_#   floor (#1)                                    '
    WRITE(*,'(A)' ) '   ceil,ceiling,\$>^#   ceiling (#1)                                  '
    WRITE(*,'(A)' ) '              abs,\$|   absolute value                                '
    WRITE(*,'(A)' ) '              minus,_   minus                                         '
    WRITE(*,'(A)' ) '          double,\$=*   multiply two                                  '
    WRITE(*,'(A)' ) '            half,\$=/   divide by two                                 '
    WRITE(*,'(A)' ) '              exp,\$^   exponential                                   '
    WRITE(*,'(A)' ) '              ln,\$~^   natural log                                   '
    WRITE(*,'(A)' ) '              log       log       (log_(#2){(#1)})                    '
    WRITE(*,'(A)' ) '              logten    log_10    (log_(10.0){(#1)})                  '
    WRITE(*,'(A)' ) '              inv,\$~   inverse   (1.0/(#1))                          '
    WRITE(*,'(A)' ) '  square,squared,\$=^   square                                        '
    WRITE(*,'(A)' ) '       sqr,sqrt,\$~=^   square root                                   '
    WRITE(*,'(A)' ) '  random,rand,rnd,\$?   random number between 0 and 1                 '
    WRITE(*,'(A)' ) '            primelist   make prime-numbers-list of size (int(#1))     '
    WRITE(*,'(A)' ) '                prime   get (int(#1))''th-prime number                '
    WRITE(*,'(A)' ) '            sin,\$%/|   sin                                           '
    WRITE(*,'(A)' ) '            cos,\$%/_   cos                                           '
    WRITE(*,'(A)' ) '            tan,\$%_|   tan                                           '
    WRITE(*,'(A)' ) '          asin,\$~%/|   asin                                          '
    WRITE(*,'(A)' ) '          acos,\$~%/_   acos                                          '
    WRITE(*,'(A)' ) '          atan,\$~%_|   atan                                          '
    WRITE(*,'(A)' ) '      atantwo,\$~%_|&   atan2                                         '
    WRITE(*,'(A)' ) '          sinh,\$%/|!   sinh                                          '
    WRITE(*,'(A)' ) '          cosh,\$%/_!   cosh                                          '
    WRITE(*,'(A)' ) '          tanh,\$%_|!   tanh                                          '
    WRITE(*,'(A)' ) '         besselj,besj   besselj(n,x),n=int(#2),x = (#1)               '
    WRITE(*,'(A)' ) '                gamma   gamma function                                '
    WRITE(*,'(A)' ) '                  erf   erf function                                  '
    WRITE(*,'(A)' ) '    integratefunc,itf   func:(##1) range:(#3)~(#2) div:(#1)           '
    WRITE(*,'(A)' ) '      averagefunc,avf   func:(##1) range:(#3)~(#2) div: (#1)          '
    WRITE(*,'(A)' ) '         difffunc,dff   func:(##1) at (#1)                            '
    WRITE(*,'(A)' ) '    findzeropoint,fzp   func:(##1) range:(#2)~(#1)                    '
    CALL psc_printcolor( cl_helptitle )
    WRITE(*,'(A)')  '======================================================================'
    CALL psc_printcolor( cl_reset )
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! hmc %{{{
  CALL next_command()
  maintag_comm   = 'hmc'
  help_comm = 'display help on math constants'
  helpja_comm = '数学定数に関するヘルプを表示する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_printcolor( cl_helptitle )
    WRITE(*,'(A)')  '======================================================================'
    WRITE(*,'(A)' ) '[Help-MC]. Math Constants                                             '
    WRITE(*,'(A)' ) '                Command        Operation                              '
    CALL psc_printcolor( cl_helpcontents )
    WRITE(*,'(A)' ) '            false,zero,\$.   false (=0)                               '
    WRITE(*,'(A)' ) ' true,one,nonzero,\$-,\$~.   true  (=1)                               '
    WRITE(*,'(A)' ) '                   two,\$=   two   (=2)                               '
    WRITE(*,'(A)' ) '                   ten,\$+   ten   (=10)                              '
    WRITE(*,'(A)' ) '                   pi,\$()   pi      (=3.141592653589793...)          '
    WRITE(*,'(A)' ) '               napier,\$-^   e       (=2.718281828459045...)          '
    WRITE(*,'(A)' ) '                    egamma   gamma   (=0.577215664901532...)          '
    WRITE(*,'(A)' ) '                      gold   gold    (=1.618033988749894...)          '
    WRITE(*,'(A)' ) '                   catalan   catalan (=9.159655941772190...)          ' 
    WRITE(*,'(A)' ) '                   glasier   glasier (=1.282427129100622...)          '
    WRITE(*,'(A)' ) '       black,color_k,\&...   black   (=0 0 0)                         '
    WRITE(*,'(A)' ) '       white,color_w,\&---   white   (=1 1 1)                         '
    WRITE(*,'(A)' ) '         red,color_r,\&-..   red     (=1 0 0)                         '
    WRITE(*,'(A)' ) '       green,color_g,\&.-.   green   (=0 1 0)                         '
    WRITE(*,'(A)' ) '        blue,color_b,\&..-   blue    (=0 0 1)                         '
    WRITE(*,'(A)' ) '     magenta,color_m,\&-.-   magenta (=1 0 1)                         '
    WRITE(*,'(A)' ) '        cyan,color_c,\&.--   cyan    (=0 1 1)                         '
    WRITE(*,'(A)' ) '      yellow,color_y,\&--.   yellow  (=1 1 0)                         '
    WRITE(*,'(A)' ) '                 gray,grey   gray    (=0.50 0.50 0.50)                '
    WRITE(*,'(A)' ) '                      dark   dark    (=0.25 0.25 0.25)                '
    WRITE(*,'(A)' ) '                     light   light   (=0.75 0.75 0.75)                '
    CALL psc_printcolor( cl_helptitle )
    WRITE(*,'(A)')  '======================================================================'
    CALL psc_printcolor( cl_reset )
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}

  ! hfi %{{{
  CALL next_command()
  maintag_comm   = 'hfi'
  help_comm = 'display help on file operations'
  helpja_comm = 'ファイル操作に関するヘルプを表示する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_printcolor( cl_helptitle )
    WRITE(*,'(A)')  '======================================================================'
    WRITE(*,'(A)' ) '[Help-FI]. File Operation                                             '
    WRITE(*,'(A)' ) '           Command        Operation                                   '
    CALL psc_printcolor( cl_helpcontents )
    WRITE(*,'(A)' ) '                          save   save current history to (##1)        '
    WRITE(*,'(A)' ) '      load,source,so,.,include   load file (##1)                      '
    WRITE(*,'(A)' ) 'exload,exsource,exso,exinclude   exchange load file                   '
    WRITE(*,'(A)' ) '                        return   return from load                     '
    WRITE(*,'(A)' ) '            fileexist   return 1 if file exist, 0 if not              '
    WRITE(*,'(A)' ) '     openout,newwrite   open (##1) for output                         '
    WRITE(*,'(A)' ) '        exout,exwrite   exchange output file                          '
    WRITE(*,'(A)' ) '         closeout,eof   close current output file                     '
    WRITE(*,'(A)' ) '       openin,newread   open (##1) for input                          '
    WRITE(*,'(A)' ) '          exin,exread   exchange input file                           '
    WRITE(*,'(A)' ) '                lines   get lines in current file                     '
    WRITE(*,'(A)' ) '              closein   close current input file                      '
    WRITE(*,'(A)' ) '                 read   read input file to (##1)                      '
    WRITE(*,'(A)' ) '          makedata,md   func:(##2) file:(##1) range:(#3)~(#2) div:(#1)'
    WRITE(*,'(A)' ) '          countf,cntf   count word:(##1) in file:(##2)                '
    WRITE(*,'(A)' ) '              grep,gr   print lines include word:(##1) in file:(##2)  '
    CALL psc_printcolor( cl_helptitle )
    WRITE(*,'(A)')  '======================================================================'
    CALL psc_printcolor( cl_reset )
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! hsr %{{{
  CALL next_command()
  maintag_comm   = 'hsr'
  help_comm = 'display help on string operations'
  helpja_comm = '文字列操作に関するヘルプを表示する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_printcolor( cl_helptitle )
    WRITE(*,'(A)')  '==========================================================================='
    WRITE(*,'(A)' ) '[Help-SR]. String Operation                                                '
    WRITE(*,'(A)' ) '                    Command        Operation                               '
    CALL psc_printcolor( cl_helpcontents )
    WRITE(*,'(A)' ) '       writestr,wstr,cvs,\$>"   real (#1) to string (##1)                  '
    WRITE(*,'(A)' ) '        readstr,rstr,cvr,\">$   string (##1) to real (#1)                  '
    WRITE(*,'(A)' ) '              comb,concat,\".   concatenate (##2) after (##1)              '
    WRITE(*,'(A)' ) '                      cut,\"/   cut (##1) at position (#1)                 '
    WRITE(*,'(A)' ) '     flushleft,adjustleft,\"<   adjust left spaces of (##1)                '
    WRITE(*,'(A)' ) '                removetopseps   remove tops separators in (##1)            '
    WRITE(*,'(A)' ) '                     cuttoken   cut first PSC-token from (##1)             '
    WRITE(*,'(A)' ) '                      cutword   cut first (space-separated) word from (##1)'
    WRITE(*,'(A)' ) '                    char,\#>"   get ascii character for number (#1)        '
    WRITE(*,'(A)' ) '                   ichar,\">#   get ascii index for (##1)                  '
    WRITE(*,'(A)' ) '               length,len,\"|   get length of string:(##1)                 '
    WRITE(*,'(A)' ) '                eqs,same,\"==   1.0 if (##1) equals (##2)                  '
    WRITE(*,'(A)' ) '     reverseletter,revl,\"<=>   reverse letter orders in (##1)             '
    WRITE(*,'(A)' ) '                          get   get ascii for position (#1) of (##1)       '
    WRITE(*,'(A)' ) '                          put   put ascii(#1) into position (#2) of (##1)  '
    WRITE(*,'(A)' ) '          searchword,srcw,\"?   search word:(##1) in string:(##2)          '
    WRITE(*,'(A)' ) '       searchletter,srcl,\"??   search letter in (##1) in string:(##2)     '
    WRITE(*,'(A)' ) 'invsearchletter,invsrcl,\"???   search letter in (##1) in string:(##2)     '
    WRITE(*,'(A)' ) '          countword,cntw,\"?#   count word:(##1) in string:(##2)           '
    WRITE(*,'(A)' ) '                  isreal,\"=$   1.0 if (##1) is recognized as number       '
    WRITE(*,'(A)' ) '                      syscall   execute shell command (##1)                '
    WRITE(*,'(A)' ) '                   pwd,getcwd   get current pass                           '
    WRITE(*,'(A)' ) '                       home,~   get homedir pass                           '
    WRITE(*,'(A)' ) '                time,date,now   get current date and time                  '
    WRITE(*,'(A)' ) '                   pstart,\"{   add "(" to string-stack                    '
    WRITE(*,'(A)' ) '                     pend,\"}   add ")" to string-stack                    '
    WRITE(*,'(A)' ) '       newlinechar,nl,eol,\"$   add newline-char to string-stack           '
    WRITE(*,'(A)' ) '                       strxxd   convert string like xxd                    '
    WRITE(*,'(A)' ) '                     strcolor   make colorfull string                      '
    CALL psc_printcolor( cl_helptitle )
    WRITE(*,'(A)')  '==========================================================================='
    CALL psc_printcolor( cl_reset )
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! hmv %{{{
  CALL next_command()
  maintag_comm   = 'hmv'
  help_comm = 'display help on macro and variables'
  helpja_comm = 'マクロと変数に関するヘルプを表示する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_printcolor( cl_helptitle )
    WRITE(*,'(A)')  '======================================================================'
    WRITE(*,'(A)' ) '[Help-MV]. Macro and Variables                                        '
    WRITE(*,'(A)' ) '                  Command        Operation                            '
    CALL psc_printcolor( cl_helpcontents )
    WRITE(*,'(A)' ) '              do,repeat,if,\?   exec (##1) for (int(#1)) times        '
    WRITE(*,'(A)' ) '                 infdo,\?()()   exec (##1) for infinity times         '
    WRITE(*,'(A)' ) '                   ifelse,\?:   exec (##2) if ((int(#1))/=0) or (##1) '
    WRITE(*,'(A)' ) '                 exec,eval,\.   exec (##1)                            '
    WRITE(*,'(A)' ) 'builtin,command,primitive,\..   exec builtin (##1)                    '
    WRITE(*,'(A)' ) '                cycle,next,\!   cycle loop of number (int(#1))        '
    WRITE(*,'(A)' ) '               break,exit,\!!   break loop of number (int(#1))        '
    WRITE(*,'(A)' ) '               cycleall,\!@||   cycle all loop                        '
    WRITE(*,'(A)' ) '              breakall,\!!@||   break all loop                        '
    WRITE(*,'(A)' ) '                      def,\=>   make def-map (##2)=>(##1)             '
    WRITE(*,'(A)' ) '                     nore,\->   make nore-map (##2)->(##1)            '
    WRITE(*,'(A)' ) '               mapexist,\??@"   1.0 if map (##1) exist                '
    WRITE(*,'(A)' ) '               defexist,\??=>   1.0 if def-map (##1) exist            '
    WRITE(*,'(A)' ) '              noreexist,\??->   1.0 if nore-map (##1) exist           '
    WRITE(*,'(A)' ) '              getmaprhs,\?@">   get mapped string                     '
    WRITE(*,'(A)' ) '           clearmaps,clm,\><"   clear all maps                        '
    WRITE(*,'(A)' ) '           let,setvalue,\!@>$   let value (#1) in variable (##1)      '
    WRITE(*,'(A)' ) '         ref,getvalue,$,\?@>$   refer value of variable (##1)         '
    WRITE(*,'(A)' ) '                  varkill,\$>   kill variable (##1)                   '
    WRITE(*,'(A)' ) '               varexist,\??@$   1.0 if variable (##1) already exist   '
    WRITE(*,'(A)' ) '           clearvars,clv,\><$   clear all variables                   '
    WRITE(*,'(A)' ) '                     bgroup,{   increment group level                 '
    WRITE(*,'(A)' ) '                     egroup,}   decrement group level                 '
    CALL psc_printcolor( cl_helptitle )
    WRITE(*,'(A)')  '======================================================================'
    CALL psc_printcolor( cl_reset )
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! hop %{{{
  CALL next_command()
  maintag_comm   = 'hop'
  help_comm = 'display help on option settings'
  helpja_comm = 'オプションの設定に関するヘルプを表示する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_printcolor( cl_helptitle )
    WRITE(*,'(A)')  '======================================================================'
    WRITE(*,'(A)' ) '[Help-OP]. Options                                          '
    WRITE(*,'(A)' ) '             Command        Operation                                 '
    CALL psc_printcolor( cl_helpcontents )
    WRITE(*,'(A)' ) '            setflag,\!&   set flag (##1) to false if (#1) is 0        '
    WRITE(*,'(A)' ) '            getflag,\?&   get flag (##1)                              '
    WRITE(*,'(A)' ) '            setunit,\!+   set incremental unit to (#1)                '
    WRITE(*,'(A)' ) '            getunit,\?+   get incremental unit                        '
    WRITE(*,'(A)' ) '           setdigit,\!.   set number of digits to (#1)                '
    WRITE(*,'(A)' ) '           getdigit,\?.   get number of digits                        '
    WRITE(*,'(A)' ) '             setfmt,\!_   set output format to (##1)                  '
    WRITE(*,'(A)' ) '             getfmt,\?_   get output format                           '
    WRITE(*,'(A)' ) '             setsep,\!,   set output separator to (##1)               '
    WRITE(*,'(A)' ) '             getsep,\?,   get output separator                        '
    WRITE(*,'(A)' ) '             setps1,\!>   set prompt string 1 to (##1)                '
    WRITE(*,'(A)' ) '             getps1,\?>   get prompt string 1                         '
    WRITE(*,'(A)' ) '            setps2,\!>>   set prompt string 2 to (##1)                '
    WRITE(*,'(A)' ) '            getps2,\?>>   get prompt string 2                         '
    WRITE(*,'(A)' ) '             setmax,\!#   set max size of stack-list to (#1)          '
    WRITE(*,'(A)' ) '             getmax,\?#   get max size of stack-list                  '
    WRITE(*,'(A)' ) '              seterrmsg   set (#1)-th error message to (##1)          '
    WRITE(*,'(A)' ) '              geterrmsg   get (#1)-th error message                   '
    CALL psc_printcolor( cl_helptitle )
    WRITE(*,'(A)')  '======================================================================'
    WRITE(*,'(A)' ) ' List of available flags                                              '
    CALL psc_printcolor( cl_helpcontents )
    WRITE(*,'(A)' ) '            quiet         suppress unimportant output                 '
    WRITE(*,'(A)' ) '            super_quiet                                               '
    WRITE(*,'(A)' ) '            error_quiet   quiet even when error occurs                '
    WRITE(*,'(A)' ) '            error_cont    continue even when error occurs             '
    WRITE(*,'(A)' ) '            quiet_load    quiet at load                               '
    WRITE(*,'(A)' ) '            color         colorfull output                            '
    WRITE(*,'(A)' ) '            autonl        print newline-character after every output  '
    CALL psc_printcolor( cl_helptitle )
    WRITE(*,'(A)' ) 'for debug options, use "hdb" command                                  '
    WRITE(*,'(A)')  '======================================================================'
    CALL psc_printcolor( cl_reset )
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! hdr %{{{
  CALL next_command()
  maintag_comm   = 'hdr'
  help_comm = 'display help on draw commands'
  helpja_comm = 'ドローに関するヘルプを表示する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_printcolor( cl_helptitle )
    WRITE(*,'(A)')  '======================================================================'
    WRITE(*,'(A)' ) '[Help-DR]. Draw Script                                                '
    ! display kdraw reference
    itmpa = 1
    CALL psc_printcolor( cl_helpcontents )
    CALL kdraw(stmpa, stmpb, itmpa)
    CALL psc_printcolor( cl_helptitle )
    WRITE(*,'(A)')  '======================================================================'
    CALL psc_printcolor( cl_reset )
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! hgr %{{{
  CALL next_command()
  maintag_comm   = 'hgr'
  help_comm = 'display help on graph'
  helpja_comm = 'グラフに関するヘルプを表示する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_printcolor( cl_helptitle )
    WRITE(*,'(A)')  '======================================================================'
    WRITE(*,'(A)' ) '[Help-GR]. Graph Command                                              '
    WRITE(*,'(A)' ) '        Command        Operation                                      '
    CALL psc_printcolor( cl_helpcontents )
    WRITE(*,'(A)' ) '            draw,d,\**    draw data (##2) to make diagram (##1)       '
    WRITE(*,'(A)' ) '             plot,p,\*    plot data (##2) to make graph (##1)         '
    WRITE(*,'(A)' ) '     multiplot,mp,\*++    plot multiple (#1) of data (##2) on graph (##1)'
    WRITE(*,'(A)' ) '     debugplot,\(*_*)*    plot data (##2) to make graph (##1) with debug messages'
    WRITE(*,'(A)' ) '              plotinit    initialize plot setting                     '
    WRITE(*,'(A)' ) '         setkpmultiopt    set multiple ANY type kplot option          '
    WRITE(*,'(A)' ) '          setkpopt,\!*    set ANY type kplot option                   '
    WRITE(*,'(A)' ) '         setkpstr,\!*"    set string type kplot option                '
    WRITE(*,'(A)' ) '         setkpint,\!*#    set integer type kplot option               '
    WRITE(*,'(A)' ) '        setkpflag,\!*?    set logical type kplot option               '
    WRITE(*,'(A)' ) '        setkpreal,\!*$    set real type kplot option                  '
    WRITE(*,'(A)' ) '       setkpcolor,\!*&    set color type kplot option                 '
    WRITE(*,'(A)' ) '       setkpstylecolor    set (#1)th style color to (#4,#3,#2)        '
    WRITE(*,'(A)' ) '        setkpstyleline    set (#1)th style line to (##1)              '
    WRITE(*,'(A)' ) '       setkpstylepoint    set (#1)th style point to (##1)             '
    WRITE(*,'(A)' ) '      setkpstylepoint2    set (#1)th style point2 to (##1)            '
    WRITE(*,'(A)' ) '        setkpstylefont    set (#1)th style font to (##1)              '
    WRITE(*,'(A)' ) '         getkpstr,\?*"    get string type kplot option                '
    WRITE(*,'(A)' ) '         getkpint,\?*#    get integer type kplot option               '
    WRITE(*,'(A)' ) '        getkpflag,\?*?    get logical type kplot option               '
    WRITE(*,'(A)' ) '        getkpreal,\?*$    get real type kplot option                  '
    WRITE(*,'(A)' ) '       getkpcolor,\?*&    get color type kplot option                 '
    WRITE(*,'(A)' ) '       getkpstylecolor    get (#1)th style color                      '
    WRITE(*,'(A)' ) '        getkpstyleline    get (#1)th style line                       '
    WRITE(*,'(A)' ) '       getkpstylepoint    get (#1)th style point                      '
    WRITE(*,'(A)' ) '      getkpstylepoint2    get (#1)th style point2                     '
    WRITE(*,'(A)' ) '        getkpstylefont    get (#1)th style font                       '
    CALL psc_printcolor( cl_helptitle )
    WRITE(*,'(A)')  '======================================================================'
    CALL psc_printcolor( cl_reset )
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! hgrint %{{{
  CALL next_command()
  maintag_comm   = 'hgrint'
  help_comm = 'display help on integer graph-variables'
  helpja_comm = '整数グラフ変数に関するヘルプを表示する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_printcolor( cl_helptitle )
    WRITE(*,'(A)')  '======================================================================'
    WRITE(*,'(A)' ) '[Help-GRINT]. Integer Graph Commands     '
    CALL psc_printcolor( cl_helpcontents )
    WRITE(*,'(A)' ) ' p_mode       plot mode (1~6)            '
    WRITE(*,'(A)' ) ' tpst         title position (0~1)       '
    WRITE(*,'(A)' ) ' kpst         DELETED                    '
    WRITE(*,'(A)' ) ' brdt         border line mode (0~15)    '
    WRITE(*,'(A)' ) ' bdlt         border line type           '
    WRITE(*,'(A)' ) ' stpt         stable point type          '
    WRITE(*,'(A)' ) ' stlt         stable line type           '
    WRITE(*,'(A)' ) ' stcl         stable color type          '
    WRITE(*,'(A)' ) ' stsz         stable size type           '
    WRITE(*,'(A)' ) ' tclt         tics line type             '
    WRITE(*,'(A)' ) ' gxlt         x grid line type           '
    WRITE(*,'(A)' ) ' gylt         y grid line type           '
    WRITE(*,'(A)' ) ' vblt         vbar   line type           '
    WRITE(*,'(A)' ) ' xr_m         x range mode (0~3)         '
    WRITE(*,'(A)' ) ' yr_m         y range mode (0~3)         '
    WRITE(*,'(A)' ) ' zr_m         z range mode (0~3)         '
    WRITE(*,'(A)' ) ' ar_m         bar range mode (0~3)       '
    WRITE(*,'(A)' ) ' xdiv         x division                 '
    WRITE(*,'(A)' ) ' mxdiv        x division start           '
    WRITE(*,'(A)' ) ' xstep        x division step            '
    WRITE(*,'(A)' ) ' ydiv         y division                 '
    WRITE(*,'(A)' ) ' mydiv        y division start           '
    WRITE(*,'(A)' ) ' ystep        y division step            '
    WRITE(*,'(A)' ) ' zdiv         z division                 '
    WRITE(*,'(A)' ) ' adiv         bar division               '
    WRITE(*,'(A)' ) ' madiv        bar division start         '
    WRITE(*,'(A)' ) ' astep        bar division step          '
    WRITE(*,'(A)' ) ' ibmax        max bond number            '
    WRITE(*,'(A)' ) ' cnodemax     DELETED: use clnum         '
    WRITE(*,'(A)' ) ' clbn         color bar division number  '
    WRITE(*,'(A)' ) ' ltnum        number of style lines      '
    WRITE(*,'(A)' ) ' ptnum        number of style points     '
    WRITE(*,'(A)' ) ' clnum        number of style colors     '
    WRITE(*,'(A)' ) ' ftnum        number of style fonts      '
    WRITE(*,'(A)' ) ' sznum        number of style (pont) size'
    WRITE(*,'(A)' ) ' xlft         x label font type          '
    WRITE(*,'(A)' ) ' ylft         y label font type          '
    WRITE(*,'(A)' ) ' y2lft        y2 label font type         '
    WRITE(*,'(A)' ) ' tcft         tics font type             '
    WRITE(*,'(A)' ) ' vbft         vertbar font type          '
    WRITE(*,'(A)' ) ' ttft         title font type            '
    WRITE(*,'(A)' ) ' kyft         key font type              '
    CALL psc_printcolor( cl_helptitle )
    WRITE(*,'(A)')  '======================================================================'
    CALL psc_printcolor( cl_reset )
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! hgrreal %{{{
  CALL next_command()
  maintag_comm   = 'hgrreal'
  help_comm = 'display help on real graph-variables'
  helpja_comm = '実数グラフ変数に関するヘルプを表示する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_printcolor( cl_helptitle )
    WRITE(*,'(A)')  '======================================================================'
    WRITE(*,'(A)' ) '[Help-GRREAL]. Real Graph Commands                  '
    CALL psc_printcolor( cl_helpcontents )
    WRITE(*,'(A)' ) ' ppx     paper width                                '
    WRITE(*,'(A)' ) ' ppy     paper height                               '
    WRITE(*,'(A)' ) ' xr_r    x range ratio (when xr_m = 1,3)            '
    WRITE(*,'(A)' ) ' yr_r    y range ratio (when yr_m = 1,3)            '
    WRITE(*,'(A)' ) ' zr_r    z range ratio (when zr_m = 1,3)            '
    WRITE(*,'(A)' ) ' br_r    bond range ratio (when showbond = true)    '
    WRITE(*,'(A)' ) ' bondlenmax    maximum bond length                  '
    WRITE(*,'(A)' ) ' ar_r    bar range ratio (when ar_m = 1,3)          '
    WRITE(*,'(A)' ) ' xmin    x range minimum (when xr_m = 2)            '
    WRITE(*,'(A)' ) ' xmax    x range maximum (when xr_m = 2)            '
    WRITE(*,'(A)' ) ' ymin    y range minimum (when yr_m = 2)            '
    WRITE(*,'(A)' ) ' ymax    y range maximum (when yr_m = 2)            '
    WRITE(*,'(A)' ) ' zmin    z range minimum (when zr_m = 2)            '
    WRITE(*,'(A)' ) ' zmax    z range maximum (when zr_m = 2)            '
    WRITE(*,'(A)' ) ' amin    bar range minimum (when ar_m = 2)          '
    WRITE(*,'(A)' ) ' amax    bar range maximum (when ar_m = 2)          '
    WRITE(*,'(A)' ) ' angth   angular theta                              '
    WRITE(*,'(A)' ) ' angph   angular phi                                '
    WRITE(*,'(A)' ) ' pprat   perspective ratio                          '
    WRITE(*,'(A)' ) ' mist    atomospheric scattering intensity          '
    WRITE(*,'(A)' ) ' lineacceptrange   line accept range (should be >=1)'
    WRITE(*,'(A)' ) ' pointacceptrange point accept range (should be >=1)'
    WRITE(*,'(A)' ) ' pnts    point size                                 '
    WRITE(*,'(A)' ) ' pnth    point hide size                            '
    WRITE(*,'(A)' ) ' ptlw    point line width                           '
    WRITE(*,'(A)' ) ' linw    line width                                 '
    WRITE(*,'(A)' ) ' linh    line hide width                            '
    WRITE(*,'(A)' ) ' bdlw    border  line width                         '
    WRITE(*,'(A)' ) ' gxlw    x grid  line width                         '
    WRITE(*,'(A)' ) ' gylw    y grid  line width                         '
    WRITE(*,'(A)' ) ' vblw    vbar    line width                         '
    WRITE(*,'(A)' ) ' tclw    tics    line width                         '
    WRITE(*,'(A)' ) ' mtclw   subtics line width                         '
    WRITE(*,'(A)' ) ' clmg    color bar margin                           '
    WRITE(*,'(A)' ) ' gxmg    graph-border x-margin from paper-end       '
    WRITE(*,'(A)' ) ' gymg    graph-border y-margin from paper-end       '
    WRITE(*,'(A)' ) ' kymg    key margin from graph-border               '
    WRITE(*,'(A)' ) ' tcll    tics line length                           '
    WRITE(*,'(A)' ) ' mtcll   sub tics line length                       '
    WRITE(*,'(A)' ) ' kyll    key line length                            '
    WRITE(*,'(A)' ) ' tcfs    tics font size                             '
    WRITE(*,'(A)' ) ' tcmg    tics margin from border                    '
    WRITE(*,'(A)' ) ' lbfs    label font size                            '
    WRITE(*,'(A)' ) ' lxmg    label x-margin from border                 '
    WRITE(*,'(A)' ) ' lymg    label y-margin from border                 '
    WRITE(*,'(A)' ) ' bbxmg   boundingbox x-margin from graph-border     '
    WRITE(*,'(A)' ) ' bbymg   boundingbox y-margin from graph-border     '
    WRITE(*,'(A)' ) ' trx     x-translation distance                     '
    WRITE(*,'(A)' ) ' try     y-translation distance                     '
    WRITE(*,'(A)' ) ' ttkx    title kern x                               '
    WRITE(*,'(A)' ) ' ttky    title kern y                               '
    WRITE(*,'(A)' ) ' kykx    key kern x                                 '
    WRITE(*,'(A)' ) ' kyky    key kern y                                 '
    WRITE(*,'(A)' ) ' xlkx    x label kern x                             '
    WRITE(*,'(A)' ) ' xlky    x label kern y                             '
    WRITE(*,'(A)' ) ' ylkx    y label kern x                             '
    WRITE(*,'(A)' ) ' ylky    y label kern y                             '
    WRITE(*,'(A)' ) ' tckx    tics label kern x                          '
    WRITE(*,'(A)' ) ' tcky    tics label kern y                          '
    CALL psc_printcolor( cl_helptitle )
    WRITE(*,'(A)')  '======================================================================'
    CALL psc_printcolor( cl_reset )
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! hgrstr %{{{
  CALL next_command()
  maintag_comm   = 'hgrstr'
  help_comm = 'display help on string graph-variables'
  helpja_comm = '文字列グラフ変数に関するヘルプを表示する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_printcolor( cl_helptitle )
    WRITE(*,'(A)')  '======================================================================'
    WRITE(*,'(A)' ) '[Help-GRSTR]. String Graph Commands        '
    CALL psc_printcolor( cl_helpcontents )
    WRITE(*,'(A)' ) ' papertype    A4,B5,Letter,userdefined,etc '
    WRITE(*,'(A)' ) ' title        title string                 '
    WRITE(*,'(A)' ) ' commentchar  comment character in datafile'
    WRITE(*,'(A)' ) ' vertbarchar  vertbar character in datafile'
    WRITE(*,'(A)' ) ' xlab         x label string               '
    WRITE(*,'(A)' ) ' ylab         y label string               '
    WRITE(*,'(A)' ) ' zlab         z label string               '
    WRITE(*,'(A)' ) ' xtlf         x tics label format          '
    WRITE(*,'(A)' ) ' ytlf         y tics label format          '
    WRITE(*,'(A)' ) ' ztlf         z tics label format          '
    WRITE(*,'(A)' ) ' atlf         bar tics label format        '
    WRITE(*,'(A)' ) ' plotmode     plot mode                    '
    WRITE(*,'(A)' ) ' using        using                        '
    CALL psc_printcolor( cl_helptitle )
    WRITE(*,'(A)')  '======================================================================'
    CALL psc_printcolor( cl_reset )
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! hgrbool %{{{
  CALL next_command()
  maintag_comm   = 'hgrbool'
  help_comm = 'display help on bool graph-variables'
  helpja_comm = 'ブールグラフ変数に関するヘルプを表示する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_printcolor( cl_helptitle )
    WRITE(*,'(A)')  '======================================================================'
    WRITE(*,'(A)' ) '[Help-GRBOOL]. Bool Graph Commands            '
    CALL psc_printcolor( cl_helpcontents )
    WRITE(*,'(A)' ) ' sizesqr       square internal plot           '
    WRITE(*,'(A)' ) ' realratio     real ratio 2D plot             '
    WRITE(*,'(A)' ) ' rotmedia      rotate paper                   '
    WRITE(*,'(A)' ) ' implicitbb    use implicit bounding box      '
    WRITE(*,'(A)' ) ' colorfull     DELETED: use stcl              '
    WRITE(*,'(A)' ) ' hidden3d      use hidden-line elimination    '
    WRITE(*,'(A)' ) ' showline      show lines                     '
    WRITE(*,'(A)' ) ' showpoint     show points                    '
    WRITE(*,'(A)' ) ' showbond      show bonds instead of lines    '
    WRITE(*,'(A)' ) ' showcube      show cube border               '
    WRITE(*,'(A)' ) ' showxtics     show x tics                    '
    WRITE(*,'(A)' ) ' showytics     show y tics                    '
    WRITE(*,'(A)' ) ' showxtlabs    show x tics labels             '
    WRITE(*,'(A)' ) ' showytlabs    show y tics labels             '
    WRITE(*,'(A)' ) ' showztlabs    show z tics labels             '
    WRITE(*,'(A)' ) ' rotxtlabs     rotate x tics labels           '
    WRITE(*,'(A)' ) ' rotytlabs     rotate y tics labels           '
    WRITE(*,'(A)' ) ' showcolorbar  show color bar                 '
    WRITE(*,'(A)' ) ' showkey       show key                       '
    WRITE(*,'(A)' ) ' showkeybox    show key box                   '
    WRITE(*,'(A)' ) ' cutextrazero  cutoff extra zero in tics label'
    WRITE(*,'(A)' ) ' implicitx     use implicit x value           '
    WRITE(*,'(A)' ) ' autofix       fix kplot option without warn  '
    WRITE(*,'(A)' ) ' hiddenbond    hide bond by connecting atoms  '
    WRITE(*,'(A)' ) ' pseudoshade   use pseudo-shading technology  '
    WRITE(*,'(A)' ) ' morecomment   validate extra comments in EPSF'
    WRITE(*,'(A)' ) ' negaposi      toggle nega-posi colors        '
    CALL psc_printcolor( cl_helptitle )
    WRITE(*,'(A)')  '======================================================================'
    CALL psc_printcolor( cl_reset )
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! hgrcolor %{{{
  CALL next_command()
  maintag_comm   = 'hgrcolor'
  help_comm = 'display help on color graph-variables'
  helpja_comm = '色グラフ変数に関するヘルプを表示する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_printcolor( cl_helptitle )
    WRITE(*,'(A)')  '======================================================================'
    WRITE(*,'(A)' ) '[Help-GRCOLOR]. Color Graph Commands'
    CALL psc_printcolor( cl_helpcontents )
    WRITE(*,'(A)' ) ' bakc   background color       '
    WRITE(*,'(A)' ) ' glbc   global background color'
    WRITE(*,'(A)' ) ' bdlc   border line color      '
    WRITE(*,'(A)' ) ' tcfc   tics font color        '
    WRITE(*,'(A)' ) ' tclc   tics line color        '
    WRITE(*,'(A)' ) ' mtclc  mtics line color       '
    WRITE(*,'(A)' ) ' linc   line color             '
    WRITE(*,'(A)' ) ' pntc   point color            '
    WRITE(*,'(A)' ) ' gxlc   x grid line color      '
    WRITE(*,'(A)' ) ' gylc   y grid line color      '
    WRITE(*,'(A)' ) ' vblc   vbar line color        '
    WRITE(*,'(A)' ) ' edgc   edge color             '
    CALL psc_printcolor( cl_helptitle )
    WRITE(*,'(A)')  '======================================================================'
    CALL psc_printcolor( cl_reset )
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! hdb %{{{
  CALL next_command()
  maintag_comm   = 'hdb'
  help_comm = 'display help around debug'
  helpja_comm = 'デバグ関係のヘルプを表示する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_printcolor( cl_helptitle )
    WRITE(*,'(A)')  '======================================================================'
    CALL psc_printcolor( cl_helpcontents )
    WRITE(*,'(A)' ) '[Help-DB]. Debug Commands                    '
    WRITE(*,'(A)' ) '        Command        Operation             '
    WRITE(*,'(A)' ) '     debug,\(;_;)    turn ON all debug flag  '
    WRITE(*,'(A)' ) '  offdebug,\(^_^)    turn OFF all debug flag '
    CALL psc_printcolor( cl_helptitle )
    WRITE(*,'(A)')  '======================================================================'
    CALL psc_printcolor( cl_helpcontents )
    WRITE(*,'(A)')  ' Command Line Options                                                     '
    WRITE(*,'(A)')  '   -d, --debug              : invoke PSC with all debug flag ON           '
    WRITE(*,'(A)')  '   -D, --off_debug          : invoke PSC with all debug flag OFF          '
    WRITE(*,'(A)' ) '   --debug_loop             : invoke PSC with debug_loop         flag ON  '
    WRITE(*,'(A)' ) '   --debug_level            : invoke PSC with debug_level        flag ON  '
    WRITE(*,'(A)' ) '   --debug_commd            : invoke PSC with debug_commd        flag ON  '
    WRITE(*,'(A)' ) '   --debug_stack            : invoke PSC with debug_stack        flag ON  '
    WRITE(*,'(A)' ) '   --debug_buffer           : invoke PSC with all debug_buffer   flag ON  '
    WRITE(*,'(A)' ) '   --debug_buffer_start     : invoke PSC with debug_buffer_start flag ON  '
    WRITE(*,'(A)' ) '   --debug_buffer_read      : invoke PSC with debug_buffer_read  flag ON  '
    WRITE(*,'(A)' ) '   --debug_buffer_clean     : invoke PSC with debug_buffer_clean flag ON  '
    WRITE(*,'(A)' ) '   --debug_buffer_map       : invoke PSC with debug_buffer_map   flag ON  '
    WRITE(*,'(A)' ) '   --debug_buffer_trim      : invoke PSC with debug_buffer_trim  flag ON  '
    WRITE(*,'(A)' ) '   --debug_buffer_last      : invoke PSC with debug_buffer_last  flag ON  '
    WRITE(*,'(A)' ) '   --debug_buffer_do        : invoke PSC with debug_buffer_do    flag ON  '
    WRITE(*,'(A)' ) '   --off_debug_loop         : invoke PSC with debug_loop         flag OFF '
    WRITE(*,'(A)' ) '   --off_debug_level        : invoke PSC with debug_level        flag OFF '
    WRITE(*,'(A)' ) '   --off_debug_commd        : invoke PSC with debug_commd        flag OFF '
    WRITE(*,'(A)' ) '   --off_debug_stack        : invoke PSC with debug_stack        flag OFF '
    WRITE(*,'(A)' ) '   --off_debug_buffer       : invoke PSC with all debug_buffer   flag OFF '
    WRITE(*,'(A)' ) '   --off_debug_buffer_start : invoke PSC with debug_buffer_start flag OFF '
    WRITE(*,'(A)' ) '   --off_debug_buffer_read  : invoke PSC with debug_buffer_read  flag OFF '
    WRITE(*,'(A)' ) '   --off_debug_buffer_clean : invoke PSC with debug_buffer_clean flag OFF '
    WRITE(*,'(A)' ) '   --off_debug_buffer_map   : invoke PSC with debug_buffer_map   flag OFF '
    WRITE(*,'(A)' ) '   --off_debug_buffer_trim  : invoke PSC with debug_buffer_trim  flag OFF '
    WRITE(*,'(A)' ) '   --off_debug_buffer_last  : invoke PSC with debug_buffer_last  flag OFF '
    WRITE(*,'(A)' ) '   --off_debug_buffer_do    : invoke PSC with debug_buffer_do    flag OFF '
    CALL psc_printcolor( cl_helptitle )
    WRITE(*,'(A)')  '======================================================================'
    CALL psc_printcolor( cl_helpcontents )
    WRITE(*,'(A)')  ' Debug Options                             '
    WRITE(*,'(A)' ) '   debug_loop          debug loop          '
    WRITE(*,'(A)' ) '   debug_level         debug level         '
    WRITE(*,'(A)' ) '   debug_commd         debug command       '
    WRITE(*,'(A)' ) '   debug_stack         debug stack         '
    WRITE(*,'(A)' ) '   debug_buffer        debug buffer        '
    WRITE(*,'(A)' ) '   debug_buffer_start  debug buffer_start  '
    WRITE(*,'(A)' ) '   debug_buffer_read   debug buffer_read   '
    WRITE(*,'(A)' ) '   debug_buffer_clean  debug buffer_clean  '
    WRITE(*,'(A)' ) '   debug_buffer_map    debug buffer_map    '
    WRITE(*,'(A)' ) '   debug_buffer_trim   debug buffer_trim   '
    WRITE(*,'(A)' ) '   debug_buffer_last   debug buffer_last   '
    WRITE(*,'(A)' ) '   debug_buffer_do     debug buffer_do     '
    CALL psc_printcolor( cl_helptitle )
    WRITE(*,'(A)')  '======================================================================'
    CALL psc_printcolor( cl_helpcontents )
    CALL psc_printcolor( cl_reset )
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! hall, \?? %{{{
  CALL next_command()
  maintag_comm   = 'hall'
  subtag_comm(1) = '\??'
  help_comm = 'display full help'
  helpja_comm = '全てのヘルプを表示する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_printcolor( cl_helptitle )
    WRITE(*,'(A)' ) '============================================================'
    WRITE(*,'(A)' ) '[Help-ALL]. Full Reference                           '
    CALL psc_printcolor( cl_reset )
    bufin = 'hdi,hst,hmb,hmf,hmc,hfi,hsr,hmv,hop,hdr,hgr,hgrint,hgrreal,hgrstr,hgrbool,hgrcolor,hdb,'//TRIM(bufin)
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! cmdhelp %{{{
  CALL next_command()
  maintag_comm   = 'cmdhelp'
  subtag_comm(1) = 'ch'
  help_comm = 'displany help for command (##1)'
  helpja_comm = 'コマンド(##1)のヘルプを表示する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize < 1 )THEN
      CALL psc_error( cword, 4 )
      RETURN
    END IF
    stmpa = macro(1)
    itmpa = LEN_TRIM( stmpa )
    bufin = stmpa(1:itmpa-1) // ',' // TRIM(bufin)
    CALL psc_strst_remove
    exec_mode = 1
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  !%}}}

  ! File Input and Output(17) %{{{
  ! save %{{{
  CALL next_command()
  maintag_comm   = 'save'
  help_comm = 'save current condition to file (##1)'
  helpja_comm = 'ファイル(##1)に現在の状態を保存する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize == 0 )THEN
      CALL psc_error( cword, 4 )
      RETURN
    END IF
    stmpa = ADJUSTL( macro(1) )
    itmpa = LEN_TRIM( stmpa )
    stmpa = stmpa(1:itmpa-1)
    INQUIRE( FILE= TRIM(stmpa), EXIST= ltmpa, OPENED= ltmpb )
    IF( ltmpa .AND. ltmpb  )THEN
      CALL psc_error(cword,11)
      RETURN
    END IF
    CALL psc_strst_remove
    intfsave = psc_free_unit()
    OPEN( UNIT= intfsave, FILE= TRIM(stmpa), STATUS= 'replace' )
    WRITE(intfsave,'(A)') '"PSC-' // TRIM(pscversion)
    !=====================================================
    ! record date
    CALL date_and_time(systime(1), systime(2), systime(3), datime)
    DO idoa = 1, 3
      WRITE(dtm(idoa),'(I5)') datime(idoa)
      dtm(idoa) = ADJUSTL(dtm(idoa))
    END DO
    DO idoa = 5, 7
      WRITE(dtm(idoa),'(I2.2)') datime(idoa) ! hh:mm:ss
    END DO
    WRITE(intfsave,'(A$)') '"'
    WRITE(dtm(8),'(I3.3)') datime(8)  ! mili-sec
    WRITE(intfsave,'(A$)') TRIM(dtm(1)) // '/'
    WRITE(intfsave,'(A$)') TRIM(dtm(2)) // '/'
    WRITE(intfsave,'(A$)') TRIM(dtm(3)) // ' '
    WRITE(intfsave,'(A$)') TRIM(dtm(5)) // ':'
    WRITE(intfsave,'(A$)') TRIM(dtm(6)) // ':'
    WRITE(intfsave,'(A$)') TRIM(dtm(7)) // '.'
    WRITE(intfsave,'(A)' ) TRIM(dtm(8))
    WRITE(intfsave,'(A)' ) '"'
    !=====================================================
    ! save options
    WRITE(intfsave,'(A)') '" ==============================='
    WRITE(intfsave,'(A)') '" *Options*'
    !-----------------------
    IF( quiet )THEN
      WRITE(intfsave,'(A)') 'true (quiet) setflag'
    ELSE
      WRITE(intfsave,'(A)') 'false (quiet) setflag'
    END IF
    !-----------------------
    IF( color )THEN
      WRITE(intfsave,'(A)') 'true (color) setflag'
    ELSE
      WRITE(intfsave,'(A)') 'false (color) setflag'
    END IF
    !-----------------------
    IF( autonl )THEN
      WRITE(intfsave,'(A)') 'true (autonl) setflag'
    ELSE
      WRITE(intfsave,'(A)') 'false (autonl) setflag'
    END IF
    !-----------------------
    WRITE(stmpa,*) incunit
    CALL psc_cut_extra_zero( stmpa )
    stmpa = ADJUSTL( stmpa )
    WRITE(intfsave,'(A)') TRIM(stmpa)//' setunit'
    IF( decimals > 0  )THEN
      WRITE(stmpa,*) decimals
      stmpa = ADJUSTL( stmpa )
      WRITE(intfsave,'(A)') TRIM(stmpa) // ' setdigit'
    ELSE
      WRITE(intfsave,'(A)') TRIM(fmt_a) // ' setfmt'
    ENDIF
    WRITE(intfsave,'(A)') '(' // TRIM(sepstr) // ' setsep'
    !-----------------------
    DO idoa = 1, iinnow
      WRITE(intfsave,'(A)') '(' // TRIM( dev_in(idoa)%tag ) // &
      & ') openin'
    END DO
    DO idoa = 1, iounow
      WRITE(intfsave,'(A)') '(' // TRIM( dev_ou(idoa)%tag ) // &
      & ') openout'
    END DO
    ! ファイルのロードは再現できないので諦める
    !-----------------------
    WRITE(intfsave,'(A)') '" ==============================='
    WRITE(intfsave,'(A)') '" *Command-Maps*'
    IF( mapsize < 1 )THEN
      WRITE(intfsave,'(A)') '" is empty'
      WRITE(intfsave,'(A)') '" sample: (sinc)(dup,sin,exch,div)nore'
    ELSE
      DO idoa =  mapsize, 1, -1
        WRITE(intfsave,'(A$)') '(' // TRIM( map(idoa)%lhs )
        WRITE(intfsave,'(A$)') '(' // TRIM( map(idoa)%rhs )
        IF( map(idoa) % no )THEN
          WRITE(intfsave,'(A)') 'nore'
        ELSE
          WRITE(intfsave,'(A)') 'def'
        END IF
      END DO
    END IF
    WRITE(intfsave,'(A)') '" ==============================='
    WRITE(intfsave,'(A)') '" *Character-Stacks*'
    IF( macrosize < 1 )THEN
      WRITE(intfsave,'(A)') '" is empty'
      WRITE(intfsave,'(A)') '" sample: (string)'
    ELSE
      DO idoa =  macrosize, 1, -1
        WRITE(intfsave,'(A)') ' ('//TRIM( macro(idoa) )
      END DO
    END IF
    CLOSE(intfsave)
    IF( (.NOT.quiet).AND.(.NOT.super_quiet) )THEN
      WRITE(*,*) 'Save Done Successfully to file '''//TRIM(macro(1))//''''
    END IF
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! load  , source  , so  , include %{{{
  CALL next_command()
  maintag_comm   = 'load'
  subtag_comm(1) = 'source'
  subtag_comm(2) = 'so'
  subtag_comm(3) = 'include'
  help_comm = 'load file (##1)'
  helpja_comm = 'ファイル(##1)をロードする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize == 0 )THEN
      CALL psc_error( cword, 4 )
      RETURN
    END IF
    stmpa = ADJUSTL( macro(1) )
    itmpa = LEN_TRIM( stmpa )
    stmpa = stmpa(1:itmpa-1)
    INQUIRE( FILE= TRIM(stmpa), EXIST= ltmpa, OPENED= ltmpb )
    IF( .NOT. ltmpa  )THEN
      CALL psc_error(cword,10)
      RETURN
    END IF
    IF( ltmpb  )THEN
      CALL psc_error(cword,11)
      RETURN
    END IF
    CALL psc_strst_remove
    ! save current buffer string into holdbuffer(:) 
    !   this string will be loaded when you returns
    holdbuffer(ildnow) = bufin
    bufin = ''
    ! open
    ildnow = ildnow + 1
    dev_ld(ildnow) = device( psc_free_unit(), TRIM(stmpa) )
    codepoint(ildnow) = 0
    OPEN( UNIT = dev_ld(ildnow) % devnum, &
    & FILE= TRIM( dev_ld(ildnow) % tag ), STATUS= 'old' )
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! exload, exsource, exso, exinclude %{{{
  CALL next_command()
  maintag_comm   = 'exload'
  subtag_comm(1) = 'exsource'
  subtag_comm(2) = 'exso'
  subtag_comm(3) = 'exinclude'
  help_comm = 'exchange top two load files'
  helpja_comm = '先頭２つのロードファイルを交換する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( ildnow == 0 )THEN
      CALL psc_error( cword, 21 )
      RETURN
    END IF
    dev_ld(ildnow+1) = dev_ld(ildnow)
    dev_ld(ildnow) = dev_ld(ildnow-1)
    dev_ld(ildnow-1) = dev_ld(ildnow+1)
    stmpa = holdbuffer(ildnow)
    holdbuffer(ildnow) = holdbuffer(ildnow-1)
    holdbuffer(ildnow-1) = stmpa
    itmpa = codepoint(ildnow)
    codepoint(ildnow) = codepoint(ildnow-1)
    codepoint(ildnow-1) = itmpa
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! return %{{{
  CALL next_command()
  maintag_comm   = 'return'
  help_comm = 'return from load'
  helpja_comm = 'ロードから帰還する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( ildnow == 0 )THEN
      CALL psc_error( cword, 21 )
      RETURN
    END IF
    CLOSE(dev_ld(ildnow)%devnum)
    ildnow = ildnow - 1
    bufin = holdbuffer(ildnow)
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! numload %{{{
  CALL next_command()
  maintag_comm   = 'numload'
  help_comm = 'push load depth'
  helpja_comm = 'ロードの深さをプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_stack_append
    IF( iferror ) RETURN
    stack(stz) = DBLE( ildnow )
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! fileexist %{{{
  CALL next_command()
  maintag_comm   = 'fileexist'
  help_comm = 'push 1 if file (##1) exit, otherwise push 0'
  helpja_comm = 'ファイル(##1)が存在するならば1を、そうでないなら0をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize == 0  )THEN
      CALL psc_error( cword, 4 )
      RETURN
    END IF
    stmpa = ADJUSTL( macro(1) )
    itmpa = LEN_TRIM( stmpa )
    stmpa = stmpa(1:itmpa-1)
    INQUIRE( FILE= TRIM(stmpa), EXIST = ltmpa )
    CALL psc_strst_remove
    CALL psc_stack_append
    IF( iferror ) RETURN
    stack(stz) = 0.d0
    IF( ltmpa ) stack(stz) = 1.d0
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! openout , openwrite %{{{
  CALL next_command()
  maintag_comm   = 'openout'
  subtag_comm(1) = 'openwrite'
  help_comm = 'open file (##1) as write-only'
  helpja_comm = 'ファイル(##1)を書き込み用に開く'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize == 0 )THEN
      CALL psc_error( cword, 4 )
      RETURN
    END IF
    stmpa = ADJUSTL( macro(1) )
    itmpa = LEN_TRIM( stmpa )
    stmpa = stmpa(1:itmpa-1)
    INQUIRE( FILE= TRIM(stmpa), EXIST= ltmpa, OPENED= ltmpb )
    IF( ltmpa .AND. ltmpb  )THEN
      CALL psc_error( cword, 11 )
      RETURN
    END IF
    iounow = iounow + 1
    dev_ou(iounow) = device( psc_free_unit(), TRIM(stmpa) )
    OPEN( UNIT = dev_ou(iounow)%devnum, &
    & FILE= TRIM(dev_ou(iounow)%tag), STATUS= 'replace')
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! exout   , exwrite %{{{
  CALL next_command()
  maintag_comm   = 'exout'
  subtag_comm(1) = 'exwrite'
  help_comm = 'exchange top two write-only files'
  helpja_comm = '先頭２つの書き込み専用ファイルを交換する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF(iounow == 0)THEN
      CALL psc_error( cword, 24 )
      RETURN
    END IF
    dev_ou(iounow+1) = dev_ou(iounow)
    dev_ou(iounow) = dev_ou(iounow-1)
    dev_ou(iounow-1) = dev_ou(iounow+1)
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! closeout, closewrite %{{{
  CALL next_command()
  maintag_comm   = 'closeout'
  subtag_comm(1) = 'closewrite'
  help_comm = 'close top write-only files'
  helpja_comm = '先頭の書き込み専用ファイルを閉じる'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( iounow > 0 )THEN
      CLOSE(dev_ou(iounow)%devnum)
      dev_ou(iounow) = device( 6, 'stdout' )
      iounow = iounow - 1
    END IF
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! openin , openread %{{{
  CALL next_command()
  maintag_comm   = 'openin'
  subtag_comm(1) = 'openread'
  help_comm = 'open file (##1) as read-only'
  helpja_comm = 'ファイル(##1)を読み込み用に開く'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize == 0 )THEN
      CALL psc_error( cword, 4 )
      RETURN
    END IF
    stmpa = ADJUSTL( macro(1) )
    itmpa = LEN_TRIM( stmpa )
    stmpa = stmpa(1:itmpa-1)
    INQUIRE( FILE= TRIM(stmpa), EXIST= ltmpa, OPENED= ltmpb )
    IF( .NOT. ltmpa  )THEN
      CALL psc_error( cword, 10 )
      RETURN
    END IF
    IF( ltmpb  )THEN
      CALL psc_error( cword, 11 )
      RETURN
    END IF
    iinnow = iinnow + 1
    dev_in(iinnow) = device( psc_free_unit(), TRIM(stmpa) )
    CALL psc_strst_remove
    ! 入力用ファイルを開くときにファイルの行数を調べておく
    ! 空行でもreadすることはできるので行数に含まれる
    itmpa = 0
    OPEN( UNIT= dev_in(iinnow)%devnum, &
    & FILE= TRIM(dev_in(iinnow)%tag), STATUS= 'old' )
    DO
      itmpa = itmpa+1
      READ( dev_in(iinnow)%devnum,'(A)', IOSTAT= ierror) stmpa
      IF( ierror/=0) EXIT
    END DO
    lines(iinnow) = itmpa - 1
    ! maybe 'rewind()' will do better
    CLOSE(dev_in(iinnow)%devnum)
    OPEN( UNIT= dev_in(iinnow)%devnum, &
    & FILE= TRIM(dev_in(iinnow)%tag), STATUS= 'old' )
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! exin   , exread %{{{
  CALL next_command()
  maintag_comm   = 'exin'
  subtag_comm(1) = 'exread'
  help_comm = 'exchange top two read-only files'
  helpja_comm = '先頭２つの読み込み専用ファイルを交換する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF(iinnow == 0)THEN
      CALL psc_error( cword, 23 )
      RETURN
    END IF
    dev_in(iinnow+1) = dev_in(iinnow)
    dev_in(iinnow) = dev_in(iinnow-1)
    dev_in(iinnow-1) = dev_in(iinnow+1)
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! closein, closeread %{{{
  CALL next_command()
  maintag_comm   = 'closein'
  subtag_comm(1) = 'closeread'
  help_comm = 'close top read-only files'
  helpja_comm = '先頭の読み込み専用ファイルを閉じる'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( iinnow > 0 )THEN
      CLOSE(dev_in(iinnow)%devnum)
      iinnow = iinnow - 1
    END IF
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! lines %{{{
  CALL next_command()
  maintag_comm   = 'lines'
  help_comm = 'push the number of lines of current read-only file'
  helpja_comm = '最新の読み込み専用ファイルの行数をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( iinnow == 0 )THEN
      CALL psc_error( cword, 23 )
      RETURN
    END IF
    CALL psc_stack_append
    IF( iferror ) RETURN
    stack(stz) = DBLE( lines(iinnow) )
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! read %{{{
  CALL next_command()
  maintag_comm   = 'read'
  help_comm = 'read one line from current read-only file'
  helpja_comm = '最新の読み込み専用ファイルから一行読み込む'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    READ( UNIT= dev_in(iinnow)%devnum, FMT= '(A)', &
    & IOSTAT= ierror ) stmpa
    IF( ierror /= 0 )THEN
      CALL psc_error( cword, 12 )
      RETURN
    END IF
    CALL psc_strst_append
    IF( iferror ) RETURN
    macro(1) = TRIM( stmpa ) // ')'
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! makedata, md %{{{
  CALL next_command()
  maintag_comm   = 'makedata'
  subtag_comm(1) = 'md'
  help_comm = 'make data file (##1) with function (##2) over range (#3)~(#2) with division (#1)'
  helpja_comm = '関数(##2)を範囲(#3)~(#2)にわたって分割数(#1)でデータファイル(##1)を作成する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    ! [usage] (sin)(new.dat)0,pi,500,makedata
    IF( macrosize < 2  )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    IF( stz < 3  )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    IF( NINT(stack(stz)) < 3  )THEN
      CALL psc_error( cword, 8 )
      RETURN
    END IF
    ! macro(2)   : function
    ! macro(1)   : filename
    ! stack(stz-2) : xmin
    ! stack(stz-1) : xmax
    ! stack(stz) : divnum
    rtmpa = stack(stz-2)
    rtmpb = (stack(stz-1)-stack(stz-2)) / DBLE(NINT(stack(stz)))
    itmpc = NINT( stack(stz) ) + 1
    WRITE( stmpa, * ) rtmpa ! xmin
    WRITE( stmpb, * ) rtmpb ! dx
    WRITE( stmpc, * ) itmpc ! divnum+1
    stmpa = TRIM( ADJUSTL( stmpa ) )
    stmpb = TRIM( ADJUSTL( stmpb ) )
    stmpc = TRIM( ADJUSTL( stmpc ) )
    CALL psc_cut_extra_zero( stmpa )
    CALL psc_cut_extra_zero( stmpb )
    !
    ! ここは実装次第ではさらに高速化(効率化)できる可能性がある
    stmpa = '(' // TRIM(macro(1)) // 'openout,' // &
    & '(' // TRIM(macro(2)) // &
    & TRIM(stmpb) // ',' // TRIM(stmpa) // &
    & '(dupop,dup,dups,exec,pop' // &
    & ',exch,dup,3,1,roll,+,nl,==)' // TRIM(stmpc) // &
    & ',do,closeout,kill,kill,kills,'
    IF( autonl )THEN
      autonl = .FALSE.
      stmpa = TRIM(stmpa) // 'SYSTEM_USE_NEWLINE,'
    END IF
    bufin = TRIM(stmpa) // TRIM(bufin)
    !
    CALL psc_strst_remove
    CALL psc_strst_remove
    CALL psc_stack_remove
    CALL psc_stack_remove
    CALL psc_stack_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! countf, cntf %{{{
  CALL next_command()
  maintag_comm   = 'countf'
  subtag_comm(1) = 'cntf'
  help_comm = 'count word (##1) in file (##2)'
  helpja_comm = 'ファイル(##2)中の単語(##1)を数える'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    ! count word:(##1) in file:(##2)
    IF( macrosize < 2 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    stmpa = ADJUSTL( macro(2) )
    itmpa = LEN_TRIM( stmpa )
    stmpa = stmpa(1:itmpa-1)
    INQUIRE( FILE= TRIM(stmpa), EXIST= ltmpa, OPENED= ltmpb )
    IF( .NOT. ltmpa  )THEN
      CALL psc_error( cword, 10 )
      RETURN
    END IF
    IF( ltmpb  )THEN
      CALL psc_error( cword, 11 )
      RETURN
    END IF
    itmpa = LEN_TRIM( stmpa )
    !
    stmpa = '(' // TRIM(stmpa) // ')openin,0(read(' // &
    & TRIM(macro(1)) // 'cntw,+)lines,do,closein,'
    bufin = TRIM(stmpa) // TRIM(bufin)
    CALL psc_strst_remove
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! grep, gr %{{{
  CALL next_command()
  maintag_comm   = 'grep'
  subtag_comm(1) = 'gr'
  help_comm = 'print lines in file (##2) including word (##1) '
  helpja_comm = 'ファイル(##2)中の単語(##1)を含む行を出力する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    !  print lines include word:(##1) in file:(##2)
    IF( macrosize < 2 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    stmpa = ADJUSTL( macro(2) )
    itmpa = LEN_TRIM( stmpa )
    stmpa = stmpa(1:itmpa-1)
    INQUIRE( FILE= TRIM(stmpa), EXIST= ltmpa, OPENED= ltmpb )
    IF( .NOT.ltmpa )THEN
      CALL psc_error( cword, 10 )
      RETURN
    END IF
    IF( ltmpb  )THEN
      CALL psc_error( cword, 11 )
      RETURN
    END IF
    !
    stmpa = '(' // TRIM(stmpa) // ')openin(read,dups(' // &
    & TRIM(macro(1)) // 'srcw,0,>(dupops,nl,==)if,kills)' // &
    & 'lines,do,closein,'
    IF( autonl )THEN
      autonl = .FALSE.
      stmpa = TRIM(stmpa) // 'SYSTEM_USE_NEWLINE,'
    END IF
    bufin = TRIM(stmpa) // TRIM(bufin)
    CALL psc_strst_remove
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! End Application %}}}
  ! Strings(29) %{{{
  ! writestr, wstr, cvs , \$>" %{{{
  CALL next_command()
  maintag_comm   = 'writestr'
  subtag_comm(1) = 'wstr'
  subtag_comm(2) = 'cvs'
  subtag_comm(3) = '\$>"'
  help_comm = 'convert (#1) into string and push'
  helpja_comm = '(#1)を文字列に変換しプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    CALL psc_strst_append
    IF( iferror ) RETURN
    CALL psc_printnum(stack(stz), fmt_a, stmpa, &
    & -1, .FALSE., sepstr)
    macro(1) = stmpa
    CALL psc_stack_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! readstr , rstr, cvr , \">$ %{{{
  CALL next_command()
  maintag_comm   = 'readstr'
  subtag_comm(1) = 'rstr'
  subtag_comm(2) = 'cvr'
  subtag_comm(3) = '\">$'
  help_comm = 'convert (##1) into real-number and push'
  helpja_comm = '(##1)を実数に変換しプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize == 0 )THEN
      CALL psc_error( cword, 4 )
      RETURN
    END IF
    stmpa = macro(1)
    itmpa = LEN_TRIM( stmpa )
    stmpa = stmpa(1:itmpa-1)
    READ( stmpa, *, IOSTAT= ierror ) rtmpa
    IF( ierror /= 0 )THEN
      CALL psc_error( cword, 9 )
      RETURN
    END IF
    CALL psc_stack_append
    IF( iferror ) RETURN
    stack(stz) = rtmpa
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! comb, concat, \". %{{{
  CALL next_command()
  maintag_comm   = 'comb'
  subtag_comm(1) = 'concat'
  subtag_comm(2) = '\".'
  help_comm = 'concatenate (##1) after (##2)'
  helpja_comm = '(##2)の後ろに(##1)を結合する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize < 2 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    stmpa = macro(2)
    itmpa = LEN_TRIM( stmpa )
    macro(2) = stmpa(1:itmpa-1) // macro(1)
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! cut, \"/ %{{{
  CALL next_command()
  maintag_comm   = 'cut'
  subtag_comm(1) = '\"/'
  help_comm = 'cut (##1) at position (#1)'
  helpja_comm = '(##1)を位置(#1)で分割する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize == 0 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    IF( stz < 1 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    IF( ( NINT(stack(stz)) <= 0 ) .OR. &
    & ( NINT(stack(stz)) > LEN_TRIM(macro(1))-1 ) )THEN
      CALL psc_error( cword, 8 )
      RETURN
    END IF
    stmpa = macro(1)
    CALL psc_strst_append
    IF( iferror ) RETURN
    macro(1) = stmpa(1:NINT(stack(stz))) // ')'
    macro(2) = stmpa(NINT(stack(stz))+1:)
    CALL psc_stack_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! flushleft, adjustleft, \"< %{{{
  CALL next_command()
  maintag_comm   = 'flushleft'
  subtag_comm(1) = 'adjustleft'
  subtag_comm(2) = '\"<'
  help_comm = 'remove consecutive spaces at beginning of (##1)'
  helpja_comm = '(##1)冒頭の連続したスペースを取り除く'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize < 1 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    macro(1) = ADJUSTL(macro(1))
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! removetopseps %{{{
  CALL next_command()
  maintag_comm   = 'removetopseps'
  help_comm = 'remove consecutive PSC-separators at beginning of (##1)'
  helpja_comm = '(##1)冒頭の連続したPSCセパレータを取り除く'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize < 1 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    stmpa = bufin
    bufin = macro(1)
    CALL psc_buf_rmtopsep
    macro(1) = bufin
    bufin = stmpa
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! cuttoken %{{{
  CALL next_command()
  maintag_comm   = 'cuttoken'
  help_comm = 'cut first PSC-token from (##1)'
  helpja_comm = '(##1)から最初のPSCトークンを分離する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize < 1 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    stmpa = bufin
    bufin = macro(1)
    CALL psc_buf_gettoklen
    IF( bufin(1:1) == '(' )THEN
      CALL psc_buf_getstrlen
    END IF
    macro(1) = bufin
    bufin = stmpa
    stmpa = macro(1)
    CALL psc_strst_append
    IF( iferror ) RETURN
    IF( wlen > 0 )THEN
      macro(1) = stmpa(1:wlen) // ')'
      macro(2) = stmpa(wlen+1:)
    ELSE
      macro(1) = stmpa
      macro(2) = ')'
    END IF
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! cutword %{{{
  CALL next_command()
  maintag_comm   = 'cutword'
  help_comm = 'cut first space-separated word from (##1)'
  helpja_comm = '(##1)からスペースで区切られた最初の単語を分離する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize < 1 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    stmpa = ADJUSTL(macro(1))
    CALL psc_strst_append
    IF( iferror ) RETURN
    wlen = INDEX(stmpa, ' ')
    IF( wlen > LEN_TRIM( stmpa ) )THEN
      macro(1) = stmpa
      macro(2) = ')'
    ELSE
      macro(1) = stmpa(1:wlen-1) // ')'
      stmpa = ADJUSTL(stmpa(wlen:))
      macro(2) = stmpa 
    END IF
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! char  , achar , \#>" %{{{
  CALL next_command()
  maintag_comm   = 'char'
  subtag_comm(1) = 'achar'
  subtag_comm(2) = '\#>"'
  help_comm = 'push (#1)''th ASCII character'
  helpja_comm = '(#1)番目のアスキー文字をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    CALL psc_strst_append
    IF( iferror ) RETURN
    macro(1) = ACHAR( NINT( stack(stz) ) ) // ')'
    CALL psc_stack_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! ichar , iachar, \"># %{{{
  CALL next_command()
  maintag_comm   = 'ichar'
  subtag_comm(1) = 'iachar'
  subtag_comm(2) = '\">#'
  help_comm = 'push ASCII code of (##1)'
  helpja_comm = '(##1)のアスキーコードをプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize == 0 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    CALL psc_stack_append
    IF( iferror ) RETURN
    stmpa = macro(1)
    IF( iferror ) RETURN
    IF( stmpa == ')' )THEN
      stack(stz) = 0.d0
    ELSE
      stack(stz) = DBLE( IACHAR( stmpa(1:1) ) )
    END IF
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! length, len   , \"| %{{{
  CALL next_command()
  maintag_comm   = 'length'
  subtag_comm(1) = 'len'
  subtag_comm(2) = '\"|'
  help_comm = 'push string-length of (##1)'
  helpja_comm = '(##1)の文字列長をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize < 1 )THEN
      CALL psc_error( cword, 4 )
      RETURN
    END IF
    CALL psc_stack_append
    IF( iferror ) RETURN
    stack(stz) = DBLE( LEN_TRIM( macro(1) ) - 1 )
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! reverseletter, revl, \"<=> %{{{
  CALL next_command()
  maintag_comm   = 'reverseletter'
  subtag_comm(1) = 'revl'
  subtag_comm(2) = '\"<=>'
  help_comm = 'reverse letter orders in (##1)'
  helpja_comm = '(##1)の文字の順序を反転する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize < 1 )THEN
      CALL psc_error( cword, 4 )
      RETURN
    END IF
    itmpa = LEN_TRIM( macro(1) )  - 1
    stmpa = macro(1)
    DO idoa = 1, itmpa / 2
      ctmpa = stmpa(idoa:idoa)
      stmpa(idoa:idoa) = stmpa(itmpa+1-idoa:itmpa+1-idoa)
      stmpa(itmpa+1-idoa:itmpa+1-idoa) = ctmpa
    END DO
    macro(1) = stmpa
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! eqs, same, \"== %{{{
  CALL next_command()
  maintag_comm   = 'eqs'
  subtag_comm(1) = 'same'
  subtag_comm(2) = '\"=='
  help_comm = 'push 1 if (##1)==(##2), otherwise push 0'
  helpja_comm = '(##1)==(##2)ならば1を、そうでないなら0をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize < 2 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    CALL psc_stack_append
    IF( iferror ) RETURN
    IF( TRIM(macro(1)) == TRIM(macro(2)) )THEN
      stack(stz) = 1.d0
    ELSE
      stack(stz) = 0.d0
    END IF
    CALL psc_strst_remove
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! get %{{{
  CALL next_command()
  maintag_comm   = 'get'
  help_comm = 'push ASCII code of (#1)''th character of (##1)'
  helpja_comm = '(##1)の(#1)番目の文字のアスキーコードをプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize == 0 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    IF( stz < 1 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    IF( ( NINT(stack(stz)) <= 0 ) .OR. &
    & ( NINT(stack(stz)) > LEN(macro(1)) ) )THEN
      CALL psc_error( cword, 8 )
      RETURN
    END IF
    stmpa = macro(1)
    itmpa = NINT(stack(stz))
    CALL psc_strst_remove
    IF( itmpa > LEN_TRIM(stmpa) - 1 )THEN
      stack(stz) = 0.d0
    ELSE
      stack(stz) = DBLE(IACHAR(stmpa(itmpa:itmpa)))
    END IF
    stackchanged = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! put %{{{
  CALL next_command()
  maintag_comm   = 'put'
  help_comm = 'set (#2)''th character of (##1) into ASCII (#1)''th character'
  helpja_comm = '(##1)の(#2)番目の文字をアスキー(#1)番目の文字にする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize == 0 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    IF( stz < 2 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    IF( ( NINT(stack(stz)) <= 0 ) .OR. &
    & ( NINT(stack(stz)) > LEN(macro(1)) ) )THEN
      CALL psc_error( cword, 8 )
      RETURN
    END IF
    itmpa = NINT(stack(stz-1)) ! string-index
    itmpb = NINT(stack(stz)) ! int
    stmpa = macro(1)
    CALL psc_stack_remove
    CALL psc_stack_remove
    IF( itmpa > LEN_TRIM(stmpa) - 1 )THEN
      itmpc = LEN_TRIM(stmpa)
      stmpa = stmpa(1:itmpc-1)
      stmpa(itmpa:itmpa) = ACHAR(itmpb)
      macro(1) = stmpa(1:itmpa) // ')'
    ELSE
      stmpa(itmpa:itmpa) = ACHAR(itmpb)
      macro(1) = TRIM(stmpa)
    END IF
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! searchword, srcw, \"? %{{{
  CALL next_command()
  maintag_comm   = 'searchword'
  subtag_comm(1) = 'srcw'
  subtag_comm(2) = '\"?'
  help_comm = 'push position of furst occurence of substring (##1) in (##2), push 0 if not found'
  helpja_comm = '(##2)の中で最初に部分文字列(##1)の現れる位置を' // &
  & 'プッシュする、見つからなければ0をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    ! same as FORTRAN 'INDEX' function
    IF( macrosize < 2 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    stmpa = macro(2)
    itmpa = LEN_TRIM(stmpa)
    stmpb = macro(1)
    itmpb = LEN_TRIM(stmpb)
    !
    itmpa = INDEX(stmpa(1:itmpa-1), stmpb(1:itmpb-1))
    CALL psc_stack_append
    IF( iferror ) RETURN
    stack(stz) = DBLE(itmpa)
    CALL psc_strst_remove
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! searchletter, srcl, \"?? %{{{
  CALL next_command()
  maintag_comm   = 'searchletter'
  subtag_comm(1) = 'srcl'
  subtag_comm(2) = '\"??'
  help_comm = 'push position of first occurence of one of characters of (##1) in (##2), push 0 if not found'
  helpja_comm = '(##2)の中で最初に(##1)の中の一つの文字が現れる位置を' // &
  & 'プッシュする、見つからなければ0をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    ! same as FORTRAN 'SCAN' function
    IF( macrosize < 2 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    stmpa = macro(2)
    itmpa = LEN_TRIM(stmpa)
    stmpb = macro(1)
    itmpb = LEN_TRIM(stmpb)
    !
    itmpa = SCAN(stmpa(1:itmpa-1), stmpb(1:itmpb-1))
    CALL psc_stack_append
    IF( iferror ) RETURN
    stack(stz) = DBLE(itmpa)
    CALL psc_strst_remove
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! invsearchletter, invsrcl, \"??? %{{{
  CALL next_command()
  maintag_comm   = 'invsearchletter'
  subtag_comm(1) = 'invsrcl'
  subtag_comm(2) = '\"???'
  help_comm = 'push position of first anti-occurence of one of characters of (##1) in (##2), push 0 if not found'
  helpja_comm = '(##2)の中で最初に(##1)の中の一つの文字が現れない位置を' // &
  & 'プッシュする、見つからなければ0をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    ! same as FORTRAN 'VERIFY' function
    IF( macrosize < 2 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    stmpa = macro(2)
    itmpa = LEN_TRIM(stmpa)
    stmpb = macro(1)
    itmpb = LEN_TRIM(stmpb)
    !
    itmpa = VERIFY(stmpa(1:itmpa-1), stmpb(1:itmpb-1))
    CALL psc_stack_append
    IF( iferror ) RETURN
    stack(stz) = DBLE(itmpa)
    CALL psc_strst_remove
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! countword, cntw, \"?# %{{{
  CALL next_command()
  maintag_comm   = 'countword'
  subtag_comm(1) = 'cntw'
  subtag_comm(2) = '\"?#'
  help_comm = 'push number of occurences of substring (##1) in (##2)'
  helpja_comm = '(##2)の中で部分文字列(##1)の現れる数をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize < 2  )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    stmpa = macro(2)
    itmpa = LEN_TRIM(stmpa) - 1
    stmpb = macro(1) ! word
    itmpb = LEN_TRIM(macro(1)) - 1 ! word len
    itmpc = 0  ! counter
    itmpd = 1  ! search pos
    DO
      itmpe = INDEX( stmpa(itmpd:), stmpb(1:itmpb) )
      IF( itmpe == 0 ) EXIT
      IF( itmpe + itmpd - 1 + itmpb - 1 > itmpa ) EXIT
      itmpc = itmpc + 1
      itmpd = itmpd + itmpe
    END DO
    CALL psc_stack_append
    IF( iferror ) RETURN
    stack(stz) = DBLE(itmpc)
    CALL psc_strst_remove
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! isreal, \"=$ %{{{
  CALL next_command()
  maintag_comm   = 'isreal'
  subtag_comm(1) = '\"=$'
  help_comm = 'push 1 if (##1) is readable as numbers, otherwise push 0'
  helpja_comm = '(##1)が数として読めるならば1を、そうでないなら0をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize < 1 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    CALL psc_stack_append
    IF( iferror ) RETURN
    stmpa = macro(1)
    itmpa = LEN_TRIM( stmpa )
    stmpa = stmpa(1:itmpa-1)
    IF( psc_isreal(stmpa) )THEN
      stack(stz) = 1.d0
    ELSE
      stack(stz) = 0.d0
    END IF
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! syscall %{{{
  CALL next_command()
  maintag_comm   = 'syscall'
  help_comm = 'execute system command (##1)'
  helpja_comm = 'システムのコマンド(##1)を実行する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize == 0 )THEN
      CALL psc_error( cword, 4 )
      RETURN
    END IF
    stmpa = macro(1)
    itmpa = LEN_TRIM( stmpa )
    stmpa = stmpa(1:itmpa-1)
    CALL SYSTEM(TRIM(stmpa))
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! pwd, getcwd %{{{
  CALL next_command()
  maintag_comm   = 'pwd'
  subtag_comm(1) = 'getcwd'
  help_comm = 'push path-string of current directory'
  helpja_comm = 'カレントディレクトリのパス文字列をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_strst_append
    IF( iferror ) RETURN
    CALL getcwd(stmpa)
    macro(1) = TRIM(stmpa) // ')'
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! home, ~ %{{{
  CALL next_command()
  maintag_comm   = 'home'
  subtag_comm(1) = '~'
  help_comm = 'push path-string of home directory'
  helpja_comm = 'ホームディレクトリのパス文字列をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_strst_append
    IF( iferror ) RETURN
    CALL SYSTEM('echo $HOME>.psc_system_gethomeaddress')
    itmpa = psc_free_unit()
    OPEN(itmpa, FILE='.psc_system_gethomeaddress', STATUS= 'old')
    READ(itmpa,'(1000A)') stmpa
    CLOSE(itmpa)
    CALL SYSTEM('rm .psc_system_gethomeaddress')
    macro(1) = TRIM( stmpa ) // ')'
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! time, date, now, \|| %{{{
  CALL next_command()
  maintag_comm   = 'time'
  subtag_comm(1) = 'date'
  subtag_comm(2) = 'now'
  subtag_comm(3) = '\||'
  help_comm = 'push string representing current time'
  helpja_comm = '現在時刻を表す文字列をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    cword = 'now'
    CALL psc_comm_comm(cword)
    CALL psc_strst_append
    IF( iferror ) RETURN
    macro(1) = TRIM( cword ) // ')'
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! pstart, \"{ %{{{
  CALL next_command()
  maintag_comm   = 'pstart'
  subtag_comm(1) = '\"{'
  help_comm = 'push string ''('''
  helpja_comm = '文字列''(''をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_strst_append
    IF( iferror ) RETURN
    macro(1) = '()'
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! pend, \"} %{{{
  CALL next_command()
  maintag_comm   = 'pend'
  subtag_comm(1) = '\"}'
  help_comm = 'push string '')'''
  helpja_comm = '文字列'')''をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_strst_append
    IF( iferror ) RETURN
    macro(1) = '))'
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! newlinechar, nl, eol, \"$ %{{{
  CALL next_command()
  maintag_comm   = 'newlinechar'
  subtag_comm(1) = 'nl'
  subtag_comm(2) = 'eol'
  subtag_comm(3) = '\"$'
  help_comm = 'push newline character'
  helpja_comm = '改行文字をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_strst_append
    IF( iferror ) RETURN
    macro(1) = ACHAR(10) // ')'
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! strxxd %{{{
  CALL next_command()
  maintag_comm   = 'strxxd'
  help_comm = 'convert string like xxd'
  helpja_comm = 'xxdのように文字列を変換する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize == 0 )THEN
      CALL psc_error( cword, 4 )
      RETURN
    END IF
    stmpa = macro(1) ! original string
    itmpa = LEN_TRIM( stmpa ) - 1 ! string length
    stmpb = '' ! string for result
    itmpc = 0 ! position
    !
    DO idoa = 1, itmpa/16
      DO idob = 1, 16
        itmpc = itmpc + 1
        WRITE(stmpb, '(A,1X,Z2.2)') TRIM(stmpb), IACHAR(stmpa(itmpc:itmpc))
      END DO
      stmpb = TRIM(stmpb) // ACHAR(10)
    END DO
    IF( MOD(itmpa, 16) == 0 ) stmpb(LEN_TRIM(stmpb):LEN_TRIM(stmpb)) = ''
    DO idob =  1, MOD(itmpa, 16)
      itmpc = itmpc + 1
      WRITE(stmpb, '(A,1X,Z2.2)') TRIM(stmpb), stmpa(itmpc:itmpc)
    END DO
    macro(1) = TRIM(stmpb) // ')'
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! strcolor %{{{
  CALL next_command()
  maintag_comm   = 'strcolor'
  help_comm = 'colorize (##1):foreground_RGB=(#6)(#5)(#4):background_RGB=(#3)(#2)(#1)'
  helpja_comm = '(##1)に色を付ける：前景RGB=(#6)(#5)(#4):背景RGB=(#3)(#2)(#1)'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    ! [usage] "(hello) black red strcolor"
    IF( macrosize == 0 )THEN
      CALL psc_error( cword, 4 )
      RETURN
    END IF
    IF( stz < 6 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    IF( iferror ) RETURN
    stmpd = macro(1)
    itmpd = LEN_TRIM( macro(1) )
    ! foreground color
    itmpa = FLOOR(256.d0*stack(stz-2))
    itmpb = FLOOR(256.d0*stack(stz-1))
    itmpc = FLOOR(256.d0*stack(stz))
    IF( itmpa == 256)itmpa = 255
    IF( itmpb == 256)itmpb = 255
    IF( itmpc == 256)itmpc = 255
    WRITE(stmpa,'(A,I3.3,A,I3.3,A,I3.3,A)') ACHAR(27)//'[38;2;',&
    & itmpa,';',itmpb,';',itmpc,'m'
    CALL psc_stack_remove
    CALL psc_stack_remove
    CALL psc_stack_remove
    ! background color
    itmpa = FLOOR(256.d0*stack(stz-2))
    itmpb = FLOOR(256.d0*stack(stz-1))
    itmpc = FLOOR(256.d0*stack(stz))
    IF( itmpa == 256)itmpa = 255
    IF( itmpb == 256)itmpb = 255
    IF( itmpc == 256)itmpc = 255
    WRITE(stmpb,'(A,I3.3,A,I3.3,A,I3.3,A)') ACHAR(27)//'[48;2;',&
    & itmpa,';',itmpb,';',itmpc,'m'
    macro(1) = TRIM(stmpa) // TRIM(stmpb) // stmpd(1:itmpd-1) // &
    & ACHAR(27) // '[m'
    CALL psc_stack_remove
    CALL psc_stack_remove
    CALL psc_stack_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  !%}}}
  ! Macros(25) %{{{
  ! do, repeat, if, \? %{{{
  CALL next_command()
  maintag_comm   = 'do'
  subtag_comm(1) = 'repeat'
  subtag_comm(2) = 'if'
  subtag_comm(3) = '\?'
  help_comm = 'execute (##1) for (#1) times'
  helpja_comm = '(##1)を(#1)回実行する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz == 0 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    IF( macrosize == 0 )THEN
      CALL psc_error( cword, 4 )
      RETURN
    END IF
    IF( (NINT(stack(stz)) < 1) )THEN
      CALL psc_stack_remove
      IF( debug_buffer_last ) WRITE(*,*) 'bufin:last :='//TRIM(bufin)
      CALL psc_strst_remove
      ifloop = .FALSE.
      ! 多重ループの内側ループから脱出した場合でもifloopは解除されることに注意
      ! その場合SYSTEM_END_DOによって判定される
    ELSE
      stmpb = macro(1)
      itmpb = LEN_TRIM( stmpb)
      ! rest execution time
      WRITE(stmpa,*) NINT(stack(stz))-1
      stmpa = ADJUSTL( stmpa )
      stmpa = TRIM(stmpa) // ' SYSTEM_END_DO do,'
      stmpa = stmpb(1:itmpb-1) // ' SYSTEM_CONTENTS_END(' // &
      & stmpb(1:itmpb-1) // ')' // TRIM(stmpa)
      bufin = TRIM(stmpa) // TRIM(bufin)
      ! Note: SYSTEM_CONTENTS_END is used for cycle
      !       SYSTEM_END_DO is used for break
      IF( debug_buffer_do ) WRITE(*,*) 'bufin:do   :=' // TRIM(bufin)
      CALL psc_stack_remove
      CALL psc_strst_remove
      ifloop = .TRUE.
      RETURN
    END IF
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! infdo, \?()() %{{{
  CALL next_command()
  maintag_comm   = 'infdo'
  subtag_comm(1) = '\?()()'
  help_comm = 'execute (##1) infinitely'
  helpja_comm = '(##1)を無限に実行する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize == 0 )THEN
      CALL psc_error( cword, 4 )
      RETURN
    END IF
    stmpb = macro(1)
    itmpb = LEN_TRIM( stmpb )
    !
    stmpa = stmpb(1:itmpb-1) // ',SYSTEM_CONTENTS_END(' // &
    & TRIM(macro(1)) // 'SYSTEM_END_DO infdo,'

    ifloop = .TRUE.
    bufin = TRIM( stmpa ) // TRIM(bufin)
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! ifelse, \?: %{{{
  CALL next_command()
  maintag_comm   = 'ifelse'
  subtag_comm(1) = '\?:'
  help_comm = 'execute (##2) if int(#1)/=0, otherwise execute (##1)'
  helpja_comm = 'int(#1)/=0ならば(##2)を実行する、そうでなければ(##1)を実行する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz == 0 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    IF( macrosize <= 1 )THEN
      CALL psc_error( cword, 4 )
      RETURN
    END IF
    IF( NINT(stack(stz)) == 0 )THEN
      stmpa = macro(1)
    ELSE
      stmpa = macro(2)
    END IF
    itmpa = LEN_TRIM( stmpa )
    bufin = stmpa(1:itmpa-1) // '\\' // TRIM(bufin)
    ! bufin may have other tokens, so be careful not to erase them
    IF( debug_buffer_do ) WRITE(*,*) 'bufin:do   :='//TRIM(bufin)
    CALL psc_stack_remove
    CALL psc_strst_remove
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! exec, eval, \. %{{{
  CALL next_command()
  maintag_comm   = 'exec'
  subtag_comm(1) = 'eval'
  subtag_comm(2) = '\.'
  help_comm = 'execute (##1)'
  helpja_comm = '(##1)を実行する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize == 0 )THEN
      CALL psc_error( cword, 4 )
      RETURN
    END IF
    stmpa = macro(1)
    itmpa = LEN_TRIM( stmpa )
    bufin = stmpa(1:itmpa-1) // '\\' // TRIM(bufin)
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! builtin, command, primitive, \.. %{{{
  CALL next_command()
  maintag_comm   = 'builtin'
  subtag_comm(1) = 'command'
  subtag_comm(2) = 'primitive'
  subtag_comm(3) = '\..'
  help_comm = 'execute (##1) without maps'
  helpja_comm = '(##1)をマップなしで実行する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize == 0 )THEN
      CALL psc_error( cword, 4 )
      RETURN
    END IF
    stmpa = macro(1)
    itmpa = LEN_TRIM( stmpa )
    bufin = stmpa(1:itmpa-1) // '\\SYSTEM_END_MAP\\' // TRIM(bufin)
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! cycle, next, \! %{{{
  CALL next_command()
  maintag_comm   = 'cycle'
  subtag_comm(1) = 'next'
  subtag_comm(2) = '\!'
  help_comm = 'cycle loops at level (#1)'
  helpja_comm = 'レベル(##1)のループをサイクルする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz == 0 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    itmpa = NINT(stack(stz)) ! cycle level
    CALL psc_stack_remove
    IF( itmpa < 1 )THEN
      RETURN
    END IF
    DO idoa = 1, itmpa
      itmpb = INDEX(bufin,'SYSTEM_CONTENTS_END')
      IF( itmpb == 0 ) EXIT
      bufin(1:itmpb+LEN_TRIM('SYSTEM_CONTENTS_END')-1)=''
      bufin = ADJUSTL(bufin)
    END DO
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! break, exit, \!! %{{{
  CALL next_command()
  maintag_comm   = 'break'
  subtag_comm(1) = 'exit'
  subtag_comm(2) = '\!!'
  help_comm = 'break loops at level (#1)'
  helpja_comm = 'レベル(##1)のループをブレークする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz == 0 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    itmpa = NINT(stack(stz)) ! break level
    CALL psc_stack_remove
    IF( itmpa < 1 )THEN
      RETURN
    END IF
    DO idoa = 1, itmpa
      itmpb = INDEX(bufin,'SYSTEM_END_DO do')
      itmpc = INDEX(bufin,'SYSTEM_END_DO infdo')
      IF( itmpb == 0 .AND. itmpc == 0 ) EXIT
      IF( itmpc == 0 )THEN
        ltmpa = .TRUE. ! current loop is normal loop
      ELSE IF( itmpb == 0 )THEN
        ltmpa = .FALSE. ! current loop is inf loop
      ELSE IF( itmpb < itmpc )THEN
        ltmpa = .TRUE. ! current loop is normal loop
      ELSE
        ltmpa = .FALSE. ! current loop is inf loop
      END IF
      IF( ltmpa )THEN
        bufin(1:itmpb+LEN_TRIM('SYSTEM_END_DO do')-1) = ''
        bufin = ADJUSTL(bufin)
      ELSE
        bufin(1:itmpc+LEN_TRIM('SYSTEM_END_DO infdo')-1) = ''
        bufin = ADJUSTL(bufin)
      END IF
    END DO
    ifloop = .FALSE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! cycleall, \!@|| %{{{
  CALL next_command()
  maintag_comm   = 'cycleall'
  subtag_comm(1) = '\!@||'
  help_comm = 'cycle all loops'
  helpja_comm = '全ループをサイクルする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    DO
      itmpb = INDEX(bufin,'SYSTEM_CONTENTS_END')
      IF( itmpb == 0 ) EXIT
      bufin(1:itmpb+LEN_TRIM('SYSTEM_CONTENTS_END')-1)=''
      bufin = ADJUSTL(bufin)
    END DO
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! breakall, \!!@|| %{{{
  CALL next_command()
  maintag_comm   = 'breakall'
  subtag_comm(1) = '\!!@||'
  help_comm = 'break all loops'
  helpja_comm = '全ループをブレークする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    DO
      itmpb = INDEX(bufin,'SYSTEM_END_DO do')
      IF( itmpb == 0 ) EXIT
      bufin(1:itmpb+LEN_TRIM('SYSTEM_END_DO do')-1)=''
      bufin = ADJUSTL(bufin)
    END DO
    ifloop = .FALSE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! SYSTEM_END_DO %{{{
  CALL next_command()
  maintag_comm   = 'SYSTEM_END_DO'
  help_comm = 'system command: identify loop ends'
  helpja_comm = 'システムコマンド：ループ終了を識別する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CONTINUE
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! SYSTEM_CONTENTS_END %{{{
  CALL next_command()
  maintag_comm   = 'SYSTEM_CONTENTS_END'
  help_comm = 'system command: identify loop contents'
  helpja_comm = 'システムコマンド：ループコンテンツを識別する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CONTINUE
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  !
  ! def, \=>, nore, \-> %{{{
  CALL next_command()
  maintag_comm   = 'def'
  subtag_comm(1) = '\=>'
  subtag_comm(2) = 'nore'
  subtag_comm(3) = '\->'
  ! [TODO] defとnoreを同一グループにしているためにヘルプの扱いがうまくいかない
  ! 処理をサブルーチンにわけては？
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    ! "def"  (or "\=>") DOES allow re-map
    ! "nore" (or "\->") does NOT allow re-map
    IF( macrosize < 2 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    IF( macro(2) == '\\)' .OR. macro(2) == '\..)' )THEN
      ! does not allow user to map "\\" or "\.." command
      CALL psc_error( cword, 9 )
      RETURN
    END IF
    ! ltmpa if-and-only "nore" (we use this flag in this section)
    ltmpa = .FALSE.
    IF( (cword=='nore').OR.(cword=='\->'))ltmpa = .TRUE.
    ! ltmpb if-and-only self-map (we use)
    ltmpb = .FALSE.
    IF( macro(1) == macro(2) ) ltmpb = .TRUE.
    ! def does NOT allow self-map!
    IF( (.NOT.ltmpa).AND.(ltmpb) )THEN
      CALL psc_error( cword, 9 )
      RETURN
    END IF
    ! search map
    itmpb = 0
    DO idoa = 1, mapsize
      IF( TRIM(macro(2)) == map(idoa)%lhs )THEN
        itmpb = idoa
        EXIT
      END IF
    END DO
    !
    IF( ( itmpb == 0 ) )THEN
      IF( .NOT.ltmpb )THEN
        ! map is NEW, and not self-map -> make new map
        mapsize = mapsize+1
        map(mapsize)%lhs = TRIM(macro(2))
        map(mapsize)%rhs = TRIM(macro(1))
        ! print
        IF( ltmpa )THEN
          map(mapsize)%no = .TRUE.
          WRITE(stmpa,*) TRIM(map(mapsize)%lhs)//' -> '//TRIM(map(mapsize)%rhs)
          CALL psc_inform(stmpa)
        ELSE
          map(mapsize)%no = .FALSE.
          WRITE(stmpa,*) TRIM(map(mapsize)%lhs)//' => '//TRIM(map(mapsize)%rhs)
          CALL psc_inform(stmpa)
        END IF
      END IF
    ELSE
      ! map is already exist
      IF( ltmpa )THEN ! nore
        ! if nore, replace map at first anyway
        map(itmpb)%rhs = TRIM(macro(1))
        IF( ltmpb )THEN
          ! nore if self-map -> delete map
          IF( itmpb/=mapsize )THEN
            map(itmpb)%lhs = map(mapsize)%lhs
            map(itmpb)%rhs = map(mapsize)%rhs
          END IF
          map(mapsize)%lhs = ''
          map(mapsize)%rhs = ''
          mapsize = mapsize-1
        ELSE
          ! nore is not self-map -> success map
          WRITE(stmpa,*) TRIM(map(itmpb)%lhs)//' -> '//TRIM(map(itmpb)%rhs)
          CALL psc_inform(stmpa)
        END IF
      ELSE
        ! def does NOT allow self-map
        IF( .NOT.ltmpb )THEN
          map(itmpb)%rhs = TRIM(macro(1))
          WRITE(stmpa,*) TRIM(map(itmpb)%lhs)//' => '//TRIM(map(itmpb)%rhs)
          CALL psc_inform(stmpa)
        END IF
      END IF
    END IF
    IF( ltmpa.AND.ltmpb )THEN
      ! message for nore self-map
      WRITE(stmpa,*) TRIM(macro(2))//' ->'
      CALL psc_inform(stmpa)
    END IF
    ! delete string stack
    CALL psc_strst_remove
    CALL psc_strst_remove
    RETURN
  END IF
  !%}}}
  ! mapexist, \??@" %{{{
  CALL next_command()
  maintag_comm   = 'mapexist'
  subtag_comm(1) = '\??@"'
  help_comm = 'push 1 if map (##1) exist, otherwise push 0'
  helpja_comm = 'マップ(##1)が存在すれば1を、そうでないなら0をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize == 0 )THEN
      CALL psc_error( cword, 4 )
      RETURN
    END IF
    ltmpa = .FALSE.
    DO idoa =  mapsize, 1, -1
      IF( macro(1) == map(idoa)%lhs )THEN
        ltmpa = .TRUE.
        EXIT
      END IF
    END DO
    CALL psc_stack_append
    IF( iferror ) RETURN
    IF( ltmpa )THEN
      stack(stz) = 1.d0
    ELSE
      stack(stz) = 0.d0
    END IF
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! defexist, \??=> %{{{
  CALL next_command()
  maintag_comm   = 'defexist'
  subtag_comm(1) = '\??=>'
  help_comm = 'push 1 if map (##1) with allowing remap exist, otherwise push 0'
  helpja_comm = '再マップを許すマップ(##1)が存在すれば1を、そうでないなら0をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize == 0 )THEN
      CALL psc_error( cword, 4 )
      RETURN
    END IF
    ltmpa = .FALSE.
    DO idoa =  mapsize, 1, -1
      IF( (macro(1) == map(idoa)%lhs ) .AND. ( .NOT. map(idoa)%no ) )THEN
        ltmpa = .TRUE.
        EXIT
      END IF
    END DO
    CALL psc_stack_append
    IF( iferror ) RETURN
    IF( ltmpa )THEN
      stack(stz) = 1.d0
    ELSE
      stack(stz) = 0.d0
    END IF
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! noreexist, \??-> %{{{
  CALL next_command()
  maintag_comm   = 'noreexist'
  subtag_comm(1) = '\??->'
  help_comm = 'push 1 if map (##1) without allowing remap exist, otherwise push 0'
  helpja_comm = '再マップを許さないマップ(##1)が存在すれば1を、そうでないなら0をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize == 0 )THEN
      CALL psc_error( cword, 4 )
      RETURN
    END IF
    ltmpa = .FALSE.
    DO idoa =  mapsize, 1, -1
      IF( (macro(1) == map(idoa)%lhs ) .AND. ( map(idoa)%no ) )THEN
        ltmpa = .TRUE.
        EXIT
      END IF
    END DO
    CALL psc_stack_append
    IF( iferror ) RETURN
    IF( ltmpa )THEN
      stack(stz) = 1.d0
    ELSE
      stack(stz) = 0.d0
    END IF
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! getmaprhs, \?@"> %{{{
  CALL next_command()
  maintag_comm   = 'getmaprhs'
  subtag_comm(1) = '\?@">'
  help_comm = 'push mapped string from map (##1)'
  helpja_comm = 'マップ(##1)からマップされる文字列をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize == 0 )THEN
      CALL psc_error( cword, 4 )
      RETURN
    END IF
    ltmpa = .FALSE.
    DO idoa =  mapsize, 1, -1
      IF( macro(1) == map(idoa)%lhs )THEN
        ltmpa = .TRUE.
        EXIT
      END IF
    END DO
    IF( .NOT. ltmpa )THEN
      CALL psc_error( cword, 17 )
      RETURN
    END IF
    macro(1) = map(idoa)%rhs
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! clearmaps, clm, \><" %{{{
  CALL next_command()
  maintag_comm   = 'clearmaps'
  subtag_comm(1) = 'clm'
  subtag_comm(2) = '\><"'
  help_comm = 'kill all maps'
  helpja_comm = 'マップを全て破棄する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    mapsize = 0
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! SYSTEM_END_MAP %{{{
  CALL next_command()
  maintag_comm   = 'SYSTEM_END_MAP'
  help_comm = 'system command: identify map ends'
  helpja_comm = 'システムコマンド：マップ終了を識別する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  !
  ! let, setvalue, \!@>$  %{{{
  CALL next_command()
  maintag_comm   = 'let'
  subtag_comm(1) = 'setvalue'
  subtag_comm(2) = '\!@>$'
  help_comm = 'set the value of variable (##1) to (#1)'
  helpja_comm = '変数(##1)の値を(#1)に設定する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz == 0 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    IF( macrosize == 0 )THEN
      CALL psc_error( cword, 4 )
      RETURN
    END IF
    ! 既に定義されている変数か調べる
    itmpb = 0
    DO idoa = 1, varsize
      IF( macro(1) == var(idoa)%tag )THEN
        ! 同じローカルレベルでないといけない
        IF( loclev == var(idoa)%loc  )THEN
          itmpb = idoa
          EXIT
        END IF
      END IF
    END DO
    IF( itmpb == 0 )THEN
      ! 未定義の場合変数を宣言して代入
      varsize = varsize + 1
      var(varsize)%tag = TRIM(macro(1))
      var(varsize)%val = stack(stz)
      var(varsize)%loc = loclev
    ELSE
      ! 既にある場合は上書き
      var(itmpb)%val = stack(stz)
    END IF
    CALL psc_printnum(stack(stz), fmt_a, stmpa, &
    & -1, .FALSE., sepstr)
    itmpa = LEN_TRIM(stmpa)
    stmpa = ' (' // TRIM(macro(1)) // ' = ' // stmpa(1:itmpa-1)
    IF( INDEX(bufin,'SYSTEM_') == 0 )THEN
      CALL psc_inform(stmpa)
    END IF
    CALL psc_stack_remove
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! ref, getvalue, $, \?@>$ %{{{
  CALL next_command()
  maintag_comm   = 'ref'
  subtag_comm(1) = 'getvalue'
  subtag_comm(2) = '$'
  subtag_comm(3) = '\?@>$'
  help_comm = 'push the value of variable (##1)'
  helpja_comm = '変数(##1)の値をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize == 0 )THEN
      CALL psc_error( cword, 4 )
      RETURN
    END IF
    ! ローカルレベルの近いものから順に検索し、最も近いものを得る
    DO idoc =  loclev, 0, -1
      itmpb = 0
      DO idoa = 1, varsize
        IF( macro(1) == var(idoa)%tag )THEN
          ! ローカルレベルの判定
          IF( idoc == var(idoa)%loc )THEN
            itmpb = idoa
            EXIT
          END IF
        END IF
      END DO
      ! もしこの階層で見つかっていればitmpb/=0のはず
      IF( itmpb/=0)EXIT
    END DO
    IF( itmpb == 0 )THEN
      ! 変数がない場合
      CALL psc_error( cword, 16 )
      RETURN
    END IF
    ! 変数がある場合
    CALL psc_stack_append
    IF( iferror ) RETURN
    stack(stz) = var(itmpb)%val
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! varkill, \$> %{{{
  CALL next_command()
  maintag_comm   = 'varkill'
  subtag_comm(1) = '\$>'
  help_comm = 'kill variable (##1)'
  helpja_comm = '変数(##1)を破棄する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize == 0 )THEN
      CALL psc_error( cword, 4 )
      RETURN
    END IF
    ! ローカルレベルの近いものから順に検索し、最も近いものを得る
    DO idoc =  loclev, 0, -1
      itmpb = 0
      DO idoa = 1, varsize
        IF( macro(1) == var(idoa)%tag )THEN
          ! ローカルレベルの判定
          IF( idoc == var(idoa)%loc )THEN
            itmpb = idoa
            EXIT
          END IF
        END IF
      END DO
      ! もしこの階層で見つかっていればitmpb/=0のはず
      IF( itmpb /= 0 ) EXIT
    END DO
    IF( itmpb == 0 )THEN
      ! 変数がない場合
      CALL psc_error( cword, 16 )
      RETURN
    END IF
    ! 変数がある場合
    CALL psc_strst_remove
    ! 変数の削除
    var(itmpb)%tag = var(varsize)%tag
    var(itmpb)%val = var(varsize)%val
    var(itmpb)%loc = var(varsize)%loc
    varsize = varsize-1
    WRITE(stmpa,*) '('//TRIM(var(itmpb)%tag)//') = void'
    IF( INDEX(bufin,'SYSTEM_') == 0 )THEN
      CALL psc_inform(stmpa)
    END IF
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! varexist, \??@$ %{{{
  CALL next_command()
  maintag_comm   = 'varexist'
  subtag_comm(1) = '\??@$'
  help_comm = 'push 1 if variable (##1) exist, otherwise push 0'
  helpja_comm = '変数(##1)が存在すれば1を、そうでないなら0をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize == 0 )THEN
      CALL psc_error( cword, 4 )
      RETURN
    END IF
    ! ローカルレベルの近いものから順に検索し、最も近いものを得る
    DO idoc =  loclev, 0, -1
      itmpb = 0
      DO idoa = 1, varsize
        IF( macro(1) == var(idoa)%tag )THEN
          ! ローカルレベルの判定
          IF( idoc == var(idoa)%loc )THEN
            itmpb = idoa
            EXIT
          END IF
        END IF
      END DO
      ! もしこの階層で見つかっていればitmpb/=0のはず
      IF( itmpb /= 0 ) EXIT
    END DO
    CALL psc_stack_append
    IF( iferror ) RETURN
    IF( itmpb == 0 )THEN
      ! 変数がない場合
      stack(stz) = 0.d0
    ELSE
      ! 変数がある場合
      stack(stz) = 1.d0
    END IF
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! clearvars, clv, \><$ %{{{
  CALL next_command()
  maintag_comm   = 'clearvars'
  subtag_comm(1) = 'clv'
  subtag_comm(2) = '\><$'
  help_comm = 'kill all variables'
  helpja_comm = '変数を全て破棄する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    varsize = 0
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! bgroup, { %{{{
  CALL next_command()
  maintag_comm   = 'bgroup'
  subtag_comm(1) = '{'
  help_comm = 'begin group'
  helpja_comm = 'グループを開始する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    loclev = loclev+1
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! egroup, } %{{{
  CALL next_command()
  maintag_comm   = 'egroup'
  subtag_comm(1) = '}'
  help_comm = 'end group'
  helpja_comm = 'グループを終了する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( loclev == 0 ) RETURN
    idoa = 1
    DO
      IF( idoa >= varsize ) EXIT
      IF( var(idoa)%loc == loclev )THEN
        var(idoa)%tag = var(varsize)%tag
        var(idoa)%val = var(varsize)%val
        var(idoa)%loc = var(varsize)%loc
        varsize = varsize-1
      ELSE
        idoa = idoa+1
      END IF
    END DO
    IF( var(varsize)%loc == loclev )THEN
      varsize = varsize-1
    END IF
    loclev = loclev-1
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  !%}}}
  ! Options(17) %{{{
  ! SYSTEM_USE_NEWLINE %{{{
  CALL next_command()
  maintag_comm   = 'SYSTEM_USE_NEWLINE'
  help_comm = 'system command: set autonl flag ON'
  helpja_comm = 'システムコマンド：autonlフラグをONに設定する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    autonl = .TRUE.
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  !
  ! setflag, \!& %{{{
  CALL next_command()
  maintag_comm   = 'setflag'
  subtag_comm(1) = '\!&'
  help_comm = 'set PSC flag (##1) ON if int(#1)/=0, otherwise OFF'
  helpja_comm = 'int(#1)/=0ならばPSCフラグ(##1)をONに設定する、そうでなければOFFに設定する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize == 0 )THEN
      CALL psc_error( cword, 4 )
      RETURN
    END IF
    IF( stz == 0 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    stmpa = macro(1)
    itmpa = LEN_TRIM( stmpa )
    stmpa = stmpa(1:itmpa-1)
    itmpa = NINT( stack(stz) )
    IF( stmpa == 'quiet' )THEN
      quiet = .TRUE.
      IF( itmpa == 0) quiet = .FALSE.
    ELSE IF( stmpa == 'super_quiet' )THEN
      super_quiet = .TRUE.
      IF( itmpa == 0) super_quiet = .FALSE.
    ELSE IF( stmpa == 'error_quiet' )THEN
      error_quiet = .TRUE.
      IF( itmpa == 0) error_quiet = .FALSE.
    ELSE IF( stmpa == 'error_cont' )THEN
      error_cont = .TRUE.
      IF( itmpa == 0) error_cont = .FALSE.
    ELSE IF( stmpa == 'quiet_load' )THEN
      quiet_load = .TRUE.
      IF( itmpa == 0) quiet_load = .FALSE.
    ELSE IF( stmpa == 'color' )THEN
      color = .TRUE.
      IF( itmpa == 0) color = .FALSE.
    ELSE IF( stmpa == 'autonl' )THEN
      autonl = .TRUE.
      IF( itmpa == 0) autonl = .FALSE.
    ELSE IF( stmpa == 'debug_buffer' )THEN
      IF( itmpa == 1 ) THEN
        debug_buffer_start = .TRUE.
        debug_buffer_read = .TRUE.
        debug_buffer_clean = .TRUE.
        debug_buffer_map = .TRUE.
        debug_buffer_trim = .TRUE.
        debug_buffer_last = .TRUE.
        debug_buffer_do = .TRUE.
      ELSE
        debug_buffer_start = .FALSE.
        debug_buffer_read = .FALSE.
        debug_buffer_clean = .FALSE.
        debug_buffer_map = .FALSE.
        debug_buffer_trim = .FALSE.
        debug_buffer_last = .FALSE.
        debug_buffer_do = .FALSE.
      END IF
    ELSE IF( stmpa == 'debug_loop' )THEN
      debug_loop = .TRUE.
      IF( itmpa == 0) debug_loop = .FALSE.
    ELSE IF( stmpa == 'debug_level' )THEN
      debug_level = .TRUE.
      IF( itmpa == 0) debug_level = .FALSE.
    ELSE IF( stmpa == 'debug_commd' )THEN
      debug_commd = .TRUE.
      IF( itmpa == 0) debug_commd = .FALSE.
    ELSE IF( stmpa == 'debug_stack' )THEN
      debug_stack = .TRUE.
      IF( itmpa == 0) debug_stack = .FALSE.
    ELSE
      CALL psc_error( cword, 8 )
      RETURN
    END IF
    CALL psc_stack_remove
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! getflag, \?& %{{{
  CALL next_command()
  maintag_comm   = 'getflag'
  subtag_comm(1) = '\?&'
  help_comm = 'push 1 if PSC flag (##1) is ON, otherwise push 0'
  helpja_comm = 'PSCフラグ(##1)がONならば1をプッシュする、そうでなければ0をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize == 0 )THEN
      CALL psc_error( cword, 4 )
      RETURN
    END IF
    stmpa = macro(1)
    itmpa = LEN_TRIM( stmpa )
    stmpa = stmpa(1:itmpa-1)
    CALL psc_stack_append
    IF( iferror ) RETURN
    CALL psc_strst_remove
    itmpa = 0
    ltmpa = .FALSE.
    IF( stmpa =='quiet' )THEN
      ltmpa = quiet
    ELSE IF( stmpa =='super_quiet' )THEN
      ltmpa = super_quiet
    ELSE IF( stmpa =='error_quiet' )THEN
      ltmpa = error_quiet
    ELSE IF( stmpa =='error_cont' )THEN
      ltmpa = error_cont
    ELSE IF( stmpa =='quiet_load' )THEN
      ltmpa = quiet_load
    ELSE IF( stmpa =='color' )THEN
      ltmpa = color
    ELSE IF( stmpa =='autonl' )THEN
      ltmpa = autonl
    ELSE IF( stmpa =='debug_loop' )THEN
      ltmpa = debug_loop
    ELSE IF( stmpa =='debug_level' )THEN
      ltmpa = debug_level
    ELSE IF( stmpa =='debug_commd' )THEN
      ltmpa = debug_commd
    ELSE IF( stmpa =='debug_stack' )THEN
      ltmpa = debug_stack
    ELSE
      CALL psc_error( cword, 8 )
      RETURN
    END IF
    IF( ltmpa ) stack(stz) = 1.d0
    IF( .NOT. ltmpa ) stack(stz) = 0.d0
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  !
  ! setunit, \!+ %{{{
  CALL next_command()
  maintag_comm   = 'setunit'
  subtag_comm(1) = '\!+'
  help_comm = 'set increment unit to (#1)'
  helpja_comm = 'インクリメントの単位を(#1)に設定する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz == 0 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    incunit = stack(stz)
    CALL psc_stack_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! getunit, \?+ %{{{
  CALL next_command()
  maintag_comm   = 'getunit'
  subtag_comm(1) = '\?+'
  help_comm = 'push increment unit'
  helpja_comm = 'インクリメントの単位をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_stack_append
    IF( iferror ) RETURN
    stack(stz) = incunit
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! setdigit, \!. %{{{
  CALL next_command()
  maintag_comm   = 'setdigit'
  subtag_comm(1) = '\!.'
  help_comm = 'set digits-of-print to (#1)'
  helpja_comm = '出力の桁数を(#1)に設定する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz == 0 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    IF( ( NINT(stack(stz)) < 2 ) .OR. ( NINT(stack(stz)) > 20 ) )THEN
      CALL psc_error( cword, 8 )
      RETURN
    END IF
    decimals = NINT(stack(stz))
    WRITE(stmpa,'(I5)') 10+decimals
    WRITE(stmpb,'(I5)') decimals
    stmpa = ADJUSTL(stmpa)
    stmpb = ADJUSTL(stmpb)
    fmt_a = '(G'//TRIM(stmpa)//'.'//TRIM(stmpb)//')'
    CALL psc_stack_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! getdigit, \?. %{{{
  CALL next_command()
  maintag_comm   = 'getdigit'
  subtag_comm(1) = '\?.'
  help_comm = 'push digits-of-print'
  helpja_comm = '出力の桁数をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_stack_append
    IF( iferror ) RETURN
    stack(stz) = DBLE(decimals)
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! setfmt, \!_ %{{{
  CALL next_command()
  maintag_comm   = 'setfmt'
  subtag_comm(1) = '\!_'
  help_comm = 'set printing-format to (##1)'
  helpja_comm = '出力の書式を(##1)に設定する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize == 0 )THEN
      CALL psc_error( cword, 4 )
      RETURN
    END IF
    fmt_a = '(' // TRIM(macro(1))
    decimals = 0
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! getfmt, \?_ %{{{
  CALL next_command()
  maintag_comm   = 'getfmt'
  subtag_comm(1) = '\?_'
  help_comm = 'push printing-format'
  helpja_comm = '出力の書式をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_strst_append
    IF( iferror ) RETURN
    macro(1) = fmt_a(2:)
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! setsep, \!, %{{{
  CALL next_command()
  maintag_comm   = 'setsep'
  subtag_comm(1) = '\!,'
  help_comm = 'set separator-string to (##1)'
  helpja_comm = 'セパレータ文字列を(##1)に設定する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize == 0 )THEN
      CALL psc_error( cword, 4 )
      RETURN
    END IF
    sepstr = TRIM(macro(1))
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! getsep, \?, %{{{
  CALL next_command()
  maintag_comm   = 'getsep'
  subtag_comm(1) = '\?,'
  help_comm = 'push separator-string'
  helpja_comm = 'セパレータ文字列をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_strst_append
    macro(1) = sepstr
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  !
  ! setps1, \!> %{{{
  CALL next_command()
  maintag_comm   = 'setps1'
  subtag_comm(1) = '\!>'
  help_comm = 'set main-prompt-string to (##1)'
  helpja_comm = 'メインプロンプト文字列を(##1)に設定する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize == 0 )THEN
      CALL psc_error( cword, 4 )
      RETURN
    END IF
    stmpa = macro(1)
    itmpa = LEN_TRIM( stmpa )
    prompt_normal = stmpa(1:itmpa-1)
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! getps1, \?> %{{{
  CALL next_command()
  maintag_comm   = 'getps1'
  subtag_comm(1) = '\?>'
  help_comm = 'push main-prompt-string'
  helpja_comm = 'メインプロンプト文字列をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_strst_append
    macro(1) = TRIM( prompt_normal ) // ')'
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! setps2, \!>> %{{{
  CALL next_command()
  maintag_comm   = 'setps2'
  subtag_comm(1) = '\!>>'
  help_comm = 'set sub-prompt-string to (##1)'
  helpja_comm = 'サブプロンプト文字列を(##1)に設定する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize == 0 )THEN
      CALL psc_error( cword, 4 )
      RETURN
    END IF
    stmpa = macro(1)
    itmpa = LEN_TRIM( stmpa )
    prompt_special = stmpa(1:itmpa-1)
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! getps2, \?>> %{{{
  CALL next_command()
  maintag_comm   = 'getps2'
  subtag_comm(1) = '\?>>'
  help_comm = 'push sub-prompt-string'
  helpja_comm = 'サブプロンプト文字列をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL psc_strst_append
    macro(1) = TRIM( prompt_special ) // ')'
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  !
  ! seterrmsg %{{{
  CALL next_command()
  maintag_comm   = 'seterrmsg'
  help_comm = 'set error message string for error code (#1) to (##1)'
  helpja_comm = 'エラーコード(#1)に対するエラーメッセージ文字列を(##1)に設定する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz == 0 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    IF( ( NINT(stack(stz)) < 1 ) .OR. ( NINT(stack(stz)) > UBOUND(errmsg,1) ) )THEN
      CALL psc_error( cword, 8 )
      RETURN
    END IF
    IF( macrosize == 0 )THEN
      CALL psc_error( cword, 4 )
      RETURN
    END IF
    stmpa = macro(1)
    itmpa = LEN_TRIM( stmpa )
    errmsg(NINT(stack(stz))) = stmpa(1:itmpa-1)
    CALL psc_strst_remove
    CALL psc_stack_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! geterrmsg %{{{
  CALL next_command()
  maintag_comm   = 'geterrmsg'
  help_comm = 'push error message string for error code (#1)'
  helpja_comm = 'エラーコード(#1)に対するエラーメッセージ文字列をプッシュする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz == 0 )THEN
      CALL psc_error( cword, 2 )
      RETURN
    END IF
    IF( ( NINT(stack(stz)) < 1 ) .OR. ( NINT(stack(stz)) > UBOUND(errmsg,1) ) )THEN
      CALL psc_error( cword, 8 )
      RETURN
    END IF
    CALL psc_strst_append
    IF( iferror ) RETURN
    macro(1) = TRIM(errmsg(NINT(stack(stz)))) // ')'
    CALL psc_stack_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  !%}}}
  ! Graph Plot(22) %{{{
  ! draw, d, \** %{{{
  CALL next_command()
  maintag_comm   = 'draw'
  subtag_comm(1) = 'd'
  subtag_comm(2) = '\**'
  help_comm = 'make eps image file (##1) from kdraw file (##2)'
  helpja_comm = 'kdrawファイル(##2)からeps画像ファイル(##1)を作成する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize < 2 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    stmpa = macro(2)
    stmpb = macro(1)
    stmpa = stmpa(1:LEN_TRIM(stmpa)-1)
    stmpb = stmpb(1:LEN_TRIM(stmpb)-1)
    itmpa = 0
    CALL kdraw(stmpa, stmpb, itmpa)
    IF( itmpa/=0 )THEN
      IF( itmpa == 1 )THEN
        CALL psc_error( cword, 9 )
      ELSE IF( itmpa == 2 )THEN
        CALL psc_error( cword, 11 )
      ELSE IF( itmpa == 3 )THEN
        CALL psc_error( cword, 10 )
      ELSE
        CALL psc_error( cword, 100 ) ! Fatal Error
      END IF
    END IF
    CALL psc_strst_remove
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! plot, p, \* %{{{
  CALL next_command()
  maintag_comm   = 'plot'
  subtag_comm(1) = 'p'
  subtag_comm(2) = '\*'
  help_comm = 'make eps image file (##1) from data file (##2)'
  helpja_comm = 'データファイル(##2)からeps画像ファイル(##1)を作成する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize < 2 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    stmpa = macro(2) ! data (in)
    stmpb = macro(1) ! graph (out)
    stmpa = stmpa(1:LEN_TRIM(stmpa)-1)
    stmpb = stmpb(1:LEN_TRIM(stmpb)-1)
    itmpb = 0 ! error detection (out)
    CALL kplot(1, stmpa, stmpb, itmpb, stmpc)
    IF( itmpb/=0 )THEN
      ! ここのエラー処理はerrorサブルーチンに統合すべき
      WRITE(0,'(A)') 'Error on ''plot'''
      WRITE(0,'(AI5,A)') 'E',itmpb,': '//TRIM(stmpc)
    END IF
    CALL psc_strst_remove
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! multiplot, mp, \*++ %{{{
  CALL next_command()
  maintag_comm   = 'multiplot'
  subtag_comm(1) = 'mp'
  subtag_comm(2) = '\*++'
  help_comm = 'plot multiple (#1) of data (##2) on graph (##1)'
  helpja_comm = '複数の(#1)データ(##2)をグラフ(##1)にプロットする'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize < 2 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    IF( stz < 1 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    stmpa = macro(2) ! data (in)
    stmpb = macro(1) ! graph (out)
    stmpa = stmpa(1:LEN_TRIM(stmpa)-1)
    stmpb = stmpb(1:LEN_TRIM(stmpb)-1)
    itmpa = NINT(stack(stz)) ! number of data files (in)
    itmpb = 0 ! error detection (out)
    CALL kplot(itmpa, stmpa, stmpb, itmpb, stmpc)
    IF( itmpb/=0 )THEN
      ! ここのエラー処理はerrorサブルーチンに統合すべき
      WRITE(0,'(A)') 'Error on ''multiplot'''
      WRITE(0,'(AI5,A)') 'E',itmpb,': '//TRIM(stmpc)
    END IF
    CALL kplot_initializer()
    CALL psc_stack_remove
    CALL psc_strst_remove
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! debugplot, \(*_*)* %{{{
  CALL next_command()
  maintag_comm   = 'debugplot'
  subtag_comm(1) = '\(*_*)*'
  help_comm = 'make eps image file (##1) from data file (##2) with debug messages'
  helpja_comm = 'デバグメッセージとともにデータファイル(##2)からeps画像ファイル(##1)を作成する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize < 2 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    stmpa = macro(2)
    stmpb = macro(1)
    stmpa = stmpa(1:LEN_TRIM(stmpa)-1)
    stmpb = stmpb(1:LEN_TRIM(stmpb)-1)
    itmpb = 1
    CALL kplot(1, stmpa, stmpb, itmpb, stmpc)
    IF( itmpb/=0 )THEN
      ! ここのエラー処理はerrorサブルーチンに統合すべき
      WRITE(0,'(A)') 'Error on ''plot'''
      WRITE(0,'(AI5,A)') 'E',itmpb,': '//TRIM(stmpc)
    END IF
    CALL kplot_initializer()
    CALL psc_strst_remove
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! plotinit %{{{
  CALL next_command()
  maintag_comm   = 'plotinit'
  help_comm = 'initialize plot setting'
  helpja_comm = 'プロット設定を初期化する'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    CALL kplot_initializer()
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  !
  ! 型ごとのsetkp,getkpコマンドは段階的に廃止したい
  ! setkpmultiopt %{{{
  CALL next_command()
  maintag_comm   = 'setkpmultiopt'
  help_comm = 'set multiple kplot option'
  helpja_comm = 'kplotの複数のオプションを設定する'
  usage_comm = '(<arguments>) setkpmultiopt'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize < 1 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    stmpa = macro(1)
    CALL psc_strst_remove
    CALL psc_strst_append
    macro(1) = 'SYSTEM_END_SETKPMULTIOPT)'
    stmpa = stmpa(1:LEN_TRIM(stmpa)-1)
    bufin = TRIM(stmpa) // ' SYSTEM_SETKPMULTIOPT_MAIN,' // TRIM(bufin)
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! SETKPMULTIOPT_MAIN %{{{
  CALL next_command()
  maintag_comm   = 'SYSTEM_SETKPMULTIOPT_MAIN'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize < 1 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    IF( macro(1) == 'SYSTEM_END_SETKPMULTIOPT)' )THEN
      CALL psc_strst_remove
      RETURN
    END IF
    stmpa = macro(1)
    stmpa = stmpa(1:LEN_TRIM(stmpa)-1)
    CALL setkpopt(cword, stmpa)
    CALL psc_strst_remove
    !
    bufin = 'SYSTEM_SETKPMULTIOPT_MAIN,' // TRIM(bufin)
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! setkpopt  , \!* %{{{
  CALL next_command()
  maintag_comm   = 'setkpopt'
  subtag_comm(1) = '\!*'
  help_comm = 'set kplot option <name> to value <values>'
  helpja_comm = 'kplotのオプション<name>の値を<values>に設定する'
  usage_comm = '<values>(<name>)setkpopt'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize < 1 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    stmpa = macro(1)
    stmpa = stmpa(1:LEN_TRIM(stmpa)-1)
    CALL setkpopt(cword, stmpa)
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! setkpstr  , \!*" %{{{
  CALL next_command()
  maintag_comm   = 'setkpstr'
  subtag_comm(1) = '\!*"'
  help_comm = 'set kplot string option <name> to value <value>'
  helpja_comm = 'kplotの文字列オプション<name>の値を<value>に設定する'
  usage_comm = '(<value>)(<name>)setkpstr'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize < 2 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    stmpa = macro(2)
    stmpb = macro(1)
    stmpa = stmpa(1:LEN_TRIM(stmpa)-1)
    stmpb = stmpb(1:LEN_TRIM(stmpb)-1)
    CALL setkpopt_string(cword, stmpb, stmpa)
    CALL psc_strst_remove
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! setkpint  , \!*# %{{{
  CALL next_command()
  maintag_comm   = 'setkpint'
  subtag_comm(1) = '\!*#'
  help_comm = 'set kplot integer option <name> to value <value>'
  helpja_comm = 'kplotの整数オプション<name>の値を<value>に設定する'
  usage_comm = '<value>(<name>)setkpint'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize < 1 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    IF( stz < 1 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    stmpa = macro(1)
    stmpa = stmpa(1:LEN_TRIM(stmpa)-1)
    itmpa = NINT(stack(stz))
    CALL setkpopt_integer(cword, stmpa, itmpa)
    CALL psc_stack_remove
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! setkpflag , \!*? %{{{
  CALL next_command()
  maintag_comm   = 'setkpflag'
  subtag_comm(1) = '\!*?'
  help_comm = 'set kplot flag option <name> to value <value>'
  helpja_comm = 'kplotのフラグオプション<name>の値を<value>に設定する'
  usage_comm = '<value>(<name>)setkpflag'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize < 1 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    IF( stz < 1 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    stmpa = macro(1)
    stmpa = stmpa(1:LEN_TRIM(stmpa)-1)
    itmpa = NINT(stack(stz))
    IF( itmpa == 0 )THEN
      ltmpa = .FALSE.
    ELSE
      ltmpa = .TRUE.
    END IF
    CALL setkpopt_flag(cword, stmpa, ltmpa)
    CALL psc_stack_remove
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! setkpreal , \!*$ %{{{
  CALL next_command()
  maintag_comm   = 'setkpreal'
  subtag_comm(1) = '\!*$'
  help_comm = 'set kplot real-number option <name> to value <value>'
  helpja_comm = 'kplotの実数オプション<name>の値を<value>に設定する'
  usage_comm = '<value>(<name>)setkpreal'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize < 1 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    IF( stz < 1 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    stmpa = macro(1)
    stmpa = stmpa(1:LEN_TRIM(stmpa)-1)
    rtmpa = stack(stz)
    CALL setkpopt_real(cword, stmpa, rtmpa)
    CALL psc_stack_remove
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! setkpcolor, \!*& %{{{
  CALL next_command()
  maintag_comm   = 'setkpcolor'
  subtag_comm(1) = '\!*&'
  help_comm = 'set kplot color option <name> to value RGB=[<value_red>,<value_green>,<value_blue>]'
  helpja_comm = 'kplotの色オプション<name>を値RGB=[<value_red>,<value_green>,<value_blue>]に設定する'
  usage_comm = '<value_red> <value_green> <value_blue>(<name>)setkpcolor'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize < 1 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    IF( stz < 3 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    stmpa = macro(1)
    stmpa = stmpa(1:LEN_TRIM(stmpa)-1)
    rtmpa = stack(stz-2)
    rtmpb = stack(stz-1)
    rtmpc = stack(stz)
    IF( rtmpa > 1.d0 ) rtmpa = 1.d0
    IF( rtmpb > 1.d0 ) rtmpb = 1.d0
    IF( rtmpc > 1.d0 ) rtmpc = 1.d0
    IF( rtmpa < 0.d0 ) rtmpa = 0.d0
    IF( rtmpb < 0.d0 ) rtmpb = 0.d0
    IF( rtmpc < 0.d0 ) rtmpc = 0.d0
    CALL setkpopt_color(cword, stmpa, rtmpa, rtmpb, rtmpc)
    CALL psc_stack_remove
    CALL psc_stack_remove
    CALL psc_stack_remove
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! setkpstylecolor %{{{
  CALL next_command()
  maintag_comm   = 'setkpstylecolor'
  help_comm = 'set <i>-th kplot style color option to value &
  &RGB=[<value_red>,<value_green>,<value_blue>]'
  helpja_comm = 'kplotの<i>番目のスタイル色オプションを値&
  &RGB=[<value_red>,<value_green>,<value_blue>]に設定する'
  usage_comm = '<value_red> <value_green> <value_blue> <i> setkpstylecolor'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    ! [usage] red green blue num setkpstylecolor
    IF( stz < 4 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    rtmpa = stack(stz-3)
    rtmpb = stack(stz-2)
    rtmpc = stack(stz-1)
    IF( rtmpa > 1.d0) rtmpa = 1.d0
    IF( rtmpb > 1.d0) rtmpb = 1.d0
    IF( rtmpc > 1.d0) rtmpc = 1.d0
    IF( rtmpa < 0.d0) rtmpa = 0.d0
    IF( rtmpb < 0.d0) rtmpb = 0.d0
    IF( rtmpc < 0.d0) rtmpc = 0.d0
    CALL setkpopt_stylecolor(cword, NINT(stack(stz)), rtmpa, rtmpb, rtmpc )
    CALL psc_stack_remove
    CALL psc_stack_remove
    CALL psc_stack_remove
    CALL psc_stack_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! setkpstyleline %{{{
  CALL next_command()
  maintag_comm   = 'setkpstyleline'
  help_comm = 'set <i>-th kplot style line option to <value>'
  helpja_comm = 'kplotの<i>番目のスタイル線オプションを<value>に設定する'
  usage_comm = '(<value>) <i> setkpstyleline'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    IF( macrosize < 1 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    rtmpc = stack(stz)
    stmpa = macro(1)
    stmpa = stmpa(1:LEN_TRIM(stmpa)-1)
    CALL setkpopt_styleline(cword, NINT(stack(stz)), stmpa)
    CALL psc_stack_remove
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! setkpstylepoint %{{{
  CALL next_command()
  maintag_comm   = 'setkpstylepoint'
  help_comm = 'set <i>-th kplot style point option to <value>'
  helpja_comm = 'kplotの<i>番目のスタイル点オプションを<value>に設定する'
  usage_comm = '(<value>) <i> setkpstylepoint'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    ! [usage] (<option>)(<value>)setkpstr
    IF( stz < 1 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    IF( macrosize < 1 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    rtmpc = stack(stz)
    stmpa = macro(1)
    stmpa = stmpa(1:LEN_TRIM(stmpa)-1)
    CALL setkpopt_stylepoint(cword, NINT(stack(stz)), stmpa)
    CALL psc_stack_remove
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! setkpstylefont %{{{
  CALL next_command()
  maintag_comm   = 'setkpstylefont'
  help_comm = 'set <i>-th kplot style font option to <value>'
  helpja_comm = 'kplotの<i>番目のスタイルフォントオプションを<value>に設定する'
  usage_comm = '(<value>) <i> setkpstylefont'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    ! [usage] (<option>)(<value>)setkpstr
    IF( stz < 1 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    IF( macrosize < 1 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    rtmpc = stack(stz)
    stmpa = macro(1)
    stmpa = stmpa(1:LEN_TRIM(stmpa)-1)
    CALL setkpopt_stylefont(cword, NINT(stack(stz)), stmpa)
    CALL psc_stack_remove
    CALL psc_strst_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! setkpstylesize %{{{
  CALL next_command()
  maintag_comm   = 'setkpstylesize'
  help_comm = 'set <i>-th kplot style size option to value <value>'
  helpja_comm = 'kplotの<i>番目のスタイルサイズオプションを値<value>に設定する'
  usage_comm = '(<value>) <i> setkpstylesize'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    ! [usage] value num setkpstylesize
    IF( stz < 2 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    rtmpa = stack(stz-1)
    CALL setkpopt_stylesize(cword, NINT(stack(stz)), rtmpa )
    CALL psc_stack_remove
    CALL psc_stack_remove
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  !
  ! getkpstr  , \?*" %{{{
  CALL next_command()
  maintag_comm   = 'getkpstr'
  subtag_comm(1) = '\?*"'
  help_comm = 'push the value of kplot string option <name>'
  helpja_comm = 'kplotの文字列オプション<name>の値をプッシュする'
  usage_comm = '(<name>)getkpstr'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize < 1 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    stmpa = macro(1)
    stmpa = stmpa(1:LEN_TRIM(stmpa)-1)
    CALL psc_strst_remove
    CALL getkpopt_string(cword, stmpa, stmpb)
    CALL psc_strst_append
    macro(1) = TRIM(stmpb) // ')'
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! getkpint  , \?*# %{{{
  CALL next_command()
  maintag_comm   = 'getkpint'
  subtag_comm(1) = '\?*#'
  help_comm = 'push the value of kplot integer option <name>'
  helpja_comm = 'kplotの整数オプション<name>の値をプッシュする'
  usage_comm = '(<name>)getkpint'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize < 1 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    stmpa = macro(1)
    stmpa = stmpa(1:LEN_TRIM(stmpa)-1)
    CALL psc_stack_append
    CALL psc_strst_remove
    CALL getkpopt_integer(cword, stmpa, itmpa)
    stack(stz) = DBLE( itmpa )
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! getkpflag , \?*? %{{{
  CALL next_command()
  maintag_comm   = 'getkpflag'
  subtag_comm(1) = '\?*?'
  help_comm = 'push the value of kplot flag option <name>'
  helpja_comm = 'kplotのフラグオプション<name>の値をプッシュする'
  usage_comm = '(<name>)getkpflag'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize < 1 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    stmpa = macro(1)
    stmpa = stmpa(1:LEN_TRIM(stmpa)-1)
    CALL psc_strst_remove
    CALL psc_stack_append
    CALL getkpopt_flag(cword, stmpa, ltmpa)
    IF( ltmpa )THEN
      stack(stz) = 1.d0
    ELSE
      stack(stz) = 0.d0
    END IF
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! getkpreal , \?*$ %{{{
  CALL next_command()
  maintag_comm   = 'getkpreal'
  subtag_comm(1) = '\?*$'
  help_comm = 'push the value of kplot real-number option <name>'
  helpja_comm = 'kplotの実数オプション<name>の値をプッシュする'
  usage_comm = '(<name>)getkpreal'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize < 1 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    stmpa = macro(1)
    stmpa = stmpa(1:LEN_TRIM(stmpa)-1)
    CALL psc_strst_remove
    CALL getkpopt_real(cword, stmpa, rtmpa)
    CALL psc_stack_append
    stack(stz) = rtmpa
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! getkpcolor, \?*& %{{{
  CALL next_command()
  maintag_comm   = 'getkpcolor'
  subtag_comm(1) = '\?*&'
  help_comm = 'push the value of kplot color option <name>'
  helpja_comm = 'kplotの色オプション<name>の値をプッシュする'
  usage_comm = '(<name>)getkpcolor'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( macrosize < 1 )THEN
      CALL psc_error( cword, 5 )
      RETURN
    END IF
    stmpa = macro(1)
    stmpa = stmpa(1:LEN_TRIM(stmpa)-1)
    CALL psc_strst_remove
    CALL psc_stack_append
    CALL psc_stack_append
    CALL psc_stack_append
    CALL getkpopt_color(cword, stmpa, rtmpa, rtmpb, rtmpc)
    stack(stz-2) = rtmpa
    stack(stz-1) = rtmpb
    stack(stz) = rtmpc
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! getkpstylecolor %{{{
  CALL next_command()
  maintag_comm   = 'getkpstylecolor'
  help_comm = 'push the value of <i>-th kplot style color option '
  helpja_comm = 'kplotの<i>番目のスタイル色オプションの値をプッシュする'
  usage_comm = '<i> getkpstylecolor'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    itmpa = NINT( stack(stz) )
    CALL psc_stack_remove
    CALL getkpopt_stylecolor(cword, itmpa, rtmpa, rtmpb, rtmpc )
    CALL psc_stack_append
    CALL psc_stack_append
    CALL psc_stack_append
    stack(stz-2) = rtmpa
    stack(stz-1) = rtmpb
    stack(stz) = rtmpc
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! getkpstyleline %{{{
  CALL next_command()
  maintag_comm   = 'getkpstyleline'
  help_comm = 'push the value of <i>-th kplot style line option'
  helpja_comm = 'kplotの<i>番目のスタイル線オプションの値をプッシュする'
  usage_comm = '<i> getkpstyleline'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    itmpa = NINT( stack(stz) )
    CALL psc_stack_remove
    CALL getkpopt_styleline(cword, itmpa, stmpa)
    CALL psc_strst_append
    macro(1) = TRIM(stmpa) // ')'
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! getkpstylepoint %{{{
  CALL next_command()
  maintag_comm   = 'getkpstylepoint'
  help_comm = 'push the value of <i>-th kplot style point option'
  helpja_comm = 'kplotの<i>番目のスタイル点オプションの値をプッシュする'
  usage_comm = '<i> getkpstylepoint'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    itmpa = NINT( stack(stz) )
    CALL psc_stack_remove
    CALL getkpopt_stylepoint(cword, itmpa, stmpa)
    CALL psc_strst_append
    macro(1) = TRIM(stmpa) // ')'
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! getkpstylefont %{{{
  CALL next_command()
  maintag_comm   = 'getkpstylefont'
  help_comm = 'push the value of <i>-th kplot style font option'
  helpja_comm = 'kplotの<i>番目のスタイルフォントオプションの値をプッシュする'
  usage_comm = '<i> getkpstylefont'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    itmpa = NINT( stack(stz) )
    CALL psc_stack_remove
    CALL getkpopt_stylefont(cword, itmpa, stmpa)
    CALL psc_strst_append
    macro(1) = TRIM(stmpa) // ')'
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  ! getkpstylesize %{{{
  CALL next_command()
  maintag_comm   = 'getkpstylesize'
  help_comm = 'push the value of <i>-th kplot style size option '
  helpja_comm = 'kplotの<i>番目のスタイルサイズオプションの値をプッシュする'
  usage_comm = '<i> getkpstylesize'
  commandmode = get_commandmode()
  IF( commandmode == 1 )THEN
    IF( stz < 1 )THEN
      CALL psc_error( cword, 3 )
      RETURN
    END IF
    itmpa = NINT( stack(stz) )
    CALL psc_stack_remove
    CALL getkpopt_stylecolor(cword, itmpa, rtmpa )
    CALL psc_stack_append
    stack(stz) = rtmpa
    RETURN
  ELSE IF( commandmode == 2 )THEN
    call psc_cmdhelp()
    RETURN
  END IF
  !%}}}
  !%}}}

  ! UNDEFINED or REAL %{{{
  ! FATAL ERROR %{{{
  IF( (cword == '' ) )THEN
    ! FATAL ERROR
    CALL psc_error( cword, 102 )
    RETURN
  END IF
  !%}}}
  ! HTML color %{{{
  IF( cword(1:1)=='#' )THEN
    ! HTML-color
    CALL read_html_color(cword, rtmpa, rtmpb, rtmpc)
    CALL psc_stack_append
    CALL psc_stack_append
    CALL psc_stack_append
    IF( iferror ) RETURN
    stack(stz-2) = rtmpa
    stack(stz-1) = rtmpb
    stack(stz) = rtmpc
    RETURN
  END IF
  !%}}}
  ! read real%{{{
  IF( psc_isreal(cword) )THEN
    READ(cword,*) rtmpa
    IF( debug_loop .OR. ( INDEX(bufin, 'SYSTEM_') == 0 ) )THEN
      IF( debug_commd) WRITE(*,'(A)') 'commd: number: '//TRIM(cword)
    END IF
    CALL psc_stack_append
    IF( iferror ) RETURN
    stack(stz) = rtmpa
    RETURN
  END IF
  !%}}}
  ! rest -> ERROR %{{{
  print*, 'DEBUG 211036:', id_comm
  CALL psc_error( cword, 1 )
  RETURN
  !%}}}
  !%}}}
END SUBROUTINE

! SUBROUTINE next_command() %{{{
SUBROUTINE next_command()
USE cv_psc
IMPLICIT NONE
id_comm = id_comm + 1
! 設定し忘れ対策で初期化しておく
maintag_comm   = ''
subtag_comm(:) = ''
help_comm = ''
helpja_comm = ''
usage_comm = ''
END SUBROUTINE
!%}}}

!----------------------------------------------------------------
!-- FUNCTIONS ---------------------------------------------------
!----------------------------------------------------------------
! INTEGER( KIND= 4 ) FUNCTION get_commandmode() %{{{
INTEGER( KIND= 4 ) FUNCTION get_commandmode() RESULT ( res )
! 0 : command not match
! 1 : normal execution
! 2 : display help
USE cv_psc
IMPLICIT NONE
INTEGER( KIND = 4 ) :: idoa
LOGICAL :: cmd_match
res = 0

cmd_match = .FALSE.
IF( cword == maintag_comm )THEN
  cmd_match = .TRUE.
ELSE
  DO idoa = 1, psc_maxtag
    IF( subtag_comm( idoa ) == '' )THEN
      EXIT
    ELSEIF( cword == subtag_comm( idoa ) )THEN
      cmd_match = .TRUE.
      EXIT
    END IF
  END DO
END IF
!
IF( .NOT. cmd_match )THEN
  res = 0
  RETURN
ELSE IF( exec_mode == 0 )THEN
  ! normal execution
  res = 1
  RETURN
ELSE IF( exec_mode == 1 )THEN
  ! help display mode
  res = 2
  RETURN
ELSE IF( exec_mode == 2 )THEN
  ! usage display mode
  res = 3
  RETURN
END IF
END FUNCTION
!%}}}
! CHARACTER (LEN=16) FUNCTION psc_getintstr(intin) %{{{
CHARACTER (LEN=16) FUNCTION psc_getintstr(intin) RESULT ( res )
IMPLICIT NONE
INTEGER, INTENT(IN) :: intin
CHARACTER (LEN=20) str
WRITE(str,*) intin
res = TRIM(str)
RETURN
END FUNCTION
!%}}}

!EOF
