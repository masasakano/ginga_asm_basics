% ginga_asm_basics 解説

背景と用語定義
=======

目的は、GingaのTelemetry FITSを読み込み、対応する Ginga LAC FRFにある姿勢関係情報を付け加えて、ASMデータをFITS tableとして出力すること。

* `asmmkevt` : 本プログラム。ASM-mode がONの時のデータのみ抽出して、ASM event FITSを作成。
* `asm2qdp` : 副プログラム。ASM event FITSから、QDP/PCO ファイルを作成。
* `asmtelemetryout` : 副プログラム。Telemetry FITSにある全データ時間領域について、ただし、指定したパラメーターのテーブルのみ出力(パラメーターを全く与えなければ、`asmmkevt` と同様に全パラメーターが出力される)。

いずれも `-h` オプションだけ与えると、ヘルプメッセージが表示される。

## GingaのTelemetryデータ構造 ##

1. 一つのTelemetry FITSあるいは、FRF FITSは、複数の SF を含む。SFは、歴史的理由で(日本語)「サブフレーム」と呼称されるが、事実上の意味は super-frame。1SFがGingaのデータの主要単位。
   * 各SFには、各FRFにおいて、通し番号 SFn がつけられる(対応するTelemetryにある最初のSFを1として数えている様子だ)。SFn は、各FRFに固有であり、異なるFRF間では同じSFnが存在する。
2. 1 SF は、データが完全である場合は、64 frames からなる。ただし、当時の衛星と基地局との通信事情などにより、データに欠落があって、64個ない場合もあり得る。
   * 各SFにおいて、i-1番目のFrame番号を Fi と呼称する。iは、0..63。
3. 1 frame は、128 bytes からなり、この各byteを「ワード」と呼称する。各frameにおいて、j-1番目のWord番号を Wj と呼称する。jは、0..127。
3. 1ワードは、定義上、8 bits からなる。
   * 最上位bit (MSB)から、最下位bit (LSB)までをそれぞれ Bk (k=0..7) と呼ぶ。すなわち、B0..B7
   * すなわち、ワード値が1の時、そのB0は0、B7は1である。この数え方は、Fortran 90 の`BTEST()`などとは、逆であることに注意。

そして、各SFにおいて、ある特定の Fi のある特定の Wi (そして時には特定の Bk)にのみある特定の意味を持つデータが存在することがある(例: ASMのスイッチが入っているかどうかのフラグ)。

GingaのTelemetry FITSでは、各frameにそれぞれ16-byte frame-headerを先頭に足したものをrow (行)として、各rowに 16+128=144バイトの並びを保持する。

### Frame-header (16 bytes) ###

各Frame-headerは、以下の形式になっている。

* Byte00=month
* Byte01=day
* Byte02=hour
* Byte03=minute
* Byte04=second
* Byte05+06=millisecond
  *  `millisec = Byte05 * 256 + Byte06` (`readfits_SF_WD.c` による)
* Byte07=Counter_A1  (TI counter)
* Byte08=Counter_A2  (SF+Frame番号情報)
* Byte09=Counter_B1  (TI counter)
* Byte10=Counter_B2  (TI counter)
  * TIの値はおそらく `Byte07*65536 + Byte09*256 + Byte10`
* Byte11=real(1) or stored(2)
* Byte12=bit-rate-low(0) or high(1)
* Byte13, 14, 15: not used

ここで、Bytes00..06の時刻は、

> リアルタイムデータの場合、各フレーム（128bytes）の先頭部が地上に到達した時刻

stored の場合は、

> プレイバックデータにつきましては、リアルデータの時刻とＴＩカウンタからプレイバックデータＴＩカウンタを元に時刻を計算したもの
 
になる(大興電子による)。

すなわち、大雑把に言って、(通信の遅れなどを無視すれば)フレーム開始時刻ということになる。

なお、その「年」については、Telemetry FITSのFITS Headerに観測日(DATE-OBS)の記載があるので、それを信用する(Telemetry filesのファイル名に現れる数字の最初の4桁の、FRFのファイル名の最初の2桁、も観測年と一致するはずである---年末年始を跨ぐ観測についてどうなるかは知らないが)。なお、念のため、本パッケージのプログラムでは、年末年始を跨いだ観測(Gingaの寿命中、数回あると思われる)についても考慮し、正しく年を推定している。

### Frameのメインデータ (128 bytes (=ワード)) ###

一方、各frameの標準形式は、ASTRO-C(Ginga)中間報告書 Sec.5.5 pp.198 を参照。

Frame自体は、あくまで(144 bytesではなく) 128 bytes(=ワード)として定義されていることに注意。

ASM-modeの時のメインデータの構造は、以下に定義されている。

* PHA Mode : Table 5.5.5, pp.233
* Time Mode: Table 5.5.6, pp.234

-----------

仕様補足
====

* Telemetry dataでASM-ModeがONの時、FRFの該当framesは欠落している。したがって、ASM-Mode中の較正済の正確な姿勢情報を得ることができない。
* 次善の策として、ASM-Modeに入る直前と入った直後のできるだけ近い時刻のEuler角などの姿勢情報をFITS Headerに記載する。
* もし該当するデータが存在しない場合(例: ASM-Modeのまま、Telemetry FITSが終了した場合のASM-Mode直後の姿勢)、そのデータには、-360よりも小さい値を入れる(具体的には -1024.0)。
* なお、一般的に本パッケージでは、浮動小数点データの初期値として -1024.0, 整数データの初期値として -999 をセットしている。そのデータを評価しようとした結果として評価不能の場合は、整数データには、原則として -1 をセットする。たとえば、FRFが無い時は、`sunps`の値は -1 である。本パッケージは、すべてのデータを評価しようと試みるはずなので、出力ファイルのヘッダーには、値 -999 は存在しないはずである。

-----------

Coding style
======

必ずしも一貫していないが、以下のスタイルを採用。

* 原則として、Fortran 77 の書き方ではなく、Fortran 90 の書き方を採用。
* 事実上のグローバル定数(端的には`asm_fits_common.f90` などで parameter として定義されているもの)は、全て大文字。
* Subroutine/Function内の定数(parameter)は、最初だけ大文字。
* 変数やFortran90のコマンドや関数は、原則としてすべて小文字(snake case)。
  * ただし、Fortran77の関数(端的には、FITSIOの関数)は、すべて大文字のものが多い。
* すべての配列の添字は、1から始まる。

Parameters
======

基本的なパラメーターは、`asm_fits_common.f90` にある。

* parameter :: `OUTFTCOMMENT1`: すべてのFITSの第1 extensionの末尾につけられるコメント
* `type t_telem_word_from0`: どのキーワード(例: `dp`)がどのワードに対応するかを定義(ワード Wi につき、i=0..127)
  * parameter :: `TELEM_WORD_FROM0`: 事実上は、上のtypeを代表するこの定数を使っている。
* `type t_telem_loc`: どの情報がどのフレームのどのワード(のどのBit)にあるかを定義。
  * parameter :: `TELEM_LOC`: 事実上は、上のtypeを代表するこの定数を使っている。
* `type t_form_unit`: 出力FITSファイルのテーブル関係キーワードのうちで TFORM, TUNIT (のテンプレート)を定義する。
  * parameter :: `COL_FORM_UNITS`: 事実上は、上のtypeを代表するこの定数を使っている。
* `type t_asm_colhead`: 出力FITSファイルのテーブル関係キーワードのうちで TTYPE を定義する。
  * ほとんどのTTYPEについては、対応する`COL_FORM_UNITS`のメンバー"root"(および"key")と一致する。ただし、たとえば"Euler"であれば、`COL_FORM_UNITS%root`は"Euler"であるのに対し、この変数のメンバー"type"は、"Euler1", "Euler2"など、具体的に指すもの(そしてFITS fileに出力するもの)によって変わる。

したがって、将来的にたとえば〇〇modeのBIT番号を変更したいなどの場合、上のtypesを変更するとよい。

他の主要type-s (特に読み込んだり計算したデータを保持するもの)
========

* `type asm_telem_row`: Telemetry FITSを読んで、各row (つまり16-byte frame-header と 128-byte frameデータ)を解釈して、(typeのメンバーとしての)名前をつけてここに格納。
  * Telemetry FITS fileには一個あたり万の単位でframesが存在するので、それらをこのtypeの配列として保持する。変数名は `trows` を使っていることが多い。
* `type asm_frfrow`: FRFを読んで、各SFを解釈して、(typeのメンバーとしての)名前をつけてここに格納。
  * FRF FITS fileには一個あたり万の単位でSFsが存在するので、それらをこのtypeの配列として保持する。変数名は `frfrows` を使っていることが多い(`sfrows`の場合もあるかも?)。
  * FRFを読むには、`ginga_tool` にあるsubroutines `SFCHCK()` と `GETOAT()` を使用。
* `type asm_sfrow`: Telemetry FITSから読み込んだ各「SF」について、FRF FITSとマッチングし、両者の関係を保持する "relational" type。
  * このtypeの配列の変数名は `relrows` を使っていることが多い(`sfrows`の場合もあるかも? ---上とconfusingなのだが!)。
  * 各SFには64 framesあると期待されるため、それが成立するならば、 `size(relrows) == size(trows)/64` 
  * なお、Telemetry FITSにおいて、Framesが欠けている場合も考慮して SFを完璧に推定するのは(可能だが)難しく、本プログラムでは、完璧は期していない。ただし、1SF内にFramesが欠けている場合は、そのSFのFramesは出力ファイルには全く含めない仕様なので、問題になる可能性は極めて低い(FRFとのマッチングの問題があることが判明したため、当初の目論見のように理論的に可能性がゼロにまではならない)。
    * ただし、そもそも Telemetry FITSにおいて、Framesが欠けている場合はもうすでにフィルタリングされているかも知れない。
* `type type fits_header`: FITS header を表す
  * (読み込んだTelemetry/FRF FITSに存在せず)新規に出力するパラメーターに関しては、デフォルトとのコメントもここで定義する。

これらの変数の内容を表示するための subroutine `dump_type()` が用意されている(`INTERFACE` を使ってtype非依存にしている)。

アルゴリズム
======

1. 主プログラム(`asmmkevt.f90`, `asmtelemetryout.f90`, `asm2qdp.f90`)
   * コマンドライン引数の処理、出力FITSファイルのopen/closeが主な仕事
   * その間に、(`asm2qdp.f90`を除き)大雑把に言って以下の二つのmodulesで定義された関数やサブルーチンを呼ぶ。
2. Module `asm_read_telemetry.f90`: Telemetry FITS と FRF FITSの読み込み
   * 読み込んだデータをそれぞれ `trows` (type `asm_telem_row`) と `frfrows` (type `asm_frfrow`)にセットして返す。
     * それぞれのFITS headersも返す。
     * subroutines `mk_telem_rows()`, `mk_frf_rows()`
3. Module `asm_fitsout.f90`: Telemetry FITS と FRF FITSのマッチングから出力まで
   * `trows` と `frfrows` から `relrows` (type `type asm_sfrow`)を作って返す(第1段階)
     * function `get_asm_sfrow()`, subroutine `update_asm_sfrow_modes()`
   * また、付随するFITS headersのデータから、出力headerを作成して返す(第2段階)
     * function `get_asm_fits_header()`
   * それらと与えられた変数`type(t_asm_colhead)`に応じて、FITSのheadersとテーブルとを出力する(第3段階)
     * subroutine `write_asm_evt_fits()`
     * `asmmkevt.f90` の場合は、単純に `COL_FORM_UNITS`にあるものを(ほぼ?)すべて出力するように、`type(t_asm_colhead)`を作ってこのmoduleに与える。
     * `asmtelemetryout.f90` の場合は、与えられたコマンドライン引数に応じて出力するコラムを選んで`type(t_asm_colhead)`を作ってこのmoduleに与える。
4. Module `asm_fits_common.f90`: 上の二つのmodulesの事実上の親となるmodule。基本的なルーチンや共通ルーチンはこちらに定義されている。
   * 最も基本的な定数は、``asm_consts.f90` にていぎされている。
   * その中で、`get_index()` (とそのラッパーである`get_element()`)は、変数を抽象化して、Ruby/PerlのHash (PythonのDictionary)のように扱うためのinterface。実は、Fortran 2008 (gfortranではすでに対応済み)では、組込関数 `FINDLOC()`として似たような機能が実装されていたことをあとになって知った。
5. Module `asm_read_evt.f90`: `asm2qdp.f90` の核となるModule。
   * ASM event file の読み込みと、QDP/PCO filesの書き出し。
6. その他のユーティリティModules
   * `asm_aux.f90`: Auxiliary functions/subroutines specific in this package (NOT Fortran-generic utility).
   * `err_exit.f90`: Error handling routines.
   * `fort_util.f90`: General Fortran90 utility routines.

その他
===

## Frame番号とSF番号 ##

Telemetry FITSの各Frameでは、frame-header (16-bytes) の Byte08 (`Counter_A2`)および frame-main-data (128-bytes)のW3(`FI` (Frame-Information?) と呼ばれる)は、SF番号とFrame番号との情報を含む(両者は完全に一致するはず)。

FormatはASTRO-C(Ginga)中間報告書 Table 5.1.3, pp.201。端的には上位2 bitsがSF番号(わずか0〜3だけで回転)、下位6 bitsがFrame番号である。

Frame番号は、各SFにつき、0〜63まで順番に回っていく。SF番号は、0〜3で回っていく……が、たまに不連続になったりあるいは同じSF番号が異なるはずのSFで連続するケースが認められた(bitrate が変化した時らしい)。

FRF においてつけられる、SF番号は、Telemetry FITSの(Frame番号から推測した)SFの通し番号と一致するように見える。たとえば、FRFの方で途中の(ある時間帯の)データに欠落があっても、その時間帯の後のデータでは、SFの通し番号がTelemetry FITSから予想される番号と一致する。がただし、FRFの方がSFsの数がずっと多いケースもあった。

Telemetryにおいては、framesは、基本的に欠落なく、各SFごとに64 framesあるはずながら、保証はされていない。特に(storedではなく)リアルデータの場合、欠落があり得る。もっともありがちな欠落のタイミングは、データの初め、終わり、および中間の(地上の受信)アンテナ反転の時。

## FRFとのマッチング ##

* (LACの)FRFでは、ASM-mode のデータ(もしくはLACモードではないデータ)のSFは欠落しているようだ。
* FRFから `GETOAT()` で得たMJDは、第32Frame (=F31)の先頭の時刻と1μ秒程度の精度で一致する(1μ秒程度はずれることがあり、ずれは一定ではない)。
  * すなわち、SFの開始時刻ではなく、そのSFの真ん中の時刻。
  * Frame同士の時間間隔は Bitrate-H でも 62.5 ms (ASTRO-C(Ginga)中間報告書 Table 4.1.2, pp.187)なので、1μ秒程度のずれはおそらく問題にならない。

---------

