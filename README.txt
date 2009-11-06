
BulletSML / 弾幕記述言語

* 概要

BulletML の S式版

BulletML:
http://www.asahi-net.or.jp/~cs8k-cyu/bulletml/


動かすには Gauche の SDL バインドが必要（未アップ）


* 必要なもの

data/*.bmp
	画像データ
bullet/*.scm
	弾幕データ

* キー操作

- カーソル：自機移動
- F5：リロード
- q, w：ランク調整



* ToDo

- github にあげる
- デモで使うのは見栄えのいい弾幕に厳選する


-[v] エミッターとバレットがわかれてるのをいっしょくたにする？
-- 現在、エミッタは描画してない
--[v] エミッタの関数自体削除する？
-[v] rank の処理
- emitter, bullet でキャラを変えられるようにする
-[v] 弾と自機の当たり判定を取る
-[v] rank をいじれるようにする

- $rand, $rank などを使えるようにする
-[v] 弾からも弾を撃てるので、emitter と bullet の構造を同じにするか、
仮想関数的なものを用意して振り分けれるようにする必要がある
-- この辺の扱いがどうなってるのか、BulletMLだか白い弾幕くんを見ること

-[v] <direction> タグで打ち出し角度をセットしに言ってるところでemitterの角度を
呼び出しているところが、弾の中で<changeDirection>したときにも呼び出され
角度が書き換わってしまっている

- Release 版で動かない
-- STRINGPがFALSEを返す


* その他
- 白い弾幕くんでは lua を組み込んで拡張できるようになっている
-- Lisp で DSL だから、その辺も普通にできるよねって話
- Bulletsmorph
-- http://www.asahi-net.or.jp/~cs8k-cyu/bulletml/bulletsmorph/index.html



* Reload 対策：
- (require) じゃなくて (load) を使う
- (define *hoge* some-val) としているとリロード時にクリアされてしまうので、
それで都合が悪い場合は
(unless (global-variable-bound? (current-module) '*hoge*)
  (define hoge some-val))
などとする。
マクロを作ってしまうのも手：
(define-macro (define-once name . body)
  `(unless (global-variable-bound? (current-module) ',name)
     (define ,name ,@body)))





弾幕やさん
http://shinh.skr.jp/sdmkun/zipup.cgi
BulletMLだけじゃなくて、Luaのほうが多め？
Luaの場合、直進じゃない弾も書けるのかな？


http://pc5.2ch.net/tech/kako/1034/10341/1034182349.html

479 名前：468 ：03/05/06 22:40
白い弾幕君で真面目にサポートしてる<W 
このスレもしかして見られてるのかな？

480 名前：デフォルトの名無しさん ：03/05/07 15:57
>>479 
見てます。別の弾幕記述言語として、 
Lua使ってみたいなーと思って検索したらこのスレ見つけました。 
ゲームの組み込みには本当にいいですね。

501 名前：デフォルトの名無しさん ：03/06/24 21:17
俺もLuaで弾幕(というか敵キャラモーション)記述言語に挑戦中。 
coroutineつかって1キャラ1スレッドとかやってみた。 
BulletMLみたいに逐次っぽく書けるやつ。それっぽく動いてる。 

LuaXMLとかやったひといる？使い勝手とかどうなんでしょ。 
http://lua-users.org/wiki/LuaXml


キレイ
http://www.voidelement.com/bullet/
