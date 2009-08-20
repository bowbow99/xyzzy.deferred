「あとでこれやっといて」ってするもの

以下殴り書き。意味不明なほど変更される可能性あり。

インストール
=====================
    (require "deferred")
    (use-package "deferred/core")


関数やらマクロやら
====================

deferred: args &body body
----------------------------
lambda 式みたいに書いて deferred を作る。


add-callbacks: deferred &rest callbacks
-------------------------------------------
deferred から連なる deferred chain の最後に callbacks をくっつける。


reproduce deferred
----------------------
deferred から連なる deferred chain の結果を再現する。結果だけで、副作用は再現されない。 

例
====
    * (setq d1 (deferred () (values 1 2 3))
            d2 (deferred (&rest vals) (values-list (mapcar #'1+ vals)))
            d3 (deferred (&rest vals) (msgbox "~{~S~^, ~}" vals)))
    => #S(deferred/core::*deferred ...)
    
    ;;; d1->d2->d3 と繋げる
    * (add-callbacks d1 d2 d3)
    => #S(deferred/core::*deferred ...)

deferred 式は引数無しの deferred を作った場合即座に実行するので、d1 は最初の setq した直後に実行され、result に戻り値のリスト `(1 2 3)` が保存される。この時点で d1 に callback があればそれも実行されるのだが、まだ無いので何も起きない。
add-callbacks した直後に d1 の result を引数として d2 が実行され、続いてその結果を引数として d3 が実行される。
    d1 => 1, 2, 3
    d2(1, 2, 3) => 2, 3, 4
    d3(2, 3, 4)
となり、最終的には msgbox で "1, 2, 3" と表示される。




動作
=======
deferred object は内部的には deferred::*deferred という structure で、以下の slots を持つ。
- description: 説明文字列
- body: 自身の処理（関数）
- result: body の結果を保存する
- callback: 次の処理（deferred）
- condition: どうなったか
  - condition object: 投げられた condition
  - :returned: ふつーに値を返して終了した

deferred (body) の実行は deferred::invoke* によって行われる。そのなかで deferred の body を funcall する関数を start-timer に仕掛けることで擬似的に非同期で実行される。
body を実行後、その結果を引数として callback にセットされた次の deferred を実行する。

deferred は処理を実行して、結果は自身の result に保存しつつ次の callback に渡すだけなので、その結果を使ってどうこうするには
- どうこうする callback を追加する
- どこかから reproduce して結果を取得する
のいずれかをする必要がある。

