「あとでこれやっといて」とかいう

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

deferred: function
---------------------
その function を呼び出す deferred を作る。


fire: deferred &rest args
----------------------------
その deferred を実行する。funcall 互換。

fire*: deferred &optional args
---------------------------------
その deferred を実行する。apply 互換。


add-callbacks: deferred &rest callbacks
-------------------------------------------
deferred から連なる deferred chain の最後に callbacks をくっつける。


reproduce deferred
----------------------
deferred から連なる deferred chain の結果を再現する。結果だけで、副作用は再現されない。 



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



例
====
    * (setq d1 (deferred () (values 1 2 3))
            d2 (deferred (&rest vals) (mapcar #'1+ vals))
            d3 (deferred (partial #'msgbox "~{~S~^, ~}")))
    => #S(deferred/core::*deferred ...)

d1 は多値で 1, 2, 3 を返す。
d2 は受け取った number にそれぞれ 1+ してリストにまとめる。
d3 は受け取ったリストの各要素を msgbox で表示する。

    ;;; d1->d2->d3 と繋げる
    * (add-callbacks d1 d2 d3)
    => #S(deferred/core::*deferred ...)

この時点では各 deferred は作成されただけでまだ実行されていない。

    * (fire d1)

d1 が実行されると、その結果である [1, 2, 3] をもって d2 が呼び出され、さらにその結果である (2, 3, 4) を引数に d3 が呼び出され、msgbox で "2, 3, 4" と表示される。
