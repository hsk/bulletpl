## BulletPL

BulletPL is Prolog version BulletML Library.

## Installation

This package requires Swi-Prolog 7.x.

    ?- pack_install(rtg).


### TODO

- ○ 絶対角度指定 まとめて、上、右、下、左にとぶこと
    - 上に角度0で飛ぶ
    - 右に角度90で飛ぶ
    - 下に角度180で飛ぶ
    - 左に角度270で飛ぶ

- ○ 自機指定 まとめて、上、右、下、左にとぶこと
    - 上に角度0で飛ぶ
    - 右に角度90で飛ぶ
    - 下に角度180で飛ぶ
    - 左に角度270で飛ぶ

- fireとbulletの両方にdirパラメータ指定した場合の例を作って見る
    - dirAim(-90),dirAim(0) bulletが優先される
    - dirAim(-90),dirSeq(0) -90が生きる
    - dirAim(-90),dirAbs(0)
    - dirAim(-30),dirRel(30)

### Links

  - [Official site](http://www.asahi-net.or.jp/~cs8k-cyu/bulletml/index_e.html)
  - [BulletML Ref](http://www.asahi-net.or.jp/~cs8k-cyu/bulletml/bulletml_ref_e.html)
