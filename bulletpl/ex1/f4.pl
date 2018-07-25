bulletML(none,[
  top:action([
    text('fire:dirRel 自機方向に撃った弾が途中で分裂'),
    repeat(6,[
      wait(100),
      fire(dirAim(0),spdAbs(0.5),bullet(none,none,[
        wait(150),
        fire(dirAim(10),spdRel(0.5),bullet(none,none,[])),
        fire(dirAim(-10),spdRel(0.5),bullet(none,none,[])),
        vanish
      ])),
      wait(200)
    ]),
    vanish
  ])
]).
