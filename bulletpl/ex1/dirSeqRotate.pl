bulletML(none,[
  top:action([
    text('dirSeq 回転弾'),
    wait(100),
    fire(dirAim(-1*10),spdAbs(1.0),bullet(none,none,[])),
    repeat(72,[
      wait(4),
      fire(dirSeq(10),spdAbs(1),bullet(none,none,[]))
    ]),
    wait(500),
    vanish
  ])
]).
