bulletML(none,[
  top:action([
    text('ランダム分裂弾'),
    repeat(6,[
        wait(100),
        repeat(($rank * 0.5) + 5,[
          wait(3),
          fire(dirAim($rand*100-50),spdAbs(1),bullet(dirAim(0),spdAbs(1),[]))
        ]),
        rankUp,
        text(rank:($rank))
    ]),
    vanish
  ])
]).
