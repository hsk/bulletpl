bulletML(none,[
  top:action([
    text('dirSeq 8方向にとぶはず'),
    repeat(3,[
      wait(400),
      fire(dirAim(0),spdAbs(1.0),bullet(none,none,[])),
      repeat(7,[
        fire(dirSeq(45),spdAbs(1),bullet(none,none,[]))
      ])
    ]),
    vanish
  ])
]).