bulletML(none,[
  top:action([
    text('dirSeq 3way'),
    repeat(3,[
      wait(100),
      fire(dirAim(-1*10),spdAbs(1.0),bullet(none,none,[])),
      repeat(2,[
        fire(dirSeq(10),spdAbs(1),bullet(none,none,[]))
      ])
    ]),
    vanish
  ])
]).
