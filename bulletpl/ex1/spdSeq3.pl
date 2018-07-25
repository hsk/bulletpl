bulletML(none,[
  top:action([
    text('fire:spdSeq 自機方向にスピードかえて３発'),
    repeat(3,[
      wait(100),
      fire(dirAim(0),spdAbs(1.0),bullet(none,none,[])),
      repeat(2,[
        fire(dirAim(0),spdSeq(0.1),bullet(none,none,[]))
      ]),
      wait(200)
    ]),
    vanish
  ])
]).
