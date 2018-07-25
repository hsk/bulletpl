bulletML(none,[
  top:action([
    text('fire:spdRel 自機方向にスピードかえて３発'),
    repeat(3,[
      wait(100),
      fire(dirAim(0),spdAbs(1.0),bullet(none,none,[
        fire(dirAim(0),spdRel(0.1),bullet(none,none,[])),
        fire(dirAim(0),spdRel(0.2),bullet(none,none,[]))
      ])),
      wait(200)
    ]),
    vanish
  ])
]).

