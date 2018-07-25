bulletML(none,[
  top:action([
    text('dirAbs 上、右、下、左に飛ぶ'),
    repeat(3,[
      wait(30),
      fire(dirAbs(0),spdAbs(1.0),bullet(dirAbs(0),spdAbs(1.0),[])),
      wait(30),
      fire(dirAbs(90),spdAbs(1.0),bullet(dirAbs(0),spdAbs(1.0),[])),
      wait(30),
      fire(dirAbs(180),spdAbs(1.0),bullet(dirAbs(0),spdAbs(1.0),[])),
      wait(30),
      fire(dirAbs(270),spdAbs(1.0),bullet(dirAbs(0),spdAbs(1.0),[])),
      wait(30)
    ]),
    vanish
  ])
]).
