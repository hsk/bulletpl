bulletML(none,[
  top:action([
    text('dirAim4 自機方向にあわせて４方向'),
    repeat(3,[
      wait(20),
      fire(dirAim(0),spdAbs(1.0),bullet(dirAbs(0),spdAbs(1.0),[])),
      wait(20),
      fire(dirAim(90),spdAbs(1.0),bullet(dirAbs(0),spdAbs(1.0),[])),
      wait(20),
      fire(dirAim(180),spdAbs(1.0),bullet(dirAbs(0),spdAbs(1.0),[])),
      wait(20),
      fire(dirAim(270),spdAbs(1.0),bullet(dirAbs(0),spdAbs(1.0),[])),
      wait(20)
    ]),
    vanish
  ])
]).
