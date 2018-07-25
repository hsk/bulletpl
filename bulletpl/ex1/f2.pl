bulletML(none,[
  top:action([
    text('NWay actionRef fireRef'),
    actionRef(a2,[]),
    vanish
  ]),
  a2:action([
    repeat(12,[
      wait(100),
      actionRef(a3,[$rank,30]),
      rankUp,
      text(rank:($rank))
    ])
  ]),
  a3:action([
    wait(3),
    fire(dirAim(- $2/2* ($1-1)),spdAbs(1),bullet(none,none,[])),
    repeat($1-1,[
      fireRef(f1,[30])
    ])
  ]),
  f1:fire(dirSeq($1),spdAbs(1),bullet(none,none,[]))
]).
