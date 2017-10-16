module Q1
where

import RunDFA

q1_DFA :: DFA
q1_DFA
  = ([1..6], "ab", t, 1, [1,4])
    where
      t = [ ((1, 'a'), 2)
          , ((2, 'a'), 3)
          , ((3, 'a'), 3)
          , ((3, 'b'), 4)
          , ((4, 'a'), 5)
          , ((4, 'b'), 6)
          , ((5, 'a'), 3)
          , ((5, 'b'), 4)
          , ((6, 'b'), 4)
          ]