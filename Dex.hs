module Dex
where

import RunDFA

dex :: DFA
dex
  = ([0..4], "abc", t, 0, [1,2,3])
    where
      t = [ ((0, 'a'), 1)
          , ((0, 'b'), 0)
          , ((0, 'c'), 0)
          , ((1, 'a'), 1)
          , ((1, 'b'), 2)
          , ((1, 'c'), 3)
          , ((2, 'a'), 0)
          , ((2, 'b'), 2)
          , ((2, 'c'), 3)
          , ((3, 'a'), 0)
          , ((3, 'b'), 0)
          , ((3, 'c'), 3)
          , ((4, 'a'), 0)
          , ((4, 'b'), 0)
          , ((4, 'c'), 0)
          ]