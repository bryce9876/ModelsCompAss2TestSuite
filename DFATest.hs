module DFATest
where

import DFA
import Data.List (nub, sort, group)

ok_DFA :: DFA -> Bool
ok_DFA (states, alphabet, delta, start_state, accept_states) =
    -- check the start state and accept states are valid states
    start_state `elem` states
    && all (`elem` states) accept_states
    
    -- check that the transition function's inputs and outputs
    -- are valid states too
    && all (`elem` valid_inputs) delta_inputs
    && all (`elem` states) delta_outputs
    
    -- and, checks that the function is deterministic (it should not
    -- map from any input more than once)
    && all (<=1) (map length (group (sort (delta_inputs))))
    
    where
        -- valid function inputs are state, symbol pairs
        valid_inputs = [(s, a) | s <- states, a <- alphabet]
        
        -- actual function inputs are the first ('fst') part of each
        -- tuple in delta; outputs are the second ('snd') part
        delta_inputs  = map fst delta
        delta_outputs = map snd delta
    