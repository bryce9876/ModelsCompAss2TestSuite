module DFA
where

-- DFA type definitions

type State  = Int
type Symbol = Char
type Input  = [Symbol]
type Trans  = ((State, Symbol), State)
type DFA    = ([State], [Symbol], [Trans], State, [State])