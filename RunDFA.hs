module RunDFA (
  DFA
, Decision (..)
, run_DFA
, trace_DFA
, use
, trace
) where

import Data.Char (isDigit, isLower)
import Data.List (sort, nub, (\\))

{------------------------------------------------------------------------
    A DFA simulator written in Haskell.

    The function 'step' is just a stub, for students to replace with a
    correct version.

    Harald Sondergaard
    The University of Melbourne
    September 2016
------------------------------------------------------------------------}

--  Usage:
--
--  `run_DFA d input' where d is a DFA, that is, a tuple
--
--        (states, alphabet, delta, start_state, accept_states)
--
--  Here delta is a list that encodes the DFA's transition function.
--  In this list, ((i,s),j) means: If, in state i you read symbol s,
--  go to state j.  States are assumed to be natural numbers.  The
--  alphabet must consist of digits and/or lower-case letters only.

--  The function `use' is useful for testing.  The action `use d'
--  will query for input to d, run d on the input, print Accept
--  or Reject, as appropriate, and query for new input.  This makes
--  it easy to try the same DFA on different input.  The function
--  will exit the read-eval loop when the user types `Quit'.

--  The function `trace' is similar to `use' and also useful for testing.
--  Rather than printing the DFA's decision, it will print the history
--  of state/input configurations in which the DFA has been.

--  We rely on module DFA to provide definitions of types as well as
--  tools for checking the well-formedness of DFAs. If the transition
--  function is partial, it will be assumed that it is to be `completed'
--  in the usual way, by introducing an input-consuming reject state.

-------------------------------------------------------------------------

type State  = Int
type Symbol = Char
type Input  = [Symbol]
type Trans  = ((State, Symbol), State)
type DFA    = ([State], [Symbol], [Trans], State, [State])

--  The result of running the DFA on some input is a Decision:

data Decision
  = Accept | Reject
    deriving (Eq, Show)

--  We introduce a special reject state, to be used if the given
--  transition function hasn't been completed.

reject_all :: State
reject_all
  = -1

--  The DFA steps through successive configurations.  A configuration
--  is a pair (state, remaining input). A computation history is a list
--  of configurations.

type Configuration = (State, Input)
type History = [Configuration]

--  Initially we need to explore the configuration (start_state, input).

run_DFA :: DFA -> Input -> Bool
run_DFA dfa input
  = if state `elem` accept_states then True else False
    where
      (_, _, _, start_state, accept_states) = dfa
      history = run dfa (start_state, input)
      (state,"") = head history

--  The function trace_DFA is like run_DFA but returns the DFA's
--  computation history rather than its decision.

trace_DFA :: DFA -> Input -> History
trace_DFA dfa input
  = reverse history
    where
      (_, _, _, start_state, _) = dfa
      history = run dfa (start_state, input)

--  The function `run' does the step-by-step simulation of its argument dfa.
--  It takes the current configuration (state, remaining_input), looks at
--  dfa's transition function to see if there is a transition for the symbol
--  that is next in the remaining input, and rejects if there is not.
--  Otherwise it  updates the configuration according to what the transition
--  function dictates.

run :: DFA -> Configuration -> History
run dfa config
  = until final (step delta) [config]
    where
      (_, _, delta, _, _) = dfa
      final ((_,s):_) = null s     -- Halt when remaining input is gone

--  The function `step' does a single step of the simulation.  It
--  takes as input the DFA to simulate, and the (reverse) computation
--  history. This means it has the current configuration at the head
--  of the history record.

step :: [Trans] -> History -> History
step delta history
  = configuration next_state : history
  where
    -- get the current state and next symbol
    -- from the last configuration
    (state, symbol:next_symbols) = head history

    -- lookup the next state in the transition
    -- function
    next_state = lookup (state, symbol) delta
    
    -- a helper function to convert from the
    -- result of lookup to a new configuration
    configuration :: Maybe State -> Configuration
    configuration Nothing           = (reject_all, "")
    configuration (Just next_state) = (next_state, next_symbols)

--  The function `use' takes a DFA d and enters a dialogue with the user,
--  repeatedly asking for input to d, and printing d's decision.

use :: DFA -> IO ()
use d
  = do
      putStr "Input: "
      input <- getLine
      if compliant input
        then do {putStrLn (show (run_DFA d input)); use d}
        else if (head input) == 'Q'
          then return ()
          else do
            putStrLn "Input must be digits and/or lower case letters"
            use d

--  The function `trace' is like `use', except it prints the
--  DFA's computation history for the given input.

trace :: DFA -> IO ()
trace d
  = do
      putStr "Input: "
      input <- getLine
      if compliant input
        then do {mapM putStrLn (map show (trace_DFA d input)); trace d}
        else if (head input) == 'Q'
          then return ()
          else do
            putStrLn "Input must be digits and/or lower case letters"

--  The function `compliant' checks that input consists entirely of
--  digits and/or lower case letters.

compliant :: Input -> Bool
compliant
  = all comply
    where
      comply c = isLower c || isDigit c
