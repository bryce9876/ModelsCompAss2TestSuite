--  Author   : Bryce Matheson
--  Purpose  : test cases for Ass 2

import RunDFA
import Dex
import Q1
import HaskellTest

suite = 
  TimeLimit 2.0 $
  Suite [
    -- These are to test the testing suite itself
    --Test 1:
    expect (run_DFA dex "abcc") True,
    --Test 2:
    expect (run_DFA dex "bc") False,


    -- Tests the DFA for Q1
    --Test 3:
    expect (run_DFA q1_DFA "") True,
    --Test 4:
    expect (run_DFA q1_DFA "aab") True,
    --Test 5:
    expect (run_DFA q1_DFA "aabab") True,
    --Test 6:
    expect (run_DFA q1_DFA "aab") True,
    --Test 7:
    expect (run_DFA q1_DFA "aaab") True,
    --Test 8:
    expect (run_DFA q1_DFA "aaaaaaaaaaaaaab") True,
    --Test 9:
    expect (run_DFA q1_DFA "a") False,
    --Test 10:
    expect (run_DFA q1_DFA "b") False,
    --Test 11:
    expect (run_DFA q1_DFA "aa") False,
    --Test 12:
    expect (run_DFA q1_DFA "ab") False,
    --Test 13:
    expect (run_DFA q1_DFA "aaaaaaa") False,
    --Test 14:
    expect (run_DFA q1_DFA "aabb") False
    ]

main :: IO ()
main = do
  testVerbose suite