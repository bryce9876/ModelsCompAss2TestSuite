--  Author   : Bryce Matheson
--  Purpose  : test cases for Ass 2

import RunDFA
import DFATest
import Dex
import Qs
import HaskellTest

suite = 
  TimeLimit 2.0 $
  Suite [
    -- These are to test the testing suite itself
    expect (ok_DFA dex) True,
    expect (run_DFA dex "abcc") True,
    expect (run_DFA dex "bc") False,


    -- Tests the DFA for Q1
    expect (ok_DFA q1_DFA) True,
    expect (run_DFA q1_DFA "") True,
    expect (run_DFA q1_DFA "aab") True,
    expect (run_DFA q1_DFA "aabab") True,
    expect (run_DFA q1_DFA "aab") True,
    expect (run_DFA q1_DFA "aaab") True,
    expect (run_DFA q1_DFA "aaaaaaaaaaaaaab") True,
    expect (run_DFA q1_DFA "a") False,
    expect (run_DFA q1_DFA "b") False,
    expect (run_DFA q1_DFA "aa") False,
    expect (run_DFA q1_DFA "ab") False,
    expect (run_DFA q1_DFA "aaaaaaa") False,
    expect (run_DFA q1_DFA "aabb") False,



    -- Tests the DFA for Q2
    expect (ok_DFA q2_DFA) True,
    expect (run_DFA q2_DFA "") True,
    expect (run_DFA q2_DFA "aa") True,
    expect (run_DFA q2_DFA "ca") True,
    expect (run_DFA q2_DFA "cba") True,
    expect (run_DFA q2_DFA "bca") True,
    expect (run_DFA q2_DFA "aabaacaab") False,
    expect (run_DFA q2_DFA "cac") False,
    expect (run_DFA q2_DFA "ccb") False,
    expect (run_DFA q2_DFA "ccc") False,
    expect (run_DFA q2_DFA "caa") True,
    expect (run_DFA q2_DFA "caac") True,
    expect (run_DFA q2_DFA "caacbbcba") True,
    expect (run_DFA q2_DFA "caacbb") True,
    expect (run_DFA q2_DFA "caacbbc") True,
    expect (run_DFA q2_DFA "aab") False,
    expect (run_DFA q2_DFA "bba") False,
    expect (run_DFA q2_DFA "bbac") False,
    expect (run_DFA q2_DFA "a") True,
    expect (run_DFA q2_DFA "cc") True,
    expect (run_DFA q2_DFA "bbc") True,
    expect (run_DFA q2_DFA "cabcbbca") True,
    expect (run_DFA q2_DFA "acaacbacbb") True,
    expect (run_DFA q2_DFA "caaa") False,
    expect (run_DFA q2_DFA "bbcaacbcaac") False
    ]

main :: IO ()
main = do
  testVerbose suite