module Tarefa4_2022li1g030_Spec where

import LI12223
import Tarefa4_2022li1g030
import Test.HUnit

testsT4 :: Test
testsT4 = TestLabel "Testes Tarefa 4" $ test ["Teste 1" ~: 1 ~=? 1]
