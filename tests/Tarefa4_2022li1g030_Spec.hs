module Tarefa4_2022li1g030_Spec where

import LI12223
import Tarefa4_2022li1g030
import Test.HUnit

testsT4 :: Test
testsT4 = TestLabel "Testes Tarefa 4" $ test [testList1]

-- mapa exemplo
m = (Mapa 5 [(Relva, [Nenhum, Arvore, Nenhum, Arvore, Nenhum])
            ,(Rio 1, [Tronco, Tronco, Tronco, Nenhum, Nenhum])
            ,(Estrada 1, [Nenhum, Nenhum, Nenhum, Carro, Carro])])

-- jogador fora do mapa à esquerda
j1 = Jogo (Jogador (-3,2)) m

-- jogador fora do mapa à direita
j2 = Jogo (Jogador (5,2)) m

-- jogador fora do mapa por baixo
j3 = Jogo (Jogador (3,3)) m

-- jogador fora do mapa por cima
j4 = Jogo (Jogador (3,-1)) m

-- jogador na água, terminou
j5 = Jogo (Jogador (3,1)) m

-- jogador debaixo de carro, terminou
j6 = Jogo (Jogador (3,2)) m

-- jogo não terminou
j7 = Jogo (Jogador (2,1)) m


testList1 = "Testes função jogoTerminou" ~: test [test1, test2, test3, test4, test5, test6, test7]

test1 = "Teste 1: jogador fora do mapa à esquerda" ~: True ~=? jogoTerminou j1
test2 = "Teste 2: jogador fora do mapa à direita" ~: True ~=? jogoTerminou j2
test3 = "Teste 3: jogador fora do mapa por baixo" ~: True ~=? jogoTerminou j3
test4 = "Teste 4: jogador fora do mapa por cima" ~: True ~=? jogoTerminou j4
test5 = "Teste 5: jogador na água" ~: True ~=? jogoTerminou j5
test6 = "Teste 6: jogador debaixo de carro" ~: True ~=? jogoTerminou j6
test7 = "Teste 7: jogo não terminou" ~: False ~=? jogoTerminou j7