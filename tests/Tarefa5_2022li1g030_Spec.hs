module Tarefa5_2022li1g030_Spec where

import LI12223
import Tarefa1_2022li1g030
import Tarefa2_2022li1g030
import Tarefa5_2022li1g030
import Test.HUnit

testsT5 :: Test
testsT5 = TestLabel "Testes Tarefa 5" $ test [testList1]

-- seed exemplo
seed1 = 5

-- jogo exemplo
j1 = (Jogo (Jogador (1,1)) (Mapa 5 [(Relva, [Nenhum, Arvore, Nenhum, Arvore, Nenhum])
                                  ,(Rio 1, [Tronco, Tronco, Tronco, Nenhum, Nenhum])
                                  ,(Estrada 1, [Nenhum, Nenhum, Nenhum, Carro, Carro])]))

extraiMapa :: Jogo -> Mapa
extraiMapa (Jogo _ m) = m

extraiLista :: Mapa -> [(Terreno,[Obstaculo])]
extraiLista (Mapa n l) = l

testList1 = "Testes função deslizaJogo" ~: test [test1]

test1 = "Teste 1: jogo desliza e é válido, seed1 = 5" ~: True ~=? (length (extraiLista (extraiMapa j1)) == (length (extraiLista (extraiMapa (deslizaJogo seed1 j1))))) && ((mapaValido (extraiMapa j1)) && (mapaValido (extraiMapa (deslizaJogo seed1 j1))))
