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
seed2 = 28
seed3 = 1673

-- jogo exemplo
j1 = (Jogo (Jogador (1,1)) (Mapa 5 [(Relva, [Nenhum, Arvore, Nenhum, Arvore, Nenhum])
                                  ,(Rio 1, [Tronco, Tronco, Tronco, Nenhum, Nenhum])
                                  ,(Estrada 1, [Nenhum, Nenhum, Nenhum, Carro, Carro])]))

j2 =  (Jogo (Jogador (8,5)) (Mapa 16 [(Relva,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Nenhum]),
                                      (Rio (-1),[Tronco,Tronco,Tronco,Tronco,Nenhum,Nenhum,Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco,Nenhum,Nenhum,Nenhum,Nenhum]),
                                      (Rio 1,[Nenhum,Nenhum,Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco,Nenhum,Nenhum,Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco]),
                                      (Relva,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore]),
                                      (Estrada 1,[Nenhum,Carro,Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum]),
                                      (Relva,[Arvore,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Arvore]),
                                      (Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Arvore]),
                                      (Relva,[Arvore,Nenhum,Arvore,Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Arvore,Arvore,Arvore,Nenhum,Nenhum]),
                                      (Relva,[Arvore,Arvore,Arvore,Arvore,Nenhum,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Arvore,Arvore,Arvore])]))


extraiMapa :: Jogo -> Mapa
extraiMapa (Jogo _ m) = m

extraiLista :: Mapa -> [(Terreno,[Obstaculo])]
extraiLista (Mapa n l) = l

testList1 = "Testes função deslizaJogo" ~: test [test1,test2,test3,test4,test5,test6]

-- testar se o comprimento da lista do novo mapa é igual ao comprimento da lista do antigo mapa e se o mapa antigo e o mapa novo são válidos
test1 = "Teste 1" ~: True ~=? (length (extraiLista (extraiMapa j1)) == (length (extraiLista (extraiMapa (deslizaJogo seed1 j1))))) && ((mapaValido (extraiMapa j1)) && (mapaValido (extraiMapa (deslizaJogo seed1 j1))))
test2 = "Teste 2" ~: True ~=? (length (extraiLista (extraiMapa j1)) == (length (extraiLista (extraiMapa (deslizaJogo seed2 j1))))) && ((mapaValido (extraiMapa j1)) && (mapaValido (extraiMapa (deslizaJogo seed2 j1))))
test3 = "Teste 3" ~: True ~=? (length (extraiLista (extraiMapa j1)) == (length (extraiLista (extraiMapa (deslizaJogo seed3 j1))))) && ((mapaValido (extraiMapa j1)) && (mapaValido (extraiMapa (deslizaJogo seed3 j1))))
test4 = "Teste 4" ~: True ~=? (length (extraiLista (extraiMapa j2)) == (length (extraiLista (extraiMapa (deslizaJogo seed1 j2))))) && ((mapaValido (extraiMapa j2)) && (mapaValido (extraiMapa (deslizaJogo seed1 j2))))
test5 = "Teste 5" ~: True ~=? (length (extraiLista (extraiMapa j2)) == (length (extraiLista (extraiMapa (deslizaJogo seed2 j2))))) && ((mapaValido (extraiMapa j2)) && (mapaValido (extraiMapa (deslizaJogo seed2 j2))))
test6 = "Teste 6" ~: True ~=? (length (extraiLista (extraiMapa j2)) == (length (extraiLista (extraiMapa (deslizaJogo seed3 j2))))) && ((mapaValido (extraiMapa j2)) && (mapaValido (extraiMapa (deslizaJogo seed3 j2))))