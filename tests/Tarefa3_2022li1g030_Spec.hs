module Tarefa3_2022li1g030_Spec where

import LI12223
import Tarefa3_2022li1g030
import Test.HUnit

-- Exemplos de Mapas
m1 = (Mapa 4 [(Relva,[Nenhum,Nenhum,Arvore,Nenhum]),(Rio (-1),[Tronco,Nenhum,Tronco,Tronco]),(Rio 1, [Nenhum,Tronco,Tronco,Nenhum]),(Relva,[Nenhum,Arvore,Nenhum,Nenhum])])
m2 = (Mapa 4 [(Relva,[Nenhum,Nenhum,Arvore,Nenhum]),(Rio (-1),[Nenhum,Tronco,Tronco,Tronco]),(Rio 1, [Nenhum,Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Arvore,Nenhum,Nenhum])])

m3 = (Mapa 3 [(Relva,[Arvore,Nenhum,Arvore]),(Relva,[Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Arvore])])

m4 = (Mapa 3 [(Relva,[Arvore,Nenhum,Arvore]),(Rio (-1),[Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum])])
m5 = (Mapa 3 [(Relva,[Arvore,Nenhum,Arvore]),(Rio (-1),[Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum])])

m6 = (Mapa 4 [(Rio 1, [Tronco,Nenhum,Tronco,Nenhum]),(Rio (-1), [Tronco,Nenhum,Tronco,Nenhum])])
m7 = (Mapa 4 [(Rio 1, [Nenhum,Tronco,Nenhum,Tronco]),(Rio (-1), [Nenhum,Tronco,Nenhum,Tronco])])

m8 = (Mapa 3 [(Estrada 1, [Carro,Nenhum,Carro])])
m9 = (Mapa 3 [(Estrada 1, [Carro,Carro,Nenhum])])

-- Exemplos de Jogos
j1 = (Jogo (Jogador (0,0)) m1)
j2 = (Jogo (Jogador (3,3)) m1)

j3 = (Jogo (Jogador (1,0)) m1)
j4 = (Jogo (Jogador (2,3)) m1)

j5 = (Jogo (Jogador (1,2)) m1)
j6 = (Jogo (Jogador (2,1)) m1)

j7 = (Jogo (Jogador (2,2)) m1)

j8 = (Jogo (Jogador (1,1)) m3)

j9 = (Jogo (Jogador (2,1)) m4)
j10 = (Jogo (Jogador (0,1)) m4)
j11 = (Jogo (Jogador (2,1)) m6)
j12 = (Jogo (Jogador (2,0)) m6)

j13 = (Jogo (Jogador (1,0)) m8)

-- Exemplos de Jogadas
jo1 = (Parado)
jo2 = (Move Cima)
jo3 = (Move Baixo)
jo4 = (Move Esquerda)
jo5 = (Move Direita)

-- Testes de animaJogo

-- testar animaJogo quando o jogador se encontra nos limites do mapa
teste1 = "Teste 1" ~: Jogo (Jogador (0,0)) m2 ~=? (animaJogo j1 jo2)
teste2 = "Teste 2" ~: Jogo (Jogador (0,0)) m2 ~=? (animaJogo j1 jo4)
teste3 = "Teste 3" ~: Jogo (Jogador (3,3)) m2 ~=? (animaJogo j2 jo3)
teste4 = "Teste 4" ~: Jogo (Jogador (3,3)) m2 ~=? (animaJogo j2 jo5)

-- testar animaJogo quando o jogador se aproxima de uma Arvore
teste5 = "Teste 5" ~: Jogo (Jogador (1,0)) m2 ~=? (animaJogo j3 jo5)
teste6 = "Teste 6" ~: Jogo (Jogador (2,3)) m2 ~=? (animaJogo j4 jo4)
teste7 = "Teste 7" ~: Jogo (Jogador (2,2)) m2 ~=? (animaJogo j5 jo3)
teste8 = "Teste 8" ~: Jogo (Jogador (1,1)) m2 ~=? (animaJogo j6 jo2)

-- testar animaJogo quando o jogador está parado num tronco ou fora dele
teste9 = "Teste 9" ~: Jogo (Jogador (0,0)) m2 ~=? (animaJogo j1 jo1)
teste10 = "Teste 10" ~: Jogo (Jogador (3,2)) m2 ~=? (animaJogo j7 jo1)

-- testar animaJogo quando é possível se mover em todas as direções
teste11 = "Teste 11" ~: Jogo (Jogador (1,1)) m3 ~=? (animaJogo j8 jo1)
teste12 = "Teste 12" ~: Jogo (Jogador (1,0)) m3 ~=? (animaJogo j8 jo2)
teste13 = "Teste 13" ~: Jogo (Jogador (1,2)) m3 ~=? (animaJogo j8 jo3)
teste14 = "Teste 14" ~: Jogo (Jogador (0,1)) m3 ~=? (animaJogo j8 jo4)
teste15 = "Teste 15" ~: Jogo (Jogador (2,1)) m3 ~=? (animaJogo j8 jo5)

-- testar animaJogo quando o jogador se encontra no terreno Rio
teste16 = "Teste 16" ~: Jogo (Jogador (1,1)) m5 ~=? (animaJogo j9 jo2)
teste17 = "Teste 17" ~: Jogo (Jogador ((-1),1)) m5 ~=? (animaJogo j10 jo1)
teste18 = "Teste 18" ~: Jogo (Jogador (2,0)) m7 ~=? (animaJogo j11 jo2)
teste19 = "Teste 19" ~: Jogo (Jogador (2,0)) m7 ~=? (animaJogo j12 jo4)
teste20 = "Teste 20" ~: Jogo (Jogador (0,1)) m7 ~=? (animaJogo j11 jo4)

-- testar animaJogo quando o jogador se encontra no terreno Estrada
teste21 = "Teste 21" ~: Jogo (Jogador (1,0)) m9 ~=? (animaJogo j13 jo1)

testsT3 :: Test
testsT3 = TestLabel "Testes Tarefa 3" $ test [teste1,teste2,teste3,teste4,teste5,teste6,teste7,teste8,teste9,teste10,teste11,teste12,teste13,teste14,teste15,teste16,teste17,teste18,teste19,teste20,teste21]
