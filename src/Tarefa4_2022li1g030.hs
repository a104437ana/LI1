{- |
Module      : Tarefa4_2022li1g030
Description : Determinar se o jogo terminou
Copyright   : Ana Sá Oliveira <a104437@alunos.uminho.pt>
              Sara Campos Ramalho <a72481@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2022/23.
-}
module Tarefa4_2022li1g030 where

import LI12223

-- jogo não terminou
j = Jogo (Jogador (2,1)) (Mapa 5 [(Relva, [Nenhum, Arvore, Nenhum, Arvore, Nenhum])
                                 ,(Rio 1, [Tronco, Tronco, Tronco, Nenhum, Nenhum])
                                 ,(Estrada 1, [Nenhum, Nenhum, Nenhum, Carro, Carro])])

-- jogador na água, terminou
j1 = Jogo (Jogador (3,1)) (Mapa 5 [(Relva, [Nenhum, Arvore, Nenhum, Arvore, Nenhum])
                                 ,(Rio 1, [Tronco, Tronco, Tronco, Nenhum, Nenhum])
                                 ,(Estrada 1, [Nenhum, Nenhum, Nenhum, Carro, Carro])])

-- jogador debaixo de carro, terminou
j2 = Jogo (Jogador (3,2)) (Mapa 5 [(Relva, [Nenhum, Arvore, Nenhum, Arvore, Nenhum])
                                  ,(Rio 1, [Tronco, Tronco, Tronco, Nenhum, Nenhum])
                                  ,(Estrada 1, [Nenhum, Nenhum, Nenhum, Carro, Carro])])

-- jogador fora do mapa pela largura
j3 = Jogo (Jogador (5,2)) (Mapa 5 [(Relva, [Nenhum, Arvore, Nenhum, Arvore, Nenhum])
                                  ,(Rio 1, [Tronco, Tronco, Tronco, Nenhum, Nenhum])
                                  ,(Estrada 1, [Nenhum, Nenhum, Nenhum, Carro, Carro])])

-- jogador fora do mapa pela altura
j4 = Jogo (Jogador (3,3)) (Mapa 5 [(Relva, [Nenhum, Arvore, Nenhum, Arvore, Nenhum])
                                  ,(Rio 1, [Tronco, Tronco, Tronco, Nenhum, Nenhum])
                                  ,(Estrada 1, [Nenhum, Nenhum, Nenhum, Carro, Carro])])

-- | Calcula o obstáculo existente no terreno da posição atual do jogador
calculaObstaculo :: Int -> (Terreno, [Obstaculo]) -> (Terreno, Obstaculo)
calculaObstaculo x (b,c) = (b,(c !! x))

-- | Calcula o obstáculo existente no terreno da posição atual do jogador, através da função calculaObstaculo
jogadorPosicao :: Coordenadas -> Mapa -> (Terreno, Obstaculo)
jogadorPosicao (x,y) (Mapa a l) = calculaObstaculo x (l !! y)

-- | Verifica se o jogador está na água ou debaixo de um carro, casos em que o jogo terminou
verificaPosicao :: (Terreno, Obstaculo) -> Bool
verificaPosicao (b,d) = case b of
                          Rio _ -> d == Nenhum
                          Estrada _ -> d == Carro 
                          Relva -> False

-- | Verifica se o jogador está fora do mapa ou num obstáculo, ou seja, se o jogo terminou 
jogoTerminou :: Jogo -> Bool
jogoTerminou (Jogo (Jogador (x,y)) (Mapa a l)) = x < 0 || x >= a || y < 0 || y >= length l || verificaPosicao (jogadorPosicao (x,y) (Mapa a l))