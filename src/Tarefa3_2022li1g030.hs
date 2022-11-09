{- |
Module      : Tarefa3_2022li1g030
Description : Movimentação do personagem e obstáculos
Copyright   : Ana Sá Oliveira <a104437@alunos.uminho.pt>
              Sara Campos Ramalho <a72481@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}
module Tarefa3_2022li1g030 where

import LI12223

animaJogo :: Jogo -> Jogada -> Jogo
animaJogo (Jogo (Jogador (x,y)) (Mapa n l)) j = animaJogador (Jogo (posicaoFinal (Jogador (x,y)) (posicaoInicial (Jogo (Jogador (x,y)) (Mapa n l)))) (animaMapa (Mapa n l))) j

animaMapa :: Mapa -> Mapa
animaMapa (Mapa n l) = (Mapa n (movObstaculos l))

movObstaculos :: [(Terreno, [Obstaculo])] -> [(Terreno, [Obstaculo])]
movObstaculos ((Rio n,l):t) | n==0 = [(Rio n,l)] ++ (movObstaculos t)
                            | n>0 = [(Rio n, (drop (length l - n) l) ++ (take (length l - n) l))] ++ (movObstaculos t)
                            | n<0 = [(Rio n, (drop n l) ++ (take n l))] ++ (movObstaculos t)
movObstaculos ((Estrada n,l):t) | n==0 = [(Estrada n,l)] ++ (movObstaculos t)
                                | n>0 = [(Estrada n, (drop (length l - n) l) ++ (take (length l - n) l))] ++ (movObstaculos t)
                                | n<0 = [(Estrada n, (drop n l) ++ (take n l))] ++ (movObstaculos t)
movObstaculos ((Relva,l):t) = [(Relva,l)] ++ (movObstaculos t)
movObstaculos [] = []

posicaoInicial :: Jogo -> (Terreno,Obstaculo)
posicaoInicial (Jogo (Jogador (x,y)) (Mapa n l)) = (fst (l !! y) , ((snd (l !! y)) !! x))
    
posicaoFinal :: Jogador -> (Terreno,Obstaculo) -> Jogador
posicaoFinal (Jogador (x,y)) (Rio n,Tronco) = (Jogador (x+n,y))
posicaoFinal (Jogador (x,y)) (_,o) = (Jogador (x,y))

animaJogador :: Jogo -> Jogada -> Jogo
animaJogador (Jogo (Jogador (x,y)) (Mapa n l)) (Parado) = (Jogo (Jogador (x,y)) (Mapa n l))
animaJogador (Jogo (Jogador (x,y)) (Mapa n l)) (Move Cima) | y == 0 = (Jogo (Jogador (x,y)) (Mapa n l))
                                                           | otherwise = (Jogo (Jogador (x,y-1)) (Mapa n l))
animaJogador (Jogo (Jogador (x,y)) (Mapa n l)) (Move Baixo) | y == (length l) - 1 = (Jogo (Jogador (x,y)) (Mapa n l))
                                                            | otherwise = (Jogo (Jogador (x,y+1)) (Mapa n l))
animaJogador (Jogo (Jogador (x,y)) (Mapa n l)) (Move Esquerda) | x == 0 = (Jogo (Jogador (x,y)) (Mapa n l))
                                                               | otherwise = (Jogo (Jogador (x-1,y)) (Mapa n l))
animaJogador (Jogo (Jogador (x,y)) (Mapa n l)) (Move Direita) | x == (n-1) = (Jogo (Jogador (x,y)) (Mapa n l))
                                                              | otherwise = (Jogo (Jogador (x+1,y)) (Mapa n l))

