{- |
Module      : Tarefa5_2022li1g030
Description : Realizar o deslize do mapa
Copyright   : Ana Sá Oliveira <a104437@alunos.uminho.pt>
              Sara Campos Ramalho <a72481@alunos.uminho.pt>

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2022/23.
-}
module Tarefa5_2022li1g030 (
-- * Função deslizaJogo
-- ** Função principal
deslizaJogo
-- ** Funções auxiliares
) where

import LI12223
import Tarefa2_2022li1g030

deslizaJogo :: Int -> Jogo -> Jogo
deslizaJogo seed (Jogo (Jogador (x,y)) (Mapa n l)) = (Jogo (Jogador (x,y+1)) (estendeMapa (Mapa n (init l)) seed))