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
) where

import LI12223
import Tarefa2_2022li1g030

{- |A função 'deslizaJogo', que recebe um número e um jogo e que retorna um jogo, tal como o nome diz, desliza o jogo, ou seja, pega no jogo anterior e elimina a primeira linha do mapa e
adiciona uma nova linha no final do mapa, como se o jogo "desliza-se" na tela do computador.

Esta função utiliza a função pré-definida no Haskell init para eliminar da lista do mapa inicial a primeira linha (a cabeça da lista). Depois usamos a função auxiliar 'estendeMapa',
definida na Tarefa 2 da Fase 1 deste projeto, para adicionar uma nova linha no final do mapa. Por fim, apenas temos de mover o jogador uma unidade para cima, para o jogador continuar no
mesmo local onde estava no mapa anterior (ou seja, se o mapa se moveu, o jogador também terá de se mover para continuar no mesmo local).

Assim, a função 'deslizaJogo' pode ser definida da seguinte forma:

@
deslizaJogo :: Int -> Jogo -> Jogo
deslizaJogo seed (Jogo (Jogador (x,y)) (Mapa n l)) = (Jogo (Jogador (x,y+1)) (estendeMapa (Mapa n (init l)) seed))
@

-}

deslizaJogo :: Int -> Jogo -> Jogo
deslizaJogo seed (Jogo (Jogador (x,y)) (Mapa n l)) = (Jogo (Jogador (x,y+1)) (estendeMapa (Mapa n (init l)) seed))