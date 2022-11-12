{- |
Module      : Tarefa3_2022li1g030
Description : Movimentação do personagem e obstáculos
Copyright   : Ana Sá Oliveira <a104437@alunos.uminho.pt>
              Sara Campos Ramalho <a72481@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}
module Tarefa3_2022li1g030 (
-- * Função animaJogo
-- ** Função principal
animaJogo,
-- ** Funções auxiliares
movObstaculos, animaLista, animaMapa, posicaoInicial, posicaoFinal, animaJogador
) where

import LI12223

{- |A função 'animaJogo', que recebe um jogo e uma jogada e que retorna um jogo, movimenta os obstáculos, de acordo com a velocidade do terreno em que se encontram, e o personagem, de 
acordo com a jogada dada.

A função 'animaJogo' pode ser definida em função de outras funções auxiliares: a função 'animaMapa', a função 'posicaoInicial', a função 'posicaoFinal' e a função 'animaJogador'. A função 
'animaMapa' contém ainda outra função auxiliar, a função 'animaLista' que por sua vez contém outra função auxiliar, a função 'movObstaculos'.

Assim, a função 'animaJogo' pode ser definida da seguinte forma:

@
animaJogo (Jogo (Jogador (x,y)) (Mapa n l)) j = animaJogador (Jogo (posicaoFinal (Jogador (x,y)) (posicaoInicial (Jogo (Jogador (x,y)) (Mapa n l)))) (animaMapa (Mapa n l))) j
@

-}

animaJogo :: Jogo -> Jogada -> Jogo
animaJogo (Jogo (Jogador (x,y)) (Mapa n l)) j = animaJogador (Jogo (posicaoFinal (Jogador (x,y)) (posicaoInicial (Jogo (Jogador (x,y)) (Mapa n l)))) (animaMapa (Mapa n l))) j

{- |A função 'movObstaculos', que recebe um inteiro e um par de terreno e de lista de obstáculos e que retorna um par de terreno e de lista de obstáculos, movimenta os obstáculos de um 
rio ou estrada de acordo com a velocidade do terreno. Numa estrada ou rio com velocidade v, os obstáculos devem mover-se |v| unidades na direção determinada. Se a velocidade v for igual
a 0, os obstáculos não se movimentam, ou seja, a função irá retornar a mesma lista que recebeu, porque nada se alterou. Se a velocidade v for positiva, os obstáculos movem-se |v| unidades
da esquerda para a direita. Se a velocidade v for negativa, os obstáculos movem-se |v| unidades da direita para a esquerda. Sabemos que ao deslocar os obstáculos de uma linha, estes 
desaparecerem por um dos lados do mapa e devem reaparecer no lado oposto. Assim, para mover os obstáculos apenas temos de mudar a ordem da lista de obstáculos. Quando a lista de obstáculos
se move para a direita, quando a velocidade v é positiva, pegamos nos ultímos v elementos da lista e os colocamos na frente da lista. Para fazer isto podemos usar as funções last, init e
(!!) pré-definidas no Haskell. Quando a lista de obstáculos se move para a esquerda, quando a velocidade v é negativa, pegamos nos primeiros v elementos da lista e os colocamos atrás da 
lista. Para fazer isto podemos usar as funções head, tail e (!!) pré-definidas no Haskell. 

Assim, a função recursiva 'movObstaculos' pode ser definida da seguinte forma:

@
movObstaculos n (Rio v, []) = (Rio v, [])
movObstaculos n (Estrada v,[]) = (Estrada v, [])
movObstaculos n (Rio v,l) | n==0 = (Rio v,l)
                          | n>0 = movObstaculos (n-1) (Rio v,[last l] ++ (init l))
                          | n<0 = movObstaculos (n+1) (Rio v,(tail l) ++ [head l])
movObstaculos n (Estrada v,l) | n==0 = (Estrada v,l)
                              | n>0 = movObstaculos (n-1) (Estrada v,[last l] ++ (init l))
                              | n<0 = movObstaculos (n+1) (Estrada v,(tail l) ++ [head l])
@

-}

movObstaculos :: Int -> (Terreno,[Obstaculo]) -> (Terreno,[Obstaculo])
movObstaculos n (Rio v, []) = (Rio v, [])
movObstaculos n (Estrada v,[]) = (Estrada v, [])
movObstaculos n (Rio v,l) | n==0 = (Rio v,l)
                          | n>0 = movObstaculos (n-1) (Rio v,[last l] ++ (init l))
                          | n<0 = movObstaculos (n+1) (Rio v,(tail l) ++ [head l])
movObstaculos n (Estrada v,l) | n==0 = (Estrada v,l)
                              | n>0 = movObstaculos (n-1) (Estrada v,[last l] ++ (init l))
                              | n<0 = movObstaculos (n+1) (Estrada v,(tail l) ++ [head l])

{- |A função 'animaLista', que recebe uma lista de pares de terreno e de lista de obstáculos e que retorna uma lista de pares de terreno e de lista de obstáculos, utiliza a função
anterior ('movObstaculos') para obter as listas de obstáculos dos rios e das estradas. Já numa relva, não existe velocidade e os obstáculos não se movimentam, ou seja, a função irá
retornar a mesmma lista que recebeu, porque nada se alterou. Para além disso, se a função receber uma lista vazia irá retornar lista vazia, pois não existem obstáculos que se podem mover
(a lista vazia não tem elementos logo não tem obstáculos).

Assim, a função recursiva 'animaLista' pode ser definida da seguinte forma:

@
animaLista ((Rio n,l):t) = [movObstaculos n (Rio n,l)] ++ (animaLista t)
animaLista ((Estrada n,l):t) = [movObstaculos n (Estrada n,l)] ++ (animaLista t)
animaLista ((Relva,l):t) = [(Relva,l)] ++ (animaLista t)
animaLista [] = []
@

-}

animaLista :: [(Terreno, [Obstaculo])] -> [(Terreno, [Obstaculo])]
animaLista ((Rio n,l):t) = [movObstaculos n (Rio n,l)] ++ (animaLista t)
animaLista ((Estrada n,l):t) = [movObstaculos n (Estrada n,l)] ++ (animaLista t)
animaLista ((Relva,l):t) = [(Relva,l)] ++ (animaLista t)
animaLista [] = []

{- |A função 'animaMapa', que recebe um mapa e retorna um mapa, utiliza a função 'animaLista' para mover os obstáculos de todo o mapa.

Assim, a função 'animaMapa' pode ser definida da seguinte forma:

@
animaMapa (Mapa n l) = (Mapa n (movObstaculos l))
@

-}

animaMapa :: Mapa -> Mapa
animaMapa (Mapa n l) = (Mapa n (animaLista l))

{- |A função 'posicaoInicial', que recebe um jogo e retorna um par de terreno e de obstáculo, pega num jogo e calcula o terreno e o obstáculo onde o jogador se encontra naquele jogo. 
Para o fazer, utiliza a função (!!) pré-definida no Haskell.

Assim, a função 'posicaoInicial' pode ser definida da seguinte forma:

@
posicaoInicial (Jogo (Jogador (x,y)) (Mapa n l)) = (fst (l !! y) , ((snd (l !! y)) !! x))
@

-}

posicaoInicial :: Jogo -> (Terreno,Obstaculo)
posicaoInicial (Jogo (Jogador (x,y)) (Mapa n l)) = (fst (l !! y) , ((snd (l !! y)) !! x))

{- |A função 'posicaoFinal', que recebe um jogador e um par de terreno e obstáculo e retorna um jogador, calcula as próximas coordenadas do jogador, em função das primeiras coordenadas do
jogador e em função do terreno e do obstáculo em que o jogador se encontrava. Se a função 'posicaoFinal' receber o par de terreno e de obstáculo obtido pela função 'posicaoInicial' e se
receber o mesmo jogador do jogo recebido pela função 'posicaoInicial', então podemos calcular as próximas coordenadas do jogador. 

Mesmo quando o jogador não efetua qualquer movimento (quando a sua jogada  ́e Parado), se o personagem se encontrar em cima de um tronco, o jogador acompanha o movimento do tronco. Assim,
se o jogador se encontrava num Rio com n velocidade e num Tronco, então o jogador mover-se-á n unidades numa determinada direção dependendo do valor de n. Assim, a abcissa do jogador 
passará de x para x+n. Se o jogador se encontrar num Rio e num Nenhum, ou se se encontrar noutro tipo de terreno, então o jogador não se irá mover com o mapa, logo irá apresentar as mesmas 
coordenadas.

Assim, a função 'posicaoFinal' pode ser definida da seguinte forma:

@
posicaoFinal (Jogador (x,y)) (Rio n,Tronco) = (Jogador (x+n,y))
posicaoFinal (Jogador (x,y)) (_,o) = (Jogador (x,y))
@

-}

posicaoFinal :: Jogador -> (Terreno,Obstaculo) -> Jogador
posicaoFinal (Jogador (x,y)) (Rio n,Tronco) = (Jogador (x+n,y))
posicaoFinal (Jogador (x,y)) (_,o) = (Jogador (x,y))

{- |A função 'animaJogador', que recebe um jogo e uma jogada e que retorna um jogo, movimenta um jogador num jogo. 

Se a jogada for Parado então o jogo que retorna será o mesmo que recebeu, uma vez que o jogador não se moveu.

As jogadas Move Cima, Move Baixo, etc. fazem com que o jogador se mova 1 unidade para cima, baixo, etc, respectivamente. Se a jogada for Move Cima então a ordenada do jogador passará de y 
para y-1. Se a jogada for Move Baixo então a ordenada do jogador passará de y para y+1. Se a jogada for Move Esquerda então a abcissa do jogador passará de x para x-1. Se a jogada for Move
Direita então a abcissa do jogador passará de x para x+1. Isto acontece porque o jogador apenas pode ter coordenadas maiores ou iguais a 0 e porque o canto superior esquerdo representa as
coordenadas (0,0).

No entanto, temos que ter em atenção que o jogador não consegue escapar do mapa através dos seus movimentos. Por exemplo, se o jogador se encontrar na linha de topo do mapa, então mover-se
para cima não tem qualquer efeito, uma vez que já se encontra no limite do mapa. Assim, quando y for igual a 0 e quando a jogada for Move Cima, o jogador não altera a sua posição. Seguindo
o mesmo raciocínio, quando x for igual a 0 e a quando a jogada for Move Esquerda, o jogador não altera a sua posição. Quando x for igual á largura do mapa menos uma unidade (porque 
começamos a contar do 0) e quando a jogada for Move Direita, o jogador não altera a sua posição. Quando o y for igual ao comprimento da lista total do mapa menos uma unidade (porque
começamos a contar do 0) e quando a jogada for Move Baixo, o jogador não altera a sua posição.

Assim, a função 'animaJogador' pode ser definida da seguinte forma:

@
animaJogador (Jogo (Jogador (x,y)) (Mapa n l)) (Parado) = (Jogo (Jogador (x,y)) (Mapa n l))
animaJogador (Jogo (Jogador (x,y)) (Mapa n l)) (Move Cima) | y == 0 = (Jogo (Jogador (x,y)) (Mapa n l))
                                                           | otherwise = (Jogo (Jogador (x,y-1)) (Mapa n l))
animaJogador (Jogo (Jogador (x,y)) (Mapa n l)) (Move Baixo) | y == (length l) - 1 = (Jogo (Jogador (x,y)) (Mapa n l))
                                                            | otherwise = (Jogo (Jogador (x,y+1)) (Mapa n l))
animaJogador (Jogo (Jogador (x,y)) (Mapa n l)) (Move Esquerda) | x == 0 = (Jogo (Jogador (x,y)) (Mapa n l))
                                                               | otherwise = (Jogo (Jogador (x-1,y)) (Mapa n l))
animaJogador (Jogo (Jogador (x,y)) (Mapa n l)) (Move Direita) | x == (n-1) = (Jogo (Jogador (x,y)) (Mapa n l))
                                                              | otherwise = (Jogo (Jogador (x+1,y)) (Mapa n l))
@

-}

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
