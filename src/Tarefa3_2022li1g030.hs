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
posicaoJogador, animaJogador, ordenada, variacaoDaOrdenada, animaMapa, animaLista, movObstaculos, movCarros
) where

import LI12223

{- |A função 'animaJogo', que recebe um jogo e uma jogada e que retorna um jogo, movimenta os obstáculos, de acordo com a velocidade do terreno em que se encontram, e o personagem, de 
acordo com a jogada dada.

A função 'animaJogo' pode ser definida em função de outras funções auxiliares: a função 'animaJogador', a função 'posicaoJogador', a função 'variacaoDaOrdenada' e a função 'animaMapa'. A
função 'variacaoDaOrdenada' contém ainda outra função auxiliar, a função 'ordenada'. A função 'animaMapa' contém ainda outra função auxiliar, a função 'animaLista', que por sua vez contém
duas funções auxiliares, a função 'movObstaculos' e a função 'movCarros'.

Primeiro, aplicamos a função 'animaJogador', ou seja, primeiro, movimentamos o jogador de acordo com a jogada dada, no mapa inicial. Depois, aplicamos outras funções que analisam a
situação do jogo, a função 'posicaoJogador' e a função 'variacaoDaOrdenada', cujos resultados serão usados na função 'animaMapa'. Por fim, aplicamos a função 'animaMapa', ou seja, no final,
movimentamos o "mapa", mais especificamente os obstáculos, de acordo com a velocidade do terreno em que se encontram.

Assim, a função 'animaJogo' pode ser definida da seguinte forma:

@
animaJogo j jo = animaMapa (animaJogador j jo) (posicaoJogador j) (variacaoDaOrdenada j jo)
@

-}

animaJogo :: Jogo -> Jogada -> Jogo
animaJogo j jo = animaMapa (animaJogador j jo) (posicaoJogador j) (variacaoDaOrdenada j jo)

{- |A função 'posicaoJogador', que recebe um jogo e retorna um par de terreno e de obstáculo, pega num jogo e calcula o terreno e o obstáculo onde o jogador se encontra naquele jogo.

Consideremos (x,y) as coordenadas do jogador e l a lista do mapa. Primeiro, obtemos o par de terreno e de lista de obstáculos em que o jogador se encontra, calculando (l !! y). Depois, 
obtemos o primeiro elemento deste par, calculando (fst (l !! y)), ou seja, obtemos o terreno em que o jogador se encontra. Depois, obtemos o segundo elemento do par, calculando 
(snd (l !! y)), ou seja, obtemos a lista de obstáculos ou a linha em que o jogador se encontra, a lista de obstáculos y do mapa ou linha y do mapa. Por fim, calculando 
((snd (l !! y)) !! x), obtemos o obstáculo em que o jogador se encontra, o obstáculo x da linha y ou o elemento x da lista de obstáculos y.

Como o canto superior esquerdo do mapa representa as coordenadas (0,0) e como todas as posições no mapa tem abcissas e ordenadas não negativas (ou seja, positivas ou nulas), então
podemos utilizar a função (!!) (que apenas trabalha com listas e com números não negativos) sem preocupações.

Assim, a função 'posicaoJogador' pode ser definida da seguinte forma:

@
posicaoJogador (Jogo (Jogador (x,y)) (Mapa _ l)) = (fst (l !! y) , ((snd (l !! y)) !! x))
@

-}

posicaoJogador :: Jogo -> (Terreno,Obstaculo)
posicaoJogador (Jogo (Jogador (x,y)) (Mapa _ l)) = (fst (l !! y) , ((snd (l !! y)) !! x))

{- |A função 'animaJogador', que recebe um jogo e uma jogada e que retorna um jogo, movimenta um jogador num jogo. 

Se a jogada for Parado então o jogo que retorna será o mesmo que recebeu, uma vez que o jogador não se moveu.

As jogadas Move Cima, Move Baixo, etc. fazem com que o jogador se mova 1 unidade para cima, baixo, etc, respectivamente. Se a jogada for Move Cima então a ordenada do jogador passará de y 
para y-1. Se a jogada for Move Baixo então a ordenada do jogador passará de y para y+1. Se a jogada for Move Esquerda então a abcissa do jogador passará de x para x-1. Se a jogada for Move
Direita então a abcissa do jogador passará de x para x+1. Isto acontece porque o canto superior esquerdo do mapa representa as coordenadas (0,0) e porque todas as posições no mapa tem 
abcissas e ordenadas não negativas (ou seja, positivas ou nulas).

No entanto, temos que ter em atenção que o jogador não consegue escapar do mapa através dos seus movimentos. Por exemplo, se o jogador se encontrar na linha de topo do mapa, então mover-se
para cima não tem qualquer efeito, uma vez que já se encontra no limite do mapa. Assim, quando y for igual a 0 e quando a jogada for Move Cima, o jogador não altera a sua posição. Seguindo
o mesmo raciocínio, quando x for igual a 0 e a quando a jogada for Move Esquerda, o jogador não altera a sua posição. Quando x for igual á largura do mapa menos uma unidade (porque 
começamos a contar do 0) e quando a jogada for Move Direita, o jogador não altera a sua posição. Quando o y for igual ao comprimento da lista total do mapa menos uma unidade (porque
começamos a contar do 0) e quando a jogada for Move Baixo, o jogador não altera a sua posição.

Por fim, ainda temos de ter em atenção que o jogador não ocupa posições onde existem Arvores. Ou seja, quando o jogador vai em direção a uma Arvore, ele acaba por se manter na mesma 
posição, uma vez que não pode ocupar a posição de uma Arvore. Assim, se uma Arvore se encontrar, por exemplo, em baixo do jogador, ou seja se 
posicaoJogador (Jogo (Jogador (x,y+1)) (Mapa n l)) resultar em (Relva,Arvore) e se a jogada for Move Baixo, então o jogador não se irá mecher. Isto também acontece quando tem uma Arvore em
cima, à direita ou à esquerda do jogador e quando a jogada é respetivamente, Move Cima, Move Direita e Move Esquerda.

Assim, a função 'animaJogador' pode ser definida da seguinte forma:

@
animaJogador (Jogo (Jogador (x,y)) (Mapa n l)) j = 
    case j of (Parado) -> jo

              (Move Cima) | y == 0 -> jo
                          | posicaoJogador (Jogo (Jogador (x,y-1)) m) == (Relva,Arvore) -> jo
                          | otherwise -> (Jogo (Jogador (x,y-1)) m)

              (Move Baixo) | y == (length l) - 1 -> jo
                           | posicaoJogador (Jogo (Jogador (x,y+1)) m) == (Relva,Arvore) -> jo
                           | otherwise -> (Jogo (Jogador (x,y+1)) m)

              (Move Esquerda) | x == 0 -> jo
                              | posicaoJogador (Jogo (Jogador (x-1,y)) m) == (Relva,Arvore) -> jo
                              | otherwise -> (Jogo (Jogador (x-1,y)) m)

              (Move Direita) | x == (n-1) -> jo
                             | posicaoJogador (Jogo (Jogador (x+1,y)) m) == (Relva,Arvore) -> jo
                             | otherwise -> (Jogo (Jogador (x+1,y)) m)

     where jo = (Jogo (Jogador (x,y)) (Mapa n l))
           m = (Mapa n l)
@

-}

animaJogador :: Jogo -> Jogada -> Jogo
animaJogador (Jogo (Jogador (x,y)) (Mapa n l)) j = 
    case j of (Parado) -> jo

              (Move Cima) | y == 0 -> jo
                          | posicaoJogador (Jogo (Jogador (x,y-1)) m) == (Relva,Arvore) -> jo
                          | otherwise -> (Jogo (Jogador (x,y-1)) m)

              (Move Baixo) | y == (length l) - 1 -> jo
                           | posicaoJogador (Jogo (Jogador (x,y+1)) m) == (Relva,Arvore) -> jo
                           | otherwise -> (Jogo (Jogador (x,y+1)) m)

              (Move Esquerda) | x == 0 -> jo
                              | posicaoJogador (Jogo (Jogador (x-1,y)) m) == (Relva,Arvore) -> jo
                              | otherwise -> (Jogo (Jogador (x-1,y)) m)

              (Move Direita) | x == (n-1) -> jo
                             | posicaoJogador (Jogo (Jogador (x+1,y)) m) == (Relva,Arvore) -> jo
                             | otherwise -> (Jogo (Jogador (x+1,y)) m)

     where jo = (Jogo (Jogador (x,y)) (Mapa n l))
           m = (Mapa n l)

{- |A função 'ordenada', que recebe um jogo e retorna um número inteiro, descobre qual é a ordenada de um jogador num determinado jogo.

Assim, a função 'ordenada' pode ser definida da seguinte forma:

@
ordenada (Jogo (Jogador (_,y)) (Mapa _ _)) = y
@

-}

ordenada :: Jogo -> Int
ordenada (Jogo (Jogador (_,y)) (Mapa _ _)) = y

{- |A função 'variacaoDaOrdenada', que recebe um jogo e uma jogada e que retorna um número inteiro, calcula a variação da ordenada do jogador desde antes da jogada ser dada até depois da 
jogada ser aplicada. Calcula a diferença entre a ordenada inicial do jogador e a ordenada final do jogador após ser aplicada a função 'animaJogador' (após o jogador se movimentar). Para 
calcular as ordenadas do jogador de cada jogo, jogo inicial e jogo após a função 'animaJogador', usamos a função anterior, a função 'ordenada'. Para calcular o jogo após a função 
'animaJogador', usamos presisamente e obviamente a função 'animaJogador'.

Assim, a função 'variacaoDaOrdenada' pode ser definida da seguinte forma:

@
variacaoDaOrdenada j jo = ordenada j - ordenada (animaJogador j jo)
@

-}

variacaoDaOrdenada :: Jogo -> Jogada -> Int
variacaoDaOrdenada j jo = ordenada j - ordenada (animaJogador j jo)

{- |A função 'animaMapa', que recebe um jogo, um par de terreno e de obstáculo e um número inteiro e que retorna um jogo, anima o mapa e também anima o jogador em algumas ocasiões (quando
o jogador acompanha o movimento do tronco).

Se o par de terreno e de obstáculo resultante da função 'posicaoJogador' aplicada ao jogo for (Rio v, Tronco) então o jogador inicialmente, antes de se mover, se encontrava num Rio e num 
Tronco.

Se o número inteiro resultante da função 'variacaoDaOrdenada' aplicada ao jogo e a jogada for 0 então isto significa que a ordenada do jogador, o y do jogador não se alterou, após a
aplicação da função 'animaJogador', ou seja, o jogador ou se moveu horizontalmente, ou ficou parado. Assim, o jogador não se moveu verticalmente.

Se o jogador se encontrava inicialmente num Rio e num Tronco e se não se moveu verticalmente então o jogador irá acompanhar o movimento do tronco em que se encontrava inicialmente. Assim, 
à abcissa do jogador iremos adicionar a velocidade do rio onde o jogador se encontra, para assim, o jogador acompanhar o movimento do tronco (o movimento do rio). Para além de animar o
jogador, também iremos animar o mapa, mais precisamente, a lista do mapa. Para isso, iremos usar a função 'animaLista', que irá ser definida posteriormente.

Caso o par de terreno e de obstáculo resultante da função 'posicaoJogador' aplicada ao jogo não for (Rio v, Tronco) ou o número inteiro resultante da função 'variacaoDaOrdenada' aplicada 
ao jogo e a jogada não for 0 ou ambos, então não precisamos de animar o jogador, uma vez que este não irá acompanhar o movimento do tronco. Neste caso, apenas presisamos de animar o mapa,
mais precisamente, a lista do mapa e para isso voltaremos a usar a função 'animaLista'.

Assim, a função 'animaMapa' pode ser definida da seguinte forma:

@
animaMapa (Jogo (Jogador (x,y)) (Mapa n l)) (Rio v,Tronco) 0 = (Jogo (Jogador (x+v,y)) (Mapa n (animaLista x y l)))
animaMapa (Jogo (Jogador (x,y)) (Mapa n l)) _ _ = (Jogo (Jogador (x,y)) (Mapa n (animaLista x y l)))
@

-}

animaMapa :: Jogo -> (Terreno,Obstaculo) -> Int -> Jogo
animaMapa (Jogo (Jogador (x,y)) (Mapa n l)) (Rio v,Tronco) 0 = (Jogo (Jogador (x+v,y)) (Mapa n (animaLista x y l)))
animaMapa (Jogo (Jogador (x,y)) (Mapa n l)) _ _ = (Jogo (Jogador (x,y)) (Mapa n (animaLista x y l)))

{- |A função 'animaLista', que recebe dois números inteiros (a abcissa e a ordenada do jogador) e uma lista de pares de terreno e de lista de obstáculos e que retorna uma lista de pares de
terreno e de lista de obstáculos, anima a lista do mapa.

A abcissa do jogador não é diretamente usada nesta função. No entanto, temos que mante-la nesta função porque caso seja necessário utilizar a função 'movCarros' para animar uma linha do
mapa, então aí, na função 'movCarros' iremos necessitar da abcissa do jogador.

A ordenada do jogador é diretamente usada nesta função. Se esta for 0 e se a cabeça da lista do mapa for uma linha de Estrada, então o jogador encontra-se numa Estrada, logo pode ser
possívelmente atropelado. Tendo em conta o possível atropelamento, iremos usar uma função especial, a função 'movCarros', que move os obstáculos das estradas (os carros) a pensar num 
possível atropelamento. 

Para isto acontecer, o jogador não necessita de estar obrigatoriamente na primeira linha do mapa (linha 0) que é uma estrada. Na verdade, o jogador apenas necessita de se encontrar numa
estrada. Isto acontece porque após animarmos uma linha, aplicamos a função 'animaLista' à abcissa do jogador, à ordenada do jogador menos uma unidade e ao resto da lista do mapa. Assim,
ao retirar sempre uma unidade ao valor da ordenada do jogador, iremos eventualmente chegar ao 0 e nesse momento sabemos que a cabeça da lista do mapa é a linha onde o jogador se encontra.
Se essa linha for estrada, aplicamos a tal função 'movCarros'.

Para os outros casos, a ordenada é completamente desnecessária, no entanto temos de manter a ordenada para garantir que no caso de o jogador se encontrar numa estrada (ordenada é 0) esta
linha será animada pela função 'movCarros' e não pela função 'movObstaculos' (função mais geral mas que ignora um possível atropelamento e que também é usada em estradas).

Se a função receber uma lista vazia irá retornar lista vazia, pois não existem obstáculos que se podem mover (a lista vazia não tem elementos logo não tem obstáculos).

Se a função receber uma lista cuja a cabeça seja uma linha de Relva, então como numa relva não existe velocidade e os obstáculos não se movimentam então a função irá retornar a linha de
Relva inalterada e irá aplicar a função 'animaLista' ao resto da lista (recursividade).

Se a função receber uma lista cuja cabeça seja uma linha de Rio ou uma linha de Estrada (e a ordenada seja diferente de 0 neste último caso) então iremos aplicar a função 'movObstaculos'
para obter as listas de obstáculos dos rios e das estradas, e para o resto da lista do mapa aplicamos a função 'animaLista' (recursividade).

Assim, a função 'animaLista' pode ser definida da seguinte forma:

@
animaLista _ _ [] = []
animaLista x 0 ((Estrada v,l):t) = [movCarros x v (Estrada v,l)] ++ (animaLista x (-1) t)
animaLista x y ((Estrada v,l):t) = [movObstaculos v (Estrada v,l)] ++ (animaLista x (y-1) t)
animaLista x y ((Rio v,l):t) = [movObstaculos v (Rio v,l)] ++ (animaLista x (y-1) t)
animaLista x y ((Relva,l):t) = [(Relva,l)] ++ (animaLista x (y-1) t)
@

-}

animaLista :: Int -> Int -> [(Terreno, [Obstaculo])] -> [(Terreno, [Obstaculo])]
animaLista _ _ [] = []
animaLista x 0 ((Estrada v,l):t) = [movCarros x v (Estrada v,l)] ++ (animaLista x (-1) t)
animaLista x y ((Estrada v,l):t) = [movObstaculos v (Estrada v,l)] ++ (animaLista x (y-1) t)
animaLista x y ((Rio v,l):t) = [movObstaculos v (Rio v,l)] ++ (animaLista x (y-1) t)
animaLista x y ((Relva,l):t) = [(Relva,l)] ++ (animaLista x (y-1) t)

{- |A função 'movObstaculos', que recebe um inteiro (a velocidade do terreno) e um par de terreno e de lista de obstáculos e que retorna um par de terreno e de lista de obstáculos, 
movimenta os obstáculos de um rio ou estrada de acordo com a velocidade do terreno. 

Numa estrada ou rio com velocidade v, os obstáculos devem mover-se |v| unidades na direção determinada. Se a velocidade v for igual a 0, os obstáculos não se movimentam, ou seja, a função
irá retornar a mesma lista que recebeu, porque nada se alterou. 

Se a velocidade v for positiva, os obstáculos movem-se |v| unidades da esquerda para a direita. Se a velocidade v for negativa, os obstáculos movem-se |v| unidades da direita para a 
esquerda. 

Sabemos que ao deslocar os obstáculos de uma linha, estes desaparecerem por um dos lados do mapa e devem reaparecer no lado oposto. Assim, para mover os obstáculos apenas temos de mudar a 
ordem da lista de obstáculos. 

Quando a lista de obstáculos se move para a direita, quando a velocidade v é positiva, pegamos nos ultímos v elementos da lista e os colocamos na frente da lista. Para fazer isto podemos 
usar as funções last, init e (++) pré-definidas no Haskell. 

Quando a lista de obstáculos se move para a esquerda, quando a velocidade v é negativa, pegamos nos primeiros v elementos da lista e os colocamos atrás da lista. Para fazer isto podemos 
usar as funções head, tail e (++) pré-definidas no Haskell.

Para os obstáculos não se moverem infinitamente e pararem, então sempre que aplicamos a função 'movObstaculos' para além de alterarmos a ordem dos obstáculos também retiramos uma unidade
a velocidade do terreno se esta for positiva ou adicionamos uma unidade a velocidade do terreno se esta for negativa. Assim, ao aplicar esta função, eventualmente a velocidade irá ser 0,
e nesse momento a função irá retornar o par de terreno e de lista de obstáculos que recebeu. Quando a velocidade for 0, significa que os obstáculos já se moveram o quanto deviam mover
segundo a velocidade inicial do terreno.

Assim, a função recursiva 'movObstaculos' pode ser definida da seguinte forma:

@
movObstaculos n p = case p of (Rio v, []) -> (Rio v, [])
                              (Estrada v,[]) -> (Estrada v, [])
                              (Rio v,l) | n==0 -> (Rio v,l)
                                        | n>0 -> movObstaculos (n-1) (Rio v,[last l] ++ (init l))
                                        | n<0 -> movObstaculos (n+1) (Rio v,(tail l) ++ [head l])
                              (Estrada v,l) | n==0 -> (Estrada v,l)
                                            | n>0 -> movObstaculos (n-1) (Estrada v,[last l] ++ (init l))
                                            | n<0 -> movObstaculos (n+1) (Estrada v,(tail l) ++ [head l])
@

-}

movObstaculos :: Int -> (Terreno,[Obstaculo]) -> (Terreno,[Obstaculo])
movObstaculos n p = case p of (Rio v, []) -> (Rio v, [])
                              (Estrada v,[]) -> (Estrada v, [])
                              (Rio v,l) | n==0 -> (Rio v,l)
                                        | n>0 -> movObstaculos (n-1) (Rio v,[last l] ++ (init l))
                                        | n<0 -> movObstaculos (n+1) (Rio v,(tail l) ++ [head l])
                              (Estrada v,l) | n==0 -> (Estrada v,l)
                                            | n>0 -> movObstaculos (n-1) (Estrada v,[last l] ++ (init l))
                                            | n<0 -> movObstaculos (n+1) (Estrada v,(tail l) ++ [head l])

{- |A função 'movCarros', que recebe dois inteiros (a abcissa do jogador e a velocidade da estrada) e um par de terreno e de lista de obstáculos e retorna um par de terreno e de lista de
obstáculos, anima os obstáculos de uma estrada (os carros) tendo em conta um possível atropelamento.

Se a lista de obstáculos for vazia, a função retorna um par de terreno estrada com uma determinada velocidade e de uma lista vazia (uma vez que a lista vazia não tem elementos logo não tem
obstáculos, logo estes não se podem mover).

Se o obstáculo número x (a abcissa do jogador) da lista de obstáculos for um Carro (começando a contar a partir do 0), ou seja, se (l !! x) == Carro então o jogador encontra-se na posição
de um carro, logo foi atropelado. Nesta situação, a função devolve a linha do mapa inalterada uma vez que o movimento do obstáculo Carro ́e interrompido ao atropelar o jogador. Na verdade,
o movimento de toda a linha, de todos os obstáculos da linha é interrompido quando um carro atropela o jogador.

Nos outros casos, a função 'movCarros' haje da mesma forma que a função 'movObstaculos' com a diferença que a função 'movCarros' continua a usar a abcissa do jogador, pois eventualmente a
posição do jogador pode coincidir com a posição do carro e aí a função iria ter que retornar a linha do mapa que recebeu.

Assim, a função 'movCarros' pode ser definida da seguinte forma:

@
movCarros x n (Estrada v,l) | l == [] = (Estrada v, [])
                            | (l !! x) == Carro = (Estrada v,l)
                            | n==0 = (Estrada v,l)
                            | n>0 = movCarros x (n-1) (Estrada v,[last l] ++ (init l))
                            | n<0 = movCarros x (n+1) (Estrada v,(tail l) ++ [head l])
@

-}

movCarros :: Int -> Int -> (Terreno,[Obstaculo]) -> (Terreno,[Obstaculo])
movCarros x n (Estrada v,l) | l == [] = (Estrada v, [])
                            | (l !! x) == Carro = (Estrada v,l)
                            | n==0 = (Estrada v,l)
                            | n>0 = movCarros x (n-1) (Estrada v,[last l] ++ (init l))
                            | n<0 = movCarros x (n+1) (Estrada v,(tail l) ++ [head l])
