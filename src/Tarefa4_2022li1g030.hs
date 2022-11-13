{- |
Module      : Tarefa4_2022li1g030
Description : Determinar se o jogo terminou
Copyright   : Ana Sá Oliveira <a104437@alunos.uminho.pt>
              Sara Campos Ramalho <a72481@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2022/23.
-}
module Tarefa4_2022li1g030 (
-- * Função jogoTerminou
-- ** Função principal
jogoTerminou,
-- ** Funções auxiliares
jogadorPosicao, calculaObstaculo, verificaPosicao
) where

import LI12223


{- | A função 'jogoTerminou' verifica se a posição do jogador está fora do mapa ou se é coincidente com a posição de um obstáculo @Carro@, fazendo assim com que o jogo termine.

O jogo termina se:

* o jogador (com coordenadas @(x,y)@) estiver fora do mapa, quer pelo limite esquerdo (@x < 0@), pelo limite direito (@x >= a@, sendo @a@ a largura do mapa),
 pelo limite superior (@y < 0@) ou pelo limite inferior (@y >= length l@, sendo @l@ a lista que compreende todas as linhas constituintes do mapa).

* a posição do jogador coincidir com a posição de um obstáculo @Nenhum@ numa linha de terreno @Rio _@ ou com um obstáculo @Carro@ numa linha de terreno @Estrada _@.

Pode ser definida da seguinte forma: 

@
jogoTerminou (Jogo (Jogador (x,y)) (Mapa a l)) = x < 0 || x >= a || y < 0 || y >= length l || verificaPosicao (jogadorPosicao (x,y) (Mapa a l))
@

Para auxiliar, iremos usar as funções 'calculaObstaculo', 'jogadorPosicao' e 'verificaPosicao'.

-}

jogoTerminou :: Jogo -> Bool
jogoTerminou (Jogo (Jogador (x,y)) (Mapa a l)) = x < 0 || x >= a || y < 0 || y >= length l || verificaPosicao (jogadorPosicao (x,y) (Mapa a l))



{- | A função 'jogadorPosicao' vai fornecer à função 'calculaObstaculo' a linha com o par @(Terreno, [Obstaculo])@ em que o jogador se encontra a partir da coordenada @y@.
Juntamente com a função 'calculaObstaculo' irá calcular que obstáculo se encontra na posição do jogador.  

Define-se como:

@
jogadorPosicao (x,y) (Mapa a l) = calculaObstaculo x (l !! y)
@

-}

jogadorPosicao :: Coordenadas -> Mapa -> (Terreno, Obstaculo)
jogadorPosicao (x,y) (Mapa a l) = calculaObstaculo x (l !! y)



{- | A função auxiliar 'calculaObstaculo' recebe a coordenada @x@ do jogador e o par (Terreno, [Obstaculo] correspondente à linha em que o jogador se encontra, dados na função 'jogadorPosicao'.
A partir disso, calcula um par indicativo do terreno e do obstáculo existentes na posição atual do jogador.

Esta função define-se como: 

@
calculaObstaculo x (b,c) = (b,(c !! x))
@

-}

calculaObstaculo :: Int -> (Terreno, [Obstaculo]) -> (Terreno, Obstaculo)
calculaObstaculo x (b,c) = (b,(c !! x))


{- | A função 'verificaPosicao' indica com que pbstáculos deve coincidir a posição do jogador para que o jogo termine.
Isto é, no obstáculo @Nenhum@ se se tratar de um terreno @Rio_@, ou no obstáculo @Carro@ caso esteja num terreno @Estrada _@.

@
verificaPosicao (b,d) = case b of
                          Rio _ -> d == Nenhum
                          Estrada _ -> d == Carro 
                          Relva -> False
@

-}

verificaPosicao :: (Terreno, Obstaculo) -> Bool
verificaPosicao (b,d) = case b of
                          Rio _ -> d == Nenhum
                          Estrada _ -> d == Carro 
                          Relva -> False