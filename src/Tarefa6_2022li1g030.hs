{- |
Module      : Tarefa6_2022li1g030
Description : Aplicação gráfica completa
Copyright   : Ana Sá Oliveira <a104437@alunos.uminho.pt>
              Sara Campos Ramalho <a72481'alunos.uminho.pt>

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2022/23.
-}
module Tarefa6_2022li1g030 where

import LI12223
import Tarefa1_2022li1g030
import Tarefa2_2022li1g030
import Tarefa3_2022li1g030
import Tarefa4_2022li1g030
import Tarefa5_2022li1g030
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Picture
import System.Random hiding (Seed)
import System.Directory
import System.Exit

{-| O tipo de dados 'OpcaoMenuInicial' representa as diferentes opções apresentadas no menu inicial do jogo: 
        'NovoJogo', para iniciar um novo jogo, 
        'Continuar', para carregar um jovo previamente guardado, e 
        'Sair', para sair do jogo e fechar a janela.

@
data OpcaoMenuInicial = NovoJogo
                      | Continuar
                      | Sair
    deriving (Show, Read)
@

-}

data OpcaoMenuInicial = NovoJogo
                      | Continuar
                      | Sair
    deriving (Show, Read)

{-| O tipo de dados 'OpcaoMenuPausa' representa as opções disponíveis no menu de pausa do jogo: 
        'Retomar', para sair do menu de pausa e voltar ao mapa do jogo
        'Gravar', para guardar num ficheiro o progresso feito no mapa
        'Gravado', para confirmar que foi de facto guardado o progresso do jogo

@
data OpcaoMenuPausa = Retomar
                    | Gravar
                    | Gravado
                    | VoltarMenuInicial
    deriving (Show, Read)
@ 

-}

data OpcaoMenuPausa = Retomar
                    | Gravar
                    | Gravado
                    | VoltarMenuInicial
    deriving (Show, Read)

{-| O tipo de dados EstadoAtual representa os diferentes estados em que o jogo pode estar: 
        'MenuInicial', correspondente ao menu inicial
        'MenuPausa', correspondente ao menu de pausa
        'ModoJogo', correspondente ao modo de jogo, em que o jogador percorre o mapa
        'PerdeuJogo', correspondente ao fim de um jogo por ter perdido

@
data EstadoAtual = MenuInicial OpcaoMenuInicial
                 | MenuPausa OpcaoMenuPausa
                 | ModoJogo
                 | PerdeuJogo
    deriving (Show, Read)
@

-}

data EstadoAtual = MenuInicial OpcaoMenuInicial
                 | MenuPausa OpcaoMenuPausa
                 | ModoJogo
                 | PerdeuJogo
    deriving (Show, Read)

{-| Os tipos de dados 'Seed', 'Pontuacao', 'LinhaAtual', 'TempoDecorrido' e 'Imagens' correspondem, respetivamente: 
à seed usada para atribuir aleatoriedade ao mapa, à pontuação atingida pelo jogador, à linha do mapa em que o jogador se encontra no momento, ao tempo decorrido desde o início do jogo e ao conjunto de 'Pictures'.

@
type Seed = Int
type Pontuacao = Int
type LinhaAtual = Int
type TempoDecorrido = Int
type Imagens = [Picture]
@

-}

type Seed = Int
type Pontuacao = Int
type LinhaAtual = Int
type TempoDecorrido = Int
type Imagens = [Picture]

{-| O tipo de dados 'World' junta 'EstadoAtual', '[Seed]', 'Jogo', 'LinhaAtual', 'Pontuacao', 'TempoDecorrido' e 'Imagens' para englobar todos os elementos que vão constituir o jogo.

@
type World = (EstadoAtual, [Seed], Jogo, LinhaAtual, Pontuacao, TempoDecorrido, Imagens)
@

-}

type World = (EstadoAtual, [Seed], Jogo, LinhaAtual, Pontuacao, TempoDecorrido, Imagens)

{-| O tipo de dados 'SaveData' é constituído pelos elementos que serão usados para criar o ficheiro para guardar o progresso no jogo.

@
type SaveData = (EstadoAtual, [Seed], Jogo, LinhaAtual, Pontuacao, TempoDecorrido)
@

-}

type SaveData = (EstadoAtual, [Seed], Jogo, LinhaAtual, Pontuacao, TempoDecorrido)

{-| A função 'janela' disponibiliza a janela onde será visualizado o jogo, com o tamanho de 1600 x 900 e posição no ecrã (0,0).

@
janela :: Display
janela = InWindow "Crossy Road" (1600,900) (0,0)
@

-}

janela :: Display
janela = InWindow "Crossy Road" (1600,900) (0,0)

{-| A função 'estadoInicial', juntamente com a função auxiliar 'estendeAte' devolve o estado inicial do jogo, em que se apresenta o menu inicial com a opção de novo jogo selecionada, e o mapa inicial já preparado para ser jogado.

@
estadoInicial :: Imagens -> [Seed] -> World
estadoInicial imagens ls = (MenuInicial NovoJogo, ls, (Jogo (Jogador (8,5)) (estendeAte 5 ls (Mapa 16 [(Relva,[Arvore,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Arvore]),
                                                                                                      (Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Arvore]),
                                                                                                      (Relva,[Arvore,Nenhum,Arvore,Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Arvore,Arvore,Arvore,Nenhum,Nenhum]),
                                                                                                      (Relva,[Arvore,Arvore,Arvore,Arvore,Nenhum,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Arvore,Arvore,Arvore])]))), 0, 0, 0, imagens)
@

-}

estadoInicial :: Imagens -> [Seed] -> World
estadoInicial imagens ls = (MenuInicial NovoJogo, ls, (Jogo (Jogador (8,5)) (estendeAte 5 ls (Mapa 16 [(Relva,[Arvore,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Arvore]),
                                                                                                      (Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Arvore]),
                                                                                                      (Relva,[Arvore,Nenhum,Arvore,Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Arvore,Arvore,Arvore,Nenhum,Nenhum]),
                                                                                                      (Relva,[Arvore,Arvore,Arvore,Arvore,Nenhum,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Arvore,Arvore,Arvore])]))), 0, 0, 0, imagens)

{-| A função 'estendeAte' é utilizada na função 'estadoInicial' para criar aleatoriamente as restantes 5 linhas do mapa inicial.

@
estendeAte :: Int -> [Seed] -> Mapa -> Mapa
estendeAte 1 (s:ls) m = estendeMapa m s
estendeAte n (s:ls) m = estendeAte (n-1) ls (estendeMapa m s)  
@

-}

estendeAte :: Int -> [Seed] -> Mapa -> Mapa
estendeAte 1 (s:ls) m = estendeMapa m s
estendeAte n (s:ls) m = estendeAte (n-1) ls (estendeMapa m s)  

{-| A função 'desenhaIO', juntamente com as funções auxiliares 'desenhaMapa', 'desenhaTerreno', 'desenhaObstaculo' e 'desenhaJogador', recebe os mundos possíveis, dependendo dos estados do 'data EstadoAtual', e desenha os elementos correspondentes apresentados na janela do jogo.

@
desenhaIO :: World -> IO Picture
desenhaIO (MenuInicial NovoJogo, _, _, _, _, _, i) = return $ Pictures [fundo,translate 0 (250) (i !! 1), translate 0 50 (i !! 3),translate 0 (-75) (i !! 4),translate 0 (-200) (i !! 6)]
desenhaIO (MenuInicial Continuar, _, _, _, _, _, i) = return $ Pictures [fundo, translate 0 (250) (i !! 1), translate 0 50 (i !! 2),translate 0 (-75) (i !! 5),translate 0 (-200) (i !! 6)]
desenhaIO (MenuInicial Sair, _, _, _, _, _, i) = return $ Pictures [fundo, translate 0 (250) (i !! 1),translate 0 50 (i !! 2),translate 0 (-75) (i !! 4),translate 0 (-200) (i !! 7)]
desenhaIO (ModoJogo, _, Jogo (Jogador (x,y)) (Mapa 16 l), _, p, td, i) = return $ 
    if jogoTerminou (Jogo (Jogador (x,y)) (Mapa 16 l)) then Pictures ((desenhaMapa i 0 (Mapa 16 l)) ++ [translate (-800) (330) (color white (text (show p)))])
    else Pictures ([Pictures ((desenhaMapa i 0 (Mapa 16 l)) ++ (desenhaJogador x y))] ++ [translate (-800) (330) (color white (text (show p)))])
desenhaIO (MenuPausa Retomar, _, _, _, _, _, i) = return $ Pictures [fundo,translate 0 (250) (i !! 8), translate 0 50 (i !! 10),translate 0 (-75) (i !! 11),translate 0 (-200) (i !! 14)]
desenhaIO (MenuPausa Gravar, _, _, _, _, _, i) = return $ Pictures [fundo,translate 0 (250) (i !! 8), translate 0 50 (i !! 9),translate 0 (-75) (i !! 12),translate 0 (-200) (i !! 14)]
desenhaIO (MenuPausa Gravado, _, _, _, _, _, i) = return $ Pictures [fundo,translate 0 (250) (i !! 8), translate 0 50 (i !! 9),translate 0 (-75) (i !! 13),translate 0 (-200) (i !! 14)]
desenhaIO (MenuPausa VoltarMenuInicial, _, _, _, _, _, i) = return $ Pictures [fundo,translate 0 (250) (i !! 8), translate 0 50 (i !! 9),translate 0 (-75) (i !! 11),translate 0 (-200) (i !! 15)]
desenhaIO (PerdeuJogo, _, _, _, p, _, i) = return $ Pictures ([fundoPerdeu, translate 0 (250) (i !! 16), translate 0 0 (i !! 17), translate (-50) (-200) (i !! 18)] ++ [translate (100) (-250) (color white (text (show p)))])
@

-}

desenhaIO :: World -> IO Picture
desenhaIO (MenuInicial NovoJogo, _, _, _, _, _, i) = return $ Pictures [fundo,translate 0 (250) (i !! 1), translate 0 50 (i !! 3),translate 0 (-75) (i !! 4),translate 0 (-200) (i !! 6)]
desenhaIO (MenuInicial Continuar, _, _, _, _, _, i) = return $ Pictures [fundo, translate 0 (250) (i !! 1), translate 0 50 (i !! 2),translate 0 (-75) (i !! 5),translate 0 (-200) (i !! 6)]
desenhaIO (MenuInicial Sair, _, _, _, _, _, i) = return $ Pictures [fundo, translate 0 (250) (i !! 1),translate 0 50 (i !! 2),translate 0 (-75) (i !! 4),translate 0 (-200) (i !! 7)]
desenhaIO (ModoJogo, _, Jogo (Jogador (x,y)) (Mapa 16 l), _, p, td, i) = return $ 
    if jogoTerminou (Jogo (Jogador (x,y)) (Mapa 16 l)) then Pictures ((desenhaMapa i 0 (Mapa 16 l)) ++ [translate (-800) (330) (color white (text (show p)))])
    else Pictures ([Pictures ((desenhaMapa i 0 (Mapa 16 l)) ++ (desenhaJogador x y))] ++ [translate (-800) (330) (color white (text (show p)))])
desenhaIO (MenuPausa Retomar, _, _, _, _, _, i) = return $ Pictures [fundo,translate 0 (250) (i !! 8), translate 0 50 (i !! 10),translate 0 (-75) (i !! 11),translate 0 (-200) (i !! 14)]
desenhaIO (MenuPausa Gravar, _, _, _, _, _, i) = return $ Pictures [fundo,translate 0 (250) (i !! 8), translate 0 50 (i !! 9),translate 0 (-75) (i !! 12),translate 0 (-200) (i !! 14)]
desenhaIO (MenuPausa Gravado, _, _, _, _, _, i) = return $ Pictures [fundo,translate 0 (250) (i !! 8), translate 0 50 (i !! 9),translate 0 (-75) (i !! 13),translate 0 (-200) (i !! 14)]
desenhaIO (MenuPausa VoltarMenuInicial, _, _, _, _, _, i) = return $ Pictures [fundo,translate 0 (250) (i !! 8), translate 0 50 (i !! 9),translate 0 (-75) (i !! 11),translate 0 (-200) (i !! 15)]
desenhaIO (PerdeuJogo, _, _, _, p, _, i) = return $ Pictures ([fundoPerdeu, translate 0 (250) (i !! 16), translate 0 0 (i !! 17), translate (-50) (-200) (i !! 18)] ++ [translate (100) (-250) (color white (text (show p)))])

{- |A função 'desenhaMapa', que recebe imagens (lista de pictures), um número e um mapa e que retorna uma lista de pictures, desenha o mapa do jogo do world na tela do computador, na
janela do jogo que aparece no computador. 

Para desenharmos o mapa precisamos das imagens de formato bitmapa, precisamos de um mapa e do número da linha do mapa que iremos começar por desenhar (linha 0). Depois iremos desenhar as
outras linhas até apenas sobrar a lista vazia no mapa: desenhamos o mapa todo logo a função irá retornar lista vazia nesse momento.

Para desenharmos o mapa presisamos de desenhar os terrenos e os obstáculos de cada linha do mapa. Para desenhar os terrenos usamos a função auxiliar 'desenhaTerreno' e para desenhar os
obstáculos usamos a função auxiliar 'desenhaObstaculos'.

Assim, a função 'desenhaMapa' pode ser definida da seguinte forma:

@
desenhaMapa i p (Mapa 16 (h:t)) = (desenhaTerreno i p h) ++ (desenhaObstaculo 0 p h) ++ (desenhaMapa i (p+1) (Mapa 16 t))
desenhaMapa  i p (Mapa 16 []) = []
@

-}

desenhaMapa :: Imagens -> Int -> Mapa -> [Picture]
desenhaMapa i p (Mapa 16 (h:t)) = (desenhaTerreno i p (fst h)) ++ (desenhaObstaculo 0 p h) ++ (desenhaMapa i (p+1) (Mapa 16 t))
desenhaMapa i p (Mapa 16 []) = []

{- |A função 'desenhaTerreno', que recebe imagens (lista de pictures), um número e um par de terreno e de lista de obstáculos e que retorna uma lista de pictures, desenha o terreno de uma
linha. 

Para desenharmos o terreno de uma linha presisamos das imagens de formato bitmapa (mais em particular a primeira picture da lista, "estrada", que representa o terreno Estrada), precisamos
do número da linha do mapa do terreno (para saber onde iremos colocar as pictures na tela do computador) e precisamos do terreno para sabermos se é uma estrada (uma picture), uma relva 
(outra picture) ou um rio (outra picture).

Assim, para cada terreno apresentamos a picture correspondente que se irá posicionar no mapa em função do número da linha. 

Assim, a função 'desenhaTerreno' pode ser definida da seguinte forma:

@
desenhaTerreno i p ter = case ter of Rio _ -> [color azulRio (polygon [((-800),350+(-100)*(fromIntegral p)),((-800),450+(-100)*(fromIntegral p)),(800,450+(-100)*(fromIntegral p)),(800,350+(-100)*(fromIntegral p))])]
                                     Estrada _ -> [translate 0 (400 + (-100)*(fromIntegral p)) (head i)]
                                     Relva -> [color verdeRelva (polygon [((-800),350+(-100)*(fromIntegral p)),((-800),450+(-100)*(fromIntegral p)),(800,450+(-100)*(fromIntegral p)),(800,350+(-100)*(fromIntegral p))])]
@

-}

desenhaTerreno :: Imagens -> Int -> Terreno -> [Picture]
desenhaTerreno i p ter = case ter of Rio _ -> [color azulRio (polygon [((-800),350+(-100)*(fromIntegral p)),((-800),450+(-100)*(fromIntegral p)),(800,450+(-100)*(fromIntegral p)),(800,350+(-100)*(fromIntegral p))])]
                                     Estrada _ -> [translate 0 (400 + (-100)*(fromIntegral p)) (head i)]
                                     Relva -> [color verdeRelva (polygon [((-800),350+(-100)*(fromIntegral p)),((-800),450+(-100)*(fromIntegral p)),(800,450+(-100)*(fromIntegral p)),(800,350+(-100)*(fromIntegral p))])]

{- |A função 'desenhaObstaculo', que recebe dois número (a abcissa e a ordenada do obstáculo) e um par de terreno e de lista de obstáculos e que retorna uma lista de pictures, desenha os
obstáculos de uma linha.

Se o terreno for Relva, se o primeiro obstáculo for Nenhum não será adicionada nenhuma picture ([]) e se o primeiro obstáculo for Arvore será adicionada uma picture de uma árvore nas 
coordenadas do obstáculo. Fazemos isto para todos os obstáculos até a lista dos obstáculos ficar vazia.

Se o terreno for Estrada e a velocidade for positiva ou nula, então iremos chamar a função auxiliar 'desenhaCarrosDireita' (quando a velocidade é positiva os carros movem-se para a direita
, logo estão virados para a direita).

Se o terreno for Estrada e a velocidade for negativa, então iremos chamar a função auxiliar 'desenhaCarrosEsquerda' (quando a velocidade é negativa os carros movem-se para a esquerda, logo
estão virados para a esquerda).

Se o terreno for Rio e a velocidade for positiva ou nula, então iremos chamar a função auxiliar 'desenhaTroncosDireita' (quando a velocidade é positiva os troncos movem-se para a direita
, logo estão virados para a direita).

Se o terreno for Rio e a velocidade for negativa, então iremos chamar a função auxiliar 'desenhaTroncosEsquerda' (quando a velocidade é negativa os troncos movem-se para a esquerda, logo
estão virados para a esquerda).

Assim, a função 'desenhaObstaculo' pode ser definida da seguinte forma:

@
desenhaObstaculo n1 n2 (Estrada v,(h:t)) | v>=0 = desenhaCarrosDireita h n1 n2 (h:t)
                                         | v<0 = desenhaCarrosEsquerda (last t) n1 n2 (h:t)
desenhaObstaculo n1 n2 (Rio v,(h:t)) | v>=0 = desenhaTroncosDireita h n1 n2 (h:t)
                                     | v<0 = desenhaTroncosEsquerda (last t) n1 n2 (h:t)
desenhaObstaculo n1 n2 (Relva,[]) = []         
desenhaObstaculo n1 n2 (Relva,(h:t)) = case h of Nenhum -> (desenhaObstaculo (n1 + 1) n2 (Relva,t))
                                                 Arvore -> [color (dark (dark green)) (polygon [(((-795)+(100*(fromIntegral n1))),390+(-100)*(fromIntegral n2)),(((-795)+(100)*(fromIntegral n1)),445+(-100)*(fromIntegral n2)),(((-705)+(100*(fromIntegral n1))),445+(-100)*(fromIntegral n2)),(((-705)+(100*(fromIntegral n1))),390+(-100)*(fromIntegral n2))])] ++ [color (dark castanhoTronco) (polygon [(((-780)+(100*(fromIntegral n1))),355+(-100)*(fromIntegral n2)),(((-770)+(100)*(fromIntegral n1)),390+(-100)*(fromIntegral n2)),(((-730)+(100*(fromIntegral n1))),390+(-100)*(fromIntegral n2)),(((-720)+(100*(fromIntegral n1))),355+(-100)*(fromIntegral n2))])] ++ (desenhaObstaculo (n1 + 1) n2 (Relva,t))
@

-}

desenhaObstaculo :: Int -> Int -> (Terreno,[Obstaculo]) -> [Picture]
desenhaObstaculo n1 n2 (Estrada v,(h:t)) | v>=0 = desenhaCarrosDireita h n1 n2 (h:t)
                                         | v<0 = desenhaCarrosEsquerda (last t) n1 n2 (h:t)
desenhaObstaculo n1 n2 (Rio v,(h:t)) | v>=0 = desenhaTroncosDireita h n1 n2 (h:t)
                                     | v<0 = desenhaTroncosEsquerda (last t) n1 n2 (h:t)
desenhaObstaculo n1 n2 (Relva,[]) = []         
desenhaObstaculo n1 n2 (Relva,(h:t)) = case h of Nenhum -> (desenhaObstaculo (n1 + 1) n2 (Relva,t))
                                                 Arvore -> [color (dark (dark green)) (polygon [(((-795)+(100*(fromIntegral n1))),390+(-100)*(fromIntegral n2)),(((-795)+(100)*(fromIntegral n1)),445+(-100)*(fromIntegral n2)),(((-705)+(100*(fromIntegral n1))),445+(-100)*(fromIntegral n2)),(((-705)+(100*(fromIntegral n1))),390+(-100)*(fromIntegral n2))])] ++ [color (dark castanhoTronco) (polygon [(((-780)+(100*(fromIntegral n1))),355+(-100)*(fromIntegral n2)),(((-770)+(100)*(fromIntegral n1)),390+(-100)*(fromIntegral n2)),(((-730)+(100*(fromIntegral n1))),390+(-100)*(fromIntegral n2)),(((-720)+(100*(fromIntegral n1))),355+(-100)*(fromIntegral n2))])] ++ (desenhaObstaculo (n1 + 1) n2 (Relva,t))

{- |A função 'desenhaCarrosDireita', que recebe um obstáculo, dois números (abcissa e ordenada do obstáculo) e uma lista de obstáculos e que retorna uma lista de pictures, desenha os 
carros virados para a direita, os carros que se movimentam para a direita.

Assim, a função 'desenhaCarrosDireita' pode ser definida da seguinte forma:

@
desenhaCarrosDireita o n1 n2 [] = []
desenhaCarrosDireita o n1 n2 (h:t) = case h of Nenhum -> (desenhaCarrosDireita o (n1 + 1) n2 t)
                                               Carro | t /= [] && head t /= Carro -> [color red (polygon [(((-800)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2)),(((-800)+(100)*(fromIntegral n1)),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2))])] ++ [color black (polygon [(((-790)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2)),(((-790)+(100)*(fromIntegral n1)),380+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),380+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2))])] ++ [color black (polygon [(((-740)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2)),(((-740)+(100)*(fromIntegral n1)),380+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),380+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2))])] ++ [color white (polygon [(((-740)+(100*(fromIntegral n1))),400+(-100)*(fromIntegral n2)),(((-740)+(100)*(fromIntegral n1)),430+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),430+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),400+(-100)*(fromIntegral n2))])] ++ (desenhaCarrosDireita o (n1 + 1) n2 t)
                                                     | t /= [] && head t == Carro -> [color (dark blue) (polygon [(((-800)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2)),(((-800)+(100)*(fromIntegral n1)),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2))])] ++ [color black (polygon [(((-790)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2)),(((-790)+(100)*(fromIntegral n1)),380+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),380+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2))])] ++ [color black (polygon [(((-740)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2)),(((-740)+(100)*(fromIntegral n1)),380+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),380+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2))])] ++ (desenhaCarrosDireita o (n1 + 1) n2 t)
                                                     | t == [] && o /= Carro -> [color red (polygon [(((-800)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2)),(((-800)+(100)*(fromIntegral n1)),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2))])] ++ [color black (polygon [(((-790)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2)),(((-790)+(100)*(fromIntegral n1)),380+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),380+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2))])] ++ [color black (polygon [(((-740)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2)),(((-740)+(100)*(fromIntegral n1)),380+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),380+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2))])] ++ [color white (polygon [(((-740)+(100*(fromIntegral n1))),400+(-100)*(fromIntegral n2)),(((-740)+(100)*(fromIntegral n1)),430+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),430+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),400+(-100)*(fromIntegral n2))])] ++ (desenhaCarrosDireita o (n1 + 1) n2 t)
                                                     | t == [] && o == Carro -> [color (dark blue) (polygon [(((-800)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2)),(((-800)+(100)*(fromIntegral n1)),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2))])] ++ [color black (polygon [(((-790)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2)),(((-790)+(100)*(fromIntegral n1)),380+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),380+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2))])] ++ [color black (polygon [(((-740)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2)),(((-740)+(100)*(fromIntegral n1)),380+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),380+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2))])] ++ (desenhaCarrosDireita o (n1 + 1) n2 t)
@

-}

desenhaCarrosDireita :: Obstaculo -> Int -> Int -> [Obstaculo] -> [Picture]
desenhaCarrosDireita o n1 n2 [] = []
desenhaCarrosDireita o n1 n2 (h:t) = case h of Nenhum -> (desenhaCarrosDireita o (n1 + 1) n2 t)
                                               Carro | t /= [] && head t /= Carro -> [color red (polygon [(((-800)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2)),(((-800)+(100)*(fromIntegral n1)),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2))])] ++ [color black (polygon [(((-790)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2)),(((-790)+(100)*(fromIntegral n1)),380+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),380+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2))])] ++ [color black (polygon [(((-740)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2)),(((-740)+(100)*(fromIntegral n1)),380+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),380+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2))])] ++ [color white (polygon [(((-740)+(100*(fromIntegral n1))),400+(-100)*(fromIntegral n2)),(((-740)+(100)*(fromIntegral n1)),430+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),430+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),400+(-100)*(fromIntegral n2))])] ++ (desenhaCarrosDireita o (n1 + 1) n2 t)
                                                     | t /= [] && head t == Carro -> [color (dark blue) (polygon [(((-800)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2)),(((-800)+(100)*(fromIntegral n1)),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2))])] ++ [color black (polygon [(((-790)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2)),(((-790)+(100)*(fromIntegral n1)),380+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),380+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2))])] ++ [color black (polygon [(((-740)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2)),(((-740)+(100)*(fromIntegral n1)),380+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),380+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2))])] ++ (desenhaCarrosDireita o (n1 + 1) n2 t)
                                                     | t == [] && o /= Carro -> [color red (polygon [(((-800)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2)),(((-800)+(100)*(fromIntegral n1)),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2))])] ++ [color black (polygon [(((-790)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2)),(((-790)+(100)*(fromIntegral n1)),380+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),380+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2))])] ++ [color black (polygon [(((-740)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2)),(((-740)+(100)*(fromIntegral n1)),380+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),380+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2))])] ++ [color white (polygon [(((-740)+(100*(fromIntegral n1))),400+(-100)*(fromIntegral n2)),(((-740)+(100)*(fromIntegral n1)),430+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),430+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),400+(-100)*(fromIntegral n2))])] ++ (desenhaCarrosDireita o (n1 + 1) n2 t)
                                                     | t == [] && o == Carro -> [color (dark blue) (polygon [(((-800)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2)),(((-800)+(100)*(fromIntegral n1)),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2))])] ++ [color black (polygon [(((-790)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2)),(((-790)+(100)*(fromIntegral n1)),380+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),380+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2))])] ++ [color black (polygon [(((-740)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2)),(((-740)+(100)*(fromIntegral n1)),380+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),380+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2))])] ++ (desenhaCarrosDireita o (n1 + 1) n2 t)

{- |A função 'desenhaCarrosEsquerda', que recebe um obstáculo, dois números (abcissa e ordenada do obstáculo) e uma lista de obstáculos e que retorna uma lista de pictures, desenha os 
carros virados para a esquerda, os carros que se movimentam para a esquerda.

Assim, a função 'desenhaCarrosEsquerda' pode ser definida da seguinte forma:

@
desenhaCarrosEsquerda o n1 n2 [] = []
desenhaCarrosEsquerda o n1 n2 (h:t) = case h of Nenhum -> (desenhaCarrosEsquerda h (n1 + 1) n2 t)
                                                Carro | o /= Carro -> [color red (polygon [(((-800)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2)),(((-800)+(100)*(fromIntegral n1)),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2))])] ++ [color black (polygon [(((-790)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2)),(((-790)+(100)*(fromIntegral n1)),380+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),380+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2))])] ++ [color black (polygon [(((-740)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2)),(((-740)+(100)*(fromIntegral n1)),380+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),380+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2))])] ++ [color white (polygon [(((-790)+(100*(fromIntegral n1))),400+(-100)*(fromIntegral n2)),(((-790)+(100)*(fromIntegral n1)),430+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),430+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),400+(-100)*(fromIntegral n2))])] ++ (desenhaCarrosEsquerda h (n1 + 1) n2 t)
                                                      | o == Carro -> [color (dark blue) (polygon [(((-800)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2)),(((-800)+(100)*(fromIntegral n1)),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2))])] ++ [color black (polygon [(((-790)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2)),(((-790)+(100)*(fromIntegral n1)),380+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),380+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2))])] ++ [color black (polygon [(((-740)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2)),(((-740)+(100)*(fromIntegral n1)),380+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),380+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2))])] ++ (desenhaCarrosEsquerda h (n1 + 1) n2 t)
@

-}

desenhaCarrosEsquerda :: Obstaculo -> Int -> Int -> [Obstaculo] -> [Picture]
desenhaCarrosEsquerda o n1 n2 [] = []
desenhaCarrosEsquerda o n1 n2 (h:t) = case h of Nenhum -> (desenhaCarrosEsquerda h (n1 + 1) n2 t)
                                                Carro | o /= Carro -> [color red (polygon [(((-800)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2)),(((-800)+(100)*(fromIntegral n1)),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2))])] ++ [color black (polygon [(((-790)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2)),(((-790)+(100)*(fromIntegral n1)),380+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),380+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2))])] ++ [color black (polygon [(((-740)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2)),(((-740)+(100)*(fromIntegral n1)),380+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),380+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2))])] ++ [color white (polygon [(((-790)+(100*(fromIntegral n1))),400+(-100)*(fromIntegral n2)),(((-790)+(100)*(fromIntegral n1)),430+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),430+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),400+(-100)*(fromIntegral n2))])] ++ (desenhaCarrosEsquerda h (n1 + 1) n2 t)
                                                      | o == Carro -> [color (dark blue) (polygon [(((-800)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2)),(((-800)+(100)*(fromIntegral n1)),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2))])] ++ [color black (polygon [(((-790)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2)),(((-790)+(100)*(fromIntegral n1)),380+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),380+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2))])] ++ [color black (polygon [(((-740)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2)),(((-740)+(100)*(fromIntegral n1)),380+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),380+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2))])] ++ (desenhaCarrosEsquerda h (n1 + 1) n2 t)

{- |A função 'desenhaTroncosDireita', que recebe um obstáculo, dois números (abcissa e ordenada do obstáculo) e uma lista de obstáculos e que retorna uma lista de pictures, desenha os 
troncos virados para a direita, os troncos que se movimentam para a direita.

Assim, a função 'desenhaTroncosDireita' pode ser definida da seguinte forma:

@
desenhaTroncosDireita o n1 n2 [] = []
desenhaTroncosDireita o n1 n2 (h:t) = case h of Nenhum -> (desenhaTroncosDireita o (n1 + 1) n2 t)
                                                Tronco | t /= [] && head t /= Tronco -> [color castanhoTronco (polygon [(((-800)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2)),(((-800)+(100)*(fromIntegral n1)),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2))])] ++ [color castanhoTroncoClaro (polygon [(((-730)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2)),(((-730)+(100)*(fromIntegral n1)),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2))])] ++ (desenhaTroncosDireita o (n1 + 1) n2 t)
                                                       | t /= [] && head t == Tronco -> [color castanhoTronco (polygon [(((-800)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2)),(((-800)+(100)*(fromIntegral n1)),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2))])] ++ (desenhaTroncosDireita o (n1 + 1) n2 t)
                                                       | t == [] && o /= Tronco -> [color castanhoTronco (polygon [(((-800)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2)),(((-800)+(100)*(fromIntegral n1)),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2))])] ++ [color castanhoTroncoClaro (polygon [(((-730)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2)),(((-730)+(100)*(fromIntegral n1)),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2))])] ++ (desenhaTroncosDireita o (n1 + 1) n2 t)
                                                       | t == [] && o == Tronco -> [color castanhoTronco (polygon [(((-800)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2)),(((-800)+(100)*(fromIntegral n1)),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2))])] ++ (desenhaTroncosDireita o (n1 + 1) n2 t)
@

-}

desenhaTroncosDireita :: Obstaculo -> Int -> Int -> [Obstaculo] -> [Picture]
desenhaTroncosDireita o n1 n2 [] = []
desenhaTroncosDireita o n1 n2 (h:t) = case h of Nenhum -> (desenhaTroncosDireita o (n1 + 1) n2 t)
                                                Tronco | t /= [] && head t /= Tronco -> [color castanhoTronco (polygon [(((-800)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2)),(((-800)+(100)*(fromIntegral n1)),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2))])] ++ [color castanhoTroncoClaro (polygon [(((-730)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2)),(((-730)+(100)*(fromIntegral n1)),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2))])] ++ (desenhaTroncosDireita o (n1 + 1) n2 t)
                                                       | t /= [] && head t == Tronco -> [color castanhoTronco (polygon [(((-800)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2)),(((-800)+(100)*(fromIntegral n1)),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2))])] ++ (desenhaTroncosDireita o (n1 + 1) n2 t)
                                                       | t == [] && o /= Tronco -> [color castanhoTronco (polygon [(((-800)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2)),(((-800)+(100)*(fromIntegral n1)),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2))])] ++ [color castanhoTroncoClaro (polygon [(((-730)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2)),(((-730)+(100)*(fromIntegral n1)),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2))])] ++ (desenhaTroncosDireita o (n1 + 1) n2 t)
                                                       | t == [] && o == Tronco -> [color castanhoTronco (polygon [(((-800)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2)),(((-800)+(100)*(fromIntegral n1)),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2))])] ++ (desenhaTroncosDireita o (n1 + 1) n2 t)

{- |A função 'desenhaTroncosEsquerda', que recebe um obstáculo, dois números (abcissa e ordenada do obstáculo) e uma lista de obstáculos e que retorna uma lista de pictures, desenha os 
troncos virados para a esquerda, os troncos que se movimentam para a esquerda.

Assim, a função 'desenhaTroncosEsquerda' pode ser definida da seguinte forma:

@
desenhaTroncosEsquerda o n1 n2 [] = []
desenhaTroncosEsquerda o n1 n2 (h:t) = case h of Nenhum -> (desenhaTroncosEsquerda h (n1 + 1) n2 t)
                                                 Tronco | o /= Tronco -> [color castanhoTronco (polygon [(((-800)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2)),(((-800)+(100)*(fromIntegral n1)),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2))])] ++ [color castanhoTroncoClaro (polygon [(((-800)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2)),(((-800)+(100)*(fromIntegral n1)),440+(-100)*(fromIntegral n2)),(((-770)+(100*(fromIntegral n1))),440+(-100)*(fromIntegral n2)),(((-770)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2))])] ++ (desenhaTroncosEsquerda h (n1 + 1) n2 t)
                                                        | o == Tronco -> [color castanhoTronco (polygon [(((-800)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2)),(((-800)+(100)*(fromIntegral n1)),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2))])] ++ (desenhaTroncosEsquerda h (n1 + 1) n2 t)
@

-}

desenhaTroncosEsquerda :: Obstaculo -> Int -> Int -> [Obstaculo] -> [Picture]
desenhaTroncosEsquerda o n1 n2 [] = []
desenhaTroncosEsquerda o n1 n2 (h:t) = case h of Nenhum -> (desenhaTroncosEsquerda h (n1 + 1) n2 t)
                                                 Tronco | o /= Tronco -> [color castanhoTronco (polygon [(((-800)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2)),(((-800)+(100)*(fromIntegral n1)),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2))])] ++ [color castanhoTroncoClaro (polygon [(((-800)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2)),(((-800)+(100)*(fromIntegral n1)),440+(-100)*(fromIntegral n2)),(((-770)+(100*(fromIntegral n1))),440+(-100)*(fromIntegral n2)),(((-770)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2))])] ++ (desenhaTroncosEsquerda h (n1 + 1) n2 t)
                                                        | o == Tronco -> [color castanhoTronco (polygon [(((-800)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2)),(((-800)+(100)*(fromIntegral n1)),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2))])] ++ (desenhaTroncosEsquerda h (n1 + 1) n2 t)

{- |A função 'desenhaJogador', que recebe dois números inteiros (a abcissa e a ordenada do jogador) e que retorna uma lista de pictures, desenha o jogador no mapa. Nesta função, desenhamos
uma galinha que será posicionada no mapa em função da abcissa e ordenada do jogador de um jogo de um world.

Assim, a função 'desenhaJogador' pode ser definida da seguinte forma:

@
desenhaJogador x y = [color yellow (polygon [(((-790)+(100*(fromIntegral x))),(360+(-100)*(fromIntegral y))),((((-790) + 100*(fromIntegral x)),(440+(-100)*(fromIntegral y)))),(((-710)+(100*(fromIntegral x))),440+(-100)*(fromIntegral y)),(((-710)+(100*(fromIntegral x))),360+(-100)*(fromIntegral y))])] ++ [color red (polygon [(((-755)+(100*(fromIntegral x))),410+(-100)*(fromIntegral y)),(((-755)+(100)*(fromIntegral x)),450+(-100)*(fromIntegral y)),(((-745)+(100*(fromIntegral x))),450+(-100)*(fromIntegral y)),(((-745)+(100*(fromIntegral x))),410+(-100)*(fromIntegral y))])] ++ [color red (polygon [(((-780)+(100*(fromIntegral x))),350+(-100)*(fromIntegral y)),(((-770)+(100)*(fromIntegral x)),390+(-100)*(fromIntegral y)),(((-730)+(100*(fromIntegral x))),390+(-100)*(fromIntegral y)),(((-720)+(100*(fromIntegral x))),350+(-100)*(fromIntegral y))])] ++ [color black (polygon [(((-790)+(100*(fromIntegral x))),420+(-100)*(fromIntegral y)),(((-790)+(100)*(fromIntegral x)),440+(-100)*(fromIntegral y)),(((-770)+(100*(fromIntegral x))),440+(-100)*(fromIntegral y)),(((-770)+(100*(fromIntegral x))),420+(-100)*(fromIntegral y))])] ++ [color black (polygon [(((-730)+(100*(fromIntegral x))),420+(-100)*(fromIntegral y)),(((-730)+(100)*(fromIntegral x)),440+(-100)*(fromIntegral y)),(((-710)+(100*(fromIntegral x))),440+(-100)*(fromIntegral y)),(((-710)+(100*(fromIntegral x))),420+(-100)*(fromIntegral y))])]
@

-}            

desenhaJogador :: Int -> Int -> [Picture]
desenhaJogador x y = [color yellow (polygon [(((-790)+(100*(fromIntegral x))),(360+(-100)*(fromIntegral y))),((((-790) + 100*(fromIntegral x)),(440+(-100)*(fromIntegral y)))),(((-710)+(100*(fromIntegral x))),440+(-100)*(fromIntegral y)),(((-710)+(100*(fromIntegral x))),360+(-100)*(fromIntegral y))])] ++ [color red (polygon [(((-755)+(100*(fromIntegral x))),410+(-100)*(fromIntegral y)),(((-755)+(100)*(fromIntegral x)),450+(-100)*(fromIntegral y)),(((-745)+(100*(fromIntegral x))),450+(-100)*(fromIntegral y)),(((-745)+(100*(fromIntegral x))),410+(-100)*(fromIntegral y))])] ++ [color red (polygon [(((-780)+(100*(fromIntegral x))),350+(-100)*(fromIntegral y)),(((-770)+(100)*(fromIntegral x)),390+(-100)*(fromIntegral y)),(((-730)+(100*(fromIntegral x))),390+(-100)*(fromIntegral y)),(((-720)+(100*(fromIntegral x))),350+(-100)*(fromIntegral y))])] ++ [color black (polygon [(((-790)+(100*(fromIntegral x))),420+(-100)*(fromIntegral y)),(((-790)+(100)*(fromIntegral x)),440+(-100)*(fromIntegral y)),(((-770)+(100*(fromIntegral x))),440+(-100)*(fromIntegral y)),(((-770)+(100*(fromIntegral x))),420+(-100)*(fromIntegral y))])] ++ [color black (polygon [(((-730)+(100*(fromIntegral x))),420+(-100)*(fromIntegral y)),(((-730)+(100)*(fromIntegral x)),440+(-100)*(fromIntegral y)),(((-710)+(100*(fromIntegral x))),440+(-100)*(fromIntegral y)),(((-710)+(100*(fromIntegral x))),420+(-100)*(fromIntegral y))])]

{-| A função 'eventoIO' atribui a cada evento as reações pretendidas, ou seja, ao pressionar das teclas direcionais e Enter, para cada estado possível. Devolve o resultado encapsulado no tipo IO.

@
eventoIO :: Event -> World -> IO World
-- Menu Inicial
eventoIO (EventKey (SpecialKey KeyEnter) Down _ _) (MenuInicial NovoJogo, seed, j, la, p, s, i) = return (ModoJogo, seed, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyDown) Down _ _) (MenuInicial NovoJogo, seed, j, la, p, s, i) = return (MenuInicial Continuar, seed, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyUp) Down _ _) (MenuInicial NovoJogo, seed, j, la, p, s, i) = return (MenuInicial Sair, seed, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyDown) Down _ _) (MenuInicial Continuar, seed, j, la, p, s, i) = return (MenuInicial Sair, seed, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyEnter) Down _ _) (MenuInicial Continuar, seed, j, la, p, s, i) = carregaJogo (MenuInicial Continuar, seed, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyEnter) Down _ _) (MenuInicial Sair, seed, j, la, p, s, i) =  exitSuccess
eventoIO (EventKey (SpecialKey KeyDown) Down _ _) (MenuInicial Sair, seed, j, la, p, s, i) = return (MenuInicial NovoJogo, seed, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyUp) Down _ _) (MenuInicial Sair, seed, j, la, p, s, i) = return (MenuInicial Continuar, seed, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyUp) Down _ _) (MenuInicial Continuar, seed, j, la, p, s, i) = return (MenuInicial NovoJogo, seed, j, la, p, s, i)
-- Perdeu Jogo
eventoIO (EventKey (SpecialKey KeyEnter) Down _ _) (PerdeuJogo, _, j, la, p, s, i) = do
    seed1 <- randomIO
    seed2 <- randomIO
    seed3 <- randomIO
    seed4 <- randomIO
    seed5 <- randomIO
    return (estadoInicial i [seed1, seed2, seed3, seed4, seed5])
-- Modo Jogo
eventoIO (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo, seed, j, la, p, s, i) | jogoTerminou j = removeJogoGuardado (PerdeuJogo, seed, j, la, p, s, i)
                                                                        | variacaoDaOrdenada j (Move Cima) /= 0 && la+1 > p = return (ModoJogo, seed, animaJogador j (Move Cima), la+1, p+1, s, i)
                                                                        | variacaoDaOrdenada j (Move Cima) /= 0 && la+1 <= p = return (ModoJogo, seed, animaJogador j (Move Cima), la+1, p, s, i)
                                                                        | otherwise = return (ModoJogo, seed, animaJogador j (Move Cima), la, p, s, i)
eventoIO (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo, seed, j, la, p, s, i) | jogoTerminou j = removeJogoGuardado (PerdeuJogo, seed, j, la, p, s, i)
                                                                          | variacaoDaOrdenada j (Move Baixo) /= 0 = return (ModoJogo, seed, animaJogador j (Move Baixo), la-1, p, s, i) 
                                                                          | otherwise = return (ModoJogo, seed, animaJogador j (Move Baixo), la, p, s, i)
eventoIO (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo, seed, j, la, p, s, i) = 
    if jogoTerminou j then removeJogoGuardado (PerdeuJogo, seed, j, la, p, s, i) else return (ModoJogo, seed, animaJogador j (Move Esquerda), la, p, s, i)
eventoIO (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo, seed, j, la, p, s, i) = 
    if jogoTerminou j then removeJogoGuardado (PerdeuJogo, seed, j, la, p, s, i) else return (ModoJogo, seed, animaJogador j (Move Direita), la, p, s, i)
eventoIO (EventKey (SpecialKey _) Up _ _) (ModoJogo, seed, j, la, p, s, i) = if jogoTerminou j then removeJogoGuardado (PerdeuJogo, seed, j, la, p, s, i) else return (ModoJogo, seed, animaJogador j (Parado), la, p, s, i)
eventoIO (EventKey (SpecialKey KeyEnter) Down _ _) (ModoJogo, seed, j, la, p, s, i) = if jogoTerminou j then removeJogoGuardado (PerdeuJogo, seed, j, la, p, s, i) else return (MenuPausa Retomar, seed, j, la, p, s, i)
-- Menu Pausa
eventoIO (EventKey (SpecialKey KeyEnter) Down _ _) (MenuPausa Retomar, seed, j, la, p, s, i) = return (ModoJogo, seed,j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyDown) Down _ _) (MenuPausa Retomar, seed, j, la, p, s, i) = return (MenuPausa Gravar, seed, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyUp) Down _ _) (MenuPausa Retomar, seed, j, la, p, s, i) = return (MenuPausa VoltarMenuInicial, seed, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyEnter) Down _ _) (MenuPausa Gravar, seed, j, la, p, s, i) = guardaJogo (MenuPausa Gravado, seed, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyDown) Down _ _) (MenuPausa Gravar, seed, j, la, p, s, i) = return (MenuPausa VoltarMenuInicial, seed, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyUp) Down _ _) (MenuPausa Gravar, seed, j, la, p, s, i) = return (MenuPausa Retomar, seed, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyDown) Down _ _) (MenuPausa Gravado, seed, j, la, p, s, i) = return (MenuPausa VoltarMenuInicial, seed, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyUp) Down _ _) (MenuPausa Gravado, seed, j, la, p, s, i) = return (MenuPausa Retomar, seed, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyEnter) Down _ _) (MenuPausa VoltarMenuInicial, _, j, la, p, s, i) = do
    seed1 <- randomIO
    seed2 <- randomIO
    seed3 <- randomIO
    seed4 <- randomIO
    seed5 <- randomIO
    return (estadoInicial i [seed1, seed2, seed3, seed4, seed5])
eventoIO (EventKey (SpecialKey KeyUp) Down _ _) (MenuPausa VoltarMenuInicial, seed, j, la, p, s, i) = return (MenuPausa Gravar, seed, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyDown) Down _ _) (MenuPausa VoltarMenuInicial, seed, j, la, p, s, i) = return (MenuPausa Retomar, seed, j, la, p, s, i)
eventoIO _ (ModoJogo, seed, j, la, p, s, i) = if jogoTerminou j then removeJogoGuardado (PerdeuJogo, seed, j, la, p, s, i) else return (ModoJogo, seed, animaJogador j (Parado), la, p, s, i)
eventoIO _ w = return w
@

-}

eventoIO :: Event -> World -> IO World
-- Menu Inicial
eventoIO (EventKey (SpecialKey KeyEnter) Down _ _) (MenuInicial NovoJogo, seed, j, la, p, s, i) = return (ModoJogo, seed, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyDown) Down _ _) (MenuInicial NovoJogo, seed, j, la, p, s, i) = return (MenuInicial Continuar, seed, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyUp) Down _ _) (MenuInicial NovoJogo, seed, j, la, p, s, i) = return (MenuInicial Sair, seed, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyDown) Down _ _) (MenuInicial Continuar, seed, j, la, p, s, i) = return (MenuInicial Sair, seed, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyEnter) Down _ _) (MenuInicial Continuar, seed, j, la, p, s, i) = carregaJogo (MenuInicial Continuar, seed, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyEnter) Down _ _) (MenuInicial Sair, seed, j, la, p, s, i) =  exitSuccess
eventoIO (EventKey (SpecialKey KeyDown) Down _ _) (MenuInicial Sair, seed, j, la, p, s, i) = return (MenuInicial NovoJogo, seed, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyUp) Down _ _) (MenuInicial Sair, seed, j, la, p, s, i) = return (MenuInicial Continuar, seed, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyUp) Down _ _) (MenuInicial Continuar, seed, j, la, p, s, i) = return (MenuInicial NovoJogo, seed, j, la, p, s, i)
-- Perdeu Jogo
eventoIO (EventKey (SpecialKey KeyEnter) Down _ _) (PerdeuJogo, _, j, la, p, s, i) = do
    seed1 <- randomIO
    seed2 <- randomIO
    seed3 <- randomIO
    seed4 <- randomIO
    seed5 <- randomIO
    return (estadoInicial i [seed1, seed2, seed3, seed4, seed5])
-- Modo Jogo
eventoIO (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo, seed, j, la, p, s, i) | jogoTerminou j = removeJogoGuardado (PerdeuJogo, seed, j, la, p, s, i)
                                                                        | variacaoDaOrdenada j (Move Cima) /= 0 && la+1 > p = return (ModoJogo, seed, animaJogador j (Move Cima), la+1, p+1, s, i)
                                                                        | variacaoDaOrdenada j (Move Cima) /= 0 && la+1 <= p = return (ModoJogo, seed, animaJogador j (Move Cima), la+1, p, s, i)
                                                                        | otherwise = return (ModoJogo, seed, animaJogador j (Move Cima), la, p, s, i)
eventoIO (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo, seed, j, la, p, s, i) | jogoTerminou j = removeJogoGuardado (PerdeuJogo, seed, j, la, p, s, i)
                                                                          | variacaoDaOrdenada j (Move Baixo) /= 0 = return (ModoJogo, seed, animaJogador j (Move Baixo), la-1, p, s, i) 
                                                                          | otherwise = return (ModoJogo, seed, animaJogador j (Move Baixo), la, p, s, i)
eventoIO (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo, seed, j, la, p, s, i) = 
    if jogoTerminou j then removeJogoGuardado (PerdeuJogo, seed, j, la, p, s, i) else return (ModoJogo, seed, animaJogador j (Move Esquerda), la, p, s, i)
eventoIO (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo, seed, j, la, p, s, i) = 
    if jogoTerminou j then removeJogoGuardado (PerdeuJogo, seed, j, la, p, s, i) else return (ModoJogo, seed, animaJogador j (Move Direita), la, p, s, i)
eventoIO (EventKey (SpecialKey _) Up _ _) (ModoJogo, seed, j, la, p, s, i) = if jogoTerminou j then removeJogoGuardado (PerdeuJogo, seed, j, la, p, s, i) else return (ModoJogo, seed, animaJogador j (Parado), la, p, s, i)
eventoIO (EventKey (SpecialKey KeyEnter) Down _ _) (ModoJogo, seed, j, la, p, s, i) = if jogoTerminou j then removeJogoGuardado (PerdeuJogo, seed, j, la, p, s, i) else return (MenuPausa Retomar, seed, j, la, p, s, i)
-- Menu Pausa
eventoIO (EventKey (SpecialKey KeyEnter) Down _ _) (MenuPausa Retomar, seed, j, la, p, s, i) = return (ModoJogo, seed,j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyDown) Down _ _) (MenuPausa Retomar, seed, j, la, p, s, i) = return (MenuPausa Gravar, seed, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyUp) Down _ _) (MenuPausa Retomar, seed, j, la, p, s, i) = return (MenuPausa VoltarMenuInicial, seed, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyEnter) Down _ _) (MenuPausa Gravar, seed, j, la, p, s, i) = guardaJogo (MenuPausa Gravado, seed, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyDown) Down _ _) (MenuPausa Gravar, seed, j, la, p, s, i) = return (MenuPausa VoltarMenuInicial, seed, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyUp) Down _ _) (MenuPausa Gravar, seed, j, la, p, s, i) = return (MenuPausa Retomar, seed, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyDown) Down _ _) (MenuPausa Gravado, seed, j, la, p, s, i) = return (MenuPausa VoltarMenuInicial, seed, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyUp) Down _ _) (MenuPausa Gravado, seed, j, la, p, s, i) = return (MenuPausa Retomar, seed, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyEnter) Down _ _) (MenuPausa VoltarMenuInicial, _, j, la, p, s, i) = do
    seed1 <- randomIO
    seed2 <- randomIO
    seed3 <- randomIO
    seed4 <- randomIO
    seed5 <- randomIO
    return (estadoInicial i [seed1, seed2, seed3, seed4, seed5])
eventoIO (EventKey (SpecialKey KeyUp) Down _ _) (MenuPausa VoltarMenuInicial, seed, j, la, p, s, i) = return (MenuPausa Gravar, seed, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyDown) Down _ _) (MenuPausa VoltarMenuInicial, seed, j, la, p, s, i) = return (MenuPausa Retomar, seed, j, la, p, s, i)
eventoIO _ (ModoJogo, seed, j, la, p, s, i) = if jogoTerminou j then removeJogoGuardado (PerdeuJogo, seed, j, la, p, s, i) else return (ModoJogo, seed, animaJogador j (Parado), la, p, s, i)
eventoIO _ w = return w

{-| A função 'tempoIO' é uma função executada a cada frame, que cria uma nova seed a cada chamada, e atribui essa seed como argumento à função pura 'tempo'. Devolve o resultado encapsulado no tipo IO, de forma a cumprir o tipo estabelecido (IO World).

@
tempoIO :: Float -> World -> IO World
tempoIO n w = do
    seed <- randomIO
    return (tempo n seed w)
@

-}

tempoIO :: Float -> World -> IO World
tempoIO n w = do
    seed <- randomIO
    return (tempo n seed w)

{- |A função 'tempo', que recebe um float (que representa os segundos passados), um número inteiro (um número aleátorio) e um world e que retorna um world, é a função que muda o world em
função do tempo, em função dos segundos que se passaram. Como o terceiro parâmetro da função playIO é 1, ou seja, como o frame rate é 1, isto significa que a função 'tempo' será chamada a 
cada um novo segundo, ou seja, de um em um segundo.

Assim, a cada segundo a função 'tempo' verifica se o jogo terminou através da função 'jogoTerminou' e se tal aconteceu retorna um world em que o EstadoAtual é PerdeuJogo e em que o
TempoDecorrido aumenta um valor (passou um segundo). Se o jogo não tiver terminado, a função tempo irá animar o jogo do world ou irá deslizar e animar o jogo do world. Se passou um número
par de segundos, ou seja, se mod td 2 é igual a 0, então iremos apenas animar o jogo. Se passou um número ímpar de segundos, ou seja, se mod td 2 é desigual a 0, então iremos deslizar e
animar o jogo. Deste modo, animamos o jogo a cada um segundo e deslizamos o jogo a cada dois segundos. 

Assim, a função 'tempo' pode ser definida da seguinte forma:

@
tempo n seed (ModoJogo, s, (Jogo (Jogador (x,y)) m), la, p, td, i) = 
    if jogoTerminou (Jogo (Jogador (x,y)) m) 
        then (PerdeuJogo, s, (Jogo (Jogador (x,y)) m), la, p, td+1, i)
        -- deslizar jogo apenas quando o tempo decorrido é ímpar
        else if mod td 2 == 0 
                then (ModoJogo, s, animaJogo (Jogo (Jogador (x,y)) m) Parado, la, p, td+1, i)
                else (ModoJogo, s, deslizaJogo seed (animaJogo (Jogo (Jogador (x,y)) m) Parado), la, p, td+1, i)
tempo _ _ w = w
@

-}

tempo :: Float -> Int -> World -> World
tempo n seed (ModoJogo, s, (Jogo (Jogador (x,y)) m), la, p, td, i) = 
    if jogoTerminou (Jogo (Jogador (x,y)) m) 
        then (PerdeuJogo, s, (Jogo (Jogador (x,y)) m), la, p, td+1, i)
        -- deslizar jogo apenas quando o tempo decorrido é ímpar
        else if mod td 2 == 0 
                then (ModoJogo, s, animaJogo (Jogo (Jogador (x,y)) m) Parado, la, p, td+1, i)
                else (ModoJogo, s, deslizaJogo seed (animaJogo (Jogo (Jogador (x,y)) m) Parado), la, p, td+1, i)
tempo _ _ w = w

{-| A função 'converteWorldParaSaveData' recebe dados do tipo 'World' e converte-os para dados do tipo 'SaveData' para poderem ser usados na função 'guardaJogo'. No processo retira as 'Imagens' contidas no 'World'.

@
converteWorldParaSaveData :: World -> SaveData
converteWorldParaSaveData (m, seed, j, la, p, s, i) = (m, seed, j, la, p, s)
@

-}

converteWorldParaSaveData :: World -> SaveData
converteWorldParaSaveData (m, seed, j, la, p, s, i) = (m, seed, j, la, p, s)

{-| A função 'guardaJogo' escreve o conteúdo que recebe num ficheiro, de modo a guardar o progresso no jogo quando for chamada, com o auxílio da função 'converteWorldParaSaveData'.

@
guardaJogo :: World -> IO World
guardaJogo w = do 
    writeFile "crossyroad.sav" (show $ converteWorldParaSaveData w)
    return w
@

-}

guardaJogo :: World -> IO World
guardaJogo w = do 
    writeFile "crossyroad.sav" (show $ converteWorldParaSaveData w)
    return w

{-| A função 'converteSaveDataParaWorld' converte dados do tipo 'SaveData' para dados do tipo 'World', devolvendo também as 'Imagens' que devem estar incluídas no tipo 'World'. Esta função é usada na função 'carregaJogo'.

@
converteSaveDataParaWorld :: SaveData -> Imagens -> World
converteSaveDataParaWorld (m, seed, j, la, p, s) i = (m, seed, j, la, p, s, i)
@

-}

converteSaveDataParaWorld :: SaveData -> Imagens -> World
converteSaveDataParaWorld (m, seed, j, la, p, s) i = (m, seed, j, la, p, s, i)


{-| A função 'carregaJogo' verifica se existe algum ficheiro com o progresso de jogo guardado e, caso exista, lê-o e apresenta o jogo guardado, com o auxílio da função 'converteSaveDataParaWorld'.

@
carregaJogo :: World -> IO World 
carregaJogo (m, seed, j, la, p, s, i) = do
    fileExist <- doesFileExist "crossyroad.sav"
    saved <- if fileExist then readFile "crossyroad.sav"
                          else return (show (m, seed, j, la, p, s))
    return (converteSaveDataParaWorld (read saved) i)
@

-}

carregaJogo :: World -> IO World 
carregaJogo (m, seed, j, la, p, s, i) = do
    fileExist <- doesFileExist "crossyroad.sav"
    saved <- if fileExist then readFile "crossyroad.sav"
                          else return (show (m, seed, j, la, p, s))
    return (converteSaveDataParaWorld (read saved) i)

{-| A função 'removeJogoGuardado' elimina, caso exista, o ficheiro com progresso do jogo guardado, em caso de perder o jogo.

@
removeJogoGuardado :: World -> IO World
removeJogoGuardado w = do 
    fileExist <- doesFileExist "crossyroad.sav"
    if fileExist then removeFile "crossyroad.sav"
                 else return ()
    return w
@

-}

removeJogoGuardado :: World -> IO World
removeJogoGuardado w = do 
    fileExist <- doesFileExist "crossyroad.sav"
    if fileExist then removeFile "crossyroad.sav"
                 else return ()
    return w

{-| A função 'verdeRelva' será usada para atribuir a cor @light green@ a alguns elementos do jogo.

@
verdeRelva :: Color
verdeRelva = light green
@

-}

verdeRelva :: Color
verdeRelva = light green

{-| A função 'cinzaEstrada' será usada para atribuir a cor @dark (dark (greyN 0.8))@ a determinados elementos do jogo.

@
cinzaEstrada :: Color
cinzaEstrada = dark (dark (greyN 0.8))
@

-}

cinzaEstrada :: Color
cinzaEstrada = dark (dark (greyN 0.8))

{-| A função 'azulRio' será usada para atribuir a cor @light azure@ a certos elementos do jogo.

@
azulRio :: Color
azulRio = light azure
@

-}

azulRio :: Color
azulRio = light azure

{-| A função 'castanhoTronco' será usada para atribuir a cor personalizada @makeColor 0.4 0.2 0 0.9@ a alguns elementos do jogo.

@
castanhoTronco :: Color
castanhoTronco = (makeColor 0.4 0.2 0 0.9)
@

-}

castanhoTronco :: Color
castanhoTronco = (makeColor 0.4 0.2 0 0.9)

{-| A função 'castanhoTroncoClaro' será usada para atribuir a cor @light castanhoTronco@  a alguns elementos do jogo.

@
castanhoTroncoClaro :: Color
castanhoTroncoClaro = light castanhoTronco
@

-}

castanhoTroncoClaro :: Color
castanhoTroncoClaro = light castanhoTronco

{-| A função 'fundo' atribui a cor @light green@ ao fundo da janela, através da função 'verdeRelva'.

@
fundo :: Picture
fundo = color verdeRelva (polygon [((-800),(-450)),((-800),450),(800,450),(800,(-450))])
@

-}

fundo :: Picture
fundo = color verdeRelva (polygon [((-800),(-450)),((-800),450),(800,450),(800,(-450))])

{-| A função 'fundoPerdeu' atribui a cor @light (light red)@ ao fundo do ecrã do modo PerdeuJogo.

@
fundoPerdeu :: Picture
fundoPerdeu = color (light red) (polygon [((-800),(-450)),((-800),450),(800,450),(800,(-450))])
@

-}

fundoPerdeu :: Picture
fundoPerdeu = color (light red) (polygon [((-800),(-450)),((-800),450),(800,450),(800,(-450))])