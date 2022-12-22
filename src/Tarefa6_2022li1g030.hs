{- |
Module      : Tarefa6_2022li1g030
Description : Aplicação gráfica completa
Copyright   : Ana Sá Oliveira <a104437@alunos.uminho.pt>
              Sara Campos Ramalho <a72481@alunos.uminho.pt>

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
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Picture


data Opcao = Jogar
            | Sair

data Menu = Opcoes Opcao
          | ModoJogo 
          | ModoPausa
          | PerdeuJogo

type Pontuacao = Int

type World = (Menu, Jogo, Pontuacao)

janela :: Display
janela = InWindow "Crossy Road" (1600,900) (0,0)

estadoInicial :: World
estadoInicial = (Opcoes Jogar, (Jogo (Jogador (8,5)) (Mapa 16 [(Relva,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Nenhum]),
                                                               (Rio (-1),[Tronco,Tronco,Tronco,Tronco,Nenhum,Nenhum,Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco,Nenhum,Nenhum,Nenhum,Nenhum]),
                                                               (Rio 1,[Nenhum,Nenhum,Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco,Nenhum,Nenhum,Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco]),
                                                               (Relva,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
                                                               (Estrada 1,[Nenhum,Carro,Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum]),
                                                               (Relva,[Arvore,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Arvore]),
                                                               (Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Arvore]),
                                                               (Relva,[Arvore,Nenhum,Arvore,Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Arvore,Arvore,Arvore,Nenhum,Nenhum]),
                                                               (Relva,[Arvore,Arvore,Arvore,Arvore,Nenhum,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Arvore,Arvore,Arvore])])), 0)

desenha :: World -> Picture
desenha (Opcoes Jogar, _, _) = Pictures [fundo,translate (-400) (150) (color green (text "Crossy Road")), translate (-150) 0 (color red (text "Jogar")),translate (-125) (-150) (color black (text "Sair"))]
desenha (Opcoes Sair, _, _) = Pictures [fundo,translate (-400) (150) (color green (text "Crossy Road")), translate (-150) 0 (color black (text "Jogar")),translate (-125) (-150) (color red (text "Sair"))]
desenha (ModoJogo, Jogo (Jogador (x,y)) (Mapa 16 l), p) = Pictures ([Pictures ((desenhaMapa 0 (Mapa 16 l)) ++ (desenhaJogador x y))] ++ [translate (-800) (330) (color white (text (show p)))])
desenha (ModoPausa, _, _) = Pictures [fundo,translate (-150) 0 (color black (text "Pausa"))]
desenha (PerdeuJogo, _, p) = Pictures [color red fundo, translate (-400) (150) (color green (text "Crossy Road")), translate (-170) 0 (color black (text "Perdeu")), translate (-250) (-150) (color black (text ("Total: " ++ show p)))]

desenhaMapa :: Int -> Mapa -> [Picture]
desenhaMapa p (Mapa 16 (h:t)) = [desenhaTerreno p h] ++ (desenhaObstaculo 0 p h) ++ (desenhaMapa (p+1) (Mapa 16 t))
desenhaMapa p (Mapa 16 []) = []

desenhaTerreno :: Int -> (Terreno,[Obstaculo]) -> Picture
desenhaTerreno p (ter,(h:t)) = case ter of Rio _ -> (color azulRio (polygon [((-800),350+(-100)*(fromIntegral p)),((-800),450+(-100)*(fromIntegral p)),(800,450+(-100)*(fromIntegral p)),(800,350+(-100)*(fromIntegral p))]))
                                           Estrada _ -> (color cinzaEstrada (polygon [((-800),350+(-100)*(fromIntegral p)),((-800),450+(-100)*(fromIntegral p)),(800,450+(-100)*(fromIntegral p)),(800,350+(-100)*(fromIntegral p))]))
                                           Relva -> (color verdeRelva (polygon [((-800),350+(-100)*(fromIntegral p)),((-800),450+(-100)*(fromIntegral p)),(800,450+(-100)*(fromIntegral p)),(800,350+(-100)*(fromIntegral p))]))

desenhaObstaculo :: Int -> Int -> (Terreno,[Obstaculo]) -> [Picture]
desenhaObstaculo n1 n2 (ter,[]) = []
desenhaObstaculo n1 n2 (ter,(h:t)) = case h of Nenhum -> (desenhaObstaculo (n1 + 1) n2 (ter,t))
                                               Tronco -> [color black (polygon [(((-800)+(100*(fromIntegral n1))),350+(-100)*(fromIntegral n2)),(((-800)+(100)*(fromIntegral n1)),450+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),450+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),350+(-100)*(fromIntegral n2))])] ++ (desenhaObstaculo (n1 + 1) n2 (ter,t))
                                               Carro -> [color red (polygon [(((-800)+(100*(fromIntegral n1))),350+(-100)*(fromIntegral n2)),(((-800)+(100)*(fromIntegral n1)),450+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),450+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),350+(-100)*(fromIntegral n2))])] ++ (desenhaObstaculo (n1 + 1) n2 (ter,t))
                                               Arvore -> [color (dark (dark green)) (polygon [(((-800)+(100*(fromIntegral n1))),350+(-100)*(fromIntegral n2)),(((-800)+(100)*(fromIntegral n1)),450+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),450+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),350+(-100)*(fromIntegral n2))])] ++ (desenhaObstaculo (n1 + 1) n2 (ter,t))

desenhaJogador :: Int -> Int -> [Picture]
desenhaJogador x y = [color yellow (polygon [(((-800)+(100*(fromIntegral x))),(350+(-100)*(fromIntegral y))),((((-800) + 100*(fromIntegral x)),(450+(-100)*(fromIntegral y)))),(((-700)+(100*(fromIntegral x))),450+(-100)*(fromIntegral y)),(((-700)+(100*(fromIntegral x))),350+(-100)*(fromIntegral y))])]

evento :: Event -> World -> World
evento (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Jogar, j, p) = (ModoJogo, j, p)
evento (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Jogar, j, p) = (Opcoes Sair, j, p)
evento (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Sair, j, p) =  error "Fim de Jogo"
evento (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Sair, j, p) = (Opcoes Jogar, j, p)
evento (EventKey (SpecialKey KeyEnter) Down _ _) (PerdeuJogo, j, p) = estadoInicial
evento (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo, j, p) = if jogoTerminou j then (PerdeuJogo, j, p) else if variacaoDaOrdenada j (Move Cima) /= 0 then (ModoJogo, animaJogador j (Move Cima), p+1) else (ModoJogo, animaJogador j (Move Cima), p)
evento (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo, j, p) = if jogoTerminou j then (PerdeuJogo, j, p) else if variacaoDaOrdenada j (Move Baixo) /= 0 then (ModoJogo, animaJogador j (Move Baixo), p-1) else (ModoJogo, animaJogador j (Move Baixo), p)
evento (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo, j, p) = if jogoTerminou j then (PerdeuJogo, j, p) else (ModoJogo, animaJogador j (Move Esquerda), p)
evento (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo, j, p) = if jogoTerminou j then (PerdeuJogo, j, p) else (ModoJogo, animaJogador j (Move Direita), p)
evento (EventKey (SpecialKey _) Up _ _) (ModoJogo, j, p) = if jogoTerminou j then (PerdeuJogo, j, p) else (ModoJogo, animaJogador j (Parado), p)
evento (EventKey (SpecialKey KeyEnter) Down _ _) (ModoJogo, j, p) = if jogoTerminou j then (PerdeuJogo, j, p) else (ModoPausa, j, p)
evento (EventKey (SpecialKey KeyEnter) Down _ _) (ModoPausa, j, p) = (ModoJogo, j, p)
evento _ w = w

tempo :: Float -> World -> World
tempo 1 (ModoJogo,(Jogo (Jogador (x,y)) (Mapa n l)), p) =
    if jogoTerminou  (Jogo (Jogador (x,y)) (Mapa n l)) then (PerdeuJogo,(Jogo (Jogador (x,y)) (Mapa n l)), p) 
    else (ModoJogo, deslizaJogo y (animaJogo  (Jogo (Jogador (x,y)) (Mapa n l)) (Parado)), p)
tempo _ w = w


verdeRelva :: Color
verdeRelva = light green

cinzaEstrada :: Color
cinzaEstrada = greyN 0.8

azulRio :: Color
azulRio = light azure

{-
terreno1 :: Picture
terreno1 = color verdeRelva (polygon [((-800),(-450)),((-800),(-350)),(800,(-350)),(800,(-450))])

terreno2 :: Picture
terreno2 = color cinzaEstrada (polygon [((-800),(-350)),((-800),(-250)),(800,(-250)),(800,(-350))])

terreno3 :: Picture
terreno3 = color azulRio (polygon [((-800),(-250)),((-800),(-150)),(800,(-150)),(800,(-250))])

terreno4 :: Picture
terreno4 = color verdeRelva (polygon [((-800),(-150)),((-800),(-50)),(800,(-50)),(800,(-150))])

terreno5 :: Picture
terreno5 = color verdeRelva (polygon [((-800),(-50)),((-800),50),(800,50),(800,(-50))])

terreno6 :: Picture
terreno6 = color azulRio (polygon [((-800),50),((-800),150),(800,150),(800,50)])

terreno7 :: Picture
terreno7 = color cinzaEstrada (polygon [((-800),150),((-800),250),(800,250),(800,150)])

terreno8 :: Picture
terreno8 = color cinzaEstrada (polygon [((-800),250),((-800),350),(800,350),(800,250)])

terreno9 :: Picture
terreno9 = color azulRio (polygon [((-800),350),((-800),450),(800,450),(800,350)])

obstaculo11 :: Picture
obstaculo11 = color red (polygon [(100,(-450)),(100,(-350)),(0,(-350)),(0,(-450))])

terrenos :: Picture
terrenos = pictures [terreno1,terreno2,terreno3,terreno4,terreno5,terreno6,terreno7,terreno8,terreno9,obstaculo11]
-}

fundo :: Picture
fundo = color white (polygon [((-800),(-450)),((-800),450),(800,450),(800,(-450))])

{-
jogar :: Picture
jogar = (color black (text "Jogar"))

sair :: Picture
sair = translate 0 (-150) (color black (text "Sair"))

perdeu :: Picture
perdeu = color black (text "Perdeu")
-}