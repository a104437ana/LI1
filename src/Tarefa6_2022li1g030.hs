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
          | PerdeuJogo

type World = (Menu, Jogo)

janela :: Display
janela = InWindow "Crossy Road" (1600,900) (0,0)

estadoInicial :: World
estadoInicial = (Opcoes Jogar, (Jogo (Jogador (8,8)) (Mapa 16 [(Relva,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum])])))

desenha :: World -> Picture
desenha (Opcoes Jogar, _) = Pictures [fundo,color blue jogar,sair]
desenha (Opcoes Sair, _) = Pictures [fundo,jogar,color blue sair]
desenha (ModoJogo, Jogo (Jogador (x,y)) (Mapa 16 l)) = Pictures ((desenhaMapa 0 (Mapa 16 l)) ++ (desenhaJogador x y))
desenha (PerdeuJogo, _) = Pictures [color red fundo,perdeu]

desenhaMapa :: Int -> Mapa -> [Picture]
desenhaMapa n (Mapa 16 (h:t)) = [desenhaTerreno n h] ++ (desenhaObstaculo 0 n h) ++ (desenhaMapa (n+1) (Mapa 16 t))
desenhaMapa n (Mapa 16 []) = []

desenhaTerreno :: Int -> (Terreno,[Obstaculo]) -> Picture
desenhaTerreno n (ter,(h:t)) = case ter of Rio _ -> (color azulRio (polygon [((-800),350+(-100)*(fromIntegral n)),((-800),450+(-100)*(fromIntegral n)),(800,450+(-100)*(fromIntegral n)),(800,350+(-100)*(fromIntegral n))]))
                                           Estrada _ -> (color cinzaEstrada (polygon [((-800),350+(-100)*(fromIntegral n)),((-800),450+(-100)*(fromIntegral n)),(800,450+(-100)*(fromIntegral n)),(800,350+(-100)*(fromIntegral n))]))
                                           Relva -> (color verdeRelva (polygon [((-800),350+(-100)*(fromIntegral n)),((-800),450+(-100)*(fromIntegral n)),(800,450+(-100)*(fromIntegral n)),(800,350+(-100)*(fromIntegral n))]))

desenhaObstaculo :: Int -> Int -> (Terreno,[Obstaculo]) -> [Picture]
desenhaObstaculo n1 n2 (ter,[]) = []
desenhaObstaculo n1 n2 (ter,(h:t)) = case h of Nenhum -> (desenhaObstaculo (n1 + 1) n2 (ter,t))
                                               Tronco -> [color black (polygon [(((-800)+(100*(fromIntegral n1))),350+(-100)*(fromIntegral n2)),(((-800)+(-100)*(fromIntegral n1)),450+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),450+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),350+(-100)*(fromIntegral n2))])] ++ (desenhaObstaculo (n1 + 1) n2 (ter,t))
                                               Carro -> [color red (polygon [(((-800)+(100*(fromIntegral n1))),350+(-100)*(fromIntegral n2)),(((-800)+(-100)*(fromIntegral n1)),450+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),450+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),350+(-100)*(fromIntegral n2))])] ++ (desenhaObstaculo (n1 + 1) n2 (ter,t))
                                               Arvore -> [color green (polygon [(((-800)+(100*(fromIntegral n1))),350+(-100)*(fromIntegral n2)),(((-800)+(-100)*(fromIntegral n1)),450+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),450+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),350+(-100)*(fromIntegral n2))])] ++ (desenhaObstaculo (n1 + 1) n2 (ter,t))

desenhaJogador :: Int -> Int -> [Picture]
desenhaJogador x y = [color yellow (polygon [(((-800)+(100*(fromIntegral x))),(350+(-100)*(fromIntegral y))),((((-800) + 100*(fromIntegral x)),(450+(-100)*(fromIntegral y)))),(((-700)+(100*(fromIntegral x))),450+(-100)*(fromIntegral y)),(((-700)+(100*(fromIntegral x))),350+(-100)*(fromIntegral y))])]

evento :: Event -> World -> World
evento (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Jogar, j) = (ModoJogo, j) -- primeiroJogo é uma função que devolve o jogo com o mapa composto pelas 3 primeiras linhas pré-definidas e as 6 restantes geradas randomly
evento (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Sair, j) =  error "Fim de Jogo"-- quando chamar esta função, associar o Nothing a fechar a janela
evento (EventKey (SpecialKey KeyEnter) Down _ _) (PerdeuJogo, j) = estadoInicial
evento (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo, j) = (ModoJogo, animaJogo j (Move Cima))
evento (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo, j) = (ModoJogo, animaJogo j (Move Baixo))
evento (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo, j) = (ModoJogo, animaJogo j (Move Esquerda))
evento (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo, j) = (ModoJogo, animaJogo j (Move Direita))
evento _ s = s  -- ignora qualquer outro evento

tempo :: Float -> World -> World
tempo 3 (ModoJogo, (Jogo (Jogador (x,y)) (Mapa n l))) = (ModoJogo, deslizaJogo y (Jogo (Jogador (x,y)) (Mapa n l)))
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

jogar :: Picture
jogar = (color black (text "Jogar"))

sair :: Picture
sair = translate 0 (-150) (color black (text "Sair"))

perdeu :: Picture
perdeu = color black (text "Perdeu")
