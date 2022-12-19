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
estadoInicial = (Opcoes Jogar, (Jogo (Jogador (8,8)) (Mapa 16 [])))

desenha :: World -> Picture
desenha (Opcoes Jogar, _) = Pictures [fundo,color blue jogar,sair]
desenha (Opcoes Sair, _) = Pictures [fundo,jogar,color blue sair]
desenha (ModoJogo, jogo) = desenhaJogo (jogo)
desenha (PerdeuJogo, _) = Pictures [color red fundo,perdeu]

desenhaJogo :: Jogo -> Picture
desenhaJogo (Jogo (Jogador (x,y)) (Mapa n l)) = 

evento :: Event -> World -> World
evento =

tempo :: Float -> World -> World
tempo 15 (ModoJogo, (Jogo (Jogador (x,y)) (Mapa n l))) = (ModoJogo, deslizaJogo y (Jogo (Jogador (x,y))))

verdeRelva :: Color
verdeRelva = light green

cinzaEstrada :: Color
cinzaEstrada = greyN 0.8

azulRio :: Color
azulRio = light azure

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

fundo :: Picture
fundo = color white (polygon [((-800),(-450)),((-800),450),(800,450),((-800),450)])

jogar :: Picture
jogar = color black (text "Jogar")

sair :: Picture
sair = color black (text "Sair")

perdeu :: Picture
perdeu = color black (text "Perdeu")
