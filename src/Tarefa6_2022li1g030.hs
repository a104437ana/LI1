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

window :: Display
window = InWindow "Crossy Road" (1600,900) (0,0)

terreno1 :: Picture
terreno1 = color blue (polygon [((-800),(-450)),((-800),(-350)),(800,(-350)),(800,(-450))])

terreno2 :: Picture
terreno2 = color white (polygon [((-800),(-350)),((-800),(-250)),(800,(-250)),(800,(-350))])

terreno3 :: Picture
terreno3 = color blue (polygon [((-800),(-250)),((-800),(-150)),(800,(-150)),(800,(-250))])

terreno4 :: Picture
terreno4 = color white (polygon [((-800),(-150)),((-800),(-50)),(800,(-50)),(800,(-150))])

terreno5 :: Picture
terreno5 = color blue (polygon [((-800),(-50)),((-800),50),(800,50),(800,(-50))])

terreno6 :: Picture
terreno6 = color white (polygon [((-800),50),((-800),150),(800,150),(800,50)])

terreno7 :: Picture
terreno7 = color blue (polygon [((-800),150),((-800),250),(800,250),(800,150)])

terreno8 :: Picture
terreno8 = color white (polygon [((-800),250),((-800),350),(800,350),(800,250)])

terreno9 :: Picture
terreno9 = color blue (polygon [((-800),350),((-800),450),(800,450),(800,350)])

terrenos :: Picture
terrenos = pictures [terreno1,terreno2,terreno3,terreno4,terreno5,terreno6,terreno7,terreno8,terreno9]
