module Main where

import LI12223
import Tarefa1_2022li1g030
import Tarefa2_2022li1g030
import Tarefa3_2022li1g030
import Tarefa4_2022li1g030
import Tarefa5_2022li1g030
import Tarefa6_2022li1g030
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do 
    estrada <- loadBMP "imagem/estrada.bmp"
    let imagens = [estrada]
    playIO janela black 1 (estadoInicial imagens) desenhaIO eventoIO tempoIO