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
import System.Random

main :: IO ()
main = do
    estrada <- loadBMP "imagem/estrada.bmp" 
    titulo <- loadBMP "imagem/titulo.bmp"
    jogar <- loadBMP "imagem/jogar.bmp"
    jogar_red <- loadBMP "imagem/jogar_red.bmp"
    continuar <- loadBMP "imagem/continuar.bmp"
    continuar_red <- loadBMP "imagem/continuar_red.bmp"
    sair <- loadBMP "imagem/sair.bmp"
    sair_red <- loadBMP "imagem/sair_red.bmp"
    pausa <- loadBMP "imagem/pausa.bmp"
    retomar <- loadBMP "imagem/retomar.bmp"
    retomar_red <- loadBMP "imagem/retomar_red.bmp"
    gravar <- loadBMP "imagem/gravar.bmp"
    gravar_red <- loadBMP "imagem/gravar_red.bmp"
    gravar_blue <- loadBMP "imagem/gravar_blue.bmp"
    voltar_mi <- loadBMP "imagem/voltar_mi.bmp"
    voltar_mi_red <- loadBMP "imagem/voltar_mi_red.bmp"
    titulo_perdeu <- loadBMP "imagem/titulo_perdeu.bmp"
    perdeu <- loadBMP "imagem/perdeu.bmp"
    total <- loadBMP "imagem/total.bmp"
    let imagens = [estrada,titulo,jogar,jogar_red,continuar,continuar_red,sair,sair_red,pausa,retomar,retomar_red,gravar,gravar_red,gravar_blue,voltar_mi,voltar_mi_red,titulo_perdeu,perdeu,total]
    seed1 <- randomIO
    seed2 <- randomIO
    seed3 <- randomIO
    seed4 <- randomIO
    seed5 <- randomIO
    playIO janela black 1 (estadoInicial imagens [seed1, seed2, seed3, seed4, seed5]) desenhaIO eventoIO tempoIO