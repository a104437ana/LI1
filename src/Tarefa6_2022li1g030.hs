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
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Picture
import System.Random
import System.Directory


data OpcaoMenuInicial = NovoJogo
                      | Continuar
                      | Sair
    deriving (Show, Read)

data OpcaoMenuPausa = Retomar
                    | Gravar
                    | Gravado
                    | VoltarMenuInicial
    deriving (Show, Read)

data EstadoAtual = MenuInicial OpcaoMenuInicial
                 | MenuPausa OpcaoMenuPausa
                 | ModoJogo
                 | PerdeuJogo
    deriving (Show, Read)

type Pontuacao = Int
type LinhaAtual = Int
type TempoDecorrido = Int
type World = (EstadoAtual, Jogo, LinhaAtual, Pontuacao, TempoDecorrido)

janela :: Display
janela = InWindow "Crossy Road" (1600,900) (0,0)

estadoInicial :: World
estadoInicial = (MenuInicial NovoJogo, (Jogo (Jogador (8,5)) (Mapa 16 [(Relva,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Nenhum]),
                                                                       (Rio (-1),[Tronco,Tronco,Tronco,Tronco,Nenhum,Nenhum,Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco,Nenhum,Nenhum,Nenhum,Nenhum]),
                                                                       (Rio 1,[Nenhum,Nenhum,Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco,Nenhum,Nenhum,Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco]),
                                                                       (Relva,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
                                                                       (Estrada 1,[Nenhum,Carro,Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum]),
                                                                       (Relva,[Arvore,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Arvore]),
                                                                       (Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Arvore]),
                                                                       (Relva,[Arvore,Nenhum,Arvore,Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Arvore,Arvore,Arvore,Nenhum,Nenhum]),
                                                                       (Relva,[Arvore,Arvore,Arvore,Arvore,Nenhum,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Arvore,Arvore,Arvore])])), 0, 0, 0)

desenhaIO :: World -> IO Picture
desenhaIO w = do
    return (desenha w)

desenha :: World -> Picture
desenha (MenuInicial NovoJogo, _, _, _, _) = Pictures [fundo, translate (-600) (150) (scale 1.5 1.5 (color green (text "Crossy Road"))), translate (-150) 0 (color red (text "Jogar")),translate (-250) (-150) (color black (text "Continuar")),translate (-125) (-300) (color black (text "Sair"))]
desenha (MenuInicial Continuar, _, _, _, _) = Pictures [fundo, translate (-600) (150) (scale 1.5 1.5 (color green (text "Crossy Road"))), translate (-150) 0 (color black (text "Jogar")),translate (-250) (-150) (color red (text "Continuar")),translate (-125) (-300) (color black (text "Sair"))]
desenha (MenuInicial Sair, _, _, _, _) = Pictures [fundo, translate (-600) (150) (scale 1.5 1.5 (color green (text "Crossy Road"))), translate (-150) 0 (color black (text "Jogar")),translate (-250) (-150) (color black (text "Continuar")),translate (-125) (-300) (color red (text "Sair"))]
desenha (ModoJogo, Jogo (Jogador (x,y)) (Mapa 16 l), _, p, td) = 
    if jogoTerminou (Jogo (Jogador (x,y)) (Mapa 16 l)) then Pictures ((desenhaMapa 0 (Mapa 16 l)) ++ [translate (-800) (330) (color black (text (show p)))])
    else Pictures ([Pictures ((desenhaMapa 0 (Mapa 16 l)) ++ (desenhaJogador x y))] ++ [translate (-800) (330) (color black (text (show p)))])
desenha (MenuPausa Retomar, _, _, _, _) = Pictures [fundo, translate (-265) (150) (scale 1.5 1.5 (color blue (text "Pausa"))), translate (-235) 0 (color red (text "Retomar")),translate (-200) (-125) (color black (text "Gravar")),translate (-700) (-250) (color black (text "Voltar ao Menu Inicial"))]
desenha (MenuPausa Gravar, _, _, _, _) = Pictures [fundo, translate (-265) (150) (scale 1.5 1.5 (color blue (text "Pausa"))), translate (-235) 0 (color black (text "Retomar")),translate (-200) (-125) (color red (text "Gravar")),translate (-700) (-250) (color black (text "Voltar ao Menu Inicial"))]
desenha (MenuPausa Gravado, _, _, _, _) = Pictures [fundo, translate (-265) (150) (scale 1.5 1.5 (color blue (text "Pausa"))), translate (-235) 0 (color black (text "Retomar")),translate (-200) (-125) (color green (text "Gravar")),translate (-700) (-250) (color black (text "Voltar ao Menu Inicial"))]
desenha (MenuPausa VoltarMenuInicial, _, _, _, _) = Pictures [fundo, translate (-265) (150) (scale 1.5 1.5 (color blue (text "Pausa"))), translate (-235) 0 (color black (text "Retomar")),translate (-200) (-125) (color black (text "Gravar")),translate (-700) (-250) (color red (text "Voltar ao Menu Inicial"))]
desenha (PerdeuJogo, _, _, p, _) = Pictures [fundoPerdeu, translate (-600) (150) (scale 1.5 1.5 (color green (text "Crossy Road"))), translate (-200) 0 (color black (text "Perdeu")), translate (-250) (-150) (color black (text ("Total: " ++ show p)))]

desenhaMapa :: Int -> Mapa -> [Picture]
desenhaMapa p (Mapa 16 (h:t)) = (desenhaTerreno p h) ++ (desenhaObstaculo 0 p h) ++ (desenhaMapa (p+1) (Mapa 16 t))
desenhaMapa p (Mapa 16 []) = []

desenhaTerreno :: Int -> (Terreno,[Obstaculo]) -> [Picture]
desenhaTerreno p (ter,(h:t)) = case ter of Rio _ -> [color azulRio (polygon [((-800),350+(-100)*(fromIntegral p)),((-800),450+(-100)*(fromIntegral p)),(800,450+(-100)*(fromIntegral p)),(800,350+(-100)*(fromIntegral p))])]
                                           Estrada _ -> [color cinzaEstrada (polygon [((-800),350+(-100)*(fromIntegral p)),((-800),450+(-100)*(fromIntegral p)),(800,450+(-100)*(fromIntegral p)),(800,350+(-100)*(fromIntegral p))])] ++ [color white (polygon [((-800),440+(-100)*(fromIntegral p)),((-800),450+(-100)*(fromIntegral p)),(800,450+(-100)*(fromIntegral p)),(800,440+(-100)*(fromIntegral p))])] ++ [color white (polygon [((-800),350+(-100)*(fromIntegral p)),((-800),360+(-100)*(fromIntegral p)),(800,360+(-100)*(fromIntegral p)),(800,350+(-100)*(fromIntegral p))])]
                                           Relva -> [color verdeRelva (polygon [((-800),350+(-100)*(fromIntegral p)),((-800),450+(-100)*(fromIntegral p)),(800,450+(-100)*(fromIntegral p)),(800,350+(-100)*(fromIntegral p))])]

desenhaObstaculo :: Int -> Int -> (Terreno,[Obstaculo]) -> [Picture]
desenhaObstaculo n1 n2 (ter,[]) = []
desenhaObstaculo n1 n2 (ter,(h:t)) = case h of Nenhum -> (desenhaObstaculo (n1 + 1) n2 (ter,t))
                                               Tronco -> [color castanhoTronco (polygon [(((-800)+(100*(fromIntegral n1))),350+(-100)*(fromIntegral n2)),(((-800)+(100)*(fromIntegral n1)),450+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),450+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),350+(-100)*(fromIntegral n2))])] ++ (desenhaObstaculo (n1 + 1) n2 (ter,t))
                                               Carro -> [color red (polygon [(((-800)+(100*(fromIntegral n1))),370+(-100)*(fromIntegral n2)),(((-800)+(100)*(fromIntegral n1)),450+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),450+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),370+(-100)*(fromIntegral n2))])] ++ [color black (polygon [(((-800)+(100*(fromIntegral n1))),350+(-100)*(fromIntegral n2)),(((-800)+(100)*(fromIntegral n1)),390+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),390+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),350+(-100)*(fromIntegral n2))])] ++ [color black (polygon [(((-740)+(100*(fromIntegral n1))),350+(-100)*(fromIntegral n2)),(((-740)+(100)*(fromIntegral n1)),390+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),390+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),350+(-100)*(fromIntegral n2))])] ++ (desenhaObstaculo (n1 + 1) n2 (ter,t))
                                               Arvore -> [color (dark (dark green)) (polygon [(((-800)+(100*(fromIntegral n1))),390+(-100)*(fromIntegral n2)),(((-800)+(100)*(fromIntegral n1)),450+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),450+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),390+(-100)*(fromIntegral n2))])] ++ [color (dark castanhoTronco) (polygon [(((-770)+(100*(fromIntegral n1))),350+(-100)*(fromIntegral n2)),(((-770)+(100)*(fromIntegral n1)),390+(-100)*(fromIntegral n2)),(((-730)+(100*(fromIntegral n1))),390+(-100)*(fromIntegral n2)),(((-730)+(100*(fromIntegral n1))),350+(-100)*(fromIntegral n2))])] ++ (desenhaObstaculo (n1 + 1) n2 (ter,t))

desenhaJogador :: Int -> Int -> [Picture]
desenhaJogador x y = [color yellow (polygon [(((-790)+(100*(fromIntegral x))),(360+(-100)*(fromIntegral y))),((((-790) + 100*(fromIntegral x)),(440+(-100)*(fromIntegral y)))),(((-710)+(100*(fromIntegral x))),440+(-100)*(fromIntegral y)),(((-710)+(100*(fromIntegral x))),360+(-100)*(fromIntegral y))])] ++ [color red (polygon [(((-755)+(100*(fromIntegral x))),410+(-100)*(fromIntegral y)),(((-755)+(100)*(fromIntegral x)),450+(-100)*(fromIntegral y)),(((-745)+(100*(fromIntegral x))),450+(-100)*(fromIntegral y)),(((-745)+(100*(fromIntegral x))),410+(-100)*(fromIntegral y))])] ++ [color red (polygon [(((-770)+(100*(fromIntegral x))),350+(-100)*(fromIntegral y)),(((-770)+(100)*(fromIntegral x)),390+(-100)*(fromIntegral y)),(((-730)+(100*(fromIntegral x))),390+(-100)*(fromIntegral y)),(((-730)+(100*(fromIntegral x))),350+(-100)*(fromIntegral y))])] ++ [color black (polygon [(((-790)+(100*(fromIntegral x))),420+(-100)*(fromIntegral y)),(((-790)+(100)*(fromIntegral x)),440+(-100)*(fromIntegral y)),(((-770)+(100*(fromIntegral x))),440+(-100)*(fromIntegral y)),(((-770)+(100*(fromIntegral x))),420+(-100)*(fromIntegral y))])] ++ [color black (polygon [(((-730)+(100*(fromIntegral x))),420+(-100)*(fromIntegral y)),(((-730)+(100)*(fromIntegral x)),440+(-100)*(fromIntegral y)),(((-710)+(100*(fromIntegral x))),440+(-100)*(fromIntegral y)),(((-710)+(100*(fromIntegral x))),420+(-100)*(fromIntegral y))])]

eventoIO :: Event -> World -> IO World
-- Menu Inicial
eventoIO (EventKey (SpecialKey KeyEnter) Down _ _) (MenuInicial NovoJogo, j, la, p, s) = return (ModoJogo, j, la, p, s)
eventoIO (EventKey (SpecialKey KeyDown) Down _ _) (MenuInicial NovoJogo, j, la, p, s) = return (MenuInicial Continuar, j, la, p, s)
eventoIO (EventKey (SpecialKey KeyUp) Down _ _) (MenuInicial NovoJogo, j, la, p, s) = return (MenuInicial Sair, j, la, p, s)
eventoIO (EventKey (SpecialKey KeyDown) Down _ _) (MenuInicial Continuar, j, la, p, s) = return (MenuInicial Sair, j, la, p, s)
eventoIO (EventKey (SpecialKey KeyEnter) Down _ _) (MenuInicial Continuar, j, la, p, s) = carregaJogo (MenuInicial Continuar, j, la, p, s)
eventoIO (EventKey (SpecialKey KeyEnter) Down _ _) (MenuInicial Sair, j, la, p, s) =  error "Fim de Jogo"
eventoIO (EventKey (SpecialKey KeyDown) Down _ _) (MenuInicial Sair, j, la, p, s) = return (MenuInicial NovoJogo, j, la, p, s)
eventoIO (EventKey (SpecialKey KeyUp) Down _ _) (MenuInicial Sair, j, la, p, s) = return (MenuInicial Continuar, j, la, p, s)
eventoIO (EventKey (SpecialKey KeyUp) Down _ _) (MenuInicial Continuar, j, la, p, s) = return (MenuInicial NovoJogo, j, la, p, s)
-- Perdeu Jogo
eventoIO (EventKey (SpecialKey KeyEnter) Down _ _) (PerdeuJogo, j, la, p, s) = return estadoInicial
-- Modo Jogo
eventoIO (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo, j, la, p, s) | jogoTerminou j = removeJogoGuardado (PerdeuJogo, j, la, p, s)
                                                                        | variacaoDaOrdenada j (Move Cima) /= 0 && la+1 > p = return (ModoJogo, animaJogador j (Move Cima), la+1, p+1, s)
                                                                        | variacaoDaOrdenada j (Move Cima) /= 0 && la+1 <= p = return (ModoJogo, animaJogador j (Move Cima), la+1, p, s)
                                                                        | otherwise = return (ModoJogo, animaJogador j (Move Cima), la, p, s)
eventoIO (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo, j, la, p, s) | jogoTerminou j = removeJogoGuardado (PerdeuJogo, j, la, p, s)
                                                                          | variacaoDaOrdenada j (Move Baixo) /= 0 = return (ModoJogo, animaJogador j (Move Baixo), la-1, p, s) 
                                                                          | otherwise = return (ModoJogo, animaJogador j (Move Baixo), la, p, s)
eventoIO (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo, j, la, p, s) = 
    if jogoTerminou j then removeJogoGuardado (PerdeuJogo, j, la, p, s) else return (ModoJogo, animaJogador j (Move Esquerda), la, p, s)
eventoIO (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo, j, la, p, s) = 
    if jogoTerminou j then removeJogoGuardado (PerdeuJogo, j, la, p, s) else return (ModoJogo, animaJogador j (Move Direita), la, p, s)
eventoIO (EventKey (SpecialKey _) Up _ _) (ModoJogo, j, la, p, s) = if jogoTerminou j then removeJogoGuardado (PerdeuJogo, j, la, p, s) else return (ModoJogo, animaJogador j (Parado), la, p, s)
eventoIO (EventKey (SpecialKey KeyEnter) Down _ _) (ModoJogo, j, la, p, s) = if jogoTerminou j then removeJogoGuardado (PerdeuJogo, j, la, p, s) else return (MenuPausa Retomar, j, la, p, s)
-- Menu Pausa
eventoIO (EventKey (SpecialKey KeyEnter) Down _ _) (MenuPausa Retomar, j, la, p, s) = return (ModoJogo, j, la, p, s)
eventoIO (EventKey (SpecialKey KeyDown) Down _ _) (MenuPausa Retomar, j, la, p, s) = return (MenuPausa Gravar, j, la, p, s)
eventoIO (EventKey (SpecialKey KeyUp) Down _ _) (MenuPausa Retomar, j, la, p, s) = return (MenuPausa VoltarMenuInicial, j, la, p, s)
eventoIO (EventKey (SpecialKey KeyEnter) Down _ _) (MenuPausa Gravar, j, la, p, s) = guardaJogo (MenuPausa Gravado, j, la, p, s)
eventoIO (EventKey (SpecialKey KeyDown) Down _ _) (MenuPausa Gravar, j, la, p, s) = return (MenuPausa VoltarMenuInicial, j, la, p, s)
eventoIO (EventKey (SpecialKey KeyUp) Down _ _) (MenuPausa Gravar, j, la, p, s) = return (MenuPausa Retomar, j, la, p, s)
eventoIO (EventKey (SpecialKey KeyDown) Down _ _) (MenuPausa Gravado, j, la, p, s) = return (MenuPausa VoltarMenuInicial, j, la, p, s)
eventoIO (EventKey (SpecialKey KeyUp) Down _ _) (MenuPausa Gravado, j, la, p, s) = return (MenuPausa Retomar, j, la, p, s)
eventoIO (EventKey (SpecialKey KeyEnter) Down _ _) (MenuPausa VoltarMenuInicial, j, la, p, s) = return estadoInicial
eventoIO (EventKey (SpecialKey KeyUp) Down _ _) (MenuPausa VoltarMenuInicial, j, la, p, s) = return (MenuPausa Gravar, j, la, p, s)
eventoIO (EventKey (SpecialKey KeyDown) Down _ _) (MenuPausa VoltarMenuInicial, j, la, p, s) = return (MenuPausa Retomar, j, la, p, s)
eventoIO _ (ModoJogo, j, la, p, s) = if jogoTerminou j then removeJogoGuardado (PerdeuJogo, j, la, p, s) else return (ModoJogo, animaJogador j (Parado), la, p, s)
eventoIO _ w = return w

tempoIO :: Float -> World -> IO World
tempoIO n w = do
    seed <- randomIO
    return (tempo n seed w)

tempo :: Float -> Int -> World -> World
tempo n seed (ModoJogo,(Jogo (Jogador (x,y)) m), la, p, td) = 
    if jogoTerminou (Jogo (Jogador (x,y)) m) 
        then (PerdeuJogo,(Jogo (Jogador (x,y)) m), la, p, td+1)
        -- deslizar jogo apenas quando o tempo decorrido é ímpar
        else if mod td 2 == 0 
                then (ModoJogo, animaJogo (Jogo (Jogador (x,y)) m) Parado, la, p, td+1)
                else (ModoJogo, deslizaJogo seed (animaJogo (Jogo (Jogador (x,y)) m) Parado), la, p, td+1)
tempo _ _ w = w

guardaJogo :: World -> IO World
guardaJogo (m, j, la, p, s) = do 
    let savedata = (ModoJogo, j, la, p, s)
    writeFile "crossyroad.sav" (show savedata)
    return (m, j, la, p, s)

carregaJogo :: World -> IO World 
carregaJogo w = do
    fileExist <- doesFileExist "crossyroad.sav"
    saved <- if fileExist then readFile "crossyroad.sav"
                          else return (show w)
    return (read saved)


removeJogoGuardado :: World -> IO World
removeJogoGuardado w = do 
    fileExist <- doesFileExist "crossyroad.sav"
    if fileExist then removeFile "crossyroad.sav"
                 else return ()
    return w

verdeRelva :: Color
verdeRelva = light green

cinzaEstrada :: Color
cinzaEstrada = greyN 0.8

azulRio :: Color
azulRio = light azure

castanhoTronco :: Color
castanhoTronco = (makeColor 0.4 0.2 0 0.9)

fundo :: Picture
fundo = color white (polygon [((-800),(-450)),((-800),450),(800,450),(800,(-450))])

fundoPerdeu :: Picture
fundoPerdeu = color (light (light red)) (polygon [((-800),(-450)),((-800),450),(800,450),(800,(-450))])