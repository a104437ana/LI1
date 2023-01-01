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

{-|
O tipo de dados @OpcaoMenuInicial@ representa as diferentes opções apresentadas no menu inicial do jogo: 
        @NovoJogo@, para iniciar um novo jogo, 
        @Continuar@, para carregar um jovo previamente guardado, e 
        @Sair@, para sair do jogo e fechar a janela.

O tipo de dados @OpcaoMenuPausa@ representa as opções disponíveis no menu de pausa do jogo: 
        @Retomar@, para sair do menu de pausa e voltar ao mapa do jogo
        @Gravar@, para guardar num ficheiro o progresso feito no mapa
        @Gravado@, para confirmar que foi de facto guardado o progresso do jogo


O tipo de dados EstadoAtual representa os diferentes estados em que o jogo pode estar: 
        @MenuInicial@, correspondente ao menu inicial
        @MenuPausa@, correspondente ao menu de pausa
        @ModoJogo@, correspondente ao modo de jogo, em que o jogador percorre o mapa
        @PerdeuJogo@, correspondente ao fim de um jogo por ter perdido

Os tipos de dados @Pontuacao@, LinhaAtual e TempoDecorrido correspondem, respetivamente: 
à pontuação atingida pelo jogador, à linha do mapa em que o jogador se encontra no momento, e ao tempo decorrido desde o início do jogo,


O tipo de dados @World@ junta o @EstadoAtual@, o @Jogo@, a @LinhaAtual@, a @Pontuacao@ e o @TempoDecorrido@ para englobar todos os elementos que vão constituir o jogo.
-}

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
type Imagens = [Picture]
type World = (EstadoAtual, Jogo, LinhaAtual, Pontuacao, TempoDecorrido, Imagens)
type SaveData = (EstadoAtual, Jogo, LinhaAtual, Pontuacao, TempoDecorrido)


janela :: Display
janela = InWindow "Crossy Road" (1600,900) (0,0)

estadoInicial :: Imagens -> World
estadoInicial imagens = (MenuInicial NovoJogo, (Jogo (Jogador (8,5)) (Mapa 16 [(Relva,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Nenhum]),
                                                                       (Rio (-1),[Tronco,Tronco,Tronco,Tronco,Nenhum,Nenhum,Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco,Nenhum,Nenhum,Nenhum,Nenhum]),
                                                                       (Rio 1,[Nenhum,Nenhum,Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco,Nenhum,Nenhum,Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco]),
                                                                       (Relva,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
                                                                       (Estrada 1,[Nenhum,Carro,Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum]),
                                                                       (Relva,[Arvore,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Arvore]),
                                                                       (Relva,[Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Arvore]),
                                                                       (Relva,[Arvore,Nenhum,Arvore,Arvore,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Arvore,Arvore,Arvore,Nenhum,Nenhum]),
                                                                       (Relva,[Arvore,Arvore,Arvore,Arvore,Nenhum,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Arvore,Arvore,Arvore])])), 0, 0, 0, imagens)

desenhaIO :: World -> IO Picture
desenhaIO (MenuInicial NovoJogo, _, _, _, _, i) = return $ Pictures [fundo,translate 0 (250) (i !! 1), translate 0 50 (i !! 3),translate 0 (-75) (i !! 4),translate 0 (-200) (i !! 6)]
desenhaIO (MenuInicial Continuar, _, _, _, _, i) = return $ Pictures [fundo, translate 0 (250) (i !! 1), translate 0 50 (i !! 2),translate 0 (-75) (i !! 5),translate 0 (-200) (i !! 6)]
desenhaIO (MenuInicial Sair, _, _, _, _, i) = return $ Pictures [fundo, translate 0 (250) (i !! 1),translate 0 50 (i !! 2),translate 0 (-75) (i !! 4),translate 0 (-200) (i !! 7)]
desenhaIO (ModoJogo, Jogo (Jogador (x,y)) (Mapa 16 l), _, p, td, i) = return $ 
    if jogoTerminou (Jogo (Jogador (x,y)) (Mapa 16 l)) then Pictures ((desenhaMapa i 0 (Mapa 16 l)) ++ [translate (-800) (330) (color white (text (show p)))])
    else Pictures ([Pictures ((desenhaMapa i 0 (Mapa 16 l)) ++ (desenhaJogador x y))] ++ [translate (-800) (330) (color white (text (show p)))])
desenhaIO (MenuPausa Retomar, _, _, _, _, i) = return $ Pictures [fundo,translate 0 (250) (i !! 8), translate 0 50 (i !! 10),translate 0 (-75) (i !! 11),translate 0 (-200) (i !! 14)]
desenhaIO (MenuPausa Gravar, _, _, _, _, i) = return $ Pictures [fundo,translate 0 (250) (i !! 8), translate 0 50 (i !! 9),translate 0 (-75) (i !! 12),translate 0 (-200) (i !! 14)]
desenhaIO (MenuPausa Gravado, _, _, _, _, i) = return $ Pictures [fundo,translate 0 (250) (i !! 8), translate 0 50 (i !! 9),translate 0 (-75) (i !! 13),translate 0 (-200) (i !! 14)]
desenhaIO (MenuPausa VoltarMenuInicial, _, _, _, _, i) = return $ Pictures [fundo,translate 0 (250) (i !! 8), translate 0 50 (i !! 9),translate 0 (-75) (i !! 11),translate 0 (-200) (i !! 15)]
desenhaIO (PerdeuJogo, _, _, p, _, i) = return $ Pictures ([fundoPerdeu, translate 0 (250) (i !! 16), translate 0 0 (i !! 17), translate (-50) (-200) (i !! 18)] ++ [translate (100) (-250) (color white (text (show p)))])

desenhaMapa :: Imagens -> Int -> Mapa -> [Picture]
desenhaMapa i p (Mapa 16 (h:t)) = (desenhaTerreno i p h) ++ (desenhaObstaculo 0 p h) ++ (desenhaMapa i (p+1) (Mapa 16 t))
desenhaMapa i p (Mapa 16 []) = []

desenhaTerreno :: Imagens -> Int -> (Terreno,[Obstaculo]) -> [Picture]
desenhaTerreno i p (ter,(h:t)) = case ter of Rio _ -> [color azulRio (polygon [((-800),350+(-100)*(fromIntegral p)),((-800),450+(-100)*(fromIntegral p)),(800,450+(-100)*(fromIntegral p)),(800,350+(-100)*(fromIntegral p))])]
                                             Estrada _ -> [translate 0 (400 + (-100)*(fromIntegral p)) (head i)]
                                             Relva -> [color verdeRelva (polygon [((-800),350+(-100)*(fromIntegral p)),((-800),450+(-100)*(fromIntegral p)),(800,450+(-100)*(fromIntegral p)),(800,350+(-100)*(fromIntegral p))])]

desenhaObstaculo :: Int -> Int -> (Terreno,[Obstaculo]) -> [Picture]
desenhaObstaculo n1 n2 (ter,[]) = []
desenhaObstaculo n1 n2 (Estrada v,(h:t)) | v>=0 = desenhaCarrosDireita h n1 n2 (Estrada v,(h:t))
                                         | v<0 = desenhaCarrosEsquerda (last t) n1 n2 (Estrada v,(h:t))
desenhaObstaculo n1 n2 (ter,(h:t)) = case h of Nenhum -> (desenhaObstaculo (n1 + 1) n2 (ter,t))
                                               Tronco -> [color castanhoTronco (polygon [(((-800)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2)),(((-800)+(100)*(fromIntegral n1)),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2))])] ++ (desenhaObstaculo (n1 + 1) n2 (ter,t))
                                               Arvore -> [color (dark (dark green)) (polygon [(((-795)+(100*(fromIntegral n1))),390+(-100)*(fromIntegral n2)),(((-795)+(100)*(fromIntegral n1)),445+(-100)*(fromIntegral n2)),(((-705)+(100*(fromIntegral n1))),445+(-100)*(fromIntegral n2)),(((-705)+(100*(fromIntegral n1))),390+(-100)*(fromIntegral n2))])] ++ [color (dark castanhoTronco) (polygon [(((-780)+(100*(fromIntegral n1))),355+(-100)*(fromIntegral n2)),(((-770)+(100)*(fromIntegral n1)),390+(-100)*(fromIntegral n2)),(((-730)+(100*(fromIntegral n1))),390+(-100)*(fromIntegral n2)),(((-720)+(100*(fromIntegral n1))),355+(-100)*(fromIntegral n2))])] ++ (desenhaObstaculo (n1 + 1) n2 (ter,t))

desenhaCarrosDireita :: Obstaculo -> Int -> Int -> (Terreno,[Obstaculo]) -> [Picture]
desenhaCarrosDireita o n1 n2 (ter,[]) = []
desenhaCarrosDireita o n1 n2 (Estrada v, (h:t)) = case h of Nenhum -> (desenhaCarrosDireita o (n1 + 1) n2 (Estrada v,t))
                                                            Carro | t /= [] && head t /= Carro -> [color red (polygon [(((-800)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2)),(((-800)+(100)*(fromIntegral n1)),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2))])] ++ [color black (polygon [(((-790)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2)),(((-790)+(100)*(fromIntegral n1)),380+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),380+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2))])] ++ [color black (polygon [(((-740)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2)),(((-740)+(100)*(fromIntegral n1)),380+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),380+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2))])] ++ [color white (polygon [(((-740)+(100*(fromIntegral n1))),400+(-100)*(fromIntegral n2)),(((-740)+(100)*(fromIntegral n1)),430+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),430+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),400+(-100)*(fromIntegral n2))])] ++ (desenhaCarrosDireita o (n1 + 1) n2 (Estrada v,t))
                                                                  | t /= [] && head t == Carro -> [color (dark blue) (polygon [(((-800)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2)),(((-800)+(100)*(fromIntegral n1)),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2))])] ++ [color black (polygon [(((-790)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2)),(((-790)+(100)*(fromIntegral n1)),380+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),380+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2))])] ++ [color black (polygon [(((-740)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2)),(((-740)+(100)*(fromIntegral n1)),380+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),380+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2))])] ++ (desenhaCarrosDireita o (n1 + 1) n2 (Estrada v,t))
                                                                  | t == [] && o /= Carro -> [color red (polygon [(((-800)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2)),(((-800)+(100)*(fromIntegral n1)),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2))])] ++ [color black (polygon [(((-790)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2)),(((-790)+(100)*(fromIntegral n1)),380+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),380+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2))])] ++ [color black (polygon [(((-740)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2)),(((-740)+(100)*(fromIntegral n1)),380+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),380+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2))])] ++ [color white (polygon [(((-740)+(100*(fromIntegral n1))),400+(-100)*(fromIntegral n2)),(((-740)+(100)*(fromIntegral n1)),430+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),430+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),400+(-100)*(fromIntegral n2))])] ++ (desenhaCarrosDireita o (n1 + 1) n2 (Estrada v,t))
                                                                  | t == [] && o == Carro -> [color (dark blue) (polygon [(((-800)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2)),(((-800)+(100)*(fromIntegral n1)),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2))])] ++ [color black (polygon [(((-790)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2)),(((-790)+(100)*(fromIntegral n1)),380+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),380+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2))])] ++ [color black (polygon [(((-740)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2)),(((-740)+(100)*(fromIntegral n1)),380+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),380+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2))])] ++ (desenhaCarrosDireita o (n1 + 1) n2 (Estrada v,t))

desenhaCarrosEsquerda :: Obstaculo -> Int -> Int -> (Terreno,[Obstaculo]) -> [Picture]
desenhaCarrosEsquerda o n1 n2 (ter,[]) = []
desenhaCarrosEsquerda o n1 n2 (Estrada v, (h:t)) = case h of Nenhum -> (desenhaCarrosEsquerda h (n1 + 1) n2 (Estrada v,t))
                                                             Carro | o /= Carro -> [color red (polygon [(((-800)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2)),(((-800)+(100)*(fromIntegral n1)),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2))])] ++ [color black (polygon [(((-790)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2)),(((-790)+(100)*(fromIntegral n1)),380+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),380+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2))])] ++ [color black (polygon [(((-740)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2)),(((-740)+(100)*(fromIntegral n1)),380+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),380+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2))])] ++ [color white (polygon [(((-790)+(100*(fromIntegral n1))),400+(-100)*(fromIntegral n2)),(((-790)+(100)*(fromIntegral n1)),430+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),430+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),400+(-100)*(fromIntegral n2))])] ++ (desenhaCarrosEsquerda h (n1 + 1) n2 (Estrada v,t))
                                                                   | o == Carro -> [color (dark blue) (polygon [(((-800)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2)),(((-800)+(100)*(fromIntegral n1)),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),440+(-100)*(fromIntegral n2)),(((-700)+(100*(fromIntegral n1))),360+(-100)*(fromIntegral n2))])] ++ [color black (polygon [(((-790)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2)),(((-790)+(100)*(fromIntegral n1)),380+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),380+(-100)*(fromIntegral n2)),(((-760)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2))])] ++ [color black (polygon [(((-740)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2)),(((-740)+(100)*(fromIntegral n1)),380+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),380+(-100)*(fromIntegral n2)),(((-710)+(100*(fromIntegral n1))),340+(-100)*(fromIntegral n2))])] ++ (desenhaCarrosEsquerda h (n1 + 1) n2 (Estrada v,t))
                                                                
desenhaJogador :: Int -> Int -> [Picture]
desenhaJogador x y = [color yellow (polygon [(((-790)+(100*(fromIntegral x))),(360+(-100)*(fromIntegral y))),((((-790) + 100*(fromIntegral x)),(440+(-100)*(fromIntegral y)))),(((-710)+(100*(fromIntegral x))),440+(-100)*(fromIntegral y)),(((-710)+(100*(fromIntegral x))),360+(-100)*(fromIntegral y))])] ++ [color red (polygon [(((-755)+(100*(fromIntegral x))),410+(-100)*(fromIntegral y)),(((-755)+(100)*(fromIntegral x)),450+(-100)*(fromIntegral y)),(((-745)+(100*(fromIntegral x))),450+(-100)*(fromIntegral y)),(((-745)+(100*(fromIntegral x))),410+(-100)*(fromIntegral y))])] ++ [color red (polygon [(((-780)+(100*(fromIntegral x))),350+(-100)*(fromIntegral y)),(((-770)+(100)*(fromIntegral x)),390+(-100)*(fromIntegral y)),(((-730)+(100*(fromIntegral x))),390+(-100)*(fromIntegral y)),(((-720)+(100*(fromIntegral x))),350+(-100)*(fromIntegral y))])] ++ [color black (polygon [(((-790)+(100*(fromIntegral x))),420+(-100)*(fromIntegral y)),(((-790)+(100)*(fromIntegral x)),440+(-100)*(fromIntegral y)),(((-770)+(100*(fromIntegral x))),440+(-100)*(fromIntegral y)),(((-770)+(100*(fromIntegral x))),420+(-100)*(fromIntegral y))])] ++ [color black (polygon [(((-730)+(100*(fromIntegral x))),420+(-100)*(fromIntegral y)),(((-730)+(100)*(fromIntegral x)),440+(-100)*(fromIntegral y)),(((-710)+(100*(fromIntegral x))),440+(-100)*(fromIntegral y)),(((-710)+(100*(fromIntegral x))),420+(-100)*(fromIntegral y))])]

eventoIO :: Event -> World -> IO World
-- Menu Inicial
eventoIO (EventKey (SpecialKey KeyEnter) Down _ _) (MenuInicial NovoJogo, j, la, p, s, i) = return (ModoJogo, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyDown) Down _ _) (MenuInicial NovoJogo, j, la, p, s, i) = return (MenuInicial Continuar, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyUp) Down _ _) (MenuInicial NovoJogo, j, la, p, s, i) = return (MenuInicial Sair, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyDown) Down _ _) (MenuInicial Continuar, j, la, p, s, i) = return (MenuInicial Sair, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyEnter) Down _ _) (MenuInicial Continuar, j, la, p, s, i) = carregaJogo (MenuInicial Continuar, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyEnter) Down _ _) (MenuInicial Sair, j, la, p, s, i) =  error "Fim de Jogo"
eventoIO (EventKey (SpecialKey KeyDown) Down _ _) (MenuInicial Sair, j, la, p, s, i) = return (MenuInicial NovoJogo, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyUp) Down _ _) (MenuInicial Sair, j, la, p, s, i) = return (MenuInicial Continuar, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyUp) Down _ _) (MenuInicial Continuar, j, la, p, s, i) = return (MenuInicial NovoJogo, j, la, p, s, i)
-- Perdeu Jogo
eventoIO (EventKey (SpecialKey KeyEnter) Down _ _) (PerdeuJogo, j, la, p, s, i) = return (estadoInicial i)
-- Modo Jogo
eventoIO (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo, j, la, p, s, i) | jogoTerminou j = removeJogoGuardado (PerdeuJogo, j, la, p, s, i)
                                                                        | variacaoDaOrdenada j (Move Cima) /= 0 && la+1 > p = return (ModoJogo, animaJogador j (Move Cima), la+1, p+1, s, i)
                                                                        | variacaoDaOrdenada j (Move Cima) /= 0 && la+1 <= p = return (ModoJogo, animaJogador j (Move Cima), la+1, p, s, i)
                                                                        | otherwise = return (ModoJogo, animaJogador j (Move Cima), la, p, s, i)
eventoIO (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo, j, la, p, s, i) | jogoTerminou j = removeJogoGuardado (PerdeuJogo, j, la, p, s, i)
                                                                          | variacaoDaOrdenada j (Move Baixo) /= 0 = return (ModoJogo, animaJogador j (Move Baixo), la-1, p, s, i) 
                                                                          | otherwise = return (ModoJogo, animaJogador j (Move Baixo), la, p, s, i)
eventoIO (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo, j, la, p, s, i) = 
    if jogoTerminou j then removeJogoGuardado (PerdeuJogo, j, la, p, s, i) else return (ModoJogo, animaJogador j (Move Esquerda), la, p, s, i)
eventoIO (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo, j, la, p, s, i) = 
    if jogoTerminou j then removeJogoGuardado (PerdeuJogo, j, la, p, s, i) else return (ModoJogo, animaJogador j (Move Direita), la, p, s, i)
eventoIO (EventKey (SpecialKey _) Up _ _) (ModoJogo, j, la, p, s, i) = if jogoTerminou j then removeJogoGuardado (PerdeuJogo, j, la, p, s, i) else return (ModoJogo, animaJogador j (Parado), la, p, s, i)
eventoIO (EventKey (SpecialKey KeyEnter) Down _ _) (ModoJogo, j, la, p, s, i) = if jogoTerminou j then removeJogoGuardado (PerdeuJogo, j, la, p, s, i) else return (MenuPausa Retomar, j, la, p, s, i)
-- Menu Pausa
eventoIO (EventKey (SpecialKey KeyEnter) Down _ _) (MenuPausa Retomar, j, la, p, s, i) = return (ModoJogo, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyDown) Down _ _) (MenuPausa Retomar, j, la, p, s, i) = return (MenuPausa Gravar, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyUp) Down _ _) (MenuPausa Retomar, j, la, p, s, i) = return (MenuPausa VoltarMenuInicial, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyEnter) Down _ _) (MenuPausa Gravar, j, la, p, s, i) = guardaJogo (MenuPausa Gravado, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyDown) Down _ _) (MenuPausa Gravar, j, la, p, s, i) = return (MenuPausa VoltarMenuInicial, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyUp) Down _ _) (MenuPausa Gravar, j, la, p, s, i) = return (MenuPausa Retomar, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyDown) Down _ _) (MenuPausa Gravado, j, la, p, s, i) = return (MenuPausa VoltarMenuInicial, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyUp) Down _ _) (MenuPausa Gravado, j, la, p, s, i) = return (MenuPausa Retomar, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyEnter) Down _ _) (MenuPausa VoltarMenuInicial, j, la, p, s, i) = return (estadoInicial i)
eventoIO (EventKey (SpecialKey KeyUp) Down _ _) (MenuPausa VoltarMenuInicial, j, la, p, s, i) = return (MenuPausa Gravar, j, la, p, s, i)
eventoIO (EventKey (SpecialKey KeyDown) Down _ _) (MenuPausa VoltarMenuInicial, j, la, p, s, i) = return (MenuPausa Retomar, j, la, p, s, i)
eventoIO _ (ModoJogo, j, la, p, s, i) = if jogoTerminou j then removeJogoGuardado (PerdeuJogo, j, la, p, s, i) else return (ModoJogo, animaJogador j (Parado), la, p, s, i)
eventoIO _ w = return w

tempoIO :: Float -> World -> IO World
tempoIO n w = do
    seed <- randomIO
    return (tempo n seed w)

tempo :: Float -> Int -> World -> World
tempo n seed (ModoJogo,(Jogo (Jogador (x,y)) m), la, p, td, i) = 
    if jogoTerminou (Jogo (Jogador (x,y)) m) 
        then (PerdeuJogo,(Jogo (Jogador (x,y)) m), la, p, td+1, i)
        -- deslizar jogo apenas quando o tempo decorrido é ímpar
        else if mod td 2 == 0 
                then (ModoJogo, animaJogo (Jogo (Jogador (x,y)) m) Parado, la, p, td+1, i)
                else (ModoJogo, deslizaJogo seed (animaJogo (Jogo (Jogador (x,y)) m) Parado), la, p, td+1, i)
tempo _ _ w = w

converteWorldParaSaveData :: World -> SaveData
converteWorldParaSaveData (m, j, la, p, s, i) = (m, j, la, p, s)

converteSaveDataParaWorld :: SaveData -> Imagens -> World
converteSaveDataParaWorld (m, j, la, p, s) i = (m, j, la, p, s, i)

guardaJogo :: World -> IO World
guardaJogo w = do 
    writeFile "crossyroad.sav" (show $ converteWorldParaSaveData w)
    return w

carregaJogo :: World -> IO World 
carregaJogo (m, j, la, p, s, i) = do
    fileExist <- doesFileExist "crossyroad.sav"
    saved <- if fileExist then readFile "crossyroad.sav"
                          else return (show (m, j, la, p, s))
    return (converteSaveDataParaWorld (read saved) i)

removeJogoGuardado :: World -> IO World
removeJogoGuardado w = do 
    fileExist <- doesFileExist "crossyroad.sav"
    if fileExist then removeFile "crossyroad.sav"
                 else return ()
    return w

verdeRelva :: Color
verdeRelva = light green

cinzaEstrada :: Color
cinzaEstrada = dark (dark (greyN 0.8))

azulRio :: Color
azulRio = light azure

castanhoTronco :: Color
castanhoTronco = (makeColor 0.4 0.2 0 0.9)

fundo :: Picture
fundo = color verdeRelva (polygon [((-800),(-450)),((-800),450),(800,450),(800,(-450))])

fundoPerdeu :: Picture
fundoPerdeu = color (light red) (polygon [((-800),(-450)),((-800),450),(800,450),(800,(-450))])