{- |
Module      : Tarefa2_2022li1g030
Description : Geração contínua de um mapa
Copyright   : Ana Sá Oliveira <a104437@alunos.uminho.pt>
              Sara Campos Ramalho <a72481@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}
module Tarefa2_2022li1g030 where

import LI12223

import System.Random


proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa _ ((Rio _, _):(Rio _, _):(Rio _, _):(Rio _, _):t)) = [Estrada 0, Relva]
proximosTerrenosValidos (Mapa _ ((Estrada _, _):(Estrada _, _):(Estrada _, _):(Estrada _, _):(Estrada _, _):t)) = [Rio 0, Relva]
proximosTerrenosValidos (Mapa _ ((Relva, _):(Relva, _):(Relva, _):(Relva, _):(Relva, _):t)) = [Rio 0, Estrada 0]
proximosTerrenosValidos _ = [Rio 0, Estrada 0, Relva]


proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos n (t,l)
    | length l >= n = []
    | otherwise = case t of
                    (Rio _) -> [Nenhum, Tronco]
                    (Estrada _) -> [Nenhum, Carro]
                    Relva -> [Nenhum, Arvore]


gera :: Int -> [Int] -> [Int]
gera seed lt = let ls = take (length lt) $ randoms (mkStdGen seed)
                   l = zip lt ls
               in map (\(t,s) -> mod s t) l

geraObstaculo :: [Int] -> [Obstaculo] -> [Obstaculo]
geraObstaculo l1 l2 = [l2 !! x | x <- l1]

geraTerreno :: Int -> [Terreno] -> Terreno
geraTerreno x l = l !! x

geraVelocidade :: Int -> [Velocidade] -> Velocidade
geraVelocidade x l = l !! x

adicionaVelocidade :: Terreno -> Velocidade -> Terreno
adicionaVelocidade (Rio _) v = Rio v
adicionaVelocidade (Estrada _) v = Estrada v
adicionaVelocidade Relva _ = Relva

estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa n l) seed = let ptv = proximosTerrenosValidos (Mapa n l)
                                  t0 = geraTerreno (head (gera seed [length ptv])) ptv
                                  velocidade = geraVelocidade (head (gera seed [length [-(n-1)..(n-1)]])) [-(n-1)..(n-1)]
                                  terreno = adicionaVelocidade t0 velocidade
                                  pov = proximosObstaculosValidos n (terreno,[])
                                  obstaculos = geraObstaculo (gera seed (replicate n 2)) pov
                              in Mapa n ((terreno, obstaculos):l)