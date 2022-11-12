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

-- | A função 'proximosTerrenosValidos' calcula quais os terrenos passíveis de ser utilizados numa próxima linha do mapa
proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa _ ((Rio _, _):(Rio _, _):(Rio _, _):(Rio _, _):t)) = [Estrada 0, Relva]
proximosTerrenosValidos (Mapa _ ((Estrada _, _):(Estrada _, _):(Estrada _, _):(Estrada _, _):(Estrada _, _):t)) = [Rio 0, Relva]
proximosTerrenosValidos (Mapa _ ((Relva, _):(Relva, _):(Relva, _):(Relva, _):(Relva, _):t)) = [Rio 0, Estrada 0]
proximosTerrenosValidos _ = [Rio 0, Estrada 0, Relva]

-- | Calcula que obstáculos podem ser utilizados para preencher a lista de obstáculos de uma próxima linha do mapa
proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos n (t,l)
    | length l >= n = []
    | otherwise = case t of
                    (Rio _) -> [Nenhum, Tronco]
                    (Estrada _) -> [Nenhum, Carro]
                    Relva -> [Nenhum, Arvore]

-- | Função que gera números inteiros aleatoriamente, para serem utilizados em funções posteriores
gera :: Int -> [Int] -> [Int]
gera seed lt = let ls = take (length lt) $ randoms (mkStdGen seed)
                   l = zip lt ls
               in map (\(t,s) -> mod s t) l

-- | Dá, com base em aleatórios devolvidos pela função 'gera', uma lista de obstáculos que será usada numa próxima linha do mapa
geraObstaculo :: [Int] -> [Obstaculo] -> [Obstaculo]
geraObstaculo l1 l2 = [l2 !! x | x <- l1]

-- | Dá, com base em aleatórios devolvidos pela função 'gera', um terreno que será utilizado numa próxima linha do mapa
geraTerreno :: Int -> [Terreno] -> Terreno
geraTerreno x l = l !! x

-- | Dá, com base em aleatórios devolvidos pela função 'gera', uma velocidade que será associada a um terreno numa próxima linha do mapa
geraVelocidade :: Int -> [Velocidade] -> Velocidade
geraVelocidade x l = l !! x

-- | Associa a velocidade devolvida pela função geraVelocidade 
adicionaVelocidade :: Terreno -> Velocidade -> Terreno
adicionaVelocidade (Rio _) v = Rio v
adicionaVelocidade (Estrada _) v = Estrada v
adicionaVelocidade Relva _ = Relva

-- | acrescenta uma linha a um dado mapa
estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa n l) seed = let ptv = proximosTerrenosValidos (Mapa n l)
                                  t0 = geraTerreno (head (gera seed [length ptv])) ptv
                                  velocidade = geraVelocidade (head (gera seed [length [-(n-1)..(n-1)]])) [-(n-1)..(n-1)]
                                  terreno = adicionaVelocidade t0 velocidade
                                  pov = proximosObstaculosValidos n (terreno,[])
                                  obstaculos = geraObstaculo (gera seed (replicate n 2)) pov
                              in Mapa n ((terreno, obstaculos):l)