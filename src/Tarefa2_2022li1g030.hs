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

-- pode ser qualquer terreno a seguir
m = (Mapa 2 [(Rio 1, [Nenhum, Tronco])
            ,(Estrada (-1), [Carro, Nenhum])
            ,(Relva, [Nenhum, Arvore])])

-- 4 rios contíguos, próxima linha não pode ser Rio, apenas Estrada ou Relva
m1 = (Mapa 2 [(Rio 1, [Nenhum, Tronco])
            ,(Rio (-1), [Tronco, Nenhum])
            ,(Rio 1, [Nenhum, Tronco])
            ,(Rio (-1), [Tronco, Nenhum])])

-- 5 estradas contíguas, próxima linha não pode ser Estrada, apenas Rio ou Relva
m2 = (Mapa 2 [(Estrada 1, [Nenhum, Carro])
             ,(Estrada (-1), [Carro, Nenhum])
             ,(Estrada 1, [Nenhum, Carro])
             ,(Estrada (-1), [Carro, Nenhum])
             ,(Estrada 1, [Carro, Nenhum])])

-- 5 relvas contíguas, próxima linha não pode ser Relva, apenas Rio ou Estrada
m3 = (Mapa 2 [(Relva, [Nenhum, Arvore])
             ,(Relva, [Arvore, Nenhum])
             ,(Relva, [Nenhum, Arvore])
             ,(Relva, [Arvore, Nenhum])
             ,(Relva, [Arvore, Nenhum])])


gera :: Int -> [Int] -> [Int]
gera seed lt = let ls = take (length lt) $ randoms (mkStdGen seed)
                   l = zip lt ls
               in map (\(t,s) -> mod s t) l

estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa n ((a,b):t)) seed = undefined


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