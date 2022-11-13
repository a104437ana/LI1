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

-- | A função 'gera' devolve números inteiros aleatoriamente, para serem utilizados em funções posteriores.
gera :: Int -> Int -> [Int]
gera seed n = take n $ randoms (mkStdGen seed)

-- | A função 'proximosTerrenosValidos' calcula quais os terrenos passíveis de ser utilizados numa próxima linha do mapa.
proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa _ ((Rio _, _):(Rio _, _):(Rio _, _):(Rio _, _):t)) = [Estrada 0, Relva]
proximosTerrenosValidos (Mapa _ ((Estrada _, _):(Estrada _, _):(Estrada _, _):(Estrada _, _):(Estrada _, _):t)) = [Rio 0, Relva]
proximosTerrenosValidos (Mapa _ ((Relva, _):(Relva, _):(Relva, _):(Relva, _):(Relva, _):t)) = [Rio 0, Estrada 0]
proximosTerrenosValidos _ = [Rio 0, Estrada 0, Relva]

-- | A função 'geraTerreno' dá, com base em aleatórios devolvidos pela função 'gera', um terreno que será utilizado numa próxima linha do mapa.
-- Dado um x inteiro (proveniente da função 'gera'), que representa um índice da lista l, é devolvido o elemento correspondente a esse índice.
geraTerreno :: Int -> [Terreno] -> Terreno
geraTerreno seed l = let rn = head (gera seed 1)
                         ri = mod rn (length l)
                     in l !! ri

-- | A função 'geraVelocidade' dá, com base em aleatórios devolvidos pela função 'gera', uma velocidade que será associada a um terreno numa próxima linha do mapa.
geraVelocidade :: Int -> [Velocidade] -> Velocidade
geraVelocidade seed l = let rn = head (gera seed 1)
                            ri = mod rn (length l)
                        in l !! ri

-- | A função 'adicionaVelocidade' associa a velocidade devolvida pela função 'geraVelocidade' ao terreno selecionado pela função 'geraTerreno'.
adicionaVelocidade :: Terreno -> Velocidade -> Terreno
adicionaVelocidade (Rio _) v = Rio v
adicionaVelocidade (Estrada _) v = Estrada v
adicionaVelocidade Relva _ = Relva


contaExtremos :: [Obstaculo] -> Obstaculo -> Int
contaExtremos [] _ = 0
contaExtremos (h:t) o = if h == o then 1 + contaExtremos t o 
                                  else 0

-- | A função 'proximosObstaculosValidos' calcula que obstáculos podem ser utilizados para preencher a lista de obstáculos de uma próxima linha do mapa.
proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos n (t,l)
    | length l >= n = []
    | length l == (n-1) = if not (elem Nenhum l) then [Nenhum]
                                                 else case t of
                                                        (Rio _) -> let inicio = contaExtremos l Tronco
                                                                       fim = contaExtremos (reverse l) Tronco  
                                                                   in if inicio + fim < 5 then [Nenhum, Tronco]
                                                                                          else [Nenhum]
                                                        (Estrada _) -> let inicio = contaExtremos l Carro
                                                                           fim = contaExtremos (reverse l) Carro  
                                                                       in if inicio + fim < 3 then [Nenhum, Carro]
                                                                                              else [Nenhum]
                                                        Relva -> [Nenhum, Arvore]
    | otherwise = case t of
                    (Rio _) -> let inicio = contaExtremos l Tronco
                               in if inicio < 5 then [Nenhum, Tronco]
                                                else [Nenhum]
                    (Estrada _) -> let inicio = contaExtremos l Carro
                                   in if inicio < 3 then [Nenhum, Carro]
                                                    else [Nenhum]
                    Relva -> [Nenhum, Arvore]

-- | A função 'geraObstaculo' dá, com base em aleatórios devolvidos pela função 'gera', uma lista de obstáculos que será usada numa próxima linha do mapa. 
geraObstaculo :: [Int] -> Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
geraObstaculo [] _ _ = []
geraObstaculo (r:rs) n (t,lo)
    | length lo == n = []
    | otherwise = let pov = proximosObstaculosValidos n (t,lo)
                      po = mod r (length pov)
                  in (pov !! po) : geraObstaculo rs n (t,(pov !! po):lo)

-- | A função 'estendeMapa' acrescenta uma linha a um dado mapa.
estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa n l) seed = let ptv = proximosTerrenosValidos (Mapa n l)
                                  t0 = geraTerreno seed ptv
                                  velocidade = geraVelocidade seed [-(n-1)..(n-1)]
                                  terreno = adicionaVelocidade t0 velocidade
                                  obstaculos = geraObstaculo (gera seed n) n (terreno, [])
                              in Mapa n ((terreno, obstaculos):l)