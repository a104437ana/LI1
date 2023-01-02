{- |
Module      : Tarefa2_2022li1g030
Description : Geração contínua de um mapa
Copyright   : Ana Sá Oliveira <a104437@alunos.uminho.pt>
              Sara Campos Ramalho <a72481@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}
module Tarefa2_2022li1g030 (
-- * Função estendeMapa
-- ** Funções principais
gera, estendeMapa,
-- ** Funções auxiliares
proximosTerrenosValidos, geraTerreno, rioAnterior, velocRioAnterior, geraVelocidade, adicionaVelocidade, proximosObstaculosValidos, contaExtremos, geraObstaculo
) where

import LI12223

import System.Random
import Data.List

velocidade_max :: Int
velocidade_max = 3

{- | A função 'gera' devolve números inteiros aleatoriamente, para serem utilizados em funções posteriores de modo a criar aleatoriedade na criação das novas linhas do mapa.

@
gera seed n = take n $ randoms (mkStdGen seed)
@

-}

gera :: Int -> Int -> [Int]
gera seed n = take n $ randoms (mkStdGen seed)


{- | A função 'estendeMapa' gera e acrescenta uma nova linha válida no topo de um dado mapa, de largura @n@ e composto pela lista @l@, em que cada elemento @(Terreno, [Obstaculos])@ representa uma linha do mapa.

No caso de ainda não existir nenhuma linha no mapa, isto é, l = [], a função 'estendeMapa' gera a primeira linha.

O argumento @seed@ é um inteiro aleatório usado para acrescentar pseudo-aleatoriedade à geração da nova linha.

Esta função pode ser definida da seguinte forma: 

@
estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa n l) seed = let ptv = proximosTerrenosValidos (Mapa n l)
                                  t0 = geraTerreno seed ptv
                                  lvp = calcProxVelPossivel l t0
                                  velocidade = geraVelocidade seed lvp
                                  terreno = adicionaVelocidade t0 velocidade
                                  obstaculos = adicionaObstaculo (Mapa n l) seed terreno
                              in Mapa n ((terreno, obstaculos):l)
@

Para esta função, vamos usar duas funções auxiliares principais, a 'proximosTerrenosValidos' e a 'proximosObstaculosValidos', e outras funções auxiliares que a seguir se descrevem.
-}

estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa n l) seed = let ptv = proximosTerrenosValidos (Mapa n l)
                                  t0 = geraTerreno seed ptv
                                  lvp = calcProxVelPossivel l t0
                                  velocidade = geraVelocidade seed lvp
                                  terreno = adicionaVelocidade t0 velocidade
                                  obstaculos = adicionaObstaculo (Mapa n l) seed terreno
                              in Mapa n ((terreno, obstaculos):l)

{- |A função 'adicionaObstaculo', que recebe um mapa, uma seed e um terreno e que retorna uma lista de obstáculo, 

Assim, a função 'adicionaObstaculo' pode ser definida da seguinte forma:

@
adicionaObstaculo (Mapa n ((t,lo):_)) seed terreno = let lobst = geraObstaculo (gera seed n) n (terreno, [])
                                                     in if eRelva terreno && eRelva t then substituirEm (escolherIndice seed (elemIndices Nenhum lo)) Nenhum lobst
                                                                                      else lobst
@

-}

adicionaObstaculo :: Mapa -> Int -> Terreno -> [Obstaculo]
adicionaObstaculo (Mapa n ((t,lo):_)) seed terreno = let lobst = geraObstaculo (gera seed n) n (terreno, [])
                                                     in if eRelva terreno && eRelva t then substituirEm (escolherIndice seed (elemIndices Nenhum lo)) Nenhum lobst
                                                                                      else lobst

{- |A função 'escolherIndice', que recebe uma seed e uma lista e que retorna um número inteiro,

Assim, a função 'escolherIndice' pode ser definida da seguinte forma:

@
escolherIndice seed l = let rn = head (gera seed 1)
                            ri = mod rn (length l)
                        in l !! ri
@

-}

escolherIndice :: Int -> [Int] -> Int
escolherIndice seed l = let rn = head (gera seed 1)
                            ri = mod rn (length l)
                        in l !! ri

{- |A função 'substituirEm', que recebe um número inteiro (abcissa), um obstáculo e uma lista de obstáculos e retorna uma lista de obstáculos,

Assim, a função 'substituirEm' pode ser definida da seguinte forma:

@
substituirEm x obst [] = []
substituirEm 0 obst (h:t) = obst:t
substituirEm x obst (h:t) = h:(substituirEm (x-1) obst t)
@

-}

substituirEm :: Int -> Obstaculo -> [Obstaculo] -> [Obstaculo]
substituirEm x obst [] = []
substituirEm 0 obst (h:t) = obst:t
substituirEm x obst (h:t) = h:(substituirEm (x-1) obst t)

{- |A função 'eRelva', que recebe um terreno e retorna um bool,

Assim, a função 'eRelva' pode ser definida da seguinte forma:

@
eRelva Relva = True
eRelva _ = False
@

-}

eRelva :: Terreno -> Bool
eRelva Relva = True
eRelva _ = False

{- |A função 'eRio', que recebe um terreno e retorna um bool,

Assim, a função 'eRio' pode ser definida da seguinte forma:

@
eRio (Rio _) = True
eRio _ = False
@

-}

eRio :: Terreno -> Bool
eRio (Rio _) = True
eRio _ = False

{- |A função 'calcProxVelPossivel', que recebe uma lista de pares de terreno e de lista de obstáculos e um terreno e que retorna uma lista de velocidades,

Assim, a função 'calcProxVelPossivel' pode ser definida da seguinte forma:

@
calcProxVelPossivel l t0 = let va = if eRio t0 && rioAnterior l then velocRioAnterior l
                                                                else 0
                               lvp = if va < 0 then [1..velocidade_max]
                                               else if va > 0 then [(-velocidade_max)..(-1)]
                                                              else [(-velocidade_max)..(-1)] ++ [1..velocidade_max]
                           in lvp
@

-}

calcProxVelPossivel :: [(Terreno,[Obstaculo])] -> Terreno -> [Velocidade]
calcProxVelPossivel l t0 = let va = if eRio t0 && rioAnterior l then velocRioAnterior l
                                                                else 0
                               lvp = if va < 0 then [1..velocidade_max]
                                               else if va > 0 then [(-velocidade_max)..(-1)]
                                                              else [(-velocidade_max)..(-1)] ++ [1..velocidade_max]
                           in lvp

{- | A função 'proximosTerrenosValidos' devolve uma lista dos terrenos passíveis de ser utilizados numa próxima linha do mapa, a partir de uma ou mais linhas anteriores.
Aqui podemos limitar a quantidade de terrenos contíguos de cada tipo, com um máximo de 4 rios seguidos, e 5 estradas ou relvas seguidas.

Assim, a função 'proximosTerrrenosValidos' pode ser definida da seguinte forma:

@
proximosTerrenosValidos (Mapa _ ((Rio _, _):(Rio _, _):(Rio _, _):(Rio _, _):t)) = [Estrada 0, Relva]
proximosTerrenosValidos (Mapa _ ((Estrada _, _):(Estrada _, _):(Estrada _, _):(Estrada _, _):(Estrada _, _):t)) = [Rio 0, Relva]
proximosTerrenosValidos (Mapa _ ((Relva, _):(Relva, _):(Relva, _):(Relva, _):(Relva, _):t)) = [Rio 0, Estrada 0]
proximosTerrenosValidos _ = [Rio 0, Estrada 0, Relva]
@

-}

proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa _ ((Rio _, _):(Rio _, _):(Rio _, _):(Rio _, _):t)) = [Estrada 0, Relva]
proximosTerrenosValidos (Mapa _ ((Estrada _, _):(Estrada _, _):(Estrada _, _):(Estrada _, _):(Estrada _, _):t)) = [Rio 0, Relva]
proximosTerrenosValidos (Mapa _ ((Relva, _):(Relva, _):(Relva, _):(Relva, _):(Relva, _):t)) = [Rio 0, Estrada 0]
proximosTerrenosValidos _ = [Rio 0, Estrada 0, Relva]


{- | A função 'geraTerreno' cria, com base em aleatórios devolvidos pela função 'gera' e nas possibilidades oferecidas pela função 'proximosTerrenosValidos', 
um terreno que será utilizado numa próxima linha do mapa.

Assim, a função 'geraTerreno' pode ser definida da seguinte forma:

@
geraTerreno seed l = let rn = head (gera seed 1)
                         ri = mod rn (length l)
                     in l !! ri
@

-}

geraTerreno :: Int -> [Terreno] -> Terreno
geraTerreno seed l = let rn = head (gera seed 1)
                         ri = mod rn (length l)
                     in l !! ri


{- | A função 'rioAnterior' será usada na função 'estendeMapa' para verificar, no caso de a função 'geraTerreno' gerar Rio para a nova linha, se a linha prévia mais recente do mapa usa o terreno @Rio _@.

Assim, a função 'rioAnterior' pode ser definida da seguinte forma:

@
rioAnterior ((Rio _,_):_) = True
rioAnterior _ = False
@

-}

rioAnterior :: [(Terreno, [Obstaculo])] -> Bool
rioAnterior ((Rio _,_):_) = True
rioAnterior _ = False


{- | A função 'velocRioAnterior' será usada na função 'estendeMapa' para, no caso de a função auxiliar 'rioAnterior' devolver @True@, tomar a velocidade com que esse rio anterior se move. 

Assim, a função 'velocRioAnterior' pode ser definida da seguinte forma:

@
velocRioAnterior ((Rio v,_):_) = v
velocRioAnterior _ = 0
@

-}

velocRioAnterior :: [(Terreno, [Obstaculo])] -> Velocidade
velocRioAnterior ((Rio v,_):_) = v
velocRioAnterior _ = 0


{- | A função 'geraVelocidade' dá, com base em aleatórios devolvidos pela função 'gera', uma velocidade que será associada a um terreno numa próxima linha do mapa.

No caso de a linha anterior usar um terreno @Rio@ e de a 'geraTerreno' ter atribuído igualmente um terreno @Rio@ à nova linha, com o auxílio das funções 'rioAnterior' e 'velocRioAnterior',
esta função irá atribuir velocidades de direção oposta a rios contíguos.

Assim, a função 'geraVelocidade' pode ser definida da seguinte forma:

@
geraVelocidade seed l = let rn = head (gera seed 1)
                            ri = mod rn (length l)
                        in l !! ri
@

-}

geraVelocidade :: Int -> [Velocidade] -> Velocidade
geraVelocidade seed l = let rn = head (gera seed 1)
                            ri = mod rn (length l)
                        in l !! ri


{- | A função 'adicionaVelocidade' associa a velocidade devolvida pela função 'geraVelocidade' ao terreno selecionado pela função 'geraTerreno'.

Assim, a função 'adicionaVelocidade' pode ser definida da seguinte forma:

@
adicionaVelocidade (Rio _) v = Rio v
adicionaVelocidade (Estrada _) v = Estrada v
adicionaVelocidade Relva _ = Relva
@

-}

adicionaVelocidade :: Terreno -> Velocidade -> Terreno
adicionaVelocidade (Rio _) v = Rio v
adicionaVelocidade (Estrada _) v = Estrada v
adicionaVelocidade Relva _ = Relva


{- | A função 'proximosObstaculosValidos' calcula que obstáculos podem ser utilizados para preencher a lista de obstáculos de uma próxima linha do mapa, conforme o terreno gerado para a nova linha.

Assim, a função 'proximosObstaculosValidos' pode ser definida da seguinte forma:

@
proximosObstaculosValidos n (t,l)
    | length l >= n = []
    | length l == (n-1) && not (elem Nenhum l) = [Nenhum]
    | length l == (n-1) = case t of
                            (Rio _) -> if not (elem Tronco l) then [Tronco]
                                                              else let inicio = contaExtremos l Tronco
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
@

-}

proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos n (t,l)
    | length l >= n = []
    | length l == (n-1) && not (elem Nenhum l) = [Nenhum]
    | length l == (n-1) = case t of
                            (Rio _) -> proximosObstaculosRio l
                            (Estrada _) -> proximosObstaculosEstrada l
                            Relva -> proximosObstaculosRelva l
    | otherwise = case t of
                    (Rio _) -> let inicio = contaExtremos l Tronco
                               in if inicio < 5 then [Nenhum, Tronco]
                                                else [Nenhum]
                    (Estrada _) -> let inicio = contaExtremos l Carro
                                   in if inicio < 3 && numeroNenhum l <= 5 then [Nenhum]
                                      else if inicio < 3 then [Nenhum,Carro]
                                           else [Nenhum]
                    Relva -> [Nenhum, Arvore]

{- | A função 'numeroNenhum', que recebe uma lista de obstáculos e retorna um número inteiro, 

Assim, a função 'numeroNenhum' pode ser definida da seguinte forma:

@
numeroNenhum [] = 0
numeroNenhum (h:t) | h == Nenhum = 1 + numeroNenhum t
                   | otherwise = numeroNenhum t
@

-}

numeroNenhum :: [Obstaculo] -> Int
numeroNenhum [] = 0
numeroNenhum (h:t) | h == Nenhum = 1 + numeroNenhum t
                   | otherwise = numeroNenhum t

{- | A função 'proximosObstaculosRio', que recebe e retorna uma lista de obstáculos,

Assim, a função 'proximosObstaculosRio' pode ser definida da seguinte forma:

@
proximosObstaculosRio l = if not (elem Tronco l) then [Tronco]
                                                 else let inicio = contaExtremos l Tronco
                                                          fim = contaExtremos (reverse l) Tronco  
                                                      in if inicio + fim < 5 then [Nenhum, Tronco]
                                                                             else [Nenhum]
@

-}

proximosObstaculosRio :: [Obstaculo] -> [Obstaculo]
proximosObstaculosRio l = if not (elem Tronco l) then [Tronco]
                                                 else let inicio = contaExtremos l Tronco
                                                          fim = contaExtremos (reverse l) Tronco  
                                                      in if inicio + fim < 5 then [Nenhum, Tronco]
                                                                             else [Nenhum]

{- | A função 'proximosObstaculosEstrada', que recebe e retorna uma lista de obstáculos,

Assim, a função 'proximosObstaculosEstrada' pode ser definida da seguinte forma:

@
proximosObstaculosEstrada l = if not (elem Carro l) then [Carro]
                                                    else let inicio = contaExtremos l Carro
                                                             fim = contaExtremos (reverse l) Carro  
                                                         in if inicio + fim < 3 then [Nenhum, Carro]
                                                                                 else [Nenhum]
@

-}

proximosObstaculosEstrada :: [Obstaculo] -> [Obstaculo]
proximosObstaculosEstrada l = if not (elem Carro l) then [Carro]
                                                    else let inicio = contaExtremos l Carro
                                                             fim = contaExtremos (reverse l) Carro  
                                                         in if inicio + fim < 3 then [Nenhum, Carro]
                                                                                 else [Nenhum]

{- | A função 'proximosObstaculosRelva', que recebe e retorna uma lista de obstáculos,

Assim, a função 'proximosObstaculosRelva' pode ser definida da seguinte forma:

@
proximosObstaculosRelva l = [Nenhum, Arvore]
@

-}

proximosObstaculosRelva :: [Obstaculo] -> [Obstaculo]
proximosObstaculosRelva l = [Nenhum, Arvore]

{- | A função 'contaExtremos' é uma função auxiliar da função 'proximosObstaculosValidos' criada para garantir que obstáculos como @Tronco@ e @Carro@ não ultrapassam o seu comprimento máximo.

Assim, a função 'contaExtremos' pode ser definida da seguinte forma:

@
contaExtremos [] _ = 0
contaExtremos (h:t) o = if h == o then 1 + contaExtremos t o 
                                  else 0
@

-}

contaExtremos :: [Obstaculo] -> Obstaculo -> Int
contaExtremos [] _ = 0
contaExtremos (h:t) o = if h == o then 1 + contaExtremos t o 
                                  else 0


{- | A função 'geraObstaculo' dá, com base em aleatórios devolvidos pela função 'gera' e na lista de possibilidades criada pela função 'proximosObstaculosValidos', 
uma lista de obstáculos que será usada numa próxima linha do mapa. 

Assim, a função 'geraObstaculos' pode ser definida da seguinte forma:

@
geraObstaculo [] _ _ = []
geraObstaculo (r:rs) n (t,lo)
    | length lo == n = []
    | otherwise = let pov = proximosObstaculosValidos n (t,lo)
                      po = mod r (length pov)
                  in (pov !! po) : geraObstaculo rs n (t,(pov !! po):lo)
@

-}

geraObstaculo :: [Int] -> Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
geraObstaculo [] _ _ = []
geraObstaculo (r:rs) n (t,lo)
    | length lo == n = []
    | otherwise = let pov = proximosObstaculosValidos n (t,lo)
                      po = mod r (length pov)
                  in (pov !! po) : geraObstaculo rs n (t,(pov !! po):lo)