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


{- | A função 'gera' devolve números inteiros aleatoriamente, para serem utilizados em funções posteriores de modo a criar aleatoriedade na criação das novas linhas do mapa.

@
gera seed n = take n $ randoms (mkStdGen seed)
@

-}

gera :: Int -> Int -> [Int]
gera seed n = take n $ randoms (mkStdGen seed)


{- | A função 'estendeMapa' gera e acrescenta uma nova linha válida no topo de um dado mapa, de largura @n@ e composto pela lista @l@, em que cada elemento @(Terreno, [Obstaculos])@ representa uma linha do mapa.

No caso de ainda não existir nenhuma linha no mapa, isto é, l = [], a função 'estendeMapa' gera a primeira linha.

O argumento @seed@ é um inteiro aleatório (no intervalo @[0, 100]@), usado para acrescentar pseudo-aleatoriedade à geração da nova linha.

Esta função pode ser definida da seguinte forma: 

@
estendeMapa (Mapa n l) seed = let ptv = proximosTerrenosValidos (Mapa n l)
                                  t0 = geraTerreno seed ptv
                                  va = if t0 == Rio 0 && rioAnterior l then velocRioAnterior l
                                                                       else 0
                                  lvp = if va < 0 then [0..(n-1)]
                                                  else if va > 0 then [-(n-1)..0]
                                                                 else [-(n-1)..(n-1)]
                                  velocidade = geraVelocidade seed lvp
                                  terreno = adicionaVelocidade t0 velocidade
                                  obstaculos = geraObstaculo (gera seed n) n (terreno, [])
                              in Mapa n ((terreno, obstaculos):l)
@

Para esta função, vamos usar duas funções auxiliares principais, a 'proximosTerrenosValidos' e a 'proximosObstaculosValidos', e outras funções auxiliares que a seguir se descrevem.
-}

estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa n l) seed = let ptv = proximosTerrenosValidos (Mapa n l)
                                  t0 = geraTerreno seed ptv
                                  va = if t0 == Rio 0 && rioAnterior l then velocRioAnterior l
                                                                       else 0
                                  lvp = if va < 0 then [0..(n-1)]
                                                  else if va > 0 then [-(n-1)..0]
                                                                 else [-(n-1)..(n-1)]
                                  velocidade = geraVelocidade seed lvp
                                  terreno = adicionaVelocidade t0 velocidade
                                  obstaculos = geraObstaculo (gera seed n) n (terreno, [])
                              in Mapa n ((terreno, obstaculos):l)


{- | A função 'proximosTerrenosValidos' devolve uma lista dos terrenos passíveis de ser utilizados numa próxima linha do mapa, a partir de uma ou mais linhas anteriores.
Aqui podemos limitar a quantidade de terrenos contíguos de cada tipo, com um máximo de 4 rios seguidos, e 5 estradas ou relvas seguidas.

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

@
rioAnterior ((Rio _,_):_) = True
rioAnterior _ = False
@

-}

rioAnterior :: [(Terreno, [Obstaculo])] -> Bool
rioAnterior ((Rio _,_):_) = True
rioAnterior _ = False


{- | A função 'velocRioAnterior' será usada na função 'estendeMapa' para, no caso de a função auxiliar 'rioAnterior' devolver @True@, tomar a velocidade com que esse rio anterior se move. 

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


{- | A função 'contaExtremos' é uma função auxiliar da função 'proximosObstaculosValidos' criada para garantir que obstáculos como @Tronco@ e @Carro@ não ultrapassam o seu comprimento máximo.

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