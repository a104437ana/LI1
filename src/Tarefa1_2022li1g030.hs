{- |
Module      : Tarefa1_2022li1g030
Description : Validação de um mapa
Copyright   : Ana Sá Oliveira <a104437@alunos.uminho.pt>
              Sara Campos Ramalho <a72481@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}
module Tarefa1_2022li1g030 where

import LI12223

{- |A função mapaValido, que recebe um mapa e obtem um bool, verifica se um dado mapa é válido, ou seja, se um dado mapa não viola nenhuma das seguintes restrições:
1. Não existem obstáculos em terrenos impróprios (por exemplo troncos em estradas ou relvas, árvores em rios ou estradas, etc.);
2. Rios contíguos têm direções opostas;
3. Troncos têm, no máximo, 5 unidades de comprimento;
4. Carros têm, no máximo, 3 unidades de comprimento;
5. Em qualquer linha existe, no mínimo, um “obstáculo" Nenhum, ou seja, uma linha não pode ser composta exclusivamente por obstáculos, precisando de haver pelo menos um espaço livre;
6. O comprimento da lista de obstáculos de cada linha corresponde exatamente à largura do mapa;
7. Contiguamente, não devem existir mais do que 4 rios, nem 5 estradas ou relvas.

A função mapaValido pode ser definida em função de outras 6 funções: a função obstaculoTerrenoProprio, a função riosDirecaoOposta, a função compMaxObstaculos, a função umNenhumNoMinimo, a função larguraCompObstaculos e a função maxTerrenoContiguo.
-}
mapaValido :: Mapa -> Bool
mapaValido (Mapa n l) = obstaculoTerrenoProprio (Mapa n l) && riosDirecaoOposta (Mapa n l) && compMaxObstaculos (mapaListaObstaculosDuplicados (Mapa n l)) && umNenhumNoMinimo (Mapa n l) && larguraCompObstaculos (Mapa n l) && maxTerrenoContiguo (duplicarListaMapa (Mapa n l))

obstaculoTerrenoProprio :: Mapa -> Bool
obstaculoTerrenoProprio (Mapa n ((Rio n1,(h:t1)):t)) | h == Tronco || h == Nenhum = obstaculoTerrenoProprio (Mapa n ((Rio n1,t1):t))
                                                     | otherwise = False
obstaculoTerrenoProprio (Mapa n ((Estrada n1,(h:t1)):t)) | h == Carro || h == Nenhum = obstaculoTerrenoProprio (Mapa n ((Estrada n1,t1):t))
                                                         | otherwise = False
obstaculoTerrenoProprio (Mapa n ((Relva,(h:t1)):t)) | h == Arvore || h == Nenhum = obstaculoTerrenoProprio (Mapa n ((Relva,t1):t))
                                                    | otherwise = False
obstaculoTerrenoProprio (Mapa n ((_,[]):t)) = obstaculoTerrenoProprio (Mapa n t)
obstaculoTerrenoProprio (Mapa n []) = True

riosDirecaoOposta :: Mapa -> Bool
riosDirecaoOposta (Mapa n ((Rio n1,t1):(Rio n2,t2):t)) | n1>0 && n2<0 = riosDirecaoOposta (Mapa n ((Rio n2,t2):t))
                                                       | n1<0 && n2>0 = riosDirecaoOposta (Mapa n ((Rio n2,t2):t))
                                                       | otherwise = False 
riosDirecaoOposta (Mapa n ((_,t1):(Rio n2,t2):t)) = riosDirecaoOposta (Mapa n ((Rio n2,t2):t))
riosDirecaoOposta (Mapa n ((_,t1):(_,t2):t)) = riosDirecaoOposta (Mapa n t)
riosDirecaoOposta (Mapa n [(_,t1)]) = True
riosDirecaoOposta (Mapa n []) = True

compMaxObstaculos :: Mapa -> Bool
compMaxObstaculos (Mapa n ((_,(Tronco:Tronco:Tronco:Tronco:Tronco:Tronco:t1)):t)) = False
compMaxObstaculos (Mapa n ((_,(Carro:Carro:Carro:Carro:t1)):t)) = False
compMaxObstaculos (Mapa n ((ter,(_:t1)):t)) = compMaxObstaculos (Mapa n ((ter,t1):t))
compMaxObstaculos (Mapa n ((_,[]):t)) = compMaxObstaculos (Mapa n t)
compMaxObstaculos (Mapa n []) = True

mapaListaObstaculosDuplicados :: Mapa -> Mapa
mapaListaObstaculosDuplicados (Mapa n l) = (Mapa n (duplicarListaObstaculos l))

duplicarListaObstaculos :: [(Terreno,[Obstaculo])] -> [(Terreno,[Obstaculo])] 
duplicarListaObstaculos ((ter,l):t) = ((ter,l ++ l): duplicarListaObstaculos t)
duplicarListaObstaculos [] = []

umNenhumNoMinimo :: Mapa -> Bool
umNenhumNoMinimo (Mapa n ((_,l):t)) | elem Nenhum l == True = umNenhumNoMinimo (Mapa n t)
                                    | otherwise = False
umNenhumNoMinimo (Mapa n []) = True

larguraCompObstaculos :: Mapa -> Bool
larguraCompObstaculos (Mapa n ((_,l):t)) | n == length l = larguraCompObstaculos (Mapa n t)
                                         | otherwise = False
larguraCompObstaculos (Mapa n []) = True

maxTerrenoContiguo :: Mapa -> Bool
maxTerrenoContiguo (Mapa n ((Rio n1,l1):(Rio n2,l2):(Rio n3,l3):(Rio n4,l4):(Rio n5,l5):t)) = False
maxTerrenoContiguo (Mapa n ((Estrada n1,l1):(Estrada n2,l2):(Estrada n3,l3):(Estrada n4,l4):(Estrada n5,l5):(Estrada n6,l6):t)) = False
maxTerrenoContiguo (Mapa n ((Relva,l1):(Relva,l2):(Relva,l3):(Relva,l4):(Relva,l5):(Relva,l6):t)) = False
maxTerrenoContiguo (Mapa n (h:t)) = maxTerrenoContiguo (Mapa n t)
maxTerrenoContiguo (Mapa n []) = True

duplicarListaMapa :: Mapa -> Mapa
duplicarListaMapa (Mapa n l) = (Mapa n (l ++ l))