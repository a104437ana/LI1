{- |
Module      : Tarefa1_2022li1g030
Description : Validação de um mapa
Copyright   : Ana Sá Oliveira <a104437@alunos.uminho.pt>
              Sara Campos Ramalho <a72481@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}
module Tarefa1_2022li1g030 where

import LI12223

mapaValido :: Mapa -> Bool
mapaValido (Mapa n l) | obstaculoTerrenoProprio (Mapa n l) && riosDirecaoOposta (Mapa n l) && compMaxObstaculos (Mapa n l) && umNenhumNoMinimo (Mapa n l) && larguraCompObstaculos (Mapa n l) && maxTerrenoContiguo (Mapa n l) = True
                      | otherwise = False

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
