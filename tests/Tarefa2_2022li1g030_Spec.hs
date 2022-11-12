module Tarefa2_2022li1g030_Spec where

import LI12223
import Tarefa2_2022li1g030
import Test.HUnit

testsT2 :: Test
testsT2 = TestLabel "Testes Tarefa 2" $ test [testList1]

takeList :: Mapa -> [(Terreno,[Obstaculo])]
takeList (Mapa n l) = l


-- mapa inicial lista vazia
m1 = Mapa 4 l1
l1 = []

-- mapa inicial lista preenchida
m2 = Mapa 4 l2
l2 = [(Estrada 3, [Nenhum, Carro, Carro, Nenhum])
     ,(Rio (-1), [Tronco, Tronco, Nenhum, Nenhum])
     ,(Relva, [Nenhum, Arvore, Nenhum, Arvore])
     ,(Relva, [Nenhum, Nenhum, Nenhum, Nenhum])]

-- mapa em que a próxima linha deve ter um terreno diferente de (Rio _)
m3 = Mapa 3 [(Rio 2, [Nenhum, Tronco, Tronco])
            ,(Rio (-1), [Tronco, Nenhum, Nenhum])
            ,(Rio 1, [Nenhum, Tronco, Nenhum])
            ,(Rio (-2), [Tronco, Tronco, Nenhum])]


-- mapa em que a próxima linha deve ter um terreno diferente de (Estrada _)
m4 = Mapa 5 [(Estrada 2, [Nenhum, Nenhum, Carro, Nenhum, Nenhum])
            ,(Estrada (-1), [Nenhum, Carro, Carro, Carro, Nenhum])
            ,(Estrada (-3), [Nenhum, Nenhum, Nenhum, Carro, Carro])
            ,(Estrada 4, [Nenhum, Carro, Nenhum, Nenhum, Nenhum])
            ,(Estrada (-2), [Carro, Carro, Nenhum, Nenhum, Carro])]


-- mapa em que a próxima linha deve ter um terreno diferente de (Relva)
m5 = Mapa 4 [(Relva, [Nenhum, Arvore, Nenhum, Nenhum])
            ,(Relva, [Arvore, Nenhum, Nenhum, Nenhum])
            ,(Relva, [Nenhum, Nenhum, Arvore, Nenhum])
            ,(Relva, [Nenhum, Arvore, Nenhum, Nenhum])
            ,(Relva, [Nenhum, Nenhum, Nenhum, Arvore])]


testList1 = "Testes função estendeMapa" ~: test [test1, test2]

test1 = "Teste 1: acrescenta uma linha à lista vazia" ~: (length l1) + 1 ~=? length (takeList (estendeMapa m1 5))
test2 = "Teste 2: acrescenta uma linha à lista com elementos" ~: (length l2) + 1 ~=? length (takeList (estendeMapa m2 4))