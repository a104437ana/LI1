module Tarefa2_2022li1g030_Spec where

import LI12223
import Tarefa2_2022li1g030
import Test.HUnit

testsT2 :: Test
testsT2 = TestLabel "Testes Tarefa 2" $ test [testList1, testList2, testList3]

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

--linha em que terreno = Rio _
m6 = Mapa n1 [p1]
n1 = 3
p1 = (Rio 2, [Tronco, Nenhum])

--linha em que terreno = Estrada _
m7 = Mapa n2 [p2]
n2 = 5
p2 = (Estrada (-3), [])

--linha em que terreno = Relva
m8 = Mapa n3 [p3]
n3 = 4
p3 = (Relva, [Nenhum, Arvore, Nenhum])

-- linha em que lista de obstáculos já está preenchida
m9 = Mapa n4 [p4]
n4 = 2
p4 = (Relva, [Nenhum, Arvore])

--máximo 5 troncos
m10 = Mapa n5 [p5]
n5 = 7
p5 = (Rio (-2), [Tronco, Tronco, Tronco, Nenhum, Tronco, Tronco])

--máximo 3 carros
m11 = Mapa n6 [p6]
n6 = 7
p6 = (Estrada (2), [Carro, Nenhum, Carro, Nenhum, Carro, Carro])

--pelo menos 1 Nenhum
m12 = Mapa n7 [p7]
n7 = 5
p7 = (Relva, [Arvore, Arvore, Arvore, Arvore])


testList1 = "Testes função estendeMapa" ~: test [test1, test2]

test1 = "Teste 1: acrescenta uma linha à lista vazia" ~: (length l1) + 1 ~=? length (takeList (estendeMapa m1 5))
test2 = "Teste 2: acrescenta uma linha à lista com elementos" ~: (length l2) + 1 ~=? length (takeList (estendeMapa m2 4))


testList2 = "Testes função proximosTerrenosValidos" ~: test [test3, test4, test5, test6]

test3 = "Teste 3: máximo 4 Rios contíguos" ~: [Estrada 0, Relva] ~=? proximosTerrenosValidos m3
test4 = "Teste 4: máximo 5 Estradas contíguas" ~: [Rio 0, Relva] ~=? proximosTerrenosValidos m4
test5 = "Teste 5: máximo 5 Relvas contíguos" ~: [Rio 0, Estrada 0] ~=? proximosTerrenosValidos m5
test6 = "Teste 6: qualquer Terreno é válido" ~: [Rio 0, Estrada 0, Relva] ~=? proximosTerrenosValidos m2


testList3 = "Testes função proximosObstaculosValidos" ~: test [test7, test8, test9, test10, test11, test12, test13]

test7 = "Teste 7: terreno é Rio" ~: [Nenhum, Tronco] ~=? proximosObstaculosValidos n1 p1
test8 = "Teste 8: terreno é Estrada" ~: [Nenhum, Carro] ~=? proximosObstaculosValidos n2 p2
test9 = "Teste 8: terreno é Relva" ~: [Nenhum, Arvore] ~=? proximosObstaculosValidos n3 p3
test10 = "Teste 9: lista de obstáculos completamente preenchida" ~: [] ~=? proximosObstaculosValidos n4 p4
test11 = "Teste 11: máximo 5 troncos" ~: [Nenhum] ~=? proximosObstaculosValidos n5 p5
test12 = "Teste 12: máximo 3 carros" ~: [Nenhum] ~=? proximosObstaculosValidos n6 p6
test13 = "Teste 13: pelo menos 1 Nenhum" ~: [Nenhum] ~=? proximosObstaculosValidos n7 p7