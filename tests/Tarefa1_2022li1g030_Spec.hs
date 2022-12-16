module Tarefa1_2022li1g030_Spec where

import LI12223
import Tarefa1_2022li1g030
import Test.HUnit

-- Exemplos de Mapas Válidos

-- restrição 1 (função obstaculoTerrenoProprioMapa)
m1 = (Mapa 2 [(Rio 1,[Nenhum,Tronco]),(Estrada (-1),[Carro,Nenhum]),(Relva,[Arvore,Nenhum])])

-- restrição 2 (função riosDirecaoOposta)
m2 = (Mapa 2 [(Rio 2,[Tronco,Nenhum]),(Rio (-1),[Nenhum,Tronco]),(Estrada (-1),[Carro,Nenhum])])
m3 = (Mapa 2 [(Estrada (-1),[Carro,Nenhum]),(Rio (-1),[Nenhum,Tronco]),(Rio 1,[Nenhum,Tronco])])
m4 = (Mapa 2 [(Rio 2,[Tronco,Nenhum]),(Estrada (-1),[Carro,Nenhum]),(Rio 2,[Tronco,Nenhum])])
m5 = (Mapa 2 [(Estrada (-1),[Carro,Nenhum]),(Relva,[Arvore,Nenhum]),(Rio (-1),[Nenhum,Tronco]),(Rio 1,[Nenhum,Tronco])])

-- restrição 3 (função compMaxObstaculos em relação aos troncos)
m6 = (Mapa 8 [(Rio 3, [Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum])])
m7 = (Mapa 8 [(Relva, [Arvore,Nenhum,Arvore,Arvore,Nenhum,Nenhum,Arvore,Nenhum]),(Rio 3, [Nenhum,Nenhum,Tronco,Nenhum,Tronco,Tronco,Tronco,Nenhum])])

-- restrição 4 (função compMaxObstaculos em relação aos carros)
m8 = (Mapa 5 [(Estrada (-2), [Carro,Carro,Carro,Nenhum,Nenhum])])
m9 = (Mapa 5 [(Relva, [Arvore,Nenhum,Arvore,Arvore,Nenhum]),(Estrada 2, [Nenhum,Carro,Carro,Nenhum,Nenhum])])

-- restrição 5 (função umNenhumNoMinimo)
m10 = (Mapa 2 [(Rio 2,[Nenhum,Tronco]),(Relva,[Arvore,Nenhum])])
m11 = (Mapa 2 [(Relva, [Nenhum,Nenhum])])

-- restrição 6 (função larguraCompObstaculos)
m12 = (Mapa 4 [(Relva, [Arvore,Nenhum,Arvore,Arvore])])
m13 = (Mapa 2 [(Rio 1,[Nenhum,Tronco]),(Estrada (-1),[Nenhum,Nenhum]),(Relva,[Arvore,Nenhum])])

-- restrição 7 (função maxTerrenoContiguo)
m14 = (Mapa 1 [(Rio 1,[Nenhum]),(Rio (-1),[Nenhum]),(Rio 1,[Nenhum]),(Rio (-1),[Nenhum])])
m15 = (Mapa 2 [(Estrada (-1),[Carro,Nenhum]),(Estrada (-1),[Nenhum,Nenhum]),(Estrada 1,[Carro,Nenhum]),(Estrada (-1),[Carro,Nenhum]),(Estrada 2,[Carro,Nenhum])])
m16 = (Mapa 2 [(Relva,[Arvore,Nenhum]),(Relva,[Arvore,Nenhum]),(Relva,[Arvore,Nenhum]),(Relva,[Arvore,Nenhum]),(Relva,[Arvore,Nenhum])])

-- Exemplos de Mapas Inválidos

-- restrição 1 (função obstaculoTerrenoProprioMapa)
m17 = (Mapa 2 [(Rio 1,[Nenhum,Arvore]),(Estrada (-1),[Carro,Nenhum]),(Relva,[Arvore,Nenhum])])
m18 = (Mapa 2 [(Rio 1,[Nenhum,Tronco]),(Estrada (-1),[Tronco,Nenhum]),(Relva,[Arvore,Nenhum])])
m19 = (Mapa 2 [(Rio 1,[Nenhum,Tronco]),(Estrada (-1),[Carro,Nenhum]),(Relva,[Carro,Nenhum])])

-- restrição 2 (função riosDirecaoOposta)
m20 = (Mapa 2 [(Rio 2,[Tronco,Nenhum]),(Rio 1,[Nenhum,Tronco]),(Estrada (-1),[Carro,Nenhum])])
m21 = (Mapa 2 [(Estrada (-1),[Carro,Nenhum]),(Rio 1,[Nenhum,Tronco]),(Rio 1,[Nenhum,Tronco])])
m22 = (Mapa 2 [(Estrada (-1),[Carro,Nenhum]),(Relva,[Arvore,Nenhum]),(Rio (-1),[Nenhum,Tronco]),(Rio (-2),[Nenhum,Tronco])])

-- restrição 3 (função compMaxObstaculos em relação aos troncos)
m23 = (Mapa 8 [(Rio 3, [Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco,Tronco])])
m24 = (Mapa 8 [(Relva, [Arvore,Nenhum,Arvore,Arvore,Nenhum,Nenhum,Arvore,Nenhum]),(Rio 3, [Tronco,Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco])])

-- restrição 4 (função compMaxObstaculos em relação aos carros)
m25 = (Mapa 5 [(Estrada (-2), [Carro,Carro,Carro,Nenhum,Carro])])
m26 = (Mapa 5 [(Relva, [Arvore,Nenhum,Arvore,Arvore,Nenhum]),(Estrada 2, [Nenhum,Carro,Carro,Carro,Carro])])

-- restrição 5 (função umNenhumNoMinimo)
m27 = (Mapa 2 [(Rio 2,[Nenhum,Tronco]),(Relva,[Arvore,Arvore])])
m28 = (Mapa 3 [(Estrada (-1),[Carro,Carro,Carro])])

-- restrição 6 (função larguraCompObstaculos)
m29 = (Mapa 4 [(Relva, [Nenhum,Arvore,Arvore])])
m30 = (Mapa 2 [(Rio 1,[Nenhum,Tronco]),(Estrada (-1),[Nenhum,Nenhum]),(Relva,[Arvore])])

-- restrição 7 (função maxTerrenoContiguo)
m31 = (Mapa 1 [(Relva,[Nenhum]),(Rio 1,[Nenhum]),(Rio 1,[Nenhum]),(Rio 1,[Nenhum]),(Rio 1,[Nenhum]),(Rio 1,[Nenhum])])
m32 = (Mapa 2 [(Estrada (-1),[Carro,Nenhum]),(Estrada (-1),[Nenhum,Nenhum]),(Estrada 1,[Carro,Nenhum]),(Estrada (-1),[Carro,Nenhum]),(Estrada 2,[Carro,Nenhum]),(Estrada 2,[Carro,Nenhum])])
m33 = (Mapa 1 [(Relva,[Arvore,Nenhum]),(Relva,[Arvore,Nenhum]),(Relva,[Arvore,Nenhum]),(Relva,[Arvore,Nenhum]),(Relva,[Arvore,Nenhum]),(Relva,[Arvore,Nenhum])])

-- Testes de mapaValido

-- testar mapaValido quando o mapa é válido
teste1 = "Teste 1" ~: True ~=? (mapaValido m1)
teste2 = "Teste 2" ~: True ~=? (mapaValido m2)
teste3 = "Teste 3" ~: True ~=? (mapaValido m3)
teste4 = "Teste 4" ~: True ~=? (mapaValido m4)
teste5 = "Teste 5" ~: True ~=? (mapaValido m5)
teste6 = "Teste 6" ~: True ~=? (mapaValido m6)
teste7 = "Teste 7" ~: True ~=? (mapaValido m7)
teste8 = "Teste 8" ~: True ~=? (mapaValido m8)
teste9 = "Teste 9" ~: True ~=? (mapaValido m9)
teste10 = "Teste 10" ~: True ~=? (mapaValido m10)
teste11 = "Teste 11" ~: True ~=? (mapaValido m11)
teste12 = "Teste 12" ~: True ~=? (mapaValido m12)
teste13 = "Teste 13" ~: True ~=? (mapaValido m13)
teste14 = "Teste 14" ~: True ~=? (mapaValido m14)
teste15 = "Teste 15" ~: True ~=? (mapaValido m15)
teste16 = "Teste 16" ~: True ~=? (mapaValido m16)

-- testar mapaValido quando o mapa é inválido
teste17 = "Teste 17" ~: False ~=? (mapaValido m17)
teste18 = "Teste 18" ~: False ~=? (mapaValido m18)
teste19 = "Teste 19" ~: False ~=? (mapaValido m19)
teste20 = "Teste 20" ~: False ~=? (mapaValido m20)
teste21 = "Teste 21" ~: False ~=? (mapaValido m21)
teste22 = "Teste 22" ~: False ~=? (mapaValido m22)
teste23 = "Teste 23" ~: False ~=? (mapaValido m23)
teste24 = "Teste 24" ~: False ~=? (mapaValido m24)
teste25 = "Teste 25" ~: False ~=? (mapaValido m25)
teste26 = "Teste 26" ~: False ~=? (mapaValido m26)
teste27 = "Teste 27" ~: False ~=? (mapaValido m27)
teste28 = "Teste 28" ~: False ~=? (mapaValido m28)
teste29 = "Teste 29" ~: False ~=? (mapaValido m29)
teste30 = "Teste 30" ~: False ~=? (mapaValido m30)
teste31 = "Teste 31" ~: False ~=? (mapaValido m31)
teste32 = "Teste 32" ~: False ~=? (mapaValido m32)
teste33 = "Teste 33" ~: False ~=? (mapaValido m33)

testsT1 :: Test
testsT1 = TestLabel "Testes Tarefa 1" $ test [teste1, teste2, teste3, teste4, teste5, teste6, teste7, teste8, teste9, teste10, teste11, teste12, teste13, teste14, teste15, teste16, teste17, teste18, teste19, teste20, teste21, teste22, teste23, teste24, teste25, teste26, teste27, teste28, teste29, teste30, teste31, teste32, teste33]
