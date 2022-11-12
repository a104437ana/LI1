{- |
Module      : Tarefa1_2022li1g030
Description : Validação de um mapa
Copyright   : Ana Sá Oliveira <a104437@alunos.uminho.pt>
              Sara Campos Ramalho <a72481@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}
module Tarefa1_2022li1g030 (
-- * Função mapaValido
-- ** Função principal
mapaValido,
-- ** Funções auxiliares
obstaculoTerrenoProprio , riosDirecaoOposta , duplicarListaObstaculos , mapaListaObstaculosDuplicados , compMaxObstaculos , umNenhumNoMinimo , larguraCompObstaculos , maxTerrenoContiguo
) where

import LI12223

{- |A função 'mapaValido', que recebe um mapa e retorna um bool, verifica se um dado mapa é válido, ou seja, se um dado mapa não viola nenhuma das seguintes restrições:

* Não existem obstáculos em terrenos impróprios (por exemplo, troncos em estradas ou relvas, árvores em rios ou estradas, etc.);

* Rios contíguos têm direções opostas;

* Troncos têm, no máximo, 5 unidades de comprimento;

* Carros têm, no máximo, 3 unidades de comprimento;

* Em qualquer linha existe, no mínimo, um “obstáculo" Nenhum, ou seja, uma linha não pode ser composta exclusivamente por obstáculos, precisando de haver pelo menos um espaço livre;

* O comprimento da lista de obstáculos de cada linha corresponde exatamente à largura do mapa;

* Contiguamente, não devem existir mais do que 4 rios, nem 5 estradas ou relvas.

A função 'mapaValido' pode ser definida em função de outras funções auxiliares: a função 'obstaculoTerrenoProprio', a função 'riosDirecaoOposta', a função 'compMaxObstaculos', a função
'umNenhumNoMinimo', a função 'larguraCompObstaculos' e a função 'maxTerrenoContiguo', funções que recebem um mapa e retornam um bool e que verificam as restrições anteriormente enunciadas. 

Se uma destas funções auxiliares retornar False, ou seja, se uma das restrições não for respeitada, então automaticamente o mapa será inválido, ou seja, a função mapaValido irá retornar
False. Assim, para o mapa ser válido, ou seja, para a função mapaValido retornar True, então todas as restrições terão de ser respeitadas, logo todas as funções auxiliares terão de
retornar True.

Assim, a função 'mapaValido' pode ser definida da seguinte forma:

@
mapaValido (Mapa n l) = obstaculoTerrenoProprio (Mapa n l) && riosDirecaoOposta (Mapa n l) && compMaxObstaculos (mapaListaObstaculosDuplicados (Mapa n l)) && umNenhumNoMinimo (Mapa n l) && larguraCompObstaculos (Mapa n l) && maxTerrenoContiguo ((Mapa n l)
@

A função 'mapaValido' contém ainda outra função auxiliar, a função 'mapaListaObstaculosDuplicados' que por sua vez contém outra função auxiliar, a função 'duplicarListaObstaculos'.

-}

mapaValido :: Mapa -> Bool
mapaValido (Mapa n l) = obstaculoTerrenoProprio (Mapa n l) && riosDirecaoOposta (Mapa n l) && compMaxObstaculos (mapaListaObstaculosDuplicados (Mapa n l)) && umNenhumNoMinimo (Mapa n l) && larguraCompObstaculos (Mapa n l) && maxTerrenoContiguo (Mapa n l)

{- |A função 'obstaculoTerrenoProprio', que recebe um mapa e retorna um bool, verifica se num dado mapa não existem obstáculos em terrenos impróprios, ou seja, verifica que não existem 
Troncos em Estradas ou Relvas, que não existem Carros em Rios ou Relvas e que não existem Arvores em Rios ou Estradas.

Esta função percorre toda a lista de obstáculos á procura de um obstáculo impróprio para o terreno dado. Se encontrar um obstáculo impŕoprio para o terreno, a função retornara False. Se 
não encontrar, então a função irá chamar a ela mesma (função recursiva) para procurar na próxima lista de obstáculos um obstáculo impróprio para o terreno dado. Quando a função tiver 
percorrido todas as listas de obstáculos do Mapa sem encontrar nenhum obstáculo impróprio para o terreno dado e quando apenas sobrar a lista vazia, a função irá retornar True.

Assim, a função 'obstaculoTerrenoProprio' pode ser definida da seguinte forma:

@
obstaculoTerrenoProprio (Mapa n ((Rio n1,(h:t1)):t)) | h == Tronco || h == Nenhum = obstaculoTerrenoProprio (Mapa n ((Rio n1,t1):t))
                                                     | otherwise = False
obstaculoTerrenoProprio (Mapa n ((Estrada n1,(h:t1)):t)) | h == Carro || h == Nenhum = obstaculoTerrenoProprio (Mapa n ((Estrada n1,t1):t))
                                                         | otherwise = False
obstaculoTerrenoProprio (Mapa n ((Relva,(h:t1)):t)) | h == Arvore || h == Nenhum = obstaculoTerrenoProprio (Mapa n ((Relva,t1):t))
                                                    | otherwise = False
obstaculoTerrenoProprio (Mapa n ((_,[]):t)) = obstaculoTerrenoProprio (Mapa n t)
obstaculoTerrenoProprio (Mapa n []) = True
@

-}

obstaculoTerrenoProprio :: Mapa -> Bool
obstaculoTerrenoProprio (Mapa n ((Rio n1,(h:t1)):t)) | h == Tronco || h == Nenhum = obstaculoTerrenoProprio (Mapa n ((Rio n1,t1):t))
                                                     | otherwise = False
obstaculoTerrenoProprio (Mapa n ((Estrada n1,(h:t1)):t)) | h == Carro || h == Nenhum = obstaculoTerrenoProprio (Mapa n ((Estrada n1,t1):t))
                                                         | otherwise = False
obstaculoTerrenoProprio (Mapa n ((Relva,(h:t1)):t)) | h == Arvore || h == Nenhum = obstaculoTerrenoProprio (Mapa n ((Relva,t1):t))
                                                    | otherwise = False
obstaculoTerrenoProprio (Mapa n ((_,[]):t)) = obstaculoTerrenoProprio (Mapa n t)
obstaculoTerrenoProprio (Mapa n []) = True

{- |A função 'riosDirecaoOposta', que recebe um mapa e retorna um bool, verifica se num dado mapa rios contíguos apresentam direções opostas, ou seja, verifica se rios contíguos apresentam
velocidades de sinal oposto (por exemplo, o primeiro rio com velocidade positiva e o segundo rio contíguo ao primeiro com velocidade negativa ou vice-versa). Se isso não se verificar, a 
função retorna False. Se isso se verificar, então a função irá chamar a ela mesma (função recursiva) para verificar se o segundo rio tem direção oposta do terceiro ou se após o segundo rio
existe um terreno diferente de rio. Quando a função percorrer todo o mapa e não encontrar nenhuns rios contíguos com direções opostas e quando apenas sobrar a lista vazia ou uma lista de 
um elemento, a função irá retornar True.

Nota : iremos considerar que rios contíguos com velocidade igual a 0, ou rios contíguos em que apenas um tem velocidade igual a 0, são rios de direções opostas.

Assim, a função 'riosDirecaoOposta' pode ser definida da seguinte forma:

@
riosDirecaoOposta (Mapa n ((Rio n1,t1):(Rio n2,t2):t)) | n1>=0 && n2<=0 = riosDirecaoOposta (Mapa n ((Rio n2,t2):t))
                                                       | n1<=0 && n2>=0 = riosDirecaoOposta (Mapa n ((Rio n2,t2):t))
                                                       | otherwise = False 
riosDirecaoOposta (Mapa n ((_,t1):(Rio n2,t2):t)) = riosDirecaoOposta (Mapa n ((Rio n2,t2):t))
riosDirecaoOposta (Mapa n ((_,t1):(_,t2):t)) = riosDirecaoOposta (Mapa n t)
riosDirecaoOposta (Mapa n [(_,t1)]) = True
riosDirecaoOposta (Mapa n []) = True
@

-}

riosDirecaoOposta :: Mapa -> Bool
riosDirecaoOposta (Mapa n ((Rio n1,t1):(Rio n2,t2):t)) | n1>=0 && n2<=0 = riosDirecaoOposta (Mapa n ((Rio n2,t2):t))
                                                       | n1<=0 && n2>=0 = riosDirecaoOposta (Mapa n ((Rio n2,t2):t))
                                                       | otherwise = False 
riosDirecaoOposta (Mapa n ((_,t1):(Rio n2,t2):t)) = riosDirecaoOposta (Mapa n ((Rio n2,t2):t))
riosDirecaoOposta (Mapa n ((_,t1):(_,t2):t)) = riosDirecaoOposta (Mapa n t)
riosDirecaoOposta (Mapa n [(_,t1)]) = True
riosDirecaoOposta (Mapa n []) = True

{- |Queremos verificar se num dado mapa os troncos têm comprimento máximo de 5 unidades e os carros têm comprimento máximo de 3 unidades. Como no terreno Rio e no terreno Estrada, os 
obstáculos podem se mover numa determinada direção, então a ordem dos obstáculos pode mudar na lista de obstáculos. Assim, para além de garantirmos que não existem 6 troncos seguidos ou 4
carros seguidos na lista de obstáculos, também temos de garantir que se juntarmos o final da lista com o inicio da lista também não existem 6 troncos seguidos ou 4 carros seguidos na lista
de obstáculos. 

Para verificar isto, decidi duplicar a lista de obstáculos, ou seja, juntar a lista de obstáculos à própria lista de obstáculos através da função (++) pré-definida do Haskell. Para isso 
usei a função recursiva 'duplicarListaObstaculos', que recebe e retorna uma lista de pares de terrenos e de listas de obstáculos, que pode ser definida da seguinte forma:

@
duplicarListaObstaculos ((ter,l):t) = (ter,l ++ l): duplicarListaObstaculos t
duplicarListaObstaculos [] = []
@

-}

duplicarListaObstaculos :: [(Terreno,[Obstaculo])] -> [(Terreno,[Obstaculo])] 
duplicarListaObstaculos ((ter,l):t) = (ter,l ++ l): duplicarListaObstaculos t
duplicarListaObstaculos [] = []

{- |Depois usei a função 'mapaListaObstaculosDuplicados', que recebe um mapa e retorna um mapa, e que obtém o mapa com as listas de obstáculos duplicados. Esta função utiliza a função 
anterior ('duplicarListaObstaculos') e pode ser definida da seguinte forma:

@
mapaListaObstaculosDuplicados (Mapa n l) = (Mapa n (duplicarListaObstaculos l))
@

-}

mapaListaObstaculosDuplicados :: Mapa -> Mapa
mapaListaObstaculosDuplicados (Mapa n l) = (Mapa n (duplicarListaObstaculos l))

{- |A função 'compMaxObstaculos', que recebe um mapa e retorna um bool, verifica se num dado mapa os troncos têm comprimento máximo de 5 unidades e os carros têm comprimento máximo de 3 
unidades. Se a função 'compMaxObstaculos' receber o mapa resultante da função anterior ('mapaListaObstaculosDuplicados'), então será possível verificar se no mapa original (não duplicado)
os troncos têm comprimento máximo de 5 unidades e os carros têm comprimento máximo de 3 unidades.

Para os troncos terem comprimento máximo de 5 unidades e os carros comprimento máximo de 3 unidades, então numa lista de obstáculos não podem existir 6 troncos seguidos ou 4 carros
seguidos. Se houver 6 troncos seguidos ou 4 carros seguidos então a função retorna False. Se isso não acontecer, então a função irá chamar a ela mesma (função recursiva) para verificar se
não há 6 troncos seguidos ou 4 carros seguidos na próxima lista de obstáculos. Quando a função percorrer todo o mapa e não encontrar 6 troncos seguidos ou 4 carros seguidos em nenhuma das
listas de obstáculos e quando apenas sobrar a lista vazia, a função irá retornar True.
 
Assim, a função 'compMaxObstaculos' pode ser definida da seguinte forma:

@
compMaxObstaculos (Mapa n ((_,(Tronco:Tronco:Tronco:Tronco:Tronco:Tronco:t1)):t)) = False
compMaxObstaculos (Mapa n ((_,(Carro:Carro:Carro:Carro:t1)):t)) = False
compMaxObstaculos (Mapa n ((ter,(_:t1)):t)) = compMaxObstaculos (Mapa n ((ter,t1):t))
compMaxObstaculos (Mapa n ((_,[]):t)) = compMaxObstaculos (Mapa n t)
compMaxObstaculos (Mapa n []) = True
@

-}

compMaxObstaculos :: Mapa -> Bool
compMaxObstaculos (Mapa n ((_,(Tronco:Tronco:Tronco:Tronco:Tronco:Tronco:t1)):t)) = False
compMaxObstaculos (Mapa n ((_,(Carro:Carro:Carro:Carro:t1)):t)) = False
compMaxObstaculos (Mapa n ((ter,(_:t1)):t)) = compMaxObstaculos (Mapa n ((ter,t1):t))
compMaxObstaculos (Mapa n ((_,[]):t)) = compMaxObstaculos (Mapa n t)
compMaxObstaculos (Mapa n []) = True

{- |A função 'umNenhumNoMinimo', que recebe um mapa e retorna um bool, verifica se um dado mapa tem em todas as listas de obstáculos não vazias, no mínimo, um obstáculo Nenhum. Esta função
percorre toda a lista de obstáculos á procura de um obstáculo Nenhum, atraves da função elem pré-definida no Haskell. Se Nenhum não for elemento da lista, ou seja, se elem Nenhum l é False
então a função umNenhumNoMinimo retornara False. Se Nenhum for elemento da lista, ou seja, se elem Nenhum l é True então a função irá chamar a ela mesma (função recursiva) para verificar 
se Nenhum é elemento da próxima lista de obstáculos. Quando a função tiver percorrido todas as listas de obstáculos do Mapa e encontrar no mínimo um obstáculo Nenhum em todas elas e quando
apenas sobrar a lista vazia, a função irá retornar True.

Assim, a função 'umNenhumNoMinimo' pode ser definida da seguinte forma:

@
umNenhumNoMinimo (Mapa n ((_,l):t)) | elem Nenhum l == True = umNenhumNoMinimo (Mapa n t)
                                    | otherwise = False
umNenhumNoMinimo (Mapa n []) = True
@

-}

umNenhumNoMinimo :: Mapa -> Bool
umNenhumNoMinimo (Mapa n ((_,l):t)) | elem Nenhum l == True = umNenhumNoMinimo (Mapa n t)
                                    | otherwise = False
umNenhumNoMinimo (Mapa n []) = True

{- |A função 'larguraCompObstaculos', que recebe um mapa e retorna um bool, verifica se num dado mapa a largura do mapa é igual ao comprimento de todas as listas de obstáculos. Esta função
calcula o comprimeto da lista de obstáculos através da função length pré-definida no Haskel e verifica se esse valor é igual á largura do mapa. Se não for igual, a função retornara False.
Se for igual, então a função irá chamar a ela mesma (função recursiva) para calcular o comprimento da próxima lista de obstáculos  e comparar esse valor com a largura do mapa. Quando a 
função tiver percorrido todas as listas de obstáculos do Mapa sem encontrar nenhuma lista de comprimento diferente à largura e quando apenas sobrar a lista vazia, a função irá retornar 
True.

Assim, a função 'larguraCompObstaculos' pode ser definida da seguinte forma:

@
larguraCompObstaculos (Mapa n ((_,l):t)) | n == length l = larguraCompObstaculos (Mapa n t)
                                         | otherwise = False
larguraCompObstaculos (Mapa n []) = True
@

-}

larguraCompObstaculos :: Mapa -> Bool
larguraCompObstaculos (Mapa n ((_,l):t)) | n == length l = larguraCompObstaculos (Mapa n t)
                                         | otherwise = False
larguraCompObstaculos (Mapa n []) = True

{- |A função 'maxTerrenoContiguo', que recebe um mapa e retorna um bool, verifica se num dado mapa não existem contíguamente mais do que 4 rios, nem 5 estradas ou relvas. Esta função
verifica que os 5 primeiros terrenos não são todos Rio e que os 6 primeiros terrenos não são todos Estrada ou Relva. Se fossem, então a função retornaria False. Se não fossem, então
a função irá chamar a ela mesma (função recursiva) para verificar que os próximos 5 terrenos não são todos Rio ou que os próximos 6 terrenos não são todos Estrada ou Relva. Quando a função
percorrer toda a lista do mapa sendo que não existem contíguamente mais do que 4 rios, nem 5 estradas ou relvas, e quando apenas sobrar a lista vazia, a função irá retornar True.

Assim, a função 'maxTerrenoContiguo' pode ser definida da seguinte forma:

@
maxTerrenoContiguo (Mapa n ((Rio n1,l1):(Rio n2,l2):(Rio n3,l3):(Rio n4,l4):(Rio n5,l5):t)) = False
maxTerrenoContiguo (Mapa n ((Estrada n1,l1):(Estrada n2,l2):(Estrada n3,l3):(Estrada n4,l4):(Estrada n5,l5):(Estrada n6,l6):t)) = False
maxTerrenoContiguo (Mapa n ((Relva,l1):(Relva,l2):(Relva,l3):(Relva,l4):(Relva,l5):(Relva,l6):t)) = False
maxTerrenoContiguo (Mapa n (h:t)) = maxTerrenoContiguo (Mapa n t)
maxTerrenoContiguo (Mapa n []) = True
@

-}

maxTerrenoContiguo :: Mapa -> Bool
maxTerrenoContiguo (Mapa n ((Rio n1,l1):(Rio n2,l2):(Rio n3,l3):(Rio n4,l4):(Rio n5,l5):t)) = False
maxTerrenoContiguo (Mapa n ((Estrada n1,l1):(Estrada n2,l2):(Estrada n3,l3):(Estrada n4,l4):(Estrada n5,l5):(Estrada n6,l6):t)) = False
maxTerrenoContiguo (Mapa n ((Relva,l1):(Relva,l2):(Relva,l3):(Relva,l4):(Relva,l5):(Relva,l6):t)) = False
maxTerrenoContiguo (Mapa n (h:t)) = maxTerrenoContiguo (Mapa n t)
maxTerrenoContiguo (Mapa n []) = True
