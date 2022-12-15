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
obstaculoTerrenoProprioMapa, obstaculoTerrenoProprioLinha , riosDirecaoOposta , duplicarListaObstaculos , mapaListaObstaculosDuplicados , compMaxObstaculos , umNenhumNoMinimo , larguraCompObstaculos , maxTerrenoContiguo
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

A função 'mapaValido' pode ser definida em função de outras funções auxiliares: a função 'obstaculoTerrenoProprioMapa', a função 'riosDirecaoOposta', a função 'compMaxObstaculos', a função
'umNenhumNoMinimo', a função 'larguraCompObstaculos' e a função 'maxTerrenoContiguo', funções que recebem um mapa e retornam um bool e que verificam as restrições anteriormente enunciadas. 

Se uma destas funções auxiliares retornar False, ou seja, se uma das restrições não for respeitada, então automaticamente o mapa será inválido, ou seja, a função mapaValido irá retornar
False. Assim, para o mapa ser válido, ou seja, para a função mapaValido retornar True, então todas as restrições terão de ser respeitadas, logo todas as funções auxiliares terão de
retornar True.

Assim, a função 'mapaValido' pode ser definida da seguinte forma:

@
mapaValido m = 
    obstaculoTerrenoProprioMapa m && 
    riosDirecaoOposta m && 
    compMaxObstaculos (mapaListaObstaculosDuplicados m) && 
    umNenhumNoMinimo m && 
    larguraCompObstaculos m && 
    maxTerrenoContiguo m
@

A função 'mapaValido' contém ainda outra função auxiliar, a função 'mapaListaObstaculosDuplicados' que por sua vez contém outra função auxiliar, a função 'duplicarListaObstaculos'.

Por fim, a função 'obstaculoTerrenoProprioMapa' contém ainda uma função auxiliar, a função 'obstaculoTerrenoProprioLinha'.

-}

mapaValido :: Mapa -> Bool
mapaValido m = 
    obstaculoTerrenoProprioMapa m && 
    riosDirecaoOposta m && 
    compMaxObstaculos (mapaListaObstaculosDuplicados m) && 
    umNenhumNoMinimo m && 
    larguraCompObstaculos m && 
    maxTerrenoContiguo m

{- |A função 'obstaculoTerrenoProprioMapa', que recebe um mapa e retorna um bool, verifica se num dado mapa não existem obstáculos em terrenos impróprios, ou seja, verifica que não existem 
Troncos em Estradas ou Relvas, que não existem Carros em Rios ou Relvas e que não existem Arvores em Rios ou Estradas.

Para esta função retornar True, então todos os elementos da lista do mapa (todas as linhas do mapa) terão de satisfazer uma condição (devido a função all), condição essa, definida na 
função auxiliar 'obstaculoTerrenoProprioLinha' que iremos ver a seguir. Se a lista do mapa for vazia a função também retorna True e se nem todos os elementos da lista satisfazerem a tal 
condição, então a função irá retornar False.

Assim, a função 'obstaculoTerrenoProprioMapa' pode ser definida da seguinte forma:

@
obstaculoTerrenoProprioMapa (Mapa _ l) = if l == [] then True
                                         else all obstaculoTerrenoProprioLinha l
@

-}

obstaculoTerrenoProprioMapa :: Mapa -> Bool
obstaculoTerrenoProprioMapa (Mapa _ l) = if l == [] then True
                                         else all obstaculoTerrenoProprioLinha l

{- |A função 'obstaculoTerrenoProprioLinha', que recebe uma lista de pares de terrenos e de listas de obstáculos (uma linha do mapa) e que retorna um bool, verifica se numa linha do mapa
não existem obstáculos impróprios para o terreno dessa linha. 

Se o terreno for Rio, então sabemos que não pode existir na lista nem um carro nem uma árvore. Se o terreno for Estrada, então sabemos que não pode existir na lista nem um tronco nem uma 
árvore. Se o terreno for Relva, então sabemos que não pode existir na lista nem um tronco nem um carro. Para verificar estas condições usamos a função auxiliar pré-definida no Haskell, 
notElem, que praticamente faz o oposto da função elem, ou seja, verifica se algo não pertence à lista. Se o obstáculo impŕóprio a um terreno não pertencer à lista desse terreno (mapa 
válido), então a função retorna True. Se obstáculo impŕóprio a um terreno pertencer à lista desse terreno (mapa inválido), então a função retorna False.

Assim, a função 'obstaculoTerrenoProprioLinha' pode ser definida da seguinte forma:

@
obstaculoTerrenoProprioLinha (ter,lo) = 
    case ter of Rio _ -> notElem Carro lo && notElem Arvore lo
                Estrada _ -> notElem Tronco lo && notElem Arvore lo
                Relva -> notElem Tronco lo && notElem Carro lo
@

-}

obstaculoTerrenoProprioLinha :: (Terreno,[Obstaculo]) -> Bool
obstaculoTerrenoProprioLinha (ter,lo) = 
    case ter of Rio _ -> notElem Carro lo && notElem Arvore lo
                Estrada _ -> notElem Tronco lo && notElem Arvore lo
                Relva -> notElem Tronco lo && notElem Carro lo

{- |A função 'riosDirecaoOposta', que recebe um mapa e retorna um bool, verifica se num dado mapa rios contíguos apresentam direções opostas, ou seja, verifica se rios contíguos apresentam
velocidades de sinal oposto (por exemplo, o primeiro rio com velocidade positiva e o segundo rio contíguo ao primeiro com velocidade negativa ou vice-versa). Se isso não se verificar, a 
função retorna False. Se isso se verificar, então a função irá chamar a ela mesma (função recursiva) para verificar se o segundo rio tem direção oposta do terceiro ou se após o segundo rio
existe um terreno diferente de rio. Quando a função percorrer todo o mapa e não encontrar nenhuns rios contíguos com direções opostas e quando apenas sobrar a lista vazia ou uma lista de 
um elemento, a função irá retornar True.

Nota : iremos considerar que rios contíguos com velocidade igual a 0, ou rios contíguos em que apenas um tem velocidade igual a 0, são rios de direções opostas.

Assim, a função 'riosDirecaoOposta' pode ser definida da seguinte forma:

@
riosDirecaoOposta (Mapa n l) = 
    case l of ((Rio v1,_):(Rio v2,lo2):t) | v1>=0 && v2<=0 -> riosDirecaoOposta (Mapa n ((Rio v2,lo2):t))
                                          | v1<=0 && v2>=0 -> riosDirecaoOposta (Mapa n ((Rio 2,lo2):t))
                                          | otherwise -> False 
              ((_,_):(Rio v2,lo2):t) -> riosDirecaoOposta (Mapa n ((Rio v2,lo2):t))
              ((_,_):(_,_):t) -> riosDirecaoOposta (Mapa n t)
              [(_,_)] -> True
              [] -> True
@

-}

riosDirecaoOposta :: Mapa -> Bool
riosDirecaoOposta (Mapa n l) = 
    case l of ((Rio v1,_):(Rio v2,lo2):t) | v1>=0 && v2<=0 -> riosDirecaoOposta (Mapa n ((Rio v2,lo2):t))
                                          | v1<=0 && v2>=0 -> riosDirecaoOposta (Mapa n ((Rio 2,lo2):t))
                                          | otherwise -> False 
              ((_,_):(Rio v2,lo2):t) -> riosDirecaoOposta (Mapa n ((Rio v2,lo2):t))
              ((_,_):(_,_):t) -> riosDirecaoOposta (Mapa n t)
              [(_,_)] -> True
              [] -> True

{- |Queremos verificar se num dado mapa os troncos têm comprimento máximo de 5 unidades e os carros têm comprimento máximo de 3 unidades. Como no terreno Rio e no terreno Estrada, os 
obstáculos podem se mover numa determinada direção, então a ordem dos obstáculos pode mudar na lista de obstáculos. Assim, para além de garantirmos que não existem 6 troncos seguidos ou 4
carros seguidos na lista de obstáculos, também temos de garantir que se juntarmos o final da lista com o inicio da lista também não existem 6 troncos seguidos ou 4 carros seguidos na lista
de obstáculos. 

Para verificar isto, decidi duplicar a lista de obstáculos, ou seja, juntar a lista de obstáculos à própria lista de obstáculos através da função (++) pré-definida do Haskell. Para isso 
usei a função recursiva 'duplicarListaObstaculos', que recebe e retorna uma lista de pares de terrenos e de listas de obstáculos (a lista do mapa), que pode ser definida da seguinte forma:

@
duplicarListaObstaculos l = 
    case l of [] -> []
              ((ter,lo):t) -> (ter,lo ++ lo): duplicarListaObstaculos t
@

Nota : "Duplicar" a lista dos obstáculos para verificar o comprimento máximo dos obstáculos apenas poderia trazer problemas numa linha com 5 troncos apenas ou numa linha com 3 carros 
apenas. Nestes casos, o comprimento dos obstáculos é válido mas utilizando a função 'duplicarListaObstaculos', esta irá construir uma linha em que o comprimento dos obstáculos é inválido
(10 troncos seguidos ou então 6 carros seguidos). No entanto, podemos descartar este problema porque para o mapa ser válido, tem de existir sempre pelo menos um Nenhum em todas as linhas
(condição garantida pela função 'umNenhumNoMinimo' que iremos definir posteriormente). Por esta razão, podemos "duplicar" a lista de obstáculos sem preocupações. 

-}

duplicarListaObstaculos :: [(Terreno,[Obstaculo])] -> [(Terreno,[Obstaculo])] 
duplicarListaObstaculos l = 
    case l of [] -> []
              ((ter,lo):t) -> (ter,lo ++ lo): duplicarListaObstaculos t

{- |Depois usei a função 'mapaListaObstaculosDuplicados', que recebe um mapa (o mapa original) e retorna um mapa (o mapa com as listas de obstáculos duplicados). Esta função utiliza a 
função anterior ('duplicarListaObstaculos') e pode ser definida da seguinte forma:

@
mapaListaObstaculosDuplicados (Mapa n l) = (Mapa n (duplicarListaObstaculos l))
@

-}

mapaListaObstaculosDuplicados :: Mapa -> Mapa
mapaListaObstaculosDuplicados (Mapa n l) = (Mapa n (duplicarListaObstaculos l))

{- |A função 'compMaxObstaculos', que recebe um mapa e retorna um bool, verifica se num dado mapa os troncos têm comprimento máximo de 5 unidades e os carros têm comprimento máximo de 3 
unidades. Se a função 'compMaxObstaculos' receber o mapa resultante da função anterior 'mapaListaObstaculosDuplicados', ou seja, se receber o mapa com as listas de obstáculos duplicados,
então será possível verificar se no mapa original (não duplicado) os troncos têm comprimento máximo de 5 unidades e os carros têm comprimento máximo de 3 unidades.

Para os troncos terem comprimento máximo de 5 unidades e os carros comprimento máximo de 3 unidades, então numa lista de obstáculos não podem existir 6 troncos seguidos ou 4 carros
seguidos. Se houver 6 troncos seguidos ou 4 carros seguidos então a função retorna False. Se isso não acontecer, então a função irá chamar a ela mesma (função recursiva) para verificar se
não há 6 troncos seguidos ou 4 carros seguidos na próxima lista de obstáculos (na próxima linha). Quando a função percorrer todo o mapa e não encontrar 6 troncos seguidos ou 4 carros 
seguidos em nenhuma das listas de obstáculos (em nenhuma das linhas do mapa) e quando apenas sobrar a lista vazia, a função irá retornar True.
 
Assim, a função 'compMaxObstaculos' pode ser definida da seguinte forma:

@
compMaxObstaculos (Mapa n l) = 
    case l of ((_,(Tronco:Tronco:Tronco:Tronco:Tronco:Tronco:_)):_) -> False
              ((_,(Carro:Carro:Carro:Carro:_)):_) -> False
              ((ter,(_:t1)):t) -> compMaxObstaculos (Mapa n ((ter,t1):t))
              ((_,[]):t) -> compMaxObstaculos (Mapa n t)
              [] -> True
@

-}

compMaxObstaculos :: Mapa -> Bool
compMaxObstaculos (Mapa n l) = 
    case l of ((_,(Tronco:Tronco:Tronco:Tronco:Tronco:Tronco:_)):_) -> False
              ((_,(Carro:Carro:Carro:Carro:_)):_) -> False
              ((ter,(_:t1)):t) -> compMaxObstaculos (Mapa n ((ter,t1):t))
              ((_,[]):t) -> compMaxObstaculos (Mapa n t)
              [] -> True

{- |A função 'umNenhumNoMinimo', que recebe um mapa e retorna um bool, verifica se um dado mapa tem em todas as listas de obstáculos não vazias, no mínimo, um obstáculo Nenhum. Esta função
percorre toda a lista de obstáculos (toda a linha) á procura de um obstáculo Nenhum, através da função elem pré-definida no Haskell. Se Nenhum não for elemento da lista, ou seja, se 
elem Nenhum l é False então a função umNenhumNoMinimo retornara False. Se Nenhum for elemento da lista, ou seja, se elem Nenhum l é True então a função irá chamar a ela mesma (função 
recursiva) para verificar se Nenhum é elemento da próxima lista de obstáculos. Quando a função tiver percorrido todas as listas de obstáculos do Mapa (todas as linhas do mapa) e encontrar
no mínimo um obstáculo Nenhum em todas elas e quando apenas sobrar a lista vazia, a função irá retornar True.

Assim, a função 'umNenhumNoMinimo' pode ser definida da seguinte forma:

@
umNenhumNoMinimo (Mapa n l) = 
    case l of [] -> True
              ((_,lo):t) -> if elem Nenhum lo then umNenhumNoMinimo (Mapa n t)
                            else False
@

-}

umNenhumNoMinimo :: Mapa -> Bool
umNenhumNoMinimo (Mapa n l) = 
    case l of [] -> True
              ((_,lo):t) -> if elem Nenhum lo then umNenhumNoMinimo (Mapa n t)
                            else False

{- |A função 'larguraCompObstaculos', que recebe um mapa e retorna um bool, verifica se num dado mapa a largura do mapa é igual ao comprimento de todas as listas de obstáculos. Esta função
calcula o comprimeto da lista de obstáculos (o comprimento da linha) através da função length pré-definida no Haskel e verifica se esse valor é igual á largura do mapa. Se não for igual, a
função retornara False. Se for igual, então a função irá chamar a ela mesma (função recursiva) para calcular o comprimento da próxima lista de obstáculos  e comparar esse valor com a 
largura do mapa. Quando a função tiver percorrido todas as listas de obstáculos do Mapa (todas as linhas do mapa) sem encontrar nenhuma lista de comprimento diferente à largura e quando 
apenas sobrar a lista vazia, a função irá retornar True.

Assim, a função 'larguraCompObstaculos' pode ser definida da seguinte forma:

@
larguraCompObstaculos (Mapa n l) = 
    case l of [] -> True
              ((_,lo):t) -> if n == length lo then larguraCompObstaculos (Mapa n t)
                            else False
@

-}

larguraCompObstaculos :: Mapa -> Bool
larguraCompObstaculos (Mapa n l) = 
    case l of [] -> True
              ((_,lo):t) -> if n == length lo then larguraCompObstaculos (Mapa n t)
                            else False

{- |A função 'maxTerrenoContiguo', que recebe um mapa e retorna um bool, verifica se num dado mapa não existem contíguamente mais do que 4 rios, nem 5 estradas ou relvas. Esta função
verifica que os 5 primeiros terrenos não são todos Rio e que os 6 primeiros terrenos não são todos Estrada ou Relva. Se fossem, então a função retornaria False. Se não fossem, então
a função irá chamar a ela mesma (função recursiva) para verificar que os próximos 5 terrenos não são todos Rio ou que os próximos 6 terrenos não são todos Estrada ou Relva. Quando a função
percorrer toda a lista do mapa sendo que não existem contíguamente mais do que 4 rios, nem 5 estradas ou relvas, e quando apenas sobrar a lista vazia, a função irá retornar True.

Assim, a função 'maxTerrenoContiguo' pode ser definida da seguinte forma:

@
maxTerrenoContiguo (Mapa n l) = 
    case l of ((Rio _,_):(Rio _,_):(Rio _,_):(Rio _,_):(Rio _,_):_) -> False
              ((Estrada _,_):(Estrada _,_):(Estrada _,_):(Estrada _,_):(Estrada _,_):(Estrada _,_):_) -> False
              ((Relva,_):(Relva,_):(Relva,_):(Relva,_):(Relva,_):(Relva,_):_) -> False
              (_:t) -> maxTerrenoContiguo (Mapa n t)
              [] -> True
@

-}

maxTerrenoContiguo :: Mapa -> Bool
maxTerrenoContiguo (Mapa n l) = 
    case l of ((Rio _,_):(Rio _,_):(Rio _,_):(Rio _,_):(Rio _,_):_) -> False
              ((Estrada _,_):(Estrada _,_):(Estrada _,_):(Estrada _,_):(Estrada _,_):(Estrada _,_):_) -> False
              ((Relva,_):(Relva,_):(Relva,_):(Relva,_):(Relva,_):(Relva,_):_) -> False
              (_:t) -> maxTerrenoContiguo (Mapa n t)
              [] -> True
