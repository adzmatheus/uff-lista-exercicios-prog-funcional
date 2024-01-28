-- Exercicio 1

del_posicao_n  [] n = []
del_posicao_n (cabeca:corpo) n
 |n>0 = cabeca:(del_posicao_n corpo (n-1))
 |otherwise = corpo

-- -----------------------------------------------------------------------------------------

-- Exercicio 2

quicksort [] = []
quicksort (cabeca:corpo) = quicksort menores ++ [cabeca] ++ quicksort maiores 
 where menores = [ x | x <- corpo, x < cabeca]  
       maiores = [ y | y <- corpo, y >= cabeca]

impares [] = []
impares (cab:corpo)
 |odd cab = quicksort (cab:(impares corpo))
 |otherwise = quicksort (impares corpo)

-- -----------------------------------------------------------------------------------------

-- Exercicio 3

fibonacci' 0 = 0; fibonacci' 1 = 1; fibonacci' n = fibonacci' (n-1) + fibonacci' (n-2)
fibonacci n = map fibonacci' [0..n-1]

main :: IO ()
main = do
input <- getLine
let number = read input 
print (fibonacci number)

-- -----------------------------------------------------------------------------------------

-- Exercicio 4

-- Opcional

-- ----------------------------------------------------------------------------------------

-- Exercicio 5

-- Opcional

-- ----------------------------------------------------------------------------------------

-- Exercicio 6

compara _ [] = []
compara x (a:b)
  | x == a = [x]
  | otherwise = compara x b

intersecao _ [] = []
intersecao [] _ = []
intersecao (cab:corpo) (cab2:corpo2)
  | compara cab (cab2:corpo2) == [cab] = cab : intersecao corpo (cab2:corpo2)
  | otherwise = intersecao corpo (cab2:corpo2)

-- ------------------------------------------------------------------------------------------

-- Exercicio 7

tokString n [] = []
tokString 0 lista = []
tokString n (cabeca:corpo) = cabeca:(tokString (n-1) corpo)

tamString [] = 0
tamString (cabeca:corpo) = 1 + tamString (corpo)

busca_sub [] palavras = []
busca_sub prefixo [] = []
busca_sub prefixo (cabeca:corpo)
 |(tokString (tamString prefixo) cabeca) == prefixo = cabeca:busca_sub prefixo corpo
 |otherwise = busca_sub prefixo corpo

-- ----------------------------------------------------------------------------------------

-- Exercicio 8

repetidor 0 _ = []
repetidor _ 0 = []
repetidor n x = n:(repetidor n (x-1))

repetidor' [] = []
repetidor' (cabeca:corpo) = (repetidor cabeca cabeca) ++ (repetidor' corpo)

repete k = repetidor' [k,(k-1)..1]

-- ----------------------------------------------------------------------------------------

-- Exercicio 9

inverte [] = []
inverte (cabeca:corpo) = (inverte corpo) ++ [cabeca]

palindromo lista = lista == inverte lista

-- ----------------------------------------------------------------------------------------

-- Exercicio 10

rodar_esquerda n [] = []
rodar_esquerda 0 lista = lista
rodar_esquerda n (cabeca:corpo) = rodar_esquerda (n-1) (corpo ++ [cabeca])

take_last [] = []
take_last (cabeca:corpo)
 |corpo /= [] = [cabeca] ++ take_last corpo
 |otherwise = []

rodar_direita n [] = []
rodar_direita 0 lista = lista
rodar_direita n lista = rodar_direita (n-1) ((last lista):take_last lista)

-- ----------------------------------------------------------------------------------------

-- Exercicio 11


inverte [] = []
inverte (cabeca:corpo) = (inverte corpo) ++ [cabeca]

-- ----------------------------------------------------------------------------------------

-- Exercicio 12

junta [] = []
junta (cabeca:corpo) = cabeca ++ [" "] ++junta corpo

tamString [] = 0
tamString (cabeca:corpo) = 1 + tamString corpo

ocorre [] palavra = 0
ocorre linha [] = 0
ocorre linha palavra
 |take (tamString palavra) linha == palavra = 1 + ocorre (drop 1 linha) palavra
 |otherwise = ocorre (drop 1 linha) palavra

ocorrencias l p = ocorre (junta l) p


-- ----------------------------------------------------------------------------------------

-- Exercicio 13

limites l = limites_aux l (maxBound :: Int) 0
limites_aux [] min max = (min, max)
limites_aux (cabeca:corpo) min max
 |cabeca < min = limites_aux corpo cabeca max
 |cabeca > max = limites_aux corpo min cabeca

-- ----------------------------------------------------------------------------------------

-- Exercicio 14

remDuplicatas [] = []
remDuplicatas (a:b) = a: remDuplicatas (filter (/= a) b)

-- ----------------------------------------------------------------------------------------

-- Exercicio 15

criaLista 0 = []
criaLista n = [1,2..n]

somaLista [] = 0
somaLista (cabeca:corpo) = cabeca + (somaLista corpo)

eleva k = k*k

quadrados n = map eleva (criaLista n)

somaQuadrados n = somaLista (quadrados n)

-- ------------------------------------------------------------------------------------------

-- Exercicio 16

adicionaFila fila elemento = fila ++ [elemento]

retiraFila [] = []
retiraFila (cabeca:corpo) = corpo

tamFila [] = 0
tamFila (cabeca:corpo) = 1 + (tamFila corpo)

-- ------------------------------------------------------------------------------------------

-- Exercicio 17

adicionaPilha pilha elemento = elemento:pilha

retiraPilha [] = []
retiraPilha (cabeca:corpo) = corpo

tamPilha [] = 0
tamPilha (cabeca:corpo) = 1 + (tamPilha corpo)

-- ------------------------------------------------------------------------------------------

-- Exercicio 18

-- Opcional

-- ----------------------------------------------------------------------------------------

-- Exercicio 19

-- ----------------------------------------------------------------------------------------

-- Exercicio 20

compMat [] [] = True
compMat (cabeca1:corpo1) (cabeca2:corpo2)
 |cabeca1 == cabeca2 = compMat corpo1 corpo2
 |otherwise = False 

-- ----------------------------------------------------------------------------------------

-- Exercicio 21

somatorio min max f = foldl (+) 0 (map f [min,(min+1)..max])

-- ----------------------------------------------------------------------------------------

-- Exercicio 22

merge [] lista2 = lista2
merge lista1 [] = lista1
merge (cabeca:corpo) (cabeca2:corpo2)
 |cabeca <= cabeca2 = cabeca:(merge corpo (cabeca2:corpo2))
 |cabeca2 <= cabeca = cabeca2:(merge corpo2 (cabeca:corpo))

-- ----------------------------------------------------------------------------------------

-- Exercicio 23

-- Primeira solucao
dic 0 = "zero"
dic 1 = "um"
dic 2 = "dois"
dic 3 = "tres"
dic 4 = "quatro"
dic 5 = "cinco"
dic 6 = "seis"
dic 7 = "sete"
dic 8 = "oito"
dic 9 = "nove"

dic_10 lista = map dic lista

-- Segunda solucao
getAlg (alg,ext) = alg

getExt (alg,ext) = ext

dic_10 = [(0, "zero"),(1, "um"),(2, "dois"),(3, "tres"),(4, "quatro"),
          (5, "cinco"),(6, "seis"),(7, "sete"),(8, "oito"), (9, "nove")]

valToStr x (cabeca:corpo)
 |x == (getAlg cabeca) = (getExt cabeca)
 |otherwise = valToStr x (corpo)

conv_int_str [] = []
conv_int_str (cabeca:corpo) = (valToStr cabeca dic_10):conv_int_str corpo


-- ----------------------------------------------------------------------------------------

-- Exercicio 24

-- Opcional

-- ----------------------------------------------------------------------------------------

-- Exercicio 25


-- ----------------------------------------------------------------------------------------

-- Exercicio 26

applylist [] x = []
applylist (cabeca:corpo) x = (cabeca x): applylist corpo x

-- Segunda solucao

applylist [] _ = []
applylist x y = map ($ y) x

-- ----------------------------------------------------------------------------------------

-- Exercicio 27

-- ----------------------------------------------------------------------------------------

-- Exercicio 28

-- A funcao zip recebe duas listas e retorna uma lista de tuplas onde o primeiro do par
-- vem da primeira lista e o segundo da segunda lista

myzip [] [] = []
myzip [] (cabeca:corpo) = []
myzip (cabeca:corpo) [] = []
myzip (cabeca1:corpo1) (cabeca2:corpo2) = (cabeca1,cabeca2):myzip corpo1 corpo2

-- ----------------------------------------------------------------------------------------

-- Exercicio 29

lenght' lista = sum (map (\x -> 1) lista)
