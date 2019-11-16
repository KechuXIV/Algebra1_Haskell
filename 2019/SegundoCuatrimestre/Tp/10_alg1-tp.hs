type Circulo = [Integer]

--Ejercicio 1-------------------------------------------------------------

sonCirculosIguales :: Circulo -> Circulo -> Bool
sonCirculosIguales [] [] = True 
sonCirculosIguales _ [] = False 
sonCirculosIguales [] _ = False 
sonCirculosIguales a (x:xs) = rotar (ubicacion x (a)) a == (x:xs)

-- Rota un circulo en n posiciones
rotar :: Integer -> Circulo -> Circulo
rotar n [] = []
rotar n [a] = [a]
rotar 0 c = c
rotar n (x:xs) = (rotar (n-1) (xs ++ [x]))

-- Informa la ubicación de cierto entero en un circulo
ubicacion :: Integer -> Circulo -> Integer
ubicacion n [a] = 0
ubicacion n (x:xs)  | n == x    = 0
                    | otherwise = 1 + ubicacion n xs

--Ejercicio 2-------------------------------------------------------------

permutaciones :: Integer -> [[Integer]]
permutaciones n = permutarLista (obtenerListaDe n)  (obtenerListaDe n)

-- Permuta una lista
permutarLista :: [Integer] -> [Integer] -> [[Integer]]
permutarLista [] _ = []
permutarLista (x:[]) (y:[]) = [[x]]
permutarLista (x:xs) ys = agregarEnCadaListaPrimero x (permutarLista (sacarDeLaLista x ys) (sacarDeLaLista x ys)) ++ permutarLista xs ys

-- Obtiene una lista de enteros de 1 a n
obtenerListaDe :: Integer -> [Integer]
obtenerListaDe 1 = [1]
obtenerListaDe n = obtenerListaDe (n-1) ++ [n]  

-- Saca de la lista un entero
sacarDeLaLista :: Integer -> [Integer] -> [Integer]
sacarDeLaLista _ [] = []
sacarDeLaLista n (x:xs) | x == n = sacarDeLaLista n xs
                        | otherwise = x : sacarDeLaLista n xs

-- Agrega un entero en cada lista en la primera posiciòn
agregarEnCadaListaPrimero :: Integer -> [[Integer]] -> [[Integer]]
agregarEnCadaListaPrimero _ [] = []
agregarEnCadaListaPrimero n (x:xs) = (n : x) : (agregarEnCadaListaPrimero n (xs))

--Ejercicio 3----------------------------------------------------------------

esCirculoPrimo :: Circulo -> Bool
esCirculoPrimo [x1,x2] = esPrimo (x1+x2)
esCirculoPrimo (x1:x2:xs) = esPrimo (x1+x2) && esCirculoPrimoAux ((x2:xs) ++ [x1]) x1

-- Informa si tiene divisores menores que cierto n
noTieneDivisoresHasta :: Integer -> Integer -> Bool
noTieneDivisoresHasta m n  | m < 2                         = True
                           | (mod n m == 0) && (m /= n)    = False
                           | otherwise                     = noTieneDivisoresHasta (m-1) n

-- Informa si un entero es primo
esPrimo :: Integer -> Bool
esPrimo n = noTieneDivisoresHasta n n

-- Función auxiliar que se encarga de la recursión
esCirculoPrimoAux :: Circulo -> Integer -> Bool
esCirculoPrimoAux [x1,x2] _ = esPrimo (x1+x2)
esCirculoPrimoAux (x1:x2:xs) n | x2 == n = esPrimo (x1+n)
                               | otherwise = esPrimo (x1+x2) && (esCirculoPrimoAux (x2:xs) n)

--Ejericio 4-------------------------------------------------------------------------------

estaRepetidoPrimero :: [Circulo] -> Bool
estaRepetidoPrimero [] = False
estaRepetidoPrimero [x1] = False
estaRepetidoPrimero [x1,x2] = sonCirculosIguales x1 x2
estaRepetidoPrimero (x1:x2:xs) = sonCirculosIguales x1 x2 || estaRepetidoPrimero (x1:xs)

--Ejericio 5-------------------------------------------------------------------------------

listaCirculosPrimos :: Integer -> [Circulo]
listaCirculosPrimos 2 = [[1,2]]
listaCirculosPrimos n = eleminarRepetidos(eliminarLosNoPrimos (permutaciones n))

-- Elimina los circulos que no son primos 
eliminarLosNoPrimos :: [Circulo] -> [Circulo]
eliminarLosNoPrimos [] = []
eliminarLosNoPrimos (x:xs) | esCirculoPrimo x = x : eliminarLosNoPrimos xs
                           | otherwise        = eliminarLosNoPrimos xs

-- Elimina los circulos repetidos
eleminarRepetidos :: [Circulo] -> [Circulo]
eleminarRepetidos []     = []
eleminarRepetidos (x:xs) | estaRepetidoPrimero (x:xs) = eleminarRepetidos xs
                         | otherwise                  = x : (eleminarRepetidos xs)

--Ejericio 6-------------------------------------------------------------------------------

contarCirculosPrimos :: Integer -> Integer
contarCirculosPrimos n = longitud (listaCirculosPrimos n)

--Obtiene la longitud de una lista
longitud :: [a] -> Integer
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

--Ejericio optativo------------------------------------------------------------------------

listaCirculosPrimosEspejados :: Integer -> [Circulo]
listaCirculosPrimosEspejados n = eliminarEspejados(listaCirculosPrimos n) 

-- Espeja una lista
espejar :: [a] -> [a]
espejar [] = []
espejar [a] = [a]
espejar (x:xs) = espejar (xs) ++ [x]

-- Elimina los circulos no espejados
eliminarEspejados :: [Circulo] -> [Circulo]
eliminarEspejados []     = []
eliminarEspejados (x:xs) | estaRepetidoPrimero ((espejar x):xs) = eliminarEspejados xs
                         | otherwise                            = x : eliminarEspejados xs

-- Obtienes los circulos en una lista iguales a cierto circulo
obtenerCirculosIgualesA :: Circulo -> [Circulo] -> [Circulo]
obtenerCirculosIgualesA c []     = []
obtenerCirculosIgualesA c (x:xs) | sonCirculosIguales c x = x : (obtenerCirculosIgualesA c xs)
                                 | otherwise              = obtenerCirculosIgualesA c xs 
