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
permutaciones 1 = [[1]]
permutaciones n = insertarEnCadaLista (permutaciones (n-1)) n

--Insertar un entero en una posición especifica de la lista
insertarEn :: [Integer] -> Integer -> Integer -> [Integer] 
insertarEn (xs) a 1 = a:xs
insertarEn (x:xs) a n = x : (insertarEn xs a (n-1))

--Longitud de una lista
longitud :: [a] -> Integer
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

--Genera una lista de listas, ingresando un elemento en todas las posiciones posibles
insertarEnTodasLasPosicionesPosibles :: [Integer] -> Integer -> [[Integer]]
insertarEnTodasLasPosicionesPosibles [] n = [[n]]
insertarEnTodasLasPosicionesPosibles xs n = insertarEnTodasLasPosicionesPosiblesAux xs n ((longitud xs) + 1)

--Devuelve una lista de listas con el nuevo elemento en todas las posiciones posibles
insertarEnTodasLasPosicionesPosiblesAux :: [Integer] -> Integer -> Integer -> [[Integer]]
insertarEnTodasLasPosicionesPosiblesAux _ _ 0 = []
insertarEnTodasLasPosicionesPosiblesAux xs n a = (insertarEn xs n a) : (insertarEnTodasLasPosicionesPosiblesAux xs n (a-1))

--Insertar un elemento en cada lista de la lista
insertarEnCadaLista :: [[Integer]] -> Integer -> [[Integer]]
insertarEnCadaLista [] a = []
insertarEnCadaLista (x:xs) a = union (insertarEnTodasLasPosicionesPosibles x a) (insertarEnCadaLista xs a)

--Une 2 listas, descartando elementos iguales
union :: Eq a => [a] -> [a] -> [a]
union [] ys = ys
union xs [] = xs
union (x:xs) ys | contiene ys x = union xs ys
                | otherwise     = x : (union xs ys)

--Devuelve si una lista contiene a cierto elemento
contiene :: Eq a => [a] -> a -> Bool
contiene [] _ = False
contiene (x:xs) a = a == x || contiene xs a

--Ejercicio 3----------------------------------------------------------------

esCirculoPrimo :: Circulo -> Bool
esCirculoPrimo [x1,x2] = esPrimo (x1+x2)
esCirculoPrimo (x1:x2:xs) = esPrimo (x1+x2) && esCirculoPrimoAux ((x2:xs) ++ [x1]) x1

-- Informa si tiene divisores menores que cierto n
noTieneDivisoresHasta :: Integer -> Integer -> Bool
noTieneDivisoresHasta m n   | m < 2                         = True
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
estaRepetidoPrimero [x1,x2] = x1 == x2
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

--Ejericio optativo------------------------------------------------------------------------

listaCirculosPrimosEspejados :: Integer -> [Circulo]
listaCirculosPrimosEspejados n = eliminarNoEspejados(listaCirculosPrimos n) 

-- Espeja una lista
espejar :: [a] -> [a]
espejar [] = []
espejar [a] = [a]
espejar (x:xs) = espejar (xs) ++ [x]

-- Elimina los circulos no espejados
eliminarNoEspejados :: [Circulo] -> [Circulo]
eliminarNoEspejados []     = []
eliminarNoEspejados (x:xs) | estaRepetidoPrimero ((espejar x):xs) = [x] ++ (obtenerCirculosIgualesA (espejar x) (xs)) ++ eliminarNoEspejados xs
                           | otherwise                            = eliminarNoEspejados xs

-- Obtienes los circulos en una lista iguales a cierto circulo
obtenerCirculosIgualesA :: Circulo -> [Circulo] -> [Circulo]
obtenerCirculosIgualesA c []     = []
obtenerCirculosIgualesA c (x:xs) | sonCirculosIguales c x = x : (obtenerCirculosIgualesA c xs)
                                 | otherwise              = obtenerCirculosIgualesA c xs 
