
permutaciones :: Integer -> [[Integer]]
permutaciones 1 = [[1]]
permutaciones n = insertarEnCadaLista (permutaciones (n-1)) n

--Insertar un entero en una posición especifica de la lista
insertarEn :: [Integer] -> Integer -> Integer -> [Integer] 
insertarEn (xs) a 1 = a:xs
insertarEn (x:xs) a n = x : (insertarEn xs a (n-1))

--Longitud de una lista
longitud :: [Integer] -> Integer
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