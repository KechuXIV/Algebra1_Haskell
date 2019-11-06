type Circulo = [Integer]

--Ejercicio 1-------------------------------------------------------------

rotar :: Integer -> Circulo -> Circulo
rotar n [] = []
rotar n [a] = [a]
rotar 0 c = c
rotar n (x:xs) = (rotar (n-1) (xs ++ [x]))

ubicacion :: Integer -> Circulo -> Integer
ubicacion n [a] = 0
ubicacion n (x:xs)  | n == x    = 0
                    | otherwise = 1 + ubicacion n xs

sonCirculosIguales :: Circulo -> Circulo -> Bool
sonCirculosIguales [] [] = True 
sonCirculosIguales _ [] = False 
sonCirculosIguales [] _ = False 
sonCirculosIguales a (x:xs) = rotar (ubicacion x (a)) a == (x:xs)

------------------------------------------------------------------------



--Ejercicio 2-------------------------------------------------------------

factorial :: Integer -> Integer
factorial 0 = 1
factorial 1 = 1
factorial n = n*factorial(n-1)

permutaciones :: Integer -> [[Integer]]
permutaciones 1 = [[1]]
permutaciones n = multiplicar (cosa (n)) (factorial n)
-- permutaciones n = permutarCadaLista (multiplicar (cosa (n)) (factorial n)) n  

multiplicar :: [[Integer]] -> Integer -> [[Integer]]
multiplicar [] _ = []
multiplicar xs 1 = xs
multiplicar (x:xs) n = (x:xs) ++ multiplicar (x:xs) (n-1) 

permutarCadaLista2 :: [[Integer]] -> Integer -> [[Integer]]
permutarCadaLista2 [] _ = []
permutarCadaLista2 (x:[]) 1 = (rotar 1 x) : [] 
permutarCadaLista2 (x:xs) n = (([(head x)] ++ (rotar n (tail x))) : []) ++ (permutarCadaLista2 xs (n-1)) 

permutarCadaLista :: [[Integer]] -> Integer -> [[Integer]]
permutarCadaLista [] _ = []
permutarCadaLista (x:[]) 1 = rotar 1 x  : []
permutarCadaLista (x:xs) n = rotar n x  : permutarCadaLista xs (n-1) 

-- intercala :: Integer -> [[Integer]] -> [[Integer]]
-- intercala n [] = []
-- intercala n (x:[]) = (x++[n]) : []
-- intercala n (x1:x2:[]) = (x1 ++ n x2) : []
-- intercala n (x:xs) = (n:x) : (intercala n xs)

-- intercalar n [] = [n]
-- intercalar n (x:[]) = (x ++ [n]) : ([n] ++ x) : []
-- intercalar n (x:xs) = 

permutar :: [Integer] -> [[Integer]]
permutar [] = []
permutar (x:[]) = [x] : []
permutar (x1:x2:[]) = (x1:x2:[]) : (x2:x1:[]) : []
permutar (x:xs) = insertarEnCadaLista (permutar xs) x

intercalarEnCadaLista :: [[Integer]] -> Integer -> [[Integer]]
intercalarEnCadaLista [] _ = []
intercalarEnCadaLista (x:[]) a = (a : x) : (x ++ [a]) : []

intercalarEntreListas :: [Integer] -> [Integer] -> [Integer]
intercalarEntreListas (x:xs) ys = x : intercalarEntreListas ys xs
intercalarEntreListas [] ys = ys



cosa :: Integer -> [[Integer]]
cosa 1 = [[1]]
cosa n = insertarEnCadaLista (cosa (n-1)) n

insertarEnCadaLista :: [[Integer]] -> Integer -> [[Integer]]
insertarEnCadaLista [] a = []
insertarEnCadaLista (x:xs) a = (x ++ [a]) : insertarEnCadaLista xs a  




------------------------------------------------------------------------


type Set a =[a]

permutacionesGrupo :: Integer -> Set [Integer]
permutacionesGrupo 1 = [[1]]
permutacionesGrupo n = insertarEnCadaLista2 (permutacionesGrupo (n-1)) n

agregar :: Eq a => a -> Set a -> Set a
agregar x xs | elem x xs = xs
             |otherwise = x:xs

insertarEn :: [Integer] -> Integer -> Integer -> [Integer]
insertarEn (xs) a 1 = (a:xs)
insertarEn (x:xs) a n = x:(insertarEn xs a (n-1))

longitud :: [Integer] -> Integer
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

insertarEnTodasLasPosibles :: [Integer] -> Integer -> Set [Integer]
insertarEnTodasLasPosibles [] n = [[n]]
insertarEnTodasLasPosibles xs n = iteracionParaInsertarEn xs n ((longitud xs) + 1)

iteracionParaInsertarEn :: [Integer] -> Integer -> Integer -> Set [Integer]
iteracionParaInsertarEn _ _ 0 = []
iteracionParaInsertarEn xs n a = agregar (insertarEn xs n a) (iteracionParaInsertarEn xs n (a-1))

insertarEnCadaLista2 :: Set [Integer] -> Integer -> Set [Integer]
insertarEnCadaLista2 [] a = []
insertarEnCadaLista2 (x:xs) a = union (insertarEnTodasLasPosibles x a) (insertarEnCadaLista2 xs a)

union :: Eq a => Set a -> Set a -> Set a --ejercicio de clase 9, clase antes del parcial, lo pude hacer con ayuda
union xs ys | xs == [] = ys
            | ys == [] = xs
            | elem (head xs) ys = union (tail xs) ys
            | otherwise = head xs:(union (tail xs) ys)



--Ejercicio 3----------------------------------------------------------------


-- Calcular los factores 
--factores :: Integer -> [Integer]
--factores n = [x | x <- [1..n], n `mod` x == 0]

-- Será primo solo si sus factores son 1 y él mismo
--esPrimo :: Integer -> Bool
--esPrimo n = factores n == [1,n]

noTieneDivisoresHasta :: Integer -> Integer -> Bool
noTieneDivisoresHasta m n   | m < 2                         = True
                            | (mod n m == 0) && (m /= n)    = False
                            | otherwise                     = noTieneDivisoresHasta (m-1) n
                            
esPrimo :: Integer -> Bool
esPrimo n = noTieneDivisoresHasta n n

esCirculoPrimo :: Circulo -> Bool
esCirculoPrimo [x1,x2] = esPrimo (x1+x2)
esCirculoPrimo (x1:x2:xs) = esPrimo (x1+x2) && esCirculoPrimoAux ((x2:xs) ++ [x1]) x1

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


