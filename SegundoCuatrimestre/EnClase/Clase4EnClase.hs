sumaImparesCuyoCuadSeaMenorQue :: Integer -> Integer
sumaImparesCuyoCuadSeaMenorQue umbral = sumaAuxiliar umbral 1

sumaAuxiliar:: Integer -> Integer -> Integer
sumaAuxiliar umbral x
    | x ^ 2 > umbral = 0
    | otherwise = x + sumaAuxiliar umbral (x + 2)

divisionMia :: Integer -> Integer -> (Integer, Integer)
divisionMia a d = (func, a - (func)*d)
    where func = cantidadDeVecesQueAXLePuedoRestarY a d
    
division :: Integer -> Integer -> (Integer, Integer)
division a d    | a < d     = (0, a)
                | otherwise = (fst qr' + 1, snd qr')
                                where qr' = division (a-d) d

cantidadDeVecesQueAXLePuedoRestarY :: Integer -> Integer -> Integer
cantidadDeVecesQueAXLePuedoRestarY x y
    | y > x = 0
    | otherwise = 1 + (cantidadDeVecesQueAXLePuedoRestarY (x-y) y)
    
--(:) -> Agrega un elemento a una lista (Ej: 2 : [3,4] = [2,3,4])
divParcial :: Integer -> Integer -> [Integer]
divParcial n 1 = [1]
divParcial n m  | mod n m == 0  = m : divParcial n (m-1)
                | otherwise     = divParcial n (m -1)

divisores :: Integer -> [Integer]
divisores n = divParcial n n

esPrimo :: Integer -> Bool
esPrimo n = length (divisores n) == 2

suma :: [Integer] -> Integer
suma lista  | length lista == 0 = 0
            | otherwise = head lista + suma (tail lista)

producto :: [Integer] -> Integer
producto lista  | length lista == 0 = 1
                | otherwise = head lista * producto (tail lista)

reverso :: [a] -> [a]
reverso []      = []
reverso lista   = (reverso (tail lista)) ++ [head lista]
                
capicua :: [Integer] -> Bool
capicua lista   | length lista == 0 = True
                | length lista == 1 = True
                | otherwise = lista == reverso(lista)
                