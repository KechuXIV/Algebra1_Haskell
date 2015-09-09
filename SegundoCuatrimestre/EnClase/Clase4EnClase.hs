sumaImparesCuyoCuadSeaMenorQue :: Integer -> Integer
sumaImparesCuyoCuadSeaMenorQue umbral = sumaAuxiliar umbral 1

sumaAuxiliar:: Integer -> Integer -> Integer
sumaAuxiliar umbral x
    | x ^ 2 > umbral = 0
    | otherwise = x + sumaAuxiliar umbral (x + 2)

division :: Integer -> Integer -> (Integer, Integer)
division a d = (func, a - (func)*d)
    where func = cantidadDeVecesQueAXLePuedoRestarY a d

cantidadDeVecesQueAXLePuedoRestarY :: Integer -> Integer -> Integer
cantidadDeVecesQueAXLePuedoRestarY x y
    | y > x = 0
    | otherwise = 1 + (cantidadDeVecesQueAXLePuedoRestarY (x-y) y)