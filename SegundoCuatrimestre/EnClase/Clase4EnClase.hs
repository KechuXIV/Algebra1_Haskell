parEnClase :: Integer -> Bool
parEnClase n    | n == 0 = True
                | otherwise = not (parEnClase (n-1))

sumaImparesCuyoCuadSeaMenorQue :: Integer -> Integer
sumaImparesCuyoCuadSeaMenorQue umbral = sumaAuxiliar umbral 1

sumaAuxiliar:: Integer -> Integer -> Integer
sumaAuxiliar umbral x
    | x ^ 2 > umbral = 0
    | otherwise = x + sumaAuxiliar umbral (x + 2)
    