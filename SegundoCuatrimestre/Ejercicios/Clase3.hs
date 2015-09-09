doblefact :: Integer -> Integer
doblefact n | n == 0 = 1
            | n == 1 = 2
            | otherwise = n * doblefact(n-2)

combinatoria :: Integer -> Integer -> Integer
combinatoria n m    | (n == m) = 1
                    | (0 == m) = 1
                    | otherwise = (combinatoria (n-1) m) + combinatoria (n-1) (m-1)
                    
noTerminaConNegativos :: Integer -> Bool
noTerminaConNegativos n | n == 0 = True
                        | otherwise = noTerminaConNegativos(n-1)
                        
esMultiploDe8 :: Integer -> Bool
esMultiploDe8 n | n == 0 = True
                | n == 1 = False
                | n == 2 = False
                | n == 3 = False
                | n == 4 = False
                | n == 5 = False
                | n == 6 = False
                | n == 7 = False
                | otherwise = esMultiploDe8 (n - 8)

sumaImparesCuyoCuadSeaMenorQue :: Integer -> Integer
sumaImparesCuyoCuadSeaMenorQue n    | n <= 1 = 0
                                    | n == 2 = 1
                                    | n > ((sumaImparesCuyoCuadSeaMenorQue (n-1) + 2)^2) = (sqrt(sumaImparesCuyoCuadSeaMenorQue (n-1)) + 1)^2
                                    | otherwise = sumaImparesCuyoCuadSeaMenorQue (n-1)
                                    

