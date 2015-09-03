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
sumaImparesCuyoCuadSeaMenorQue n    | n <= 0 = 0
                                    | n == 1 = 1
                                    | esMultiploDe8 (n - 2) = sumaImparesCuyoCuadSeaMenorQue(n - 1) + 
                                                    
n = 1 -> 1
n = 2 -> 1  +  n - (2*n-1)^2
n = 3 -> 1  +   
n = 4 -> 1  +   
n = 5 -> 1  +   
n = 6 -> 1  +   
n = 7 -> 1  +   
n = 8 -> 1  +   
n = 9 -> 1  +   
n = 10 -> 1 +   3
n = 11 -> 1 +   3
n = 12 -> 1 +   3
    .
    .
    .
n = 26 -> 1 +   3   +   5           =   4 + 5   =   9
n = 50 -> 1 +   3   +   5   +   7   =   9 + 7   =   16

50 - ((2*n) - 1)^2
8 -> 16 -> 24 -> 32 -> 40