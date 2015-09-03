fib :: Integer -> Integer
fib n   | n == 0 = 1
        | n == 1 = 1
        | otherwise = fib(n-1) + fib(n-2)
        
par :: Integer -> Bool
par n   | n == 1 = False
        | n == 2 = True
        | n > 2 =  par (n - 2)
        
parEnClase :: Integer -> Bool
parEnClase n    | n == 0 = True
                | otherwise = not (parEnClase (n-1))
        
sumaImpares :: Integer -> Integer
sumaImpares n   | n == 1 = 1
                | otherwise = sumaImpares (n-1) + ((2*n) - 1)


esMultiploDe3 :: Integer -> Bool
esMultiploDe3 n | n == 0 = True
                | n == 1 = False
                | n == 2 = False
                | otherwise = esMultiploDe3 (n - 3)