menorLex :: (Float, Float, Float) -> (Float, Float, Float) -> Bool
menorLex (x1, x2, x3) (y1, y2, y3) = x1 < y1 || x2 < y2 || x3 < y3

fibonacci :: Integer -> Integer
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci a = fibonacci(a-1) + fibonacci(a-2)

sumaFibonacci :: Integer -> Integer 
sumaFibonacci 0 = 1
sumaFibonacci 1 = 2
sumaFibonacci n = fibonacci(n-1) + fibonacci(n-2) + sumaFibonacci(n-1)

obtenerDivisores :: Integer -> Integer -> [Integer]
obtenerDivisores d 1 = [1]
obtenerDivisores d n | mod d n == 0 = n : (obtenerDivisores d (n -1)) 
obtenerDivisores d n | otherwise =  (obtenerDivisores d (n -1)) 

sumar :: [Integer] -> Integer
sumar [] = 0
sumar (x:xs) = x + sumar xs

esDefectivo :: Integer -> Bool
esDefectivo n = (sumar(obtenerDivisores n (n-1))) < n

valorAbsoluto :: Integer -> Integer
valorAbsoluto n | n < 0 = -n
                | otherwise = n


obtenerElMayor :: [Integer] -> Integer
obtenerElMayor [x1] = x1
obtenerElMayor (x1:x2:xs) | x1 > x2 = obtenerElMayor (x1:xs)
                            | otherwise = obtenerElMayor (x2:xs)

obtenerElMenor :: [Integer] -> Integer
obtenerElMenor [x1] = x1
obtenerElMenor (x1:x2:xs) | x1 < x2 = obtenerElMenor (x1:xs)
                            | otherwise = obtenerElMenor (x2:xs)

maximaDistancia :: [Integer] -> Integer
maximaDistancia (x1:x2:[]) = valorAbsoluto (x1 - x2)
maximaDistancia (x1:x2:xs) | valorAbsoluto (x1 - x2) > maximaDistancia(x2:xs) = valorAbsoluto (x1 - x2)
                            | otherwise =  maximaDistancia(x2: xs)

comprimir1 :: [Integer] -> [(Integer, Integer)]
comprimir1 (x1:[]) = [(x1,1)]  
comprimir1 (x1:xs) =  (x1,1) : comprimir1(xs)

comprimir :: [Integer] -> [(Integer, Integer)]
comprimir (x1:x2:[]) | x1 == x2 = [(x1, 2)] 
                    | otherwise = [(x1,1), (x2,1)]
comprimir xs = asd (comprimir1 xs) 

asd :: [(Integer, Integer)] -> [(Integer, Integer)]
asd (x1 : x2 : []) | fst x1 == fst x2 = [(fst x1, snd x2 + snd x1)]
                         | otherwise = (x1:x2:[])
asd (x1 : x2 : xs) | fst x1 == fst x2 = asd ((fst x1, (snd x2 + snd x1)) : xs)
                         | otherwise = x1 : asd ( x2 : xs)  