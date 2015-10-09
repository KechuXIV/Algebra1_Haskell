intercalar :: [a] -> [a] -> [a]
intercalar [] ys            = ys
intercalar xs []            = xs
intercalar (x:xs) (y:ys)    = x : y : intercalar xs ys

cuentasFibonacci :: Integer -> (Integer, Integer)
cuentasFibonacci 0 = (1,0)
cuentasFibonacci 1 = (1,0)
cuentasFibonacci n = ( x1 + x2, y1 + 2)
                        where (x1,y1) = cuentasFibonacci (n-1)
                              (x2,y2) = cuentasFibonacci (n-2)
                        
duplicarTodosLos :: Integer -> [Integer] -> [Integer]
duplicarTodosLos _ []                   = []
duplicarTodosLos n (x:xs)   | n == x    = x : x : duplicarTodosLos n xs
                            | otherwise = x : duplicarTodosLos n xs
                
empaquetar :: [Char] -> [[Char]]
empaquetar []                                           = []
empaquetar (x:[])                                       = [[x]]
empaquetar (x:xs)   | x == head(head(empaquetar xs))    = (x : head(empaquetar xs)) : tail(empaquetar xs)
                    | otherwise                         = [x] : empaquetar xs


