intercalar :: [a] -> [a] -> [a]
intercalar l1 l2    | (length l1) == 1 = [head l1] ++ [head l2]
                    | otherwise = [head l1] ++ [head l2] ++ intercalar (tail l1) (tail l2)

--cuentasFibonacci :: Integer -> (Integer, Integer)
--cuentasFibonacci n  | n == 0 = (1,1)
--                    | n == 1 = (1,1)
--                    | otherwise ( (fst j1) + (fst j2), (snd j1) + 1) 
--                        where j1 = CuentasFibonacci (n-1),
--                                j2 = CuentasFibonacci (n-2)
                                
duplicarTodosLos :: Integer -> [Integer] -> [Integer]
duplicarTodosLos x xs   | xs == [] = []
                        | x == (head xs) = x : [x] ++ duplicarTodosLos x (tail xs)
                        | otherwise = [head xs] ++ duplicarTodosLos x (tail xs)
                        
pertenece :: Char -> [Char] -> Bool
pertenece x xs  | (length xs) == 0  = False
                | (head xs) == x = True
                | otherwise = True || pertenece x (tail xs)
                
nvecesx :: Integer -> a -> [a]
nvecesx n x | n == 0 = []
            | otherwise = [x] ++ nvecesx (n-1) x
            
cant :: Char -> [Char] -> Integer
cant x xs   | xs == [] = 0
            | (head xs) == x = 1 + cant x (tail xs)
            | otherwise = 0 + cant x (tail xs)
            
quitar :: Char -> [Char] -> [Char]
quitar x xs | xs == [] = []
            | (head xs) == x = quitar x (tail xs)
            | otherwise = [head xs] ++ quitar x (tail xs)
            
empaquetar :: [Char] -> [[Char]]
empaquetar xs   | xs == [] = []
                | pertenece (head xs) (tail xs) = [nvecesx (cant (head xs) xs) (head xs)] ++ empaquetar (quitar(head xs) (tail xs))
                | otherwise = [[head xs]] ++ empaquetar(tail xs)
            
