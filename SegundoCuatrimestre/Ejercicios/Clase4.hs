suma :: [Integer] -> [Integer] -> [Integer]
suma [] []                  = []
suma x y    | length x == 1 = [(head x + head y)]
            | otherwise     = [(head x + head y)] ++ suma (tail x) (tail y)

prodInterno :: [Float] -> [Float] -> Float
prodInterno [] [] = 0
prodInterno x y | length x == 1 = head x * head y
                | otherwise     = head x * head y * prodInterno (tail x) (tail y)
             
division :: Integer -> Integer -> (Integer, Integer)
division a d    | a < d     = (0, a)
                | otherwise = (fst qr' + 1, snd qr')
                                where qr' = division (a-d) d
                                

--quot x y -> Parte entera de (x/y) cuando y<0 o x<0
--rem x y -> resto de (x/y) cuando y<0 o x<0