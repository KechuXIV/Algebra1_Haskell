suma :: [Integer] -> [Integer] -> [Integer]
suma [] []                  = []
suma x y    | length x == 1 = [(head x + head y)]
            | otherwise     = [(head x + head y)] ++ suma (tail x) (tail y)

prodInterno :: [Float] -> [Float] -> Float
prodInterno [] [] = 0
prodInterno x y | length x == 1 = head x * head y
                | otherwise     = head x * head y * prodInterno (tail x) (tail y)
             
division :: Integer -> Integer -> (Integer, Integer)
division a d    | a < 0 && d < 0    = division (a*(-1)) (d*(-1))
                | a < 0             = (-(fst qr3), snd qr3)
                | d < 0             = (-(fst qr2), snd qr2)
                | a < d             = (0, a)
                | otherwise         = (fst qr' + 1, snd qr')
                                        where qr' = division (a-d) d
                                              qr2 = division a (d*(-1))
                                              qr3 = division (a*(-1)) d
                                              
noTieneDivisoresHasta :: Integer -> Integer -> Bool
noTieneDivisoresHasta m n   | m < 2                         = True
                            | (mod n m == 0) && (m /= n)    = False
                            | otherwise                     = noTieneDivisoresHasta (m-1) n
                            
esPrimo :: Integer -> Bool
esPrimo n = noTieneDivisoresHasta n n


--quot x y -> Parte entera de (x/y) cuando y<0 o x<0
--rem x y -> resto de (x/y) cuando y<0 o x<0