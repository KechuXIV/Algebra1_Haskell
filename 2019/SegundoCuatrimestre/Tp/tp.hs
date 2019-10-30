type Circulo = [Integer]

permutar :: Integer -> Circulo -> Circulo
permutar n [] = []
permutar n [a] = [a]
permutar 0 c = c
permutar n (x:xs) = (permutar (n-1) (xs ++ [x]))

ubicacion :: Integer -> Circulo -> Integer
ubicacion n [a] = 0
ubicacion n (x:xs)  | n == x 	= 0
					| otherwise = 1 + ubicacion n xs




--sonCirculosIguales :: Circulo -> Circulo -> Bool
--sonCirculosIguales x y = 