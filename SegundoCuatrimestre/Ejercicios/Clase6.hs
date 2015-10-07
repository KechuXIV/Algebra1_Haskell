type Racional = ( Integer , Integer )
type Conjunto = [Integer]

tuplas :: [a] -> [b] -> [(a,b)]
tuplas xs []			= []
tuplas [] ys			= []
tuplas (x:[]) (y:[])	= (x, y) : []
tuplas (x:xs) (y:ys)	= (x, y) : tuplas xs ys

potencia :: Racional -> Integer -> Racional
potencia (a, b) p = (a^p, b^p)

union :: Conjunto -> Conjunto -> Conjunto
union xs []			= xs
union [] ys 		= ys
union (x:[]) (y:[])	= x : y : []
union (x:xs) (y:ys) = x : y : union xs ys

interseccion :: Conjunto -> Conjunto -> Conjunto
interseccion xs []							= []
interseccion [] ys							= []
interseccion (x:xs) ys	| pertenece x ys 	= x : interseccion xs ys
interseccion (x:xs) ys	| otherwise			= interseccion xs ys

pertenece :: Integer -> [Integer] -> Bool
pertenece x []							= False
pertenece x lista	| head lista == x	= True
					| otherwise			= pertenece x (tail lista)

inclusion :: Conjunto -> Conjunto -> Bool
inclusion xs []							= False
inclusion [] ys							= True
inclusion (x:xs) ys | pertenece x ys	= True && inclusion xs ys
inclusion (x:xs) ys | otherwise			= False

igual :: Conjunto -> Conjunto -> Bool
igual xs ys = length xs == length ys && inclusion xs ys

separar ::  Integer -> Conjunto -> (Conjunto, Conjunto)
separar x [] = ([],[])
separar x (y:ys) | y <= x = (y:fst(separar x ys), snd(separar x ys))
separar x (y:ys) | y > x = (fst(separar x ys), y:snd(separar x ys))