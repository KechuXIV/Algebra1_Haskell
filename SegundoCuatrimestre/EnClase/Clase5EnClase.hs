pertenece :: Integer -> [Integer] -> Bool
pertenece x []                          = False
pertenece x lista	| head lista == x	= True
			        | otherwise         = pertenece x (tail lista)
			        
hayRepetidos :: [Integer] -> Bool
hayRepetidos []                                                     = False
hayRepetidos lista  | pertenece (head lista) (tail lista) == True   = True
                    | otherwise                                     = hayRepetidos (tail lista)

menores :: Integer -> [Integer] -> [Integer]
menores x lista | lista == []           = []
                | x > head lista        = [head lista] ++ menores x (tail lista)
                | x < head lista        = menores x (tail lista)

quitar :: Integer -> [Integer] -> [Integer]
quitar x lista  | lista == []           = []
                | x == head lista       = tail lista
                | otherwise             = [head lista] ++ quitar x (tail lista)
                
maximo :: [Integer] -> Integer
maximo lista    | elSegundoEsMayorAlPrimero lista   = head lista
                | otherwise                         = maximo (tail lista)
                
elSegundoEsMayorAlPrimero :: [Integer] -> Bool
elSegundoEsMayorAlPrimero lista = (head lista) > (head (tail lista))
                
enBase :: Integer -> Integer -> [Integer]
enBase a base | base > a    = [a]
              | otherwise   = enBase (div a base) base ++ [mod a base]
              
deBase :: Integer ->  [Integer] -> Integer
deBase base a   | length a == 1 = head a
                | otherwise     = (head a* base^(length a -1)) + (deBase base (tail a))

