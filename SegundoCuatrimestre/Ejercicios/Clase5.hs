reversoInt :: [Integer] -> [Integer]
reversoInt []       = []
reversoInt lista    = (reversoInt (tail lista)) ++ [head lista]
                
esCapicua :: [Integer] -> Bool
esCapicua lista | length lista == 0 = True
                | length lista == 1 = True
                | otherwise         = lista == reversoInt(lista)
                
enBase :: Integer -> Integer -> [Integer]
enBase a base | base > a    = [a]
              | otherwise   = enBase (div a base) base ++ [mod a base]
                
numeroALista :: Integer -> [Integer]
numeroALista n = enBase n 10

listaANumero :: [Integer] -> Integer
listaANumero n = deBase 10 n

deBase :: Integer ->  [Integer] -> Integer
deBase base a   | length a == 1 = head a
                | otherwise     = (head a* base^(length a -1)) + (deBase base (tail a))

masElReverso :: [Integer] -> Integer
masElReverso lista = (listaANumero lista) + (listaANumero (reversoInt(lista)))

capicuaPara :: [Integer] -> [Integer]
capicuaPara lista   | esCapicua lista   = lista
                    | otherwise         = capicuaPara(numeroALista(masElReverso lista))
                    
cambiarDeBase :: Integer -> Integer -> [Integer] -> [Integer]
cambiarDeBase b1 b2 a = enBase (deBase b1 a) b2

esNoDecreciente :: [Integer] -> Bool
esNoDecreciente lista   | length lista == 1 = True
                        | otherwise         = (head(lista) <= head(tail(lista))) && esNoDecreciente(tail lista)
                     
quitar :: Integer -> [Integer] -> [Integer]
quitar x lista  | lista == []           = []
                | x == head lista       = tail lista
                | otherwise             = [head lista] ++ quitar x (tail lista)
                
pertenece :: Integer -> [Integer] -> Bool
pertenece x []                          = False
pertenece x lista	| head lista == x	= True
			        | otherwise         = pertenece x (tail lista)
                        
quitarRepetidosDejandoElUltimo :: [Integer] -> [Integer]
quitarRepetidosDejandoElUltimo lista    | lista == []                               = []
                                        | pertenece (head lista) (tail lista)       = quitarRepetidosDejandoElUltimo(tail lista)
                                        | otherwise                                 = [(head lista)] ++ quitarRepetidosDejandoElUltimo(tail lista)
                                        
quitarRepetidosDejandoElPrimero :: [Integer] -> [Integer]
quitarRepetidosDejandoElPrimero lista   | lista == []                               = []
                                        | pertenece (head lista) (tail lista)       = quitarRepetidosDejandoElPrimero([(head lista)] ++ (quitar (head lista) (tail lista)))
                                        | otherwise                                 = [(head lista)] ++ quitarRepetidosDejandoElPrimero(tail lista)

unionCreciente :: [Integer] -> [Integer] -> [Integer]
unionCreciente xs ys    | xs == [] && ys == []                                          = [] 
                        | xs == [] && ys /= []                                          = [(head ys)] ++ unionCreciente xs (tail ys)
                        | xs /= [] && ys == []                                          = [(head xs)] ++ unionCreciente (tail xs) ys
                        | (length xs) == 1 && (length ys) > 1 && (head xs) > (head ys)  = [(head ys)] ++ unionCreciente xs (tail ys)
                        | (head xs) >= (head ys)                                        = [(head ys), (head xs)] ++ unionCreciente (tail xs) (tail ys)
                        | (head xs) < (head ys)                                         = [(head xs), (head ys)] ++ unionCreciente (tail xs) (tail ys)

estaOrdenado :: [Integer] -> Bool
estaOrdenado xs = esNoDecreciente xs

ordernarListaMenorAMayor :: [Integer] -> [Integer]
ordernarListaMenorAMayor xs | xs == []                      = []
                            | estaOrdenado xs               = xs
                            | (length xs) == 1              = xs
                            | otherwise                     = ordernarListaMenorAMayor(ordenarProx xs)

ordenarProx :: [Integer] -> [Integer]
ordenarProx xs  | xs == []                  = []
                | (length xs) == 1          = xs
                | head xs >= head(tail xs)  = [head(tail xs), head xs] ++ ordenarProx(tail(tail xs))
                | otherwise                 = [head xs] ++ ordenarProx(tail xs) 