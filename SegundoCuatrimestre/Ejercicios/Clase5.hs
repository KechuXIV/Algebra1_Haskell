reversoInt :: [Integer] -> [Integer]
reversoInt []   = []
reversoInt lista   = (reversoInt (tail lista)) ++ [head lista]
                
esCapicua :: [Integer] -> Bool
esCapicua lista | length lista == 0 = True
                | length lista == 1 = True
                | otherwise = lista == reverso(lista)
                
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
capicuaPara lista   | esCapicua lista = lista
                    | otherwise = capicuaPara(numeroALista(masElReverso lista))
