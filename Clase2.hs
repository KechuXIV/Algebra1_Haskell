pendiente :: (Float,Float) -> (Float,Float) -> Float
pendiente x y = (fst x - fst y)/(snd x - snd y)

--toUpper :: Char -> Char
--isSpace :: Char -> True
--isDigit :: Char -> True
--ord :: Char -> Int
--chr :: Int -> Char

imprimir :: String -> IO()
imprimir texto = print(texto)

queDevuelve :: Bool
queDevuelve = "Ginobili" == ['G', 'i', 'n', 'o', 'b', 'i', 'l', 'i']

iniciales :: String -> String -> String
iniciales nombre apellido = [head nombre, '.', head apellido, '.', ' ']

iniciales2 :: String -> String -> String
iniciales2 nombre apellido = [head nombre] ++ "." ++ [head apellido] ++ ". "