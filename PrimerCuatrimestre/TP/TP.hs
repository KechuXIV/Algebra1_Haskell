import Data.Char

inicioAnsi = 97

esMin :: Char -> Bool
esMin char = isLower char

letANat :: Char -> Integer
letANat char | esMin char  = toInteger((ord char) - inicioAnsi)

natALet :: Integer -> Char
natALet num | num > (-1) && num < 26 = chr (fromEnum(num) + inicioAnsi)
natALet num | num > 26 = natALet (num - 26)

desplazar :: Integer -> Char -> Char
desplazar num char | esMin char = natALet((letANat char) + num)
desplazar num char | otherwise  = char

cantMinusc :: String -> Integer
cantMinusc (x:[]) | esMin x     = 1
cantMinusc (x:[]) | otherwise   = 0
cantMinusc (x:xs) | esMin x     = 1 + cantMinusc xs
cantMinusc (x:xs) | otherwise   = 0 + cantMinusc xs

contar :: Char -> String -> Integer
contar char (x:[])  | char == x = 1
contar char (x:[])  | otherwise = 0
contar char (x:xs)  | char == x = 1 + (contar char xs)
contar char (x:xs)  | otherwise = 0 + (contar char xs)

codificar :: Integer -> String -> String
codificar int (x:[]) = [desplazar int x]
codificar int (x:xs) = (desplazar int x) : codificar int xs

decodificar :: Integer -> String -> String
decodificar int (x:[]) = [desplazar (int*(-1)) x]
decodificar int (x:xs) = (desplazar (int*(-1)) x) : decodificar int xs

frec :: String -> [Float]
frec string = (((contar (natALet 25) string)/(length string))*100) : (((contar (natALet 1) string)/(length string))*100)

frecuencia :: Integer -> String -> Integer
frecuencia 0 string     = [(contar 'a' string)]
frecuencia num string   = (contar (natALet num) string) : (contar (natALet (num-1)) string)