import Data.Char

funcion :: Float -> Float
funcion x | x > 0 = sqrt(x)
    | x == 0 = 1
    
pendiente :: (Float, Float) -> (Float, Float) -> Float
pendiente (x1, y1) (x2, y2) = (y2 - y1)/(x2 - x1)

--toUpper :: Char -> Char --Toma un caracter y tranforma en mayusculas
--isSpace :: Char -> Bool --Toma un caracter y informa si es espacio
--isDigit :: Char -> Bool --Toma un caracter y informa si es un digito
--ord :: Char -> Integer  --Toma un caracter y devuelve su codigo ASCII
--chr :: Integer -> Char  --Toma un entero y devuelve su ASCII

cuadrado :: Float -> Float
cuadrado x = x^2

raices :: Float -> Float -> Float -> (Float, Float)
raices a b c = ( cuadratica a b c 1 , cuadratica a b c (-1))

cuadratica :: Float -> Float -> Float -> Float -> Float
cuadratica a b c signo = (-b + (sqrt((cuadrado b)-(4*a*c))*signo) )/2*a

ite :: Bool -> a -> a -> a
ite True t e = t
ite False t e = e

f :: Bool -> a -> a
f x y = ite x x y

g :: Bool -> a -> a
g x y = if x then y else y

h :: Bool -> Bool -> Bool
h x y = (if x then y else y) && True

i :: a -> a -> Bool
i x y = h True 2