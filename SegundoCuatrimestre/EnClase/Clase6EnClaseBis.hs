--Revisar
mcd :: Integer -> Integer -> Integer
mcd a 0 = a
mcd 0 b = b
mcd a b = mcd (a - (div a b)*b) (b)

type Racional = (Integer, Integer)

producto :: Racional -> Racional -> Racional
producto (a, b) (c, d) = (a*c, b*d)

igual :: Racional -> Racional -> Bool
igual (a, b) (c, d) = (x1'/ x2') == (x3'/x4')
    where x1' = fromIntegral a
          x2' = fromIntegral b
          x3' = fromIntegral c
          x4' = fromIntegral d

mayor :: Racional -> Racional -> Bool
mayor (a, b) (c, d) = a > c || b < d

data Dia = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo deriving (Eq, Ord, Show)

esFinde :: Dia -> Bool
esFinde Sabado  = True
esFinde Domingo = True
esFinde _       = False

diaHabil :: Dia -> Bool
diaHabil Sabado     = False
diaHabil Domingo    = False
diaHabil _          = True

diaHabil2 :: Dia -> Bool
diaHabil2 dia = not (esFinde dia)

soloAlgebra :: [Dia] -> [Dia]
soloAlgebra []                  = []
soloAlgebra (Miercoles : xs)    = Miercoles : soloAlgebra xs
soloAlgebra (_ : xs) = soloAlgebra xs 