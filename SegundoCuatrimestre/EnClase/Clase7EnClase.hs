data Direccion = Norte | Sur | Este | Oeste deriving (Show)
type Tortuga = (Pos, Direccion)
type Pos = (Integer, Integer)

arranca :: Tortuga
arranca = ((0,0), Norte)

girarDerecha :: Tortuga -> Tortuga
girarDerecha ((x,y), Norte) = ((x,y), Este)
girarDerecha ((x,y), Sur  ) = ((x,y), Oeste)
girarDerecha ((x,y), Este ) = ((x,y), Sur)
girarDerecha ((x,y), Oeste) = ((x,y), Norte)

avanzar :: Tortuga -> Integer -> Tortuga
avanzar ((x,y), Norte) n    = ((x  ,y+n), Norte)
avanzar ((x,y), Sur  ) n    = ((x  ,y-n), Sur)
avanzar ((x,y), Este ) n    = ((x+n,y  ), Este)
avanzar ((x,y), Oeste) n    = ((x-n,y  ), Oeste)

data Figura =   Rectangulo Float Float Float Float
                | Circulo Float Float Float deriving (Show)
                
c1 :: Figura
c1 = Circulo 0 0 pi

r1 :: Float -> Figura
r1 n = Rectangulo 0 0 (cos(pi/4)*n) (sin(pi/4)*n)

area :: Figura -> Float
area (Rectangulo x1 y1 x2 y2)   = abs(x2 - x1)*abs(y2 - y1)
area (Circulo _ _ r)            = pi*(r^2)

data Punto = Point Float Float
data Figura2 = Rectangulo2 Punto Punto
                | Circulo2 Punto Float
                
area2 :: Figura2 -> Float
area2 (Rectangulo2 (Point x1 y1) (Point x2 y2)) = abs(x2 - x1)*abs(y2 - y1)
area2 (Circulo2 _ r) = pi*(r^2)

data ProgAritmetica = Vacio | CongruentesA Integer Integer
instance Show ProgAritmetica where
    show Vacio = "{}"
    show (CongruentesA x d) = "{a en Z | a = " ++ show(x) ++ " (mod " ++ show(d) ++ ")}"

esMultiplo :: Integer -> Integer -> Bool
esMultiplo x y = mod x y == 0

pertence :: Integer -> ProgAritmetica -> Bool
pertence _ Vacio                = False
pertence x (CongruentesA n m)   = (mod (x-n) m) == 0

incluido :: ProgAritmetica -> ProgAritmetica -> Bool
incluido Vacio _                                    =  True
incluido _ Vacio                                    =  False
incluido (CongruentesA x1 y1) (CongruentesA x2 y2)  =  (pertence x1 (CongruentesA x2 y2)) && (esMultiplo y1 y2)
