data Dia = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
data Direccion = Norte | Sur | Este | Oeste deriving Show

type Pos = (Int, Int)
type Tortuga = (Pos, Direccion)
type Punto = (Float, Float)

data Figura = Rectangulo Punto Punto | Circulo Punto Float | Triangulo Punto Punto Punto deriving (Eq, Show)

