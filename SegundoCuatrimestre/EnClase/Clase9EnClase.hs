data Dia = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo

data Direccion = Norte | Sur | Este | Oeste deriving Show
type Pos = (Int, Int)
type Tortuga = (Pos, Direccion)

data Figura = Rectangulo Punto Punto | Circulo Punto Float
    deriving (Eq, Show)
    
type Punto = (Float, Float)

data Arbol = Hoja Integer | Ramificacion Arbol Integer Arbol
    deriving (Eq, Show)
    
esHoja :: Arbol -> Bool
esHoja (Hoja _) = True
esHoja _ = False

sumaNodos :: Arbol -> Integer
sumaNodos (Hoja v)                  = v
sumaNodos (Ramificacion ar1 v ar2)  = (sumaNodos ar1) + v + (sumaNodos ar2)

altura :: Arbol -> Integer
altura (Hoja _) = 1
altura (Ramificacion ar1 v ar2) | esHoja(ar1)   = 1 + altura ar2
                                | esHoja(ar2)   = 1 + altura ar1
                                | otherwise     = 1
                                
pertence :: Integer -> Arbol -> Bool
pertence n (Hoja v)                 = n == v
pertence n (Ramificacion ar1 v ar2) = v == n || pertence n ar1 || pertence n ar2

data Dir = Der | Izq

busqueda :: [Dir] -> Arbol -> Integer
busqueda (Der : []) (Ramificacion ar1 v ar2) = ar2
busqueda (Izq : []) (Ramificacion ar1 v ar2) = ar1
busqueda (Der : xs) (Ramificacion ar1 v ar2) = ar2
busqueda (Izq : xs) (Ramificacion ar1 v ar2) = ar1