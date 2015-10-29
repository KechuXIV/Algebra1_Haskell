data Polinomio = Mono Float Integer
                | Suma Polinomio Polinomio
                | Producto Polinomio Polinomio
                deriving (Show, Eq)
                
evaluar :: Polinomio -> Float -> Float
evaluar (Mono c p) z = (c*(z)^p)
evaluar (Suma pol1 pol2) z = (evaluar pol1 z) + (evaluar pol2 z)
evaluar (Producto pol1 pol2) z = (evaluar pol1 z) * (evaluar pol2 z)

coeficientes :: Polinomio -> [Float]
coeficientes (Mono c 0)         = [c]
coeficientes (Mono c p)         = reverse(c : coeficientes (Mono 0 (p-1)))
coeficientes (Suma p1 p2)       = sumaListas (coeficientes p1) (coeficientes p2)
coeficientes (Producto p1 p2)   = porListas (coeficientes p1) (coeficientes p2)

sumaListas :: Num a => [a] -> [a] -> [a]
sumaListas [] l = l
sumaListas l [] = l
sumaListas (x:xs) (y:ys) = (x + y): (sumaListas xs ys)

porListas :: Num a => [a] -> [a] -> [a]
porListas [] l = []
porListas l [] = []
porListas [x] (l:ls) = (x*l):(porListas [x] ls)
porListas (x:xs) l = sumaListas (porListas [x] l) (0:(porListas xs l))

instance Num Polinomio where 
    (+) p q         = (Suma p q)
    (*) p q         = (Producto p q)
    negate p        = (negarPol p)
    fromInteger n   = (Mono (fromIntegral(n)) 0)
    abs p           = undefined
    signum p        = undefined
    
negarPol :: Polinomio -> Polinomio
negarPol (Mono c p)             = (Mono (negate(c)) p)
negarPol (Suma pol1 pol2)       = (Suma (negarPol(pol1)) (negarPol(pol2)))
negarPol (Producto pol1 pol2)   = (Producto pol1 (negarPol(pol2)))

mostrarPol :: Polinomio -> [Char]
mostrarPol pol = mostrarCoeficiente (reverse (coeficientes pol))

mostrarCoeficiente :: [Float] -> [Char]
mostrarCoeficiente (0:[]) = []
mostrarCoeficiente (x:[]) = show(x)
mostrarCoeficiente (x:xs) = show(x) ++ "x^" ++ show((length xs)) ++ "+" ++ (mostrarCoeficiente xs)