data Polinomio = Mono Float Integer
		| Suma Polinomio Polinomio
		| Producto Polinomio Polinomio

sumaListas :: Num a => [a] -> [a] -> [a]
sumaListas [] l = l
sumaListas l [] = l
sumaListas (x:xs) (y:ys) = (x + y): (sumaListas xs ys)

porListas :: Num a => [a] -> [a] -> [a]
porListas [] l = []
porListas l [] = []
porListas [x] (l:ls) = (x*l):(porListas [x] ls)
porListas (x:xs) l = sumaListas (porListas [x] l) (0:(porListas xs l))

evaluar :: Polinomio -> Float -> Float
evaluar (Mono a n) z = a*(z^n)
evaluar (Suma p q) z = (evaluar p z) + (evaluar q z)
evaluar (Producto p q) z = (evaluar p z) * (evaluar q z)

coeficientes :: Polinomio -> [Float]
coeficientes p = limpiarCeros (coeficientes' p)

coeficientes' (Mono a 0) = [a]
coeficientes' (Mono a n) = 0:(coeficientes' (Mono a (n-1)))
coeficientes' (Suma p q) = sumaListas (coeficientes' p) (coeficientes' q)
coeficientes' (Producto p q) = porListas (coeficientes' p) (coeficientes' q)

sacarCeros :: [Float] -> [Float]
sacarCeros (0:xs) = sacarCeros xs
sacarCeros l = l

limpiarCeros :: [Float] -> [Float]
limpiarCeros l = reverse (sacarCeros (reverse l))

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
mostrarPol (Mono c 0) = show(c)

mostrarCoeficiente :: [Float] -> [Char]
mostrarCoeficiente (x:[]) = show(x)
mostrarCoeficiente (0:xs) = (mostrarCoeficiente xs)
mostrarCoeficiente (x:xs) = show(x) ++ "x^" ++ show((length xs) - 1) ++ (mostrarCoeficiente xs)