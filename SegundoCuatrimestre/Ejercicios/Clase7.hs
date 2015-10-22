data ProgAritmetica = Vacio | CongruentesA Integer Integer
instance Show ProgAritmetica where
    show Vacio = "{}"
    show (CongruentesA x d) = "{a en Z | a = " ++ show(x) ++ " (mod " ++ show(d) ++ ")}"


suma :: ProgAritmetica -> ProgAritmetica -> ProgAritmetica
suma p Vacio = p
suma Vacio q = q
suma (CongruentesA a b) (CongruentesA c d) = CongruentesA (a+c) (b-d)

esMultiplo :: Integer -> Integer -> Bool
esMultiplo x y = mod x y == 0

pertence :: Integer -> ProgAritmetica -> Bool
pertence _ Vacio                = False
pertence x (CongruentesA n m)   = (mod (x-n) m) == 0

incluido :: ProgAritmetica -> ProgAritmetica -> Bool
incluido Vacio _                                    =  True
incluido _ Vacio                                    =  False
incluido (CongruentesA x1 y1) (CongruentesA x2 y2)  =  (pertence x1 (CongruentesA x2 y2)) && (esMultiplo y1 y2)


interseccion :: ProgAritmetica -> ProgAritmetica -> ProgAritmetica
interseccion Vacio _ = Vacio
interseccion _ Vacio = Vacio
interseccion p q | incluido p q = p
interseccion p q | incluido q p = q

iguales :: ProgAritmetica -> ProgAritmetica -> Bool
iguales p q = (incluido p q) && (incluido q p)

instance Eq ProgAritmetica where
    iguales p q == True = True