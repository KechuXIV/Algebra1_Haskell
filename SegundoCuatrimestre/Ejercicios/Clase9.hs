data Arbol t = Hoja t | Ramif (Arbol t) t (Arbol t)
    deriving (Eq, Show)

esHeap :: Ord a => Arbol a -> Bool
esHeap (Hoja n)                                       = True
esHeap (Ramif (Hoja a) b (Hoja c))                    = (a > b) && (c > b)
esHeap (Ramif (Hoja a) b (Ramif ar3 c ar4))           = (a > b) && (c > b) && esHeap (Hoja a) && esHeap (Ramif ar3 c ar4)
esHeap (Ramif (Ramif ar1 a ar2) b (Hoja c))           = (a > b) && (c > b) && esHeap (Ramif ar1 a ar2) && esHeap (Hoja c)
esHeap (Ramif (Ramif ar1 a ar2) b (Ramif ar3 c ar4))  = (a > b) && (c > b) && esHeap (Ramif ar1 a ar2) && esHeap (Ramif ar3 c ar4)

data Lista a = Vacia | Agregar a (Lista a)
instance Show a => Show (Lista a) where
    show xs = mostrarLista xs


vacia :: Lista a -> Bool
vacia Vacia = True
vacia _ = False

suma :: Lista Float -> Float
suma Vacia          = 0
suma (Agregar n xs) = n + suma xs

enPosicion :: Lista a -> Integer -> a
enPosicion (Agregar x xs) 0 = x
enPosicion (Agregar x xs) z = enPosicion xs (z-1)

iguales :: Eq a => Lista a -> Lista a -> Bool
iguales (Vacia) (Vacia) = True
iguales (Agregar x Vacia) (Agregar y Vacia) = x == y
iguales (Agregar x xs) (Agregar y ys) = (x == y) && iguales xs ys

juntar :: Lista a -> Lista b -> Lista (a, b)
juntar (Vacia) (Vacia) = Vacia
juntar (Agregar x Vacia) (Agregar y Vacia) = Agregar (x, y) Vacia
juntar (Agregar x xs) (Agregar y ys) = Agregar (x, y) (juntar xs ys)

mostrarLista :: Show a => Lista a -> [Char]
mostrarLista xs = "[" ++ (mostrarListaFin xs)

mostrarListaFin :: Show a => Lista a -> [Char]
mostrarListaFin (Agregar x Vacia)  = show(x) ++ ']' : []
mostrarListaFin (Agregar x xs)     = show(x) ++ ", " ++ (mostrarListaFin xs)