data Arbol t = Hoja t | Ramif (Arbol t) t (Arbol t)
    deriving (Eq, Show)

esHoja :: Arbol a -> Bool
esHoja (Hoja n) = True
esHoja _        = False

maximo :: Ord a => Arbol a -> a
maximo (Hoja n)                                                 = n
maximo (Ramif ar1 v ar2) | (v > maximo ar1) && (v > maximo ar2) = v
maximo (Ramif ar1 v ar2) | (maximo ar1 > maximo ar2)            = maximo ar2
maximo (Ramif ar1 v ar2) | (maximo ar2 > maximo ar1)            = maximo ar2

altura :: Arbol a -> Integer
altura (Hoja _) = 1
altura (Ramif ar1 v ar2)    | esHoja(ar1)   = 1 + altura ar2
                            | esHoja(ar2)   = 1 + altura ar1
                            | otherwise     = 1

raiz :: Arbol a -> a
raiz (Hoja n)                                       = n
raiz (Ramif ar1 v ar2) | altura ar1 > altura ar2    = raiz ar1
raiz (Ramif ar1 v ar2) | otherwise                  = raiz ar2

todosIguales :: Eq a => Arbol a -> Bool
todosIguales (Hoja n)                                       = True
todosIguales (Ramif (Hoja a) b (Hoja c))                    = (a == b) && (b == c)
todosIguales (Ramif (Hoja a) b (Ramif ar3 c ar4))           = (a == b) && (b == c) && todosIguales (Hoja a) && todosIguales (Ramif ar3 c ar4)
todosIguales (Ramif (Ramif ar1 a ar2) b (Hoja c))           = (a == b) && (b == c) && todosIguales (Ramif ar1 a ar2) && todosIguales (Hoja c)
todosIguales (Ramif (Ramif ar1 a ar2) b (Ramif ar3 c ar4))  = (a == b) && (b == c) && todosIguales (Ramif ar1 a ar2) && todosIguales (Ramif ar3 c ar4)

espejar :: Arbol a -> Arbol a
espejar (Hoja n) = (Hoja n)
espejar (Ramif (Hoja a) b (Hoja c))                     = (Ramif (Hoja c) b (Hoja a))
espejar (Ramif (Hoja a) b (Ramif ar3 c ar4))            = (Ramif (espejar(Ramif ar3 c ar4)) b (Hoja a))
espejar (Ramif (Ramif ar1 a ar2) b (Hoja c))            = (Ramif (Hoja c) b (espejar(Ramif ar1 a ar2)))
espejar (Ramif (Ramif ar1 a ar2) b (Ramif ar3 c ar4))   = (Ramif (espejar (Ramif ar3 c ar4)) b (espejar (Ramif ar1 a ar2)))