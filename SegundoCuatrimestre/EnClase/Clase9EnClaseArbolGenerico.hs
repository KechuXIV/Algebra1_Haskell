data Arbol t = Hoja t | Ramif (Arbol t) t (Arbol t)
    deriving (Eq, Show)

esHoja :: Arbol a -> Bool
esHoja (Hoja n) = True
esHoja _        = False