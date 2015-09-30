mcd :: Integer -> Integer -> Integer
mcd a b | b == 0 = a
        | otherwise = mcd b (mod a b)

fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact ( n - 1)

enIngles :: Integer -> [ Char ]
enIngles 1 = " One ! "
enIngles 2 = " Two ! "
enIngles 3 = " Three ! "
enIngles 4 = " Four ! "
enIngles 5 = " Five ! "
enIngles x = " Not sure : s "

sumarVectores :: (Int , Int) -> (Int , Int) -> (Int , Int)
sumarVectores a b = (fst a + fst b , snd a + snd b )

sumarVectores2 :: (Int , Int) -> (Int , Int) -> (Int , Int)
sumarVectores2 ( x1 , y1 ) ( x2 , y2 ) = ( x1 + x2 , y1 + y2 )

iguales :: (Int , Int ) -> Bool
iguales (x , y ) = x == y

first :: (a , b , c ) -> a
first (x , y , z ) = x


longitud :: [ a ] -> Integer
longitud [] = 0
longitud ( x :[]) = 1
longitud ( x : y :[]) = 2
longitud ( x : y : z :[]) = 3
longitud ( _ : _ : _ : xs ) = 3 + longitud xs

iniciales :: [ Char ] -> [ Char ] -> [ Char ]
iniciales nombre apellido = n : a : []
                            where   ( n : _ ) = nombre
                                    ( a : _ ) = apellido
                                    
type Racional = ( Integer , Integer )
suma :: Racional -> Racional -> Racional
suma (a , b ) (c , d ) = ( a * d + b *c , b * d )

type Punto = ( Float , Float )
dist :: Punto -> Punto -> Float
dist (a, b ) (c, d ) = sqrt( (c-a)^2 + (d-b)^2 )