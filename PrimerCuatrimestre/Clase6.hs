enIngles :: Integer -> [ Char ]
enIngles 1 = " One ! "
enIngles 2 = " Two ! "
enIngles 3 = " Three ! "
enIngles 4 = " Four ! "
enIngles 5 = " Five ! "
enIngles x = " Not sure : s "

data Dia = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo

esFinde :: Dia -> Bool
esFinde d 	| d == Sabado	= True
			| d == Domingo	= True
			| otherwise = False