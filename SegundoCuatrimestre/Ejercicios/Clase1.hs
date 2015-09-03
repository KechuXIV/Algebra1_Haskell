f n1 n2 n3 	| n2 < 10 = n1
			| n2 >= 10 = n1 + n3

nand x y = not (x && y)

nor x y = not (x || y)

cuadratica a b c = (-b + sqrt((cuadrado b)-(4*a*c)))/2*a

cuadrado x = x^2

esPitagorica a b c = (c == sqrt(cuadrado a + cuadrado b))