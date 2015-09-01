factorial :: Float -> Float
factorial 0 = 1
--factorial n < 0 = 0
factorial n = n * factorial (n-1)

combinatoria :: Float -> Float -> Float
combinatoria n m = factorial n /factorial (n - m)