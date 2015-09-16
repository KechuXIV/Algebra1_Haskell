suma :: [Integer] -> [Integer] -> [Integer]
suma [] [] = []
suma x y    | length x == 1 = [(head x + head y)]
            | otherwise = [(head x + head y)] ++ suma (tail x) (tail y)