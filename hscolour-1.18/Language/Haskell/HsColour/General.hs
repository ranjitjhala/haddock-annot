
module Language.Haskell.HsColour.General(
    dropLast, dropFirst
    ) where


dropLast :: Eq a => a -> [a] -> [a]
dropLast x [y] | x == y = []
dropLast x (y:ys) = y : dropLast x ys
dropLast x [] = []


dropFirst :: Eq a => a -> [a] -> [a]
dropFirst x (y:ys) | x == y = ys
dropFirst x ys = ys
