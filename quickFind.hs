module QuickFind where

makeArray :: Int -> [Int]
makeArray i =  [ 0 .. i-1 ]

find :: [Int] -> Int -> Int
find a i = a !! i

connected :: [Int] -> Int -> Int -> Bool
connected a p q = (a !! p) == (a !! q)

union :: [Int] -> Int -> Int -> [Int]
union a p q
    | connected a p q = a
    | otherwise = union' a pid q 0
    where pid = a !! p


union' :: [Int] -> Int -> Int -> Int -> [Int]
union' (xs) pid q i
    | reachedEnd      = xs
    | hasId           = setAndLoop
    | otherwise       = loop
    where reachedEnd  = i >= length xs
          hasId       = find xs i == pid
          inc         =  i + 1
          setNewValue = (set xs i (xs !! q))
          setAndLoop  = union' setNewValue  pid q inc
          loop        = union' xs pid q inc

set xs i n= take i xs ++ [n] ++ drop (i + 1) xs
