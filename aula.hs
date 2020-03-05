vendas :: Int -> Int
vendas 0 = 5
vendas 1 = 6
vendas 2 = 9
vendas 3 = 2


totalVendas :: Int -> Int
totalVendas n 
 | n == 0 = vendas 0
 | otherwise = vendas n + totalVendas (n-1)


maxi:: Int -> Int -> Int
maxi x y
 | x >= y = x
 | otherwise = y


maximoVendas :: Int -> Int
maximoVendas n
 | n == 0 = vendas 0
 | otherwise = maxi (vendas n) (maximoVendas (n-1))


-- Casamento de Padrão

totalVendasCP :: Int -> Int
totalVendasCP 0 = vendas 0
totalVendasCP n = vendas n + totalVendasCP (n-1)

myAnd1 :: Bool -> Bool -> Bool
myAnd1 True True   = True
myAnd1 True False  = False
myAnd1 False True  = False
myAnd1 False False = False

myAnd2 :: Bool -> Bool -> Bool
myAnd2 True True = True
myAnd2 _    _    = False

myOr1 :: Bool -> Bool -> Bool
myOr1 True True   = True
myOr1 True False  = True
myOr1 False True  = True
myOr1 False False = False 

myOr2 :: Bool -> Bool -> Bool
myOr2 True  x = True
myOr2 False x = x

myOr3 :: Bool -> Bool -> Bool
myOr3 True  _ = True
myOr3 False x = x

-- Fatorial
fat :: Int -> Int
fat n
 | n == 0 = 1
 | otherwise = n * fat(n-1)  

-- Quatro números iguais
all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal x y z w
 | x == y && y == z && z == w = True
 | otherwise = False


equalCount :: Int -> Int -> Int -> Int -> Int
equalCount x y z w
 | (x == y) && (y == z) && (z == w) = 4
 | ((x == y) && (y == z)) || ((x == y) && (y == w)) || ((x == w) && (w == z)) || ((w == y) && (y == z)) = 3
 | (x == y) || (y == z) || (z == w) || (w == x) = 2
 | otherwise = 0