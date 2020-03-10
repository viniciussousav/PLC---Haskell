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


exOr :: Bool -> Bool -> Bool
exOr x y = (x || y) && not (x && y)

exOrCasPad :: Bool -> Bool -> Bool
exOrCasPad True  x = not x
exOrCasPad False x = x  

offset = fromEnum 'a' - fromEnum 'A'

maiuscula :: Char -> Char
maiuscula ch = toEnum (fromEnum ch + offset)

ehDigito :: Char -> Bool
ehDigito ch = ('0' <= ch) && (ch <= '9')

testShow = show 3 ++ "a"
testRead = (read "3" :: Int ) + 3
testFloor = floor 4.3
testCeiling = ceiling 4.3

-- Tuplas

intP :: (Int,Int)
intP = (1,2)

addPair :: (Int, Int) -> Int
addPair (x, y) = x + y 

addPair2 :: (Int, Int) -> Int
addPair2 p = fst p + snd p

primeiro :: (Int, Int) -> Int
primeiro (x, y) = x

segundo :: (Int, Int) -> Int
segundo (x, y) = y 

terceiroDe5 :: (Int, Int, Int, Int, Int) -> Int
terceiroDe5 (a, b, c, d, e) = c


-- Definicoes locais
somaQuadrados :: Int -> Int -> Int
somaQuadrados x y = quadX + quadY
  where
      quadX = x * x
      quadY = y * y

somaQuadrados2:: Int -> Int -> Int
somaQuadrados2 x y = quadP x + quadP y
  where quadP n = n * n

somaQuadradosLetIn :: Int -> Int -> Int
somaQuadradosLetIn x y = let sqX = x * x 
                             sqY = y * y
                         in  sqX + sqY

-- Fatorial
fat :: Integer -> Integer
fat n
 | n == 0 = 1
 | n > 0 = n * fat(n-1)  

-- Quatro números iguais
all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal x y z w = x == y && y == z && z == w


--Quantos argumentos são iguais
equalCount :: Int -> Int -> Int -> Int -> Int
equalCount x y z w
 | (x == y) && (y == z) && (z == w) = 4
 | ((x == y) && (y == z)) || ((x == y) && (y == w)) || ((x == w) && (w == z)) || ((w == y) && (y == z)) = 3
 | (x == y) || (y == z) || (z == w) || (w == x) = 2
 | otherwise = 0

