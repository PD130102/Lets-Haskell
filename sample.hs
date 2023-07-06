import Data.ByteString (foldl')
import Distribution.Simple.Utils (xargs)
import Distribution.FieldGrammar (alaList')
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

reverselist :: [a] -> [a]
reverselist = reverse

sumArray :: Num a => [a] -> a
sumArray = sum

lengthfind :: [a] -> Int
lengthfind = foldl (\acc x -> acc + 1) 0

-- find length using foldr
lengthfind' :: [a] -> Int
lengthfind' = foldr (\x acc -> acc + 1) 0

-- reverse a list using foldr
reverselist' :: [a] -> [a]
reverselist' = foldr (\x acc -> acc ++ [x]) []

-- reverse a list using foldl
reverselist'' :: [a] -> [a]
reverselist'' = foldl (\acc x -> x : acc) []
length' xs = sum [1 | _ <- xs]

-- find length using foldl
length'' :: [a] -> Int
length'' = foldl (\acc x -> acc + 1) 0

mylast' :: [a] -> a
mylast' [] = error "empty list"
mylast' [x] = x
mylast' (_:xs) = mylast' xs

myLast''''' [] = error "No end for empty lists!"
myLast''''' x = x !! (length x - 1)

myLast'''''' [] = error "No end for empty lists!"
myLast'''''' x = last x

capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x : xs) = "The first letter of " ++ all ++ " is " ++ [x]

max' :: (Ord a) => a -> a -> a
max' a b | a > b = a | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
        | a > b = GT
        | a == b = EQ
        | otherwise = LT

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
        where
                (f : _) = firstname
                (l : _) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
        where
                bmi weight height = weight / height ^ 2

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of
                [] -> "empty."
                [x] -> "a singleton list."
                xs -> "a longer list."

describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
        where
        what [] = "empty."
        what [x] = "a singleton list."
        what xs = "a longer list."

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x : xs)
        | x > maxTail = x
        | otherwise = maxTail
        where
                maxTail = maximum' xs

minimum' :: (Ord a) => [a] -> a
minimum' [] = error "minimum of empty list"
minimum' [x] = x
minimum' (x : xs)
        | x < minTail = x
        | otherwise = minTail
        where
                minTail = minimum' xs


replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
        | n <= 0 = []
        | otherwise = x : replicate' (n - 1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
        | n <= 0 = []
take' _ [] = []
take' n (x : xs) = x : take' (n - 1) xs

drop' :: (Num i, Ord i) => i -> [a] -> [a]
drop' n xs
        | n <= 0 = xs
drop' _ [] = []
drop' n (_ : xs) = drop' (n - 1) xs


zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys


elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x : xs)
        | a == x = True
        | otherwise = a `elem'` xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
        let     smallerSorted = quicksort [a | a <- xs, a <= x]
                biggerSorted = quicksort [a | a <- xs, a > x]
        in smallerSorted ++ [x] ++ biggerSorted

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A' .. 'Z'])

divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)

flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y


map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x : xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x : xs)
        | p x = x : filter' p xs
        | otherwise = filter' p xs

collatz :: (Integral a ) => a -> [a]
collatz 1 = [1]
collatz n
        | even n = n : collatz (n `div` 2)
        | odd n = n : collatz (n * 3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map collatz [1 .. 100]))
        where
                isLong xs = length xs > 15

numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map collatz [1 .. 100]))

lengthofchains :: Int
lengthofchains =  length$filter (>15) ([ length$collatz x | x <- [1 .. 100]])

factorial :: (Integral a)=>a->a
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

swaptuple :: (Num a) => [(a, a)] -> [(a, a)]
swaptuple xs = [(snd x, fst x) | x <- xs]

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

head' :: [a] -> a
head' [] = error "No head for empty lists!"
head' (x : _) = x

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
   in sideArea + 2 * topArea

describeList'' :: [a] -> String
describeList'' xs =  "The list is " ++ case xs of       [] -> "empty."
                                                        [x] -> "a singleton list."
                                                        xs -> "a longer list."

head'' :: [a] -> a
head'' xs = case xs of  [] -> error "No head for empty lists!"
                        (x : _) -> x

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

oddSquareSum :: Integer
oddSquareSum = sum . takeWhile (< 10000) . filter odd . map (^ 2) $ [1 ..]

twice :: (a->a)->a ->a
twice f = f . f 

add4 = add3 4
add3 x = x + 3

map'' :: (a -> b) -> [a] -> [b]
map'' f (x:xs) = f x : map'' f xs

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

oddSquareSum' :: Integer
oddSquareSum' = sum . takeWhile (< 10000) . filter odd . map (^ 2) $ [1,3..]

map''' :: (a -> b) -> [a] -> [b]
map''' f = foldr (\x acc -> f x : acc) []

maxrec :: (Ord a) => [a] -> a
maxrec [] = error "empty list"
maxrec [x] = x
maxrec (x:xs)
        | x>maxtail = x
        | otherwise = maxtail
        where maxtail = maxrec xs

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p x = [y | y <- x, p y]

odd'' :: Int -> Bool
odd'' = not .even

all :: (a -> Bool) -> [a] -> Bool
all p = and . map p

any :: (a -> Bool) -> [a] -> Bool
any p = or . map p

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs)
        | p x = x : takeWhile' p xs
        | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs)
        | p x = dropWhile' p xs
        | otherwise = x: xs


apply :: (a -> b) -> (a -> Bool) -> [a] -> [b]
apply f p xs = map f (filter p xs)

-- apply with map and filter

apply' :: (a -> b) -> (a -> Bool) -> [a] -> [b]
apply' f p = map f . filter p

-- apply with foldr

apply'' :: (a -> b) -> (a -> Bool) -> [a] -> [b]
apply'' f p = foldr (\x acc -> if p x then f x : acc else acc) []

data Shape = Circle Float Float| Rectangle Float Float

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle x y) = x * y

data Point = Point Float Float deriving (Show)
data Shape' = Circle' Point Float | Rectangle' Point Point deriving (Show)

surface :: Shape' -> Float
surface (Circle' _ r) = pi * r ^ 2
surface (Rectangle' (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

and' :: [Bool] -> Bool
and' = foldr (&&) True

or' :: [Bool] -> Bool
or' = foldr (||) False

any' :: (a -> Bool) -> [a] -> Bool
any' p = foldr (\x acc -> p x || acc) False

all' :: (a -> Bool) -> [a] -> Bool
all' p = foldr (\x acc -> p x && acc) True

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

product'' :: (Num a) => [a] -> a
product'' = foldl (*) 1

count :: (Num a,Eq a ) => a ->[a] -> a
count e = foldr (\x acc -> if e == x then acc+1 else acc) 0

isAll :: Eq a => a -> [a] -> Bool
isAll e = foldr (&&) True . map (==e)  

isAll' :: Eq a => a -> [a] -> Bool
isAll' e = foldr(\x acc -> if x == e then acc else False) True

length''' :: (Num b) => [a] -> b
length''' = foldr (const $ (+)1) 0

map1 :: Foldable t => (a -> b) -> t a -> [b]
map1 f  = foldr((:) . f) []

mapfilter1 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapfilter1 f p = foldr((:) . f) [] . filter p

mapfilter2 :: Foldable t => (a1 -> a2) -> (a2 -> Bool) -> t a1 -> [a2]
mapfilter2 f p = filter p . foldr((:) . f) []

fact :: (Int,Int)->Int
fact (n,m) = if n==0 then m else fact(n-1,n*m)

fact' :: Int -> Int
fact' n = fact(n,1)

fact'' :: Int -> Int
fact'' 0 = 1
fact'' n = n * fact''(n-1)


myDrop :: Int -> [a]->[a]
myDrop n xs = if n<=0 || null xs then xs else myDrop (n-1) (tail xs)

flip'' :: (a -> b -> c) -> (b -> a -> c)
flip'' f = g
        where g x y = f y x

flip2' :: (a -> b -> c) -> b -> a -> c
flip2' f y x = f x y

largestDivisible :: (Integral a) => [a]->a
largestDivisible xs = head (filter p xs)
        where p x = mod x 3829 == 0

largestDivisible' :: (Integral a) =>a
largestDivisible' = head (filter p [100000,99999..])
        where p x = mod x 3829 == 0

sumoddsquares :: (Integral a) => a
sumoddsquares = sum(takeWhile(<10000)(filter odd (map (^2)[1,2..])))

fn :: (RealFrac a, Integral b, Floating a) => a -> b
fn = ceiling . negate . tan . cos . max 50
