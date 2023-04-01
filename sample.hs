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

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x 
        | n <= 0 = []
        | otherwise = x : replicate' (n - 1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
        | n <= 0 = []
take' _ [] = []
take' n (x : xs) = x : take' (n - 1) xs

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x : xs)
        | a == x = True
        | otherwise = a `elem'` xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
        let smallerSorted = quicksort [a | a <- xs, a <= x] 
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

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys  

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
