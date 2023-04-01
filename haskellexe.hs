-- List Comprehension Exercises


-- last element of a list
last' :: [a] -> a
last' = foldl1 (\_ x -> x)

evens :: Integral a => [a] -> [a]
evens [] = []
evens (x : xs)
    | even x = x : evens xs
    | otherwise = evens xs

sums :: Integral a => [a] -> a
sums [] = 0
sums (x : xs) = x + sums xs

addTuples :: [(Int, Int)] -> [Int]
addTuples xs = [x + y | (x, y) <- xs]

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = error "Empty List"
elem' e (x : xs) = (e == x) || elem' e xs

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' _ [] = error "Empty List"
elem'' e (x : xs) = if (e == x) then True else elem'' e xs

nub :: (Eq a) => [a] -> [a]
nub [] = []
nub (x : xs)
    | x `elem` xs = nub xs
    | otherwise = x : nub xs

isAsc :: [Int] -> Bool
isAsc [] = True
isAsc [x] = True
isAsc (x : xs) = if (x <= head xs) then isAsc xs else False

isAsc' :: [Int] -> Bool
isAsc' [] = True
isAsc' [x] = True
isAsc' (x : y : xs) = (x <= y) && isAsc' (y : xs)

haspath :: [(Int, Int)] -> Int -> Int -> Bool
haspath [] x y = True
haspath xs x y
    | x == y = True
    | otherwise =
        let xs' = [(n, m) | (n, m) <- xs, n /= x]
        in or [haspath xs' m y | (n, m) <- xs, n == x]

-- Folding Exercises 

rev :: [a] -> [a]
rev = foldl (\acc x -> x : acc) []

rev' :: [a] -> [a]
rev' = foldl (flip(:)) []

prefixes :: [a] -> [[a]]
prefixes = foldr (\x acc -> [x] : map (x :) acc) []