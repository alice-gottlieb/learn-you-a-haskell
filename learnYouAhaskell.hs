import qualified Data.List as L
import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set

tripleMe x = tripleUs x 0

doubleMe x = x + x

doubleUs x y = doubleMe (x + y)

tripleUs x y = 3 * (x + y)

sinApprox x =
	if (abs x < (pi / 12))
		then x
		else sin x

wordMod nums modNum word =
	[if x `mod` modNum == 0 then word else show x | x <- nums]

twoWordsMod nums modNum wordTrue wordFalse = 
	[if x `mod` modNum == 0 then wordTrue else wordFalse | x <- nums]

fizz nums fMod = 
	[if x `mod` fMod == 0 then "fizz" else show x | x <- nums]

-- fizzBuzz nums fMod bMod =
	-- zipWith (++) (wordMod nums fMod "fizz") (wordMod nums bMod "buzz")
	
pythagTriples :: Int -> [(Int, Int, Int)]
pythagTriples n = [(a,b,c) | a <- [1..n], b <- [a..n], c <- [(b+1)..n], a + b > c, c > a, c > b, a^2 + b^2 == c^2]

myFactorial :: (Integral a) => a -> a 
myFactorial 0 = 1
myFactorial n = n * myFactorial (n - 1)

fibonacci :: (Integral a) => a -> a
fibonacci 1 = 1
fibonacci 2 = 1
fibonacci n = (fibonacci (n-1)) + (fibonacci (n-2))

fibonacciSeq :: Int -> [Int]
fibonacciSeq n = take n (map fibonacci [1..])

tell :: (Show a) => [a] -> String
tell [] = "Empty"
tell (x:[]) = "One elem: " ++ show x
tell (x:y:[]) = "Two elems: " ++ show x ++ " and " ++ show y
tell (x:y:z) = "More than two elems: " ++ show x ++ ", " ++ show y ++ " and the rest " ++ show z

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

fstLetter :: String -> String
fstLetter "" = "Empty string"
fstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

lst' :: [a] -> a
lst' [x] = x
lst' (_:xs) = lst' xs

lstLetter :: String -> String
lstLetter "" = "Empty string"
lstLetter all = "The last letter of " ++ all ++ " is " ++ [lst' all]

describeList :: [a] -> String
describeList xs = "This list is " ++ case xs of [] -> "empty."
						[x] -> "a singleton."
						xs -> "a longer list."
						++ " And some extra words."

blend :: [a] -> [a] -> [a]
blend (x:xs) ys = x:(blend ys xs)
blend _ _ = []

max' :: (Ord a) => [a] -> a
max' [] = error "Attempted to take maximum of empty list."
max' [x] = x
max' (x:xs)
	| x > maxTail = x
	| otherwise = maxTail
	where maxTail = max' xs
-- max' (x:y:[]) | x > y = x | otherwise = y
-- max' (x:y:z) = max' ((max' [x,y]):z)

replicate' :: (Integral a) => a -> b -> [b]
replicate' x _
	| x <= 0 = []
replicate' x y = y:(replicate' (x-1) y)

take' :: (Integral a) => a -> [b] -> [b]
take' x _
	| x <= 0 = []
take' _ [] = []
take' x (y:ys) = y:(take' (x-1) ys)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

cycle' :: (Integral a) => a -> [b] -> [b]
cycle' x _
	| x <= 0 = []
cycle' x [] = []
cycle' x y = y ++ (cycle' (x-1) y  )

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : (zip' xs ys)

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys) 
	| x == y = True
	| otherwise = elem' x ys

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
	let lo = quicksort [a | a <- xs, a <= x]
	    hi = quicksort [a | a <- xs, a > x]
	in lo ++ [x] ++ hi

zipOfTen :: (Num b, Enum b) => [a] -> [(a,b)]
zipOfTen = (`zip` [1..10])

zipOfNats :: (Num b, Enum b) => [a] -> [(a,b)]
zipOfNats = (`zip` [1..])

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y) : (zipWith' f xs ys)

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x
-- This also works:
-- flip' f x = (`f` x)

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = (f x) : (map' f xs)

mapElementwise2D :: (a -> b) -> [[a]] -> [[b]]
mapElementwise2D f x = map' (map' f) x

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
	| f x == True = (x : filter' f xs)
	| otherwise = filter' f xs

largestDivisible :: (Integral a) => a -> a -> a
largestDivisible m d = head (filter f [m,(m-1)..])
	where f x = x `mod` d == 0

oddSquares :: (Integral a) => Float -> [a]
oddSquares n = [x^2 | x <- [1..m], x^2 `mod` 2 == 1]
	where m = (floor (s))
		where s = (sqrt n :: Float)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (y:ys)
	| (f y) = y : (takeWhile' f ys)
	| otherwise = []

oddSquares' :: (Integral a) => a -> [a]
oddSquares' n = takeWhile' (< n) [x^2 | x <- [1..], x^2 `mod` 2 == 1]

collatz :: (Integral a) => a -> [a]
collatz 1 = (1:[])
collatz n
	| even n = n : (collatz (n `div` 2))
	| odd n = n : (collatz (3 * n + 1))

foldr' :: (b -> a -> a) -> a -> [b] -> a
foldr' f acc [] = acc
foldr' f acc (x:xs) = f x (foldr' f acc xs)

foldl'' :: (a -> b -> a) -> a -> [b] -> a
foldl'' f acc [] = acc
foldl'' f acc (x:xs) = foldl'' f (f acc x) xs

foldlSeq' :: (a -> b -> a) -> a -> [b] -> a
foldlSeq' f acc [] = acc
foldlSeq' f acc (x:xs) = seq acc (foldlSeq' f (f acc x) xs)

elemFoldr' :: (Eq a) => a -> [a] -> Bool
elemFoldr' y ys = foldr' (\x acc -> if (x == y) then True else acc) False ys

elemFoldl :: (Eq a) => a -> [a] -> Bool
elemFoldl y ys = foldl (\acc x -> if (x == y) then True else acc) False ys

filterFold' :: (a -> Bool) -> [a] -> [a]
filterFold' f xs = foldr' (\y acc -> (if (f y) then y:acc else acc)) [] xs

scanr' :: (b -> a -> a) -> a -> [b] -> [a]
scanr' f acc [] = [acc]
scanr' f acc (x:xs) = (f x (foldr' f acc xs)) : (scanr' f acc xs)

-- Incorrect behavior 
-- scanl' :: (a -> b -> a) -> a -> [b] -> [a]
-- scanl' f acc [] = [acc]
-- scanl' f acc (x:xs) = (scanl' f acc xs) ++ [(foldl' f (f acc x) xs)]

-- Data.List

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub'

nub' :: (Eq a) => [a] -> [a]
nub' [] = []
nub' (x:xs) 
	| (or (map (x==) xs)) = nub' xs
	| otherwise = x : nub' xs

intersperse' :: a -> [a] -> [a]
intersperse' _ [ys] = [ys] 
intersperse' x (y:ys) = y : (x : (intersperse' x ys))

-- could add circuit breaker for more efficiency
and' :: [Bool] -> Bool
and' xs = foldr' (&&) True xs

iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : (iterate' f y)
	where y = f x

group' :: (Eq a) => [a] -> [[a]]
group' [] = []
group' all@(x:xs) = a : (group' b) 
	where (a, b) = span (x==) all

groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' _ [] = []
groupBy' f all@(x:xs) = (x : a) : (groupBy' f b) 
	where (a, b) = span (f x) xs

partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' _ [] = ([], [])
partition' f all@(x:xs)
	| (f x) = (x : a, b)
	| otherwise = (a, x : b)
	where (a, b) = partition' f xs

findKeysBy' :: (k -> Bool) -> [(k, v)] -> [v]
findKeysBy' f xs = map snd (filter (\(k,v) -> (f k)) xs)

findKeys' :: (Eq k) => k -> [(k, v)] -> [v]
findKeys' key = findKeysBy' (==key)

findKey' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey' key xs 
	| null res = Nothing
	| otherwise = Just $ head res
	where res = findKeys' key xs
