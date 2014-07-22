module ListProblems where
import System.IO

--
-- http://www.haskell.org/haskellwiki/99_questions/1_to_10
--
-- Problem 1
myLast :: [a] -> a
myLast [] = error "Cannot call myLast on empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

-- Problem 2
myButLast :: [a] -> a
myButLast [] = error "Cannot call myButLast on empty list"
myButLast [x, _] = x
myButLast (_:xs) = myButLast xs

myButLast' :: [a] -> a
myButLast' list = head (reverse (take 2 (reverse list)))

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt list 1 = head list
elementAt (_:xs) n = elementAt xs (n - 1)

-- Problem 4
myLength :: [a] -> Int
myLength list = myLength' list 0
    where myLength' [] n = n
          myLength' (_:xs) n = myLength' xs (n+1)

-- Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse list = (last list) : (myReverse $ init list)

-- Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome list =
    first == (reverse second) where
        (first, second) = split d m
        (d, m) = (length list) `divMod` 2
        split n 0 = splitAt n list
        split n 1 = (take n list, drop (n+1) list)

-- Problem 7
data NestedList a = Elem a | List [NestedList a] deriving Show

myFlatten :: NestedList a -> [a]
myFlatten (Elem a) = [a]
myFlatten (List []) = []
myFlatten (List (x:xs)) = myFlatten x ++ myFlatten (List xs)

-- Problem 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:ys)
    | x == y = compress (x:ys)
    | x /= y = x : compress (y:ys)

-- Problem 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack list@(x:_) =
    let (group, rest) = span (== x) list in
    group : pack rest

-- Problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode list = [(length xs, head xs) | xs <- pack list]

--
-- http://www.haskell.org/haskellwiki/99_questions/11_to_20
--
-- Problem 11
data Encoding a = Single a | Multiple Int a deriving Show

encodeModified :: Eq a => [a] -> [Encoding a]
encodeModified list = [if (length xs) == 1 then Single (head xs)
                       else Multiple (length xs) (head xs) | xs <- pack list]

-- Problem 12
decodeModified :: [Encoding a] -> [a]
decodeModified list = concat $ map decodeElement list where
    decodeElement (Single a) = [a]
    decodeElement (Multiple i a) = replicate i a

-- Problem 13
encodeDirect :: Eq a => [a] -> [Encoding a]
encodeDirect list = [if (length xs) == 1 then Single (head xs)
                     else Multiple (length xs) (head xs) | xs <- pack list]
