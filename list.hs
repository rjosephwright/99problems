module ListProblems where

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

myButLast'' :: [a] -> a
myButLast'' = head . tail . reverse

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
        (first, second) = halve d m
        (d, m) = (length list) `divMod` 2
        halve n 0 = splitAt n list
        halve n 1 = (take n list, drop (n+1) list)

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

-- Problem 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : (dupli xs)

-- Problem 15
repli :: [a] -> Int -> [a]
repli [] _ = []
repli _ 0 = []
repli (x:xs) n = listx x n ++ (repli xs n) where
    listx _ 0 = []
    listx a n' = a : listx a (n'-1)

-- Problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery l n
    | length l < n = l
    | otherwise =
        let (first, rest) = splitAt n l in
        take (length first-1) first ++ dropEvery rest n

-- Problem 17
split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split xs n = split' [] xs n where
    split' acc (x:xs') n'
        | length acc < n' = ([], (x:xs'))
        | length acc == n' = (acc, (x:xs'))
        | otherwise = split' (acc ++ [x]) xs' n'

-- Problem 18
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice (x:xs) start end = slice' (x:xs) (start-1) (end-1) 0 where
    slice' (x':xs') start' end' cur
        | cur < start' = slice' xs' start' end' (cur+1)
        | cur >= start' && cur <= end' = x' : slice' xs' start' end' (cur+1)
        | cur > end' = []

-- Problem 19
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs n = back ++ front where
    (front, back)
        | n > 0 = splitAt n xs
        | n < 0 = let (b, f) = splitAt (abs (n)) $ reverse xs in
                  (reverse f, reverse b)

-- Problem 20
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = let (a, b) = splitAt (n-1) xs in (head b, a ++ tail b)
