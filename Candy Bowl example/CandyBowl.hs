--Harsh Nagarkar
--10616659
--https://stackoverflow.com/questions/4573692/haskell-lists-difference
--0http://zvon.org/other/haskell/Outputlist/group_f.html
module CandyBowl
where
import Data.List
data CandyBowl a = Bowl [a] deriving Show


-- creates a new empty candy bowl.
newBowl :: CandyBowl a
newBowl = Bowl []

-- returns True if and only if the bowl is empty.
isEmpty :: CandyBowl a -> Bool
isEmpty (Bowl cb) = ans
    where
        ans = if length cb == 0
                then True
            else False

-- adds one piece of candy of kind "Kiss" to the bowl.
putIn :: CandyBowl a -> a -> CandyBowl a
putIn (Bowl candybwl) candy = ans
    where
        ans =  (Bowl (candy: candybwl))

-- returns True if and only if one or more pieces of the given kind of candy is in the bowl.
has :: Eq a=> CandyBowl a -> a -> Bool
has (Bowl bwl) find = ans
    where
        ans = if filter(\x-> x==find) bwl /= []
                then True
            else False

-- -- returns the total number of pieces of candy in the bowl (regardless of kind).
size :: CandyBowl a -> Int
size (Bowl peices) =  length peices

-- -- returns the count of the given kind of candy in the bowl.
howMany ::Eq a=> CandyBowl a -> a -> Int
howMany (Bowl bwl) peice =  count
    where
        finder = filter(\x->x==peice) bwl
        count = if finder == []
                then 0
            else  length finder

-- -- attempts to remove one piece of candy of the given kind from the bowl (so it can be eaten). If the bowl contains a piece of the given kind, the function returns the value Just bowl, where bowl is the bowl with the piece removed. If the bowl does not contain such a piece, it returns the value Nothing
takeOut :: Eq a=> CandyBowl a -> a -> Maybe (CandyBowl a)
takeOut (Bowl bwl) peice = ans
    where
        ans = if (has (Bowl bwl) peice) == False
                then Nothing
        else Just (Bowl (delete peice bwl))

-- -- returns True if and only if the two bowls have the same contents (that is the same kinds of candy and the same number of pieces of each kind).
eqBowl :: Ord a => CandyBowl a -> CandyBowl a -> Bool
eqBowl (Bowl bwl1) (Bowl bwl2) = ans
    where
        ans = if (length bwl1 == length bwl2) == False
                then False
            else check (sort bwl1) (sort bwl2)

--checks the sorted list one by one simulatneosly for till the end and return a boolean value back.
check::Ord a => Eq a => [a] -> [a] -> Bool
check [] [] = True
check (x:xs) (y:ys) = ans
    where
        ans = if x == y
                then check xs ys
            else False

-- -- returns a Haskell list of pairs (k,n), where each kind k of candy in the bowl occurs once in the list with n > 0. The list should be arranged in ascending order by kind.
inventory :: Ord a => CandyBowl a -> [(a,Int)]
inventory (Bowl bwl) = ans
    where
        groupelemts = group (sort bwl)
        ans = map (\x-> (head x,length x)) groupelemts

-- -- creates a new bowl such that for any bowl:
restock :: [(a,Int)] -> CandyBowl a
restock nums = Bowl (concatMap (\(x,y)-> replicate y x) nums)

--combines two candy  bowls
combine :: CandyBowl a -> CandyBowl a -> CandyBowl a
combine (Bowl bwl1) (Bowl bwl2) = Bowl (bwl1 ++ bwl2)

-- -- returns a bowl containing the pieces of candy in the first bowl that are not in the second bowl.
difference ::Ord a => CandyBowl a -> CandyBowl a -> CandyBowl a
difference (Bowl bwl1) (Bowl bwl2) = (Bowl ans)
    where
        ans = calDiff (sort bwl1) (sort bwl2)
-- -- For example, if the first bowl has four "Snickers" and the second has one "Snickers", then the result will have three "Snickers".
calDiff:: Ord a => [a] -> [a] -> [a]
calDiff b1 b2 = (b1 \\ b2)

-- takes a bowl and a renaming function, applies the renaming function to all the kind values in the bowl, and returns the modified bowl.
rename :: CandyBowl a -> (a -> b) -> CandyBowl b

rename (Bowl bwl1) funn = Bowl (map funn bwl1)
