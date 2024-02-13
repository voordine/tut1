-- Nadine Kraakman, studentnumber: 7244444
module Tut1 where

--1.4.1.1. The type of John is E
type T = Bool
--1.4.1.2, 1.6.1.1 and 1.7.2.1
data E = Albert | Bob | Xena | Yoko deriving (Show)

--1.4.1.3
boy :: E -> T
boy Albert = True
boy Bob = True
boy Xena = False
boy Yoko = False

--1.4.1.4
girl :: E -> T
girl Albert = False
girl Bob = False
girl Xena = True
girl Yoko = True

--1.4.1.5
likes :: E -> E -> T
likes Yoko person = boy person
likes person Xena = boy person
likes _ _ = False


--1.6.1.2
{-instance Eq E where
    (==) :: E -> E -> T
    Albert == Albert = True
    Bob == Bob = True
    Xena == Xena = True
    Yoko == Yoko = True
    _ == _ = False-}

--1.6.1.3 The derived instance of Eq is the same. show Bob == show Bob = True, just like Bob == Bob = True.


--1.7.1.1
count :: [a] -> Int
count [] = 0
count (_:t) = 1 + count t

--1.7.1.2
sumlist :: Num a => [a] -> a
sumlist [] = 0
sumlist (h:t) = h + sumlist t

--1.7.1.3
average :: (Fractional a) => [a] -> a
average [] = error "Empty list"
average ht = sumlist ht / fromIntegral (count ht)

--1.7.1.4
squared :: (Num a) => [a] -> [a]
squared = map (\ h -> h * h)


--1.7.2.2
boys :: [E] -> [E]
boys = filter boy

--1.7.2.3 Done

--1.7.2.4
my_intersection ::(Eq a) => [a] -> [a] -> [a]
my_intersection set1 set2 = [x | x <- set1, x `elem` set2]

my_union ::(Eq a)=> [a] -> [a] -> [a]
my_union set1 set2 = set1 ++ [y | y <- set2, y `notElem` set1]

--1.7.2.5
cartesian :: [a] -> [b] -> [(a,b)]
cartesian list1 list2 = [(x,y) | x <- list1, y <- list2]