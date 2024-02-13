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
likes Yoko boy = True
likes boy Xena = True
likes _ _ = False


--1.6.1.2 shortcut????
instance Eq E where
    (==) :: E -> E -> T
    Albert == Albert = True
    Bob == Bob = True
    Xena == Xena = True
    Yoko == Yoko = True
    _ == _ = False

--1.6.1.3


--1.7.1.1 eigenlijk int maar goed
count :: [a] -> Double
count [] = 0
count (_:t) = 1 + count t

--1.7.1.2
sumvalue :: [Double] -> Double
sumvalue [] = 0
sumvalue (h:t) = h + sum t

--1.7.1.3
average :: [Double] -> Double
average [] = error "Da kannie vriend"
average ht = sumvalue ht / count ht

--1.7.1.4
squared :: [Double] -> [Double]
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