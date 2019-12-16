{-
READ ME: Change the name of this file to YOURSTUDENTNUMBER.hs. For example, if your
student number were 123456789, then you would rename the file 123456789.hs.

Fill in the function definitions for each of the questions, and uncomment them. 
The names of the functions correspond to the names given in the document cwk_15_16.pdf. 

DO NOT CHANGE THE TYPE SIGNATURES, OR THE ORDER OF THE ARGUMENTS!

You may add as many extra or helper functions as you wish, but do not make any "import" statements.
-}

-- QUESTION 1 

isReflexive :: (Eq a) => [(a,a)] -> Bool
isReflexive refVar = and [elem (x, x) refVar | x <- (multiFst refVar) ++ (multiSnd refVar)]


isSymmetric :: (Eq a) => [(a,a)] -> Bool
isSymmetric symVar = and [isPart (y, x) symVar | (x, y) <- symVar]


isTransitive :: (Eq a) => [(a,a)] -> Bool
isTransitive transVar = and [isPart (x, z) transVar | (x, y) <- transVar, (y, z) <- transVar]


isEquivalence :: (Eq a) => [(a,a)] -> Bool
isEquivalence xs
	| (isReflexive xs == True) && (isTransitive xs == True) && (isSymmetric xs == True) = True
	| otherwise = False
	

eqClassOf :: (Eq a) => [(a,a)] -> a -> [a]
eqClassOf [] n = []
eqClassOf (x:xs) n 
	| (fst x == n) = snd x: eqClassOf xs n 
	| otherwise = eqClassOf xs n

-- TEST SET FOR Q1
{-
Your functions should have the following behaviour:
isReflexive [(1,2),(2,1),(1,1),(2,2)] is True
isReflexive [(1,2),(2,1),(2,2)] is False
isSymmetric [(1,2),(2,1),(1,1),(2,2)] is True
isSymmetric [(1,2),(1,1),(2,2)] is False
isTransitive [(1,2),(2,1),(1,1),(2,2)] is True
isTransitive [(1,2),(2,3)] is False
isEquivalence [(1,2),(2,1),(1,1),(2,2)] is True
eqClassOf [(1,2),(2,1),(1,1),(2,2)] 1 is [1,2]
-}

-- QUESTION 2

multiEqual :: (Eq a) => [a] -> [a] -> Bool
multiEqual [] _ = True
multiEqual (x:xs) ys
    | elem x ys && (length (x:xs) == length ys) = multiEqual xs (deleteFirst x ys)
    | otherwise = False

	
multiUnion :: (Eq a) => [a] -> [a] -> [a]
multiUnion xs ys = uniqueList xs ++ uniqueList ys


multiIntersection :: (Eq a) => [a] -> [a] -> [a]
multiIntersection xs ys = uniqueList [z | z <- xs, elem z ys]


-- TEST SET FOR Q2
{-
Your functions should have the following behaviour:
multiEqual [1,1,2] [1,2,1] is True
multiEqual [1,1,2] [1,2] is False
multiUnion [1,1,2] [1,2,2] is [1,1,2,2]
multiIntersection [1,1,2] [1,2,2] is [1,2]
-}

-- QUESTION 3


trace :: (Num a) => [[a]] -> Maybe a
trace [] = Nothing
trace (x:xs)
	|(length (x:xs) == length x) = 	Just (tracer (x:xs) 0)
	| otherwise = Nothing


matMult3 :: (Num a) => [[a]] -> [[a]] -> [[a]]
matMult3 a b = [[a!!0!!0 * b!!0!!0 + a!!0!!1 * b!!1!!0 + a!!0!!2 * b!!2!!0, a!!0!!0 * b!!0!!1 + a!!0!!1 * b!!1!!1 + a!!0!!2 * b!!2!!1, a!!0!!0 * b!!0!!2 + a!!0!!1 * b!!1!!2 + a!!0!!2 * b!!2!!2],
				[a!!1!!0 * b!!0!!0 + a!!1!!1 * b!!1!!0 + a!!1!!2 * b!!2!!0, a!!1!!0 * b!!0!!1 + a!!1!!1 * b!!1!!1 + a!!1!!2 * b!!2!!1, a!!1!!0 * b!!0!!2 + a!!1!!1 * b!!1!!2 + a!!1!!2 * b!!2!!2],
				[a!!2!!0 * b!!0!!0 + a!!2!!1 * b!!1!!0 + a!!2!!2 * b!!2!!0, a!!2!!0 * b!!0!!1 + a!!2!!1 * b!!1!!1 + a!!2!!2 * b!!2!!1, a!!2!!0 * b!!0!!2 + a!!2!!1 * b!!1!!2 + a!!2!!2 * b!!2!!2]]

				
-- TEST SET FOR Q3
{-
Your functions should have the following behaviour:
trace [[1,2],[6,4]] is Just 5
matMult3 [[1,0,1],[0,1,0],[0,0,1]] [[0,1,0],[1,0,1],[1,1,0]] is
[[1,2,0],[1,0,1],[1,1,0]]
-}

-- QUESTION 4

{-
triNumber :: Int -> Int -> [Int]
FIRST ARGUMENT IS ROW NUMBER, SECOND IS SEED/VALUE AT TIP OF TRIANGLE
-}

-- TEST SET FOR Q4
{-
Your function should have the following behaviour:
triNumber 3 1 is [2,3,5]
-}

-- QUESTION 5


combine :: Int -> Int -> (Int, Int, Int)
combine 0 b = (b, 0, 1)
combine a b = let (g, s, t) = combine (b `mod` a) a
           in (g, t - (b `div` a) * s, s)


-- TEST SET FOR Q5
{-
Your function should have the following behaviour:
combine 3 2 is (1,-1,1)
-}




-- Question 1 Helpers

-- REFLEXIVE

multiFst :: [(a,b)] -> [a]
multiFst [] = []
multiFst (x:xs) = fst x: multiFst xs

multiSnd :: [(a,b)] -> [b]
multiSnd [] = []
multiSnd (x:xs) = snd x: multiSnd xs

-- TRANSITIVE AND SYMMETRIC

isPart x [] = False
isPart x (y:ys)
	| x == y = True
	| otherwise = isPart x ys
	

--Question 2 Helpers

--UNIQUE LIST

uniqueList :: (Eq a) => [a] -> [a]
uniqueList [] = []
uniqueList (x:xs)
	| elem x xs = uniqueList xs
	| otherwise = x : uniqueList xs
	
-- IS EQUAL

deleteFirst _ [] = [] 
deleteFirst a (b:bc) 
    | a == b = bc 
    | otherwise = b : deleteFirst a bc
	

-- Question 3 Helpers

-- TRACER

tracer :: (Num a) => [[a]] -> Int -> a
tracer [] b = 0
tracer (x:xs) b = x!!b + tracer xs (b+1)