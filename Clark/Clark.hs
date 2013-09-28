
module Clark.Clark where

import Data.List(nub, all)

clark :: [[Int]] -> [[Int]]
clark [] = []
clark [x]
	| not(goodGenotype x) = error "bad genotypes"
	| countTwos x > 1 = []
	| otherwise = inferH1 (x::[Int])
clark x 
	| False `elem` map goodGenotype x = error "bad genotypes"
	| otherwise = nub $ clark' (x::[[Int]]) [] 0 (length x)


clark' :: [[Int]] -> [[Int]] -> Int -> Int -> [[Int]]
clark' [] bag _ _ = bag
clark' [x] bag _ _ = bag ++ inferH1 x
clark' (x:gens) bag k l
	| k > l = error "Clark's algorithm failed"
	| countTwos x <= 1 = clark' gens (bag ++ inferH1 x) (k + 1) l
	| canInfer x bag /= [] = clark' gens (canInfer x bag:bag) (k + 1) l
	| otherwise = clark' (rotate (x:gens)) bag (k + 1) l


goodGenotype :: [Int] -> Bool
goodGenotype = all (\z-> z `elem` [0,1,2])


rotate :: [[Int]] -> [[Int]]
rotate [] = []
rotate [x] = [x]
rotate (x:hs) = hs ++ [x]


countTwos :: [Int] -> Int
countTwos [] = 0
countTwos (x:hs)
	| x == 2 = 1 + countTwos hs
	| otherwise = countTwos hs


canInfer :: [Int] -> [[Int]] -> [Int]
canInfer x [] = []
canInfer x (b:bag)
	| inferH2 x b /= [] = inferH2 x b
	| otherwise = canInfer x bag


inferH1 :: [Int] -> [[Int]]
inferH1 [] = []
inferH1 x = [map head $ inferH1' x, map last $ inferH1' x]


inferH1' :: [Int] -> [[Int]]
inferH1' [] = []
inferH1' (x:hs)
	| x == 0 = [0, 0]:inferH1' hs
	| x == 1 = [1, 1]:inferH1' hs
	| x == 2 = [1, 0]:inferH1' hs


inferH2 :: [Int] -> [Int] -> [Int]
inferH2 [] _ = []
inferH2 _ [] = []
inferH2 x y 
	| length (inferH2' x y) < length x = []
	| otherwise = inferH2' x y


inferH2' :: [Int] -> [Int] -> [Int]
inferH2' (g:gen) (h:hap)
	| g == 1 && h == 1 = 1:inferH2 gen hap
	| g == 0 && h == 0 = 0:inferH2 gen hap
	| g == 2 && h == 1 = 0:inferH2 gen hap
	| g == 2 && h == 0 = 1:inferH2 gen hap
	| otherwise = []
