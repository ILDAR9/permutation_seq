module RandomPermutation (imperativePermutation, randomlist, generateShuffle, shuffle, shuffle1) where
import System.Random
import System.IO.Unsafe (unsafePerformIO)
import Data.List (unfoldr)
import Debug.Trace

{- | Shift 2 items in list

>>> shiftItem [0..6] 1 3
[0,3,2,1,4,5,6]

>>> shiftItem [0..4] 2 2
[0,1,2,3,4]

-}
shiftItem :: [a] -> Int -> Int -> [a]
shiftItem [] _ _ = []
shiftItem xs i j 
	|i == j = xs
	|otherwise = take (i) xs ++ [xs!!j] ++ take (j-i-1) (drop (i+1) xs) ++ [xs!!i] ++ drop (j+1) xs

randomInt :: Int -> Int
randomInt x = unsafePerformIO . randomRIO $ (0, x)

imperativePermutation :: (Num a) => [a] -> [a]
imperativePermutation [] = []
imperativePermutation xs = drop 1 $ foldl (\acc x-> modif acc x) xs' [1..len-1]
							where
								len = length xs
								xs' = [0] ++ xs
								modif ac i = shiftItem ac i $ (randomInt (len-i)) + i
								
------------     RGN      -----------------------------

randomlist :: Int -> StdGen -> [Int]
randomlist n seed = map (abs) $ take n $ unfoldr (Just . random) seed


generateShuffle :: [Int] -> [Int]
generateShuffle [] = []
generateShuffle rs = (++[0]) $ take len $ foldl (\acc x -> modif acc x) [] [0..len-2]
			where
				len = length rs - 1
				modif ac i = ac ++ [(rs!!i `mod` (len-i))]

----------- Differ realisations of shuffle  -------------------

extract:: Int -> [a] -> (a,[a])
extract 0 (h:t) = (h,t)
extract j l = loop j l []
	where
    	loop 0 (h:t) accum = (h,accum ++ t)
    	loop j (h:t) accum = loop (j-1) t (h:accum)

shuffle:: [b] -> [Int] -> [b]
shuffle [e] [] = [e]
shuffle elements (r:r_others) = let (b,rest) = extract r elements
        in b:(shuffle rest r_others)


data Tree a = Leaf a | Node !Int (Tree a) (Tree a) deriving Show

build_tree = grow_level . (map Leaf)
    where
    grow_level [node] = node
    grow_level l = grow_level $ inner l
	     
    inner [] = []
    inner x@[_] = x
    inner (e1:e2:rest) = (join e1 e2) : inner rest
	     
    join l@(Leaf _)       r@(Leaf _)       = Node 2 l r
    join l@(Node ct _ _)  r@(Leaf _)       = Node (ct+1) l r
    join l@(Leaf _)       r@(Node ct _ _)  = Node (ct+1) l r
    join l@(Node ctl _ _) r@(Node ctr _ _) = Node (ctl+ctr) l r

shuffle1 :: [a] -> [Int] -> [a]
shuffle1 elements rseq = shuffle1' (build_tree elements) rseq
    where
    shuffle1' (Leaf e) [] = [e]
    shuffle1' tree (ri:r_others) = extract_tree ri tree 
				    (\tree -> shuffle1' tree r_others)
	-- Extract_tree n дерево
	-- Извлекает n-й элемент из дерева и возвращается
	-- Этот элемент , парас деревом, у которого есть элемент
	-- Удалены ( только вместо спаривания , мы используем CPS )
	-- Функция поддерживает инвариант полноты
	-- Из дерева : все внутренние узлы всегда полны .
	-- Коллекция шаблонов ниже намеренно не завершена.
	-- Все пропавшие кейсы может не произойти ( и если они это сделают, оэто будет ошибкой)
    extract_tree 0 (Node _ (Leaf e) r) k = e:k r
    extract_tree 1 (Node 2 l@Leaf{} (Leaf r)) k = r:k l
    extract_tree n (Node c l@Leaf{} r) k =
	extract_tree (n-1) r (\new_r -> k $ Node (c-1) l new_r)
    extract_tree n (Node n1 l (Leaf e)) k | n+1 == n1 = e:k l
				       
    extract_tree n (Node c l@(Node cl _ _) r) k
	| n < cl = extract_tree n l (\new_l -> k $ Node (c-1) new_l r)
	| otherwise = extract_tree (n-cl) r (\new_r -> k $ Node (c-1) l new_r)

make_rs :: RandomGen g => Int -> g -> ([Int],g)
make_rs n g = loop [] n g
  where
  loop acc 0 g = (reverse acc,g)
  loop acc n g = let (r,g') = randomR (0,n) g 
		 in loop (r:acc) (pred n) g'

test :: Int -> Int
test  n = length $ shuffle1 [1..n+1] (fst $ make_rs n (mkStdGen 17))		