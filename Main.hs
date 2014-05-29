import RandomPermutation
import System.Random

main :: IO ()
main = do  
        contents <- readFile "/home/ildar/Projects/HaskelDev/random_permutation/input.txt"
        let numList = map readInt . words $ contents
        print "Input:"
        print numList
        print "imperative:"
        print $ imperativePermutation numList 
        seed <- newStdGen
        let randomNumbers =  randomlist (length numList) seed
        print "Random Numbers:"
        print randomNumbers
        print "generate sequence of shuffle places "
        --[2,2,3,4,3,2,1,1,1,1,1,1,1,0]
        let shufflePlaces =  generateShuffle $ randomNumbers
        print shufflePlaces
        print "shuffle"
        print $ shuffle numList shufflePlaces
        print "shuffle1"
        print $ shuffle1 numList shufflePlaces


-- alternately, main = print . map readInt . words =<< readFile "test.txt"

readInt :: String -> Int
readInt = read