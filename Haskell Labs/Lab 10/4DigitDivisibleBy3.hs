import Data.List (permutations, subsequences)

-- Function to generate all permutations of a list
perms :: [a] -> [[a]]
perms = permutations

-- Function to generate all combinations of n elements from a list
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]  -- Base case: combination of 0 elements is an empty list
combinations n xs = [y:ys | y:xs' <- tails xs, ys <- combinations (n-1) xs']

-- Function to get tails of a list
tails :: [a] -> [[a]]
tails [] = [[]]  -- Base case: tail of an empty list is an empty list
tails xs@(_:xs') = xs : tails xs'

-- Function to check if the sum of a list of digits is divisible by 3
sumDivisibleBy3 :: [Int] -> Bool
sumDivisibleBy3 xs = sum xs `mod` 3 == 0  -- Check if the sum is divisible by 3

-- Main function to count valid permutations
countValidPermutations :: Int
countValidPermutations = length [p | s <- validCombinations, p <- perms s]
  where
    validDigits = [1, 3, 4, 6, 7]  -- List of valid digits
    validCombinations = filter sumDivisibleBy3 $ combinations 4 validDigits  -- Filter combinations with sum divisible by 3

-- Main function to print the result
main :: IO ()
main = putStrLn $ "Number of 4-digit positive integers divisible by 3: " ++ show countValidPermutations
