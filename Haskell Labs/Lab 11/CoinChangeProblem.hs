import Data.Array

-- Function to solve the Coin Change Problem
coinChange :: [Int] -> Int -> Int
coinChange coins amount
    | amount < 0 = -1 -- base case
    | otherwise = dp ! amount -- Finding result using DP
    where
        -- creating DP table using Array
        dp = listArray(0,amount) $ map go [0..amount]
        -- Function to compute min coins for each amount
        go 0 = 0 -- Base case: 0 coins needed for 0 amount
        go x 
            | null validCoins = -1 -- to return -1 if no valid coins
            | otherwise = minimum validCoins+1
            where
                validCoins = [dp ! (x-c) | c <- coins, x >= c, dp ! (x-c) /= -1] -- Compute min coins among different combinations

-- Main function to print the result
main :: IO ()
main = do
    putStrLn "Enter coin denominations (seperated by spaces): "
    denominations <- map read . words <$> getLine -- Read denominations as a list of Int
    
    putStrLn "Enter the amount: "
    amount <- readLn -- Read amount as Int

    let result = coinChange denominations amount
    putStrLn $ "Min no of coins needed: " ++ show result
