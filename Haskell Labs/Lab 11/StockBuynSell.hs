import Data.List (foldl')

-- Function to calculate the maximum profit in at most k transactions
maxProfit :: Int -> [Int] -> Int
maxProfit k prices 
    -- If k is 0, no transaction => no profit (Base Case)
    | k == 0 = 0
    -- Calculating max profit using DP
    | otherwise = snd $ last $ foldl' maxProfit_DP [(maxBound,0)] prices
    where
        -- Function to update mim cost and max profit tuples
        maxProfit_DP :: [(Int,Int)] -> Int -> [(Int,Int)]
        maxProfit_DP dp price = 
            -- Update each tuple in dp with new price
            [(min (fst dp_i) (price - snd dp_i), max (snd dp_i) (price - fst dp_i)) | dp_i <- dp]

-- Main function to print result
main :: IO()
main = do
    putStrLn "Enter prices of stock each day (seperated by spaces): "
    prices <- map read . words <$> getLine -- Read prices as a list of Int

    putStrLn "Enter the transactions limit: "
    k <- readLn -- Read transaction limit as Int

    putStrLn $ "Maximum Profit acheived: " ++ show(maxProfit k prices)