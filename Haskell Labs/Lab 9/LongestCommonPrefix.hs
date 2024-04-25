import Control.Monad (replicateM)

-- Function to find the longest common prefix among strings.
longestCommonPrefix :: [String] -> String
longestCommonPrefix [] = ""  -- Base case: empty list, return empty string.
longestCommonPrefix (x:xs) =  -- Pattern match the first string and the rest.
    case foldr commonPrefix x xs of
        "" -> ""  -- If foldr returns an empty string, there's no common prefix.
        prefix -> prefix  -- Otherwise, return the common prefix found.

    where
        -- Helper function to find common prefix between two strings.
        commonPrefix :: String -> String -> String
        commonPrefix [] _ = "" 
        commonPrefix _ [] = "" 
        commonPrefix (a:as) (b:bs)
            | a == b = a : commonPrefix as bs  -- If characters match, add to prefix.
            | otherwise = ""  -- If characters don't match, common prefix ends.

-- Main function
main :: IO ()
main = do
    putStrLn "Enter the number of strings:"
    n <- getLine  -- Read number of strings from user input.
    let numStrings = read n :: Int  -- Convert the input to an integer.

    -- Prompt user to enter each string 'numStrings' times and collect them into a list.
    strings <- replicateM numStrings $ do
        putStrLn "Enter a string:"
        getLine  -- Read string from user input.

    -- Print the longest common prefix among the entered strings.
    putStrLn $ "Longest common prefix: " ++ longestCommonPrefix strings
