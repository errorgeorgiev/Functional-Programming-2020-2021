import Data.List
import Data.Char

main :: IO()
main = do
    print $ encode "Haskell" -- -> "Haskell"
    print $ encode "aaabccdefff" -- -> "3abccde3f"
    print $ encode "aaaaaaaaaaaabbb" -- -> "12a3b"
    print $ encode "aabbb" -- -> "aa3b"

    print $ decode "12a3b" -- -> "aaaaaaaaaaaabbb"
    print $ decode "a3b" -- -> "abbb"
    print $ decode "aa3b" -- -> "aabbb"

{-- first task --}
encode :: String -> String
encode = concat . map (compress) . Data.List.group
    where
        compress:: String -> String
        compress c = if length c > 2 then (show $ length c) ++ [head c] else c


{-- second task --}
decode :: String -> String
decode xs = if length xs == 0 then error "The string is empty" else decompress xs "" 0
    where   
        decompress :: String -> String -> Int -> String
        decompress [] result _ = result
        decompress (x:xs) result currInt
         | isDigit x == True = decompress xs result (currInt * 10 + digitToInt x)
         | isDigit x == False && currInt /= 0 = decompress xs (result ++ replicate currInt x) 0
         | otherwise = decompress xs (result ++ [x]) 0 
        

            

