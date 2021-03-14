import Data.Char
import Data.List

main :: IO()
main = do
    print $ getAverageBalance (accounts1,people1) (\ (_,_,city) -> city =="Burgas") -- -> 24.95 (24.950000000000003)
    print $ getAverageBalance (accounts1,people1) (\ (_,(n:xs),_) -> n =='P') -- -> 18.85
    print $ averageBalanceOfCities (accounts1,people1) ["Sofia","Gabrovo","Stara Zagora"] -- -> 67.85


    print $ countInteresting t1 -- -> 2 (4=2^2, 1=2^0)
    print $ countInteresting t2 -- -> 3 (4=2^2, 2=2^1, 1=2^0)

type Account = (Int, Int, Double)
type Person = (Int, String, String)

people1 :: [Person]
people1 = [(1,"Ivan","Sofia"), (2,"Georgi","Burgas"), (3,"Petar","Plovdiv"), (4,"Petya","Burgas")]

accounts1 :: [Account]
accounts1 = [(1,1,12.5), (2,1,123.2), (3,2,13.0), (4,2,50.2), (5,2,17.2), (6,3,18.3), (7,4,19.4)]


getAverageBalance :: ([Account], [Person]) -> (Person -> Bool) -> Double
getAverageBalance (xs,ys) p = (sum $ [c | (a,b,c) <- accounts1, (d,e,f) <- people1, p(d,e,f), b == d])
                                / (fromIntegral $ length [c | (a,b,c) <- accounts1, (d,e,f) <- people1, p(d,e,f), b == d])

averageBalanceOfCities :: ([Account],[Person]) -> [String] -> Double
averageBalanceOfCities (xs,ys) (z:zs) = if elem z [city | (_,_,city) <- people1] then getAverageBalance (xs,ys) (\ (_,_,city) -> city == z) 
                                        else averageBalanceOfCities (xs,ys) zs


data BTree = Empty | Node Int BTree BTree 

t1 :: BTree 
t1 = Node 16 (Node 0 Empty Empty) (Node 4 (Node 1 Empty Empty) (Node 0 Empty Empty))

t2 :: BTree 
t2 = Node 4 (Node 0 Empty Empty) (Node 2 (Node 1 Empty Empty) Empty) 

getElementByLevel :: BTree -> Int -> [Int]
getElementByLevel Empty _ = []
getElementByLevel (Node value left right) n
 | n == 0 = [value]
 | otherwise = getElementByLevel left (n-1) ++ getElementByLevel right (n-1)


countInteresting :: BTree -> Int
countInteresting Empty = 0
countInteresting t@(Node value left right) 
 | value == 2^(length (getElementByLevel t 1)) = 1 + (countInteresting left) + (countInteresting right)
 | otherwise = (countInteresting left) + (countInteresting right)