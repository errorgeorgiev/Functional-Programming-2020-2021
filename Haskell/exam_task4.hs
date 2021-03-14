import Data.Char
import Data.List


--Задача 4. Нека е дефинирано следното представяне на двоично дърво:
--data BTree = Empty | Node Int BTree BTree.
--Да се дефинира функция isBinarySearchTree :: BTree -> Bool, която
--проверява дали подадено двоично дърво от цели числа е двоично дърво за
--търсене. Казваме, че едно двоично дърво е двоично дърво за търсене, ако
--лявото му поддърво съдържа само възли със стойности, по-малки от тази в
--корена, а дясното му поддърво - само стойности, по-големи или равни на тази в
--корена. Освен това, трябва и самите поддървета също да са двоични дървета за
--търсене.

data BTree = Empty | Node Int BTree BTree

isBinarySearchTree :: BTree -> Bool
isBinarySearchTree Empty = True
isBinarySearchTree (Node x Empty Empty) = True
isBinarySearchTree (Node x lt rt) = not(areBiggerThanRoot x lt) && (areBiggerThanRoot x rt) && isBinarySearchTree lt && isBinarySearchTree rt
    
areBiggerThanRoot _ Empty = True
areBiggerThanRoot x (Node r lt rt) = x <= r && areBiggerThanRoot x lt && areBiggerThanRoot x rt

t1 :: BTree
t1 = Node 8 (Node 3 (Node 1 Empty Empty)(Node 4 Empty Empty)) (Node 10 (Node 9 Empty Empty)(Node 14 Empty Empty)) 
t2 :: BTree 
t2 = Node 8 (Node 3 (Node 1 Empty Empty) (Node 4 Empty Empty)) (Node 10 (Node 5 Empty Empty) (Node 14 Empty Empty)) 
t3 :: BTree 
t3 = Node 8 (Node 3 (Node 5 Empty Empty) (Node 6 Empty Empty)) (Node 10 (Node 9 Empty Empty) (Node 14 Empty Empty)) 


main :: IO()
main = do

    
    print $ isBinarySearchTree t1 -- --> True
    print $ isBinarySearchTree t2 -- --> False (в дясното поддърво има стойности, помалки от тази в корена)
    print $ isBinarySearchTree t3 -- --> False (лявото поддърво не е двоично дърво за търсене)