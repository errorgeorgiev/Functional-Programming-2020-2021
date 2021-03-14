import Data.Char
import Data.List

--Задача 2. Нека са дадени две едноместни целочислени функции f и g и списък от
--цели числа xs. Ще казваме, че функцията f доминира g върху множеството xs,
--ако за всяко x ∈ xs е вярно, че |f(x)| ≥ |g(x)|.
--Да се дефинира функция dominates :: (Int -> Int) -> (Int -> Int) ->
--[Int] -> Bool, която връща резултата от проверката дали функцията f
--доминира g върху множеството xs.

dominates :: (Int -> Int) -> (Int -> Int) -> [Int] -> Bool
dominates _ _ []   = True
dominates f g (x:xs) = if(abs (f x) >= abs (g x)) then dominates f g xs else False



main :: IO()
main = do

    print $ dominates (+4) (*2) [1..4]  -- --> True
    print $ dominates (+4) (*2) [1..5]  -- --> False 