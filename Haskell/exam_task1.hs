import Data.Char
import Data.List

--Задача 1. Чрез използването на n кубове е построена сграда. Кубът, намиращ се
--най-отдолу, т.е. основата, е с обем n3. Кубът, който е върху него, е с обем (n-1)3.
--Кубът, поставен най-отгоре, има обем 13.
--Обемът на цялата сграда е: n3 + (n-1)3 + ... + 13 = m.
--Да се дефинира функция findNb :: Integer -> Integer, която по дадено m
--да връща броя кубове n, необходими за построяване на сградата. Ако n не
--съществува, да се връща -1.

findNb :: Integer -> Integer
findNb m = helper m 0 1

helper :: Integer -> Integer -> Integer -> Integer
helper m result i 
    | result > m = -1
    | m == result = i - 1
    | otherwise = helper m (result + (i * i * i)) (i + 1)

main :: IO()
main = do

    print $ findNb 1071225 -- --> 45
    print $ findNb 40539911473216 -- --> 3568
    print $ findNb 135440716410000 -- --> 4824
    print $ findNb 4183059834009 -- --> 2022
    print $ findNb 91716553919377 -- --> -1
    print $ findNb 24723578342962 -- --> -1