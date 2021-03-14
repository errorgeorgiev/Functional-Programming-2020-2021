import Data.Char
import Data.List

--Задача 3. Нека е дефиниран типът type Point = (Double,Double),
--представящ точка с реални координати. Да се дефинира функция
--splitPoints :: Point -> Double -> [Point] -> ([Point],[Point]),
--която приема точка p, радиус r и списък от точки ps и връща като резултат двойка
--от списъци – такава, че първият съдържа тези точки от ps, които са в кръга с
--център p и радиус r, а вторият – всички останали точки от ps.

type Point = (Double,Double)

splitPoints :: Point -> Double -> [Point] -> ([Point],[Point])
splitPoints _ _ [] = ([],[])
splitPoints p r ps = ( [x | x <- ps, fst x <= fst p + r && snd x <= snd p + r], [x | x <- ps, fst x > fst p + r && snd x > snd p + r])





main :: IO()
main = do
    print $ splitPoints (1,1) 5 [(1,2),(2,3),(10,15),(-1,1),(12,14)]
 -- --> ([(1.0,2.0),(2.0,3.0),(-1.0,1.0)],[(10.0,15.0),(12.0,14.0)])