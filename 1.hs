{-# LANGUAGE MultiWayIf #-}
import Data.Char (isUpper, isLower)

{- 
    1. Функция должна находить все вещественные решения уравнения ax^2+bx+c=0,
    где a, b, c могут быть равны нулю. 
    Функция должна принимать на вход параметры и возвращать решения. 
    (Если решений нет, то использовать выражение error “Message” для отображения ошибки).
-}
solveQuadraticEquation :: (Ord a, Floating a) => a -> a -> a -> (a, a)
solveQuadraticEquation a b c =
    if  | d == 0 -> 
            let root = -b / (2 * a) 
            in (root, root)
        | d > 0 ->
            let firstRoot = (-b + sqrt d) / (2 * a)
                secondRoot = (-b - sqrt d) / (2 * a)
            in (firstRoot, secondRoot)
        | otherwise -> error "Equation has no roots"
    where
        d = b * b - 4 * a * c


{-
    2. Реализовать функцию sum’n’count :: Integer -> (Integer, Integer), 
    которая подсчитывает сумму цифр заданного числа и их количество
-}
sum'n'count :: Integer -> (Integer, Integer)
sum'n'count num = 
    if  | num' == 0  -> (0, 1)
        | num' < 10  -> (num', 1)
        | otherwise -> (snd divMod' + fst subResult, 1 + snd subResult)
    where
        num' = abs num 
        subResult = sum'n'count $ fst divMod'
        divMod' = divMod num' 10


{-
    3. Реализовать функцию, которая из заданной строки удаляет все слова, 
    которые содержат хотя бы одну заглавную букву. Функция должна 
    принимать в качестве параметра строку и возвращать строку. Функция 
    должна состоять из композиции встроенных функций. (Для 
    определения, что символ является заглавной/прописной буквой, 
    используйте функции isUpper/isLower из модуля Data.Char, для 
    подключения в начале файла добавьте строку: import Data.Char 
    (isUpper, isLower))
-}
removeWordsWithAtLeastOneUpperLetter :: String -> String
removeWordsWithAtLeastOneUpperLetter "" = ""
removeWordsWithAtLeastOneUpperLetter' text = removeWordsWithAtLeastOneUpperLetterInternal $ words text
    where
        removeWordsWithAtLeastOneUpperLetterInternal :: [String] -> String
        removeWordsWithAtLeastOneUpperLetterInternal [] = ""
        removeWordsWithAtLeastOneUpperLetterInternal (currentWord:leftWords) =
            if containsUpperLetter currentWord == True
            then subResult
            else
                if subResult == ""
                then currentWord
                else currentWord ++ " " ++ subResult
            where
                subResult = removeWordsWithAtLeastOneUpperLetterInternal $ leftWords

                containsUpperLetter :: String -> Bool
                containsUpperLetter "" = False
                containsUpperLetter (firstLetter:leftLetters)
                    | isUpper firstLetter = True
                    | otherwise = containsUpperLetter leftLetters
removeWordsWithAtLeastOneUpperLetter'' text = unwords . filter cond . words $ text
    where
        cond = all isLower


{-
    4. Найти частичную сумму первых n членов ряда: Sum(from n=1 to inf)=1/n^2
    должна принимать в качестве параметра количество слагаемых и 
    возвращать сумму. Функция должна быть реализована с 
    использованием функций zip, map или zipWith без явного 
    использования рекурсии.
-}
partSumOfRow :: Integer -> Float
partSumOfRow 0 = 0
partSumOfRow count = sum $ map rowElementFunc [1..count]
    where 
        rowElementFunc :: Integer -> Float
        rowElementFunc = \n -> 1 / ((fromIntegral n)**2)

{-
    5. Удалить из списка каждый третий элемент. Функция должна 
    принимать в качестве параметра список и возвращать список.
-}
removeEveryThirdElement :: [a] -> [a]
removeEveryThirdElement [] = []
removeEveryThirdElement list = removeEveryThirdElementInternal 0 list
    where
        removeEveryThirdElementInternal :: Integer -> [a] -> [a]
        removeEveryThirdElementInternal _ [] = []
        removeEveryThirdElementInternal index (currentItem:list) =
            if index == 2
            then removeEveryThirdElementInternal (mod (index + 1) 3) list
            else currentItem : (removeEveryThirdElementInternal (mod (index + 1) 3) list)


{-
    6. Дан список из целых чисел, нужно выделить из него все неубывающие 
    подпоследовательности максимальной длины. Функция должна 
    принимать в качестве параметра список и возвращать список списков. 
    Например, для списка [1,2,3,2,1,4,2,3,4] результат должен быть 
    [[1,2,3],[2,3,4]] (порядок важен)
-}
getMaxSubsequences :: Ord a => [a] -> [[a]]
getMaxSubsequences [] = [[]]
getMaxSubsequences list = filter (\a -> length a == maxLength) allSubsequences
    where 
        allSubsequences = getSubsequences list
        maxLength = maximum . map length $ allSubsequences

        getSubsequences :: Ord a => [a] -> [[a]]
        getSubsequences [] = [] 
        getSubsequences (x:xs) = getSubsequencesWithIndex xs [] [x] x

        getSubsequencesWithIndex :: Ord a => [a] -> [[a]] -> [a] -> a -> [[a]]
        getSubsequencesWithIndex [] _ _ _ = []
        getSubsequencesWithIndex (x:xs) subsequences currentRow prev
            | null xs = lastElement
            | prev <= x = getSubsequencesWithIndex xs subsequences (currentRow ++ [x]) x
            | otherwise = getSubsequencesWithIndex xs (subsequences ++ [currentRow]) [x] x
            where 
                lastElement = if prev <= x then subsequences ++ [currentRow ++ [x]] else subsequences ++ [currentRow] ++ [[x]]

        

{-
    7. Дан список из уникальных элементов, нужно найти все перестановки 
    заданного списка. Функция должна принимать в качестве параметра 
    список и возвращать список списков. Например, для списка [1,2,3] 
    результат должен быть [[1,2,3],[1,3,2],[[2,1,3],[2,3,1], [[3,2,1],[3,1,2]]
    (порядок не важен).
-}
permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations xs = [x : ys | x <- xs, ys <- permutations $ delete' x xs]
    where
        delete' :: Eq a => a -> [a] -> [a]
        delete' _ [] = []
        delete' x (y:ys)
            | x == y = ys
            | otherwise = y : delete' x ys