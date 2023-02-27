{- 
    1. Придумать свой класс типов, который содержит как минимум две 
    функции, одна из которых выражается через другие. Написать реализацию 
    этого класса типов для любых двух типов данных, типы данных выбирать 
    такие, чтобы их реализации отличались (можно использовать свои 
    собственные типы данных).
-}
class Informable a where
    getInfo :: Show a => a -> String
    getInfo a = "Default type description with value of " ++ (getValue a)

    getValue :: Show a => a -> String
    getValue = show

instance Informable Integer where
    getInfo a = "Integer number with value of " ++ (getValue a) ++ ". Can contain values from -2_147_483_648 to 2_147_483_647"

instance Informable String where
    getInfo a = "String " ++ a ++ " is just a set of chars"

{-

-}
data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a)
    deriving(Show)

initialize :: a -> BinaryTree a
initialize value = Node value Empty Empty

append :: (Ord a) => a -> BinaryTree a -> BinaryTree a
append value Empty = initialize value
append value (Node element left right)
    | value == element = Node value left right
    | value < element = Node element (append value left) right
    | value > element = Node element left (append value right)

toList :: BinaryTree a -> [a]
toList Empty = []
toList (Node element left right) = (toList left) ++ [element] ++ (toList right)

treeLength :: BinaryTree a -> Int
treeLength Empty = 0
treeLength (Node element left right) = 1 + max (treeLength left) (treeLength right)