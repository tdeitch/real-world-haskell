import Data.List
import Data.Char

safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead _ = Nothing

safeTail :: [a] -> Maybe [a]
safeTail (x:xs) = Just xs
safeTail _ = Nothing

safeLast :: [a] -> Maybe a
safeLast (x:[]) = Just x
safeLast (x:xs) = safeLast xs
safeLast _ = Nothing

mysquare :: Num a => [a] -> [a]
mysquare xs = map sq xs
    where sq x = x*x

asInt_fold :: String -> Int
asInt_fold ('-':s) = -1 * asInt_fold s
asInt_fold s = foldl step 0 s
    where step acc ch = 10 * acc + digitToInt ch

concat_fold :: [[a]] -> [a]
concat_fold xs = foldr step [] xs
    where step x acc = x ++ acc

takeWhile_r :: (a -> Bool) -> [a] -> [a]
takeWhile_r p (x:xs)
    | p x = x:(takeWhile_r p xs)
    | otherwise = []
takeWhile_r _ _ = []

takeWhile_fold :: (a -> Bool) -> [a] -> [a]
takeWhile_fold p xs = foldr step [] xs
    where step x acc | p x = x : acc
                     | otherwise = []

groupBy_fold :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy_fold cmp xs = foldl step [] xs
    where step acc x | null acc = [[x]]
                     | cmp (head (last (acc))) x = init acc ++ [last acc ++ [x]]
                     | otherwise = acc ++ [[x]]

any_fold :: Foldable t => (a -> Bool) -> t a -> Bool
any_fold p xs = foldl step False xs
    where step acc x | acc = True
                     | p x = True
                     | otherwise = False

