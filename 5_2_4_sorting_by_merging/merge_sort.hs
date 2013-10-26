{-
 > mergesort lst =
 >   let merge xx yy = case (xx, yy) of
 >         ([], [])         -> []
 >         (xxs, [])        -> xxs
 >         ([], yys)        -> yys
 >         (x : xs, y : ys) -> if x < y then x : merge xs yy else y : merge xx ys
 >       split lst = case lst of
 >         []           -> ([], [])
 >         [_]          -> (lst, [])
 >         x : y : rest -> let (xs, ys) = split rest in (x : xs, y : ys)
 >   in case lst of
 >     []  -> lst
 >     [_] -> lst
 >     _   -> let (a, b) = split lst
 >            in merge (mergesort a) (mergesort b)
-}

import System.Random (getStdGen, randomRs)
import Data.Ord (comparing)
import Data.List (sortBy) -- sortBy はシステム標準の安定なソート
 
-- マージソート
mergesort :: (Ord a) => [a] -> [a]
mergesort = mergesortBy compare
 
-- 比較器付きマージソート
mergesortBy :: (a -> a -> Ordering) -> [a] -> [a]
mergesortBy _ [] = [] -- 空系列をソートすれば空系列である
mergesortBy cmp xxs = mergesortN (length xxs) xxs
    -- cmp : 比較器
    -- xxs : ソートする系列
    where
        -- ソート済みの系列同士をマージ(併合)する
        merge xs [] = xs
        merge [] xs = xs
        merge lhs@(x:xs) rhs@(y:ys) -- lhs と rhs をマージする
            | cmp y x == LT = y:merge lhs ys -- 要注意(安定性の要)
            | otherwise     = x:merge xs rhs
 
        -- マージソート本体
        mergesortN 1 xs = xs
        mergesortN 2 [x, y] = if cmp y x == LT then [y, x] else [x, y] -- 高速化のためのコード
        mergesortN n xs = merge sorted_lhs sorted_rhs
            -- n  : ソートする系列の長さ
            -- xs : ソートする系列
            where
                m = n `div` 2
                (lhs, rhs) = splitAt m xs           -- 系列を左右に分割する
                sorted_lhs = mergesortN m lhs       -- 左の系列を再帰的にソート
                sorted_rhs = mergesortN (n - m) rhs -- 右の系列を再帰的にソート
 
-- 複数のフィールドのあるデータ用の比較器。a がキーの型
cmpfst :: (Ord a) => (a, b) -> (a, b) -> Ordering
cmpfst = comparing fst
 
-- 安定性の判定。OK が安定、NG が不安定
check :: Bool -> String
check True  = "OK: stable"
check False = "NG: unstable"
 
-- このマージソートの安定性をチェックする
main = do
    g <- getStdGen
    let xs = zip (take 100000 $ randomRs (0,99) g :: [Int]) [0..] -- 乱数列生成
    putStrLn $ check $ mergesortBy cmpfst xs == sortBy cmpfst xs
