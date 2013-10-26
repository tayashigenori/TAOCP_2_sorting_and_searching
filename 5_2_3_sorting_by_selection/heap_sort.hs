import Data.List (sort)
import System.Random (getStdGen, randoms)
 
data Hand = L | R     -- ２分木の枝の型(L:左、R:右)
type Path = [Hand]    -- ノード番号最大のノードへのパスの型
type MaxNodeNum = Int -- ノード番号の型(根(root)の番号が1)
data Tree a = Empty | Node !a !(Tree a) !(Tree a) -- ２分木の型
data Heap a = Heap !MaxNodeNum !(Tree a)          -- ヒープの型(完全２分木)
              -- Heap 0 Empty はノードが一つもない空の完全２分木
 
-- ノード番号 nn へのパス
{-# INLINE maxPath #-}
maxPath :: MaxNodeNum -> Path
maxPath nn = maxPath0 nn []
    where
        maxPath0 :: Int -> Path -> Path
        maxPath0 1 xs = xs
        maxPath0 n xs
            | even n = maxPath0 (n `div` 2) (L:xs) -- ノード番号が偶数なら左
            | odd  n = maxPath0 (n `div` 2) (R:xs) -- ノード番号が奇数なら右
 
-- ヒープへの要素 xx の挿入
{-# INLINE insert #-}
insert :: (Ord a) => Heap a -> a -> Heap a
insert (Heap n t) xx = Heap (succ n) (insert0 path t xx)
    where
        path = maxPath (succ n) -- 最大ノード番号を1増やす
 
        -- 再帰の初段のみ特別な処理
        insert0 :: (Ord a) => Path -> Tree a -> a -> Tree a
        insert0 (L:ps) (Node e l r) x = Node z ll r
            where (ll, z) = insert1 ps l x e
        insert0 (R:ps) (Node e l r) x = Node z l rr
            where (rr, z) = insert1 ps r x e
        insert0 _ _ x = Node x Empty Empty
 
        -- ノード番号最大の位置へ要素を挿入後、upheap を行う
        insert1 :: (Ord a) => Path -> Tree a -> a -> a -> (Tree a, a)
        insert1 (L:ps) (Node e l r) x y = (Node c ll r, p)
            where
                (ll, z) = insert1 ps l x e
                (p, c) = if y < z then (z, y) else (y, z)
        insert1 (R:ps) (Node e l r) x y = (Node c l rr, p)
            where
                (rr, z) = insert1 ps r x e
                (p, c) = if y < z then (z, y) else (y, z)
        insert1 _ _ x y = (Node c Empty Empty, p)
            where (p, c) = if y < x then (x, y) else (y, x)
 
-- ヒープからの最大値の削除
{-# INLINE deleteMax #-}
deleteMax :: (Ord a) => Heap a -> Heap a
deleteMax = downHeap . leafToRoot
    where
        -- 準ヒープからのヒープの再構成
        downHeap :: (Ord a) => Heap a -> Heap a
        downHeap h@(Heap 0 Empty) = h
        downHeap (Heap n tt) = Heap n (downHeap0 tt)
            where
                downHeap0 :: (Ord a) => Tree a -> Tree a
                downHeap0 t@(Node x l@(Node y l1 r1) r@(Node z l2 r2))
                    | y < z     = if x < z
                        then Node z l (downHeap0 (Node x l2 r2))
                        else t
                    | otherwise = if x < y
                        then Node y (downHeap0 (Node x l1 r1)) r
                        else t
                downHeap0 t@(Node x (Node y Empty Empty) Empty)
                    | x < y     = Node y (Node x Empty Empty) Empty
                    | otherwise = t
                downHeap0 t = t
 
        -- ノード番号最大の葉をヒープから切り離し、その要素を根(root)に代入
        leafToRoot :: Heap a -> Heap a
        leafToRoot (Heap 1 _) = Heap 0 Empty
        leafToRoot (Heap n t) = Heap (pred n) (Node x l r)
            where
                path = maxPath n
                (Node _ l r, x) = leafcut path t
 
                leafcut :: Path -> Tree a -> (Tree a, a)
                leafcut (L:ps) (Node e l r) = (Node e ll r, x)
                    where (ll, x) = leafcut ps l
                leafcut (R:ps) (Node e l r) = (Node e l rr, x)
                    where (rr, x) = leafcut ps r
                leafcut _ (Node e _ _) = (Empty, e)
 
-- リストからヒープを構成する
{-# INLINE makeHeap #-}
makeHeap :: (Ord a) => [a] -> Heap a
makeHeap = foldl upHeap (Heap 0 Empty)
    where
        -- upheap は挿入と等価
        upHeap :: (Ord a) => Heap a -> a -> Heap a
        upHeap = insert
 
-- ヒープの最大値を返す
{-# INLINE findMax #-}
findMax :: Heap a -> a
findMax (Heap _ (Node x _ _)) = x
 
-- ヒープソート(リスト xxs をソートして返す)
{-# INLINE heapsort #-}
heapsort :: (Ord a) => [a] -> [a]
heapsort xxs = heapsort0 (makeHeap xxs) []
    where
        heapsort0 :: (Ord a) => Heap a -> [a] -> [a]
        heapsort0 (Heap 0 Empty) xs = xs
        heapsort0 h xs = heapsort0 (deleteMax h) (findMax h:xs)
 
-- 正しくソート出来たかをチェックする
check :: Bool -> String
check x = if x then "OK" else "NG"
 
-- メインルーチン(ここでは、ヒープソートが正しいかチェックしている)
main = do
    g <- getStdGen
    let rs3 = take 12345 $ randoms g :: [Int] -- 乱数列の生成
        rs2 = take 1234 rs3
        rs1 = take 123 rs3
    putStr $ (++ " ") $ show $ length rs1
    print $ check $ heapsort rs1 == sort rs1
    putStr $ (++ " ") $ show $ length rs2
    print $ check $ heapsort rs2 == sort rs2
    putStr $ (++ " ") $ show $ length rs3
    print $ check $ heapsort rs3 == sort rs3