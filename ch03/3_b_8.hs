-- Using the binary tree type that we defined earlier in this chapter, write a
-- function that will determine the height of the tree. The height is the
-- largest number of hops from the root to an Empty. For example, the tree Empty
-- has height zero; Node "x" Empty Empty has height one; Node "x" Empty (Node
-- "y" Empty Empty) has height two; and so on.

{-- From examples/examples/ch03/Tree.hs --}
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)
{-- End of code from examples --}

treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node _ l r) = 1 + max (treeHeight l) (treeHeight r)

-- ghci> :l 3_b_8.hs
-- [1 of 1] Compiling Main             ( 3_b_8.hs, interpreted )
-- Ok, one module loaded.
-- ghci> treeHeight Empty
-- 0
-- ghci> treeHeight (Node "x" Empty Empty)
-- 1
-- ghci> treeHeight (Node "x" Empty (Node "y" Empty Empty))
-- 2
