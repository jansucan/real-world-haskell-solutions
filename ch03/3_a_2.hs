-- Define a tree type that has only one constructor, like our Java
-- example. Instead of the Empty constructor, use the Maybe type to refer to a
-- node's children.

-- The assignment doesn't explicitly specify whether it should be possible to
-- create an empty tree at the root level, so this tree type doesn't support
-- that.

data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a))
              deriving (Show)


--          parent
--            /\
--           /  \
-- left child   right child
--                  /
--                 /
--            left child

simpleTree = Node "parent" (Just (Node "left child" Nothing
                                                    Nothing))
                           (Just (Node "right child" (Just (Node "left child" Nothing
                                                                              Nothing))
                                                     Nothing))
