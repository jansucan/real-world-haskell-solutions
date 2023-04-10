-- 1. Our current pretty printer is spartan so that it will fit within our space
--    constraints, but there are a number of useful improvements we can make.
--
--    Write a function, fill, with the following type signature:
--
--      -- file: ch05/Prettify.hs
--      fill :: Int -> Doc -> Doc
--
--    It should add spaces to a document until it is the given number of columns
--    wide. If it is already wider than this value, it should add no spaces.
--
-- 2. Our pretty printer does not take nesting into account. Whenever we open
--    parentheses, braces, or brackets, any lines that follow should be indented
--    so that they are aligned with the opening character until a matching
--    closing character is encountered.
--
--    Add support for nesting, with a controllable amount of indentation.
--
--      -- file: ch05/Prettify.hs
--      fill :: Int -> Doc -> Doc

-- The name of the function 'fill' in the exercise 2. is wrong. Example file
-- Prettify.hs contains a placeholder function for solution of that exercise
-- with the correct name, which is 'nest'.

-- These exercises are very unclear. To me, this is caused by the intermediate
-- Doc type. It introduces a non-trivial concept into the topic of
-- pretty-printing.
--
-- Reading the first pages of
-- http://belle.sourceforge.net/doc/hughes95design.pdf I found in a comment for
-- the second exercise in the on-line version of the book helped me to get a
-- little bit better idea about motivation for the Doc type.
--
-- Page 126 of the book says: "How can we do this if our Doc type doesn't
-- contain any information about rendering?" The interface of the two functions
-- in this exercise uses only the Doc type, but the assignment talks about
-- width. Width is a property of a stage of printing after the Doc type, the one
-- that converts Doc into String. That stage has a lot of freedom in how to
-- format the output and this formatting information is not available to the Doc
-- stage, so it doesn't make sense to talk about the overall width in the
-- context of Doc type.  This means that it is not possible to follow the
-- assignments exactly as they are.

-- To make the implementation more simple, I put some restrictions on the input
-- and output. It is already more complicated than what could be expected from a
-- beginner reader.

-- For being able to implement the solution in a separate file, value
-- constructors of the Doc type have been exported too so they can be use in
-- pattern matching.


-- For the first exercise, I decided to affect the formatting on a more general
-- level. Instead of trying to achieve a specific width of the whole document,
-- which can be changed in an unforeseen way when converting to a string, I
-- decided to fill the document in a way so that the first character of every
-- line starts at a specified column in every representation converted into
-- String. It means, to pad the document from the left by spaces. If the padding
-- is already wider, it's not shortened.
--
-- For simplicity, the implementation works according to the
-- modified/complemented assignment only when all Lines are used in the
-- pretty-printed output (i.e., for small requested width). To fix this, the
-- padding would have to be added the Line elements, converting them to '(Concat
-- Line (Text "<padding>"))', so it's used only when a line break is used.

import Prettify
import PrettyJSON
import SimpleJSON

-- A Doc document is a tree structure. For talking about a visual representation
-- of the document it represents, we need to think about it in a serialized
-- way. This serialization is what the pretty printing functions do; they
-- extract data from the tree in a specific traversal of it.
--
-- To split this serialization from the other modifications of the document, it
-- is serialized into a list of document marks. These marks are then used for
-- reconstructing a Doc document.

data DocMark = MEmpty
             | MChar Char
             | MText String
             | MLine
             | MConcat
             | MUnion
             deriving (Show,Eq)

serialize :: Doc -> [DocMark]
serialize Empty = [MEmpty]
serialize (Char c) = [MChar c]
serialize (Text s) = [MText s]
serialize Line = [MLine]
serialize (Concat a b) = [MConcat] ++ (serialize a) ++ (serialize b)
serialize (Union a b) = [MUnion] ++ (serialize a) ++ (serialize b)

deserialize :: [DocMark] -> Doc
deserialize ms = snd (deserialize' ms)
  where deserialize' :: [DocMark] -> ([DocMark], Doc)
        deserialize' (MEmpty:ms) = (ms, Empty)
        deserialize' ((MChar c):ms) = (ms, (Char c))
        deserialize' ((MText s):ms) = (ms, (Text s))
        deserialize' (MLine:ms) = (ms, Line)
        -- The Concat and Union nodes have two subtrees. When reconstructing the
        -- subtrees from the serialized form, it needs to be done sequentially
        -- because the start of the right subtree marks depends on the left
        -- subtree. When the left subtree is reconstructed first and its marks
        -- removed from the list, the beginning of the list is where the right
        -- subtree starts.
        deserialize' (m:ms) = ms2 `seq` (ms3, doc)
          where (ms2, a) = deserialize' ms
                (ms3, b) = deserialize' ms2
                doc = if m == MConcat
                      then (Concat a b)
                      else (Union a b)

paddingLength :: String -> Int
paddingLength s = length (takeWhile isOnlySpace s)
  where isOnlySpace c = (c == ' ')

continueMeasuring :: String -> Bool
continueMeasuring s = (paddingLength s) == (length s)

makePadding :: Int -> String
makePadding width = take width (repeat '.') -- To make the added padding more
                                            -- visible in the example output,
                                            -- dots are used instead of spaces.

getStringFromMark :: DocMark -> String
getStringFromMark (MChar c) = [c]
getStringFromMark (MText s) = s
getStringFromMark _ = error "This mark does not contain any characters"

fill :: Int -> Doc -> Doc
fill width doc = deserialize (fill' width (serialize doc) True 0)

fill' :: Int -> [DocMark] -> Bool -> Int -> [DocMark]
fill' width ms@((MChar c):_) measure existingPadding =  fill'pad width ms measure existingPadding
fill' width ms@((MText s):_) measure existingPadding =  fill'pad width ms measure existingPadding
-- The Line resets measuring of an existing padding
fill' width (MLine:ms)  measure existingPadding = MLine:(fill' width ms True 0)
--No action needed for MEmpty, MConcat, and MUnion
fill' width (m:ms) measure existingPadding = m:(fill' width ms measure existingPadding)
fill' _ [] _ _ = []

fill'pad :: Int -> [DocMark] -> Bool -> Int -> [DocMark]
fill'pad width (m:ms) measure existingPadding = paddingMarks ++ (m:(fill' width ms continueNext padding))
    where str = getStringFromMark m
          continue = continueMeasuring str -- If the string is not empty, no need to continue
                                           -- measuring the leading spaces on the line
          padding = existingPadding + paddingLength str -- Existing padding on the line
          paddingMarks = if measure && (not continue) && (padding < width)
                         then MConcat:[MText (makePadding (width - padding))]
                         else []
          continueNext = measure && continue -- Measure only the leftmost padding. No need to
                                             -- measure the other sequences of space on the line

-- Test input for the first exercise
--
-- The output of 'renderJValue (JArray [(JNumber 1), (JBool True), (JString "hello")])' was manually
-- modified to contain paddings of different lengths. The pretty-printed document look like
--
-- [1.0,
--        true,
--"hello"
--    ]
testFill = Concat (Concat (Concat (Char ' ') (Char '['))
                  (Concat (Concat (Concat (Text "1.0") (Char ','))
                  (Union (Char ' ') Line))
                  (Concat (Concat (Concat (Concat (Text "        ") (Text "true")) (Char ','))
                  (Union (Char ' ') Line))
                  (Concat (Concat (Concat (Char '"') (Concat (Char 'h') (Concat (Char 'e')
                  (Concat (Char 'l') (Concat (Char 'l') (Char 'o')))))) (Char '"'))
                  (Union (Char ' ') Line)))))
                  (Concat (Text "    ") (Char ']'))

-- ghci> :l 5_a_1.hs
-- [1 of 4] Compiling Prettify         ( Prettify.hs, interpreted )
-- [2 of 4] Compiling SimpleJSON       ( SimpleJSON.hs, interpreted )
-- [3 of 4] Compiling PrettyJSON       ( PrettyJSON.hs, interpreted )
-- [4 of 4] Compiling Main             ( 5_a_1.hs, interpreted )
-- Ok, four modules loaded.
--
-- ghci> putStrLn ( pretty 1 ( fill 0 testFill ))
--  [1.0,
--         true,
-- "hello"
--     ]
-- ghci> putStrLn ( pretty 1 ( fill 1 testFill ))
--  [1.0,
--         true,
-- ."hello"
--     ]
-- ghci> putStrLn ( pretty 1 ( fill 2 testFill ))
--  .[1.0,
--         true,
-- .."hello"
--     ]
-- ghci> putStrLn ( pretty 1 ( fill 5 testFill ))
--  ....[1.0,
--         true,
-- ....."hello"
--     .]
-- ghci> putStrLn ( pretty 1 ( fill 9 testFill ))
--  ........[1.0,
--         .true,
-- ........."hello"
--     .....]


-- For the second exercise, it is based on the solution of the first one. The
-- definition of "align" from the Oxford Learner's Dictionaries is:
--
--   "align (something) (with something) to arrange something in the correct
--    position, or to be in the correct position, in relation to something else,
--    especially in a straight line"
--
-- This implementation aligns content of JSON objects and arrays in relation to
-- opening braces and brackets, not in a straight line. Aligning in a straight
-- line would require information from the later stage and this is too much out
-- of the Doc context.
--
-- For simplicity, let's assume that '[', ']', '{', and '}' are used only to
-- mark an array or an object and they are not included in other strings.

-- The implementation is modified copy-pasted first exercise. To keep things
-- simpler for the purpose of explaining, I didn't try to deduplicate code of
-- the two implementations.

nest :: Int -> Doc -> Doc
nest width doc = deserialize (nest' 0 width (serialize doc) True 0)

nest' :: Int -> Int -> [DocMark] -> Bool -> Int -> [DocMark]
nest' width unit ms@((MChar c):_) measure existingPadding =  nest'pad width unit ms measure existingPadding
nest' width unit ms@((MText s):_) measure existingPadding =  nest'pad width unit ms measure existingPadding
nest' width unit (MLine:ms)  measure existingPadding = MLine:(nest' width unit ms True 0)
nest' width unit (m:ms) measure existingPadding = m:(nest' width unit ms measure existingPadding)
nest' _ _ [] _ _ = []

nest'width :: Int -> Int -> String -> (Int, Int)
nest'width width unit str = if (str == "[") || (str == "{")
                 then (width, (width + unit)) -- Keep the expected padding width for the current
                                              -- line, increase for the subsequent lines
                 else if (str == "]") || (str == "}")
                      then (width - unit, width - unit) -- Indent the ending bracket or brace width
                                                        -- the same width as the opening one
                      else (width, width) -- No change in indentation

nest'pad :: Int -> Int -> [DocMark] -> Bool -> Int -> [DocMark]
nest'pad width unit (m:ms) measure existingPadding =
  paddingMarks ++ (m:(nest' nextWidth unit ms continueNext padding))
  where
      str = getStringFromMark m
      (currentWidth, nextWidth) = nest'width width unit str
      continue = continueMeasuring str
      padding = existingPadding + paddingLength str
      paddingMarks = if measure && (not continue) && (padding < currentWidth)
                     then MConcat:[MText (makePadding (currentWidth - padding))]
                     else []
      continueNext = measure && continue


testNext = renderJValue (JObject [("a", JNumber 1),
                                  ("bc", JBool True),
                                  ("defg", JArray [JNumber 1,
                                                   (JObject [("h", JBool False),
                                                             ("i", JNumber 3)
                                                            ]),
                                                   JString "hello"
                                                  ]),
                                   ("z", JNumber 6)
                                 ])

-- ghci> putStrLn ( pretty 1 ( nest 0 testNext ))
-- {"a": 1.0,
-- "bc": true,
-- "defg": [1.0,
-- {"h": false,
-- "i": 3.0
-- },
-- "hello"
-- ],
-- "z": 6.0
-- }
-- ghci> putStrLn ( pretty 1 ( nest 1 testNext ))
-- {"a": 1.0,
-- ."bc": true,
-- ."defg": [1.0,
-- ..{"h": false,
-- ..."i": 3.0
-- ..},
-- .."hello"
-- .],
-- ."z": 6.0
-- }
-- ghci> putStrLn ( pretty 1 ( nest 4 testNext ))
-- {"a": 1.0,
-- ...."bc": true,
-- ...."defg": [1.0,
-- ........{"h": false,
-- ............"i": 3.0
-- ........},
-- ........"hello"
-- ....],
-- ...."z": 6.0
-- }
-- ghci> putStrLn ( pretty 1 ( nest 7 testNext ))
-- {"a": 1.0,
-- ......."bc": true,
-- ......."defg": [1.0,
-- ..............{"h": false,
-- ....................."i": 3.0
-- ..............},
-- .............."hello"
-- .......],
-- ......."z": 6.0
-- }
