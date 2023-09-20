-- What you should pass to 'traverse' to traverse a directory tree in reverse
-- alphabetic order?

-- ghci> :l ControlledVisit.hs
-- [1 of 1] Compiling ControlledVisit  ( ControlledVisit.hs, interpreted )
-- Ok, one module loaded.

-- Import Data.List module for getting the 'sort' function
-- ghci> :m +Data.List

-- Output of the following command is manually shortened and formatted for clarity
-- traverse' (reverse.sort) "test-9_b_1"
-- ghci> traverse' (reverse.sort) "test-9_b_1"
-- [Info {infoPath = "test-9_b_1/dirD/H", ...},
--  Info {infoPath = "test-9_b_1/dirD/G", ...},
--  Info {infoPath = "test-9_b_1/dirD", ...},
--  Info {infoPath = "test-9_b_1/dirC/F", ...},
--  Info {infoPath = "test-9_b_1/dirC/E", ...},
--  Info {infoPath = "test-9_b_1/dirC", ...},
--  Info {infoPath = "test-9_b_1/B", ...},
--  Info {infoPath = "test-9_b_1/A", ...},
--  Info {infoPath = "test-9_b_1", ...}]
