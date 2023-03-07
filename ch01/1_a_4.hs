-- Modify the WC.hs example again, to print the number of characters in a file.

-- Type of 'input' is String. Its length is the number of characters.
main = interact wordCount
    where wordCount input = show (length input) ++ "\n"
