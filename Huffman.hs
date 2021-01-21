module Huffman where

    -- The Huffman tree
    data Htree = Leaf Char | Branch Htree Htree
    -- A Weighted tree. L = leaf, B = branch , Integer = weight
    data Wtree = L Integer Char | B Integer Wtree Wtree

    -- Takes a text-string and returns a list of tuples.
    -- This tuple is the unique Chars from the String and 
    -- number of occurrences for that Char in the String
    -- ex: "huffman" -> [(1,'h'), (1,'u'), (2,'f'), (1,'m'), (1,'a'), (1,'n')]
    statistics :: String -> [(Integer, Char)]
    statistics str = let 
        noDup = rmDuplicate str
        in buildFrequencyPair noDup str

    -- Removes duplicate elements in an Array
    -- Ex: Removes all duplicate Chars in a String
    rmDuplicate :: Eq a => [a] -> [a]
    rmDuplicate [] = []
    rmDuplicate (x:xs)  
        | x `elem` xs = rmDuplicate xs
        | otherwise   = x : rmDuplicate xs

    -- Counts all identical elements in given Array
    -- fromIntegral parses Int to Integer
    -- Ex: counts all instances of a Char in a given String
    elemInArray :: Eq a => a -> [a] -> Integer
    elemInArray elem arr = fromIntegral (length [i | i <- arr, i == elem])

    -- Maps every Char in the "no duplicates"-arr to 
    -- tuples with the Char and nr of occurrences in the String
    buildFrequencyPair :: Eq b => [b] -> [b] -> [(Integer, b)]
    buildFrequencyPair cs str = map (\char -> (elemInArray char str, char)) cs

    
    -- TODO: Deluppgift 1 -- klar?
    --       Deluppgift 2 -- konstruera Huffman-trädet (Gör ett Wtree och konvertera till Htree)
    --       Deluppgift 3 -- koda text till en bit-sekvens
    --       Deluppgift 4 -- avkoda en bit-sekvens till en text
