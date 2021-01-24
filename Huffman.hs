module Huffman where
    import Data.List ( sortBy )
    import Data.Semigroup ( Semigroup(sconcat) )

    -- The Huffman tree
    data Htree = Leaf Char | Branch Htree Htree
    -- A Weighted tree. L = leaf, B = branch , Integer = weight
    data Wtree = L Integer Char | B Integer Wtree Wtree deriving (Show)


    -- =================== Sub-problem 1 ===================  --


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
    elemInArray elem arr = fromIntegral $ length [e | e <- arr, e == elem]

    -- Maps every Char in the "no duplicates"-arr to 
    -- tuples with the Char and nr of occurrences in the String
    buildFrequencyPair :: Eq b => [b] -> [b] -> [(Integer, b)]
    buildFrequencyPair cs str = map (\char -> (elemInArray char str, char)) cs


    -- =================== Sub-problem 2 ===================  --


    -- 1. ✅ convert the tuple-list to a list of Wtree LEAFS
    --    with the least common Chars first (in increasing order)   
    -- 2. ✅ Process the new list until only 1 Wtree remains, by 
    --    taking the first two elements in list and combining to a new
    --    tree recursively. 
    -- 3. Go through tree and remove the weights (convert to Htree)


    makeTree :: [(Integer, Char)] -> Htree
    makeTree (x:xs) = let
        ordLeafs  = makeOrderedLeafs (x:xs)
        wtreeList = makeWeightedTree ordLeafs 
        in Leaf 'a' -- TODO: just a placeholder for now!

    -- Converts tuple-list to Wtree leafs. 
    -- Implicit use or input parameter. 
    -- "uncurry" will take a tuple-element and apply 
    -- the first and second argument after L. 
    -- Output of map used as input for sortBy through "." 
    makeOrderedLeafs :: [(Integer, Char)] -> [Wtree]
    makeOrderedLeafs = sortBy sortNodes . map (uncurry L)

    -- Get the weight of any Wtree
    getWeight :: Wtree -> Integer
    getWeight (L w _)   = w
    getWeight (B w _ _) = w 

    -- Helper function for sortBy. 
    -- Takes two Wtree and returns the ordering
    -- of those two, aka if the first one is less than,
    -- greater than or equals to the second. 
    sortNodes :: Wtree -> Wtree -> Ordering 
    sortNodes t1 t2 = compare (getWeight t1) (getWeight t2)


    -- Builds an array containing a single Wtree, spanning the
    -- entire Huffman tree based on combined node weights in new branches.  
    -- Needs ordered leafs as the original input. 
    -- Test with: makeWeightedTree $  makeOrderedLeafs (statistics "aaaaaaaaaaeeeeeeeeeeeeeeeiiiiiiiiiiiisssttttpppppppppppppn") 
    makeWeightedTree :: [Wtree] -> [Wtree]
    makeWeightedTree []  = []      -- No Wtree(s), Done      
    makeWeightedTree [t] = [t]     -- Done
    makeWeightedTree t@(t1:t2:xs) 
        | w1 > w2   = makeWeightedTree $ sortBy sortNodes t
        | otherwise = makeWeightedTree $ B (w1+w2) t1 t2 : xs 
        where 
            w1 = getWeight t1
            w2 = getWeight t2



    
    --       Deluppgift 1 -- klar?
    --TODO:  Deluppgift 2 -- konstruera Huffman-trädet (Gör Wtree-löv, använd till att bygga Htree)
    --       Deluppgift 3 -- koda text till en bit-sekvens
    --       Deluppgift 4 -- avkoda en bit-sekvens till en text
