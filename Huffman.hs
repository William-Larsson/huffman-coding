module Huffman (Htree, statistics, maketree, encode, decode) where
    import Data.List ( sortBy )

    -- The Huffman tree
    data Htree = Leaf Char | Branch Htree Htree deriving (Show)
    -- A Weighted tree. L = leaf, B = branch , Integer = weight
    data Wtree = L Integer Char | B Integer Wtree Wtree deriving (Show)


    -- =================== Sub-problem 1 ===================  --


    -- Takes a text-string and returns a list of tuples.
    -- This tuple is the unique Chars from the String and 
    -- number of occurrences for that Char in the String
    -- ex: "huffman" -> [(1,'h'), (1,'u'), (2,'f'), (1,'m'), (1,'a'), (1,'n')]
    statistics :: String -> [(Integer, Char)]
    statistics ""  = error "No string given as input."   
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

    -- Maps every Char in the "no duplicates"-arr to 
    -- tuples with the Char and nr of occurrences in the String
    buildFrequencyPair :: Eq b => [b] -> [b] -> [(Integer, b)]
    buildFrequencyPair cs str = map (\char -> (elemInArray char str, char)) cs

    -- Counts all identical elements in given Array
    -- fromIntegral parses Int to Integer
    -- Ex: counts all instances of a Char in a given String
    elemInArray :: Eq a => a -> [a] -> Integer
    elemInArray elem arr = fromIntegral $ length [e | e <- arr, e == elem]


    -- =================== Sub-problem 2 ===================  --


    -- Test with: maketree (statistics <string>) 
    maketree :: [(Integer, Char)] -> Htree
    maketree []     = error "Input tuple array is empty."
    maketree (x:xs) = let
        ordLeafs  = makeOrderedLeafs (x:xs)
        wtree     = makeWeightedTree ordLeafs 
        in convertToHtree wtree

    -- Converts tuple-list to Wtree leafs. 
    -- Implicit use of input parameter. 
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
    -- Takes two Wtree's and returns the ordering
    -- of those two, aka if the first one is less than,
    -- greater than or equals to the second. 
    sortNodes :: Wtree -> Wtree -> Ordering 
    sortNodes t1 t2 = compare (getWeight t1) (getWeight t2)

    -- Builds an array containing a single Wtree, spanning the
    -- entire Huffman tree based on combined node weights in new branches.  
    -- Needs ordered leafs as the original input. 
    -- Test with: makeWeightedTree $  makeOrderedLeafs (statistics "aaaaaaaaaaeeeeeeeeeeeeeeeiiiiiiiiiiiisssttttpppppppppppppn") 
    makeWeightedTree :: [Wtree] -> Wtree
    makeWeightedTree []  = error "Can't make weighted tree from empty input Wtree array"     
    makeWeightedTree [t] = t
    makeWeightedTree t@(t1:t2:xs) 
        | w1 > w2   = makeWeightedTree $ sortBy sortNodes t
        | otherwise = makeWeightedTree $ B (w1+w2) t1 t2 : xs 
        where 
            w1 = getWeight t1
            w2 = getWeight t2

    -- Traverses recursively down the Wtree structure
    -- until a leaf is found and clones that structure
    -- to a Htree with leafs of the same char value(s). 
    convertToHtree :: Wtree -> Htree
    convertToHtree (L _ c)     = Leaf c
    convertToHtree (B _ lb rb) = Branch (convertToHtree lb) (convertToHtree rb) 


    -- =================== Sub-problem 3 ===================  --


    -- Builds an Htree from a given string and uses 
    -- that tree to encode the string into a Huffman code. 
    encode :: String -> (Htree, [Integer])
    encode str = let 
        htree  = maketree $ statistics str
        bitArr = buildBitEncoding str htree
        in (htree, bitArr) 

    -- Builds Huffman bit code from given String and Huffman tree
    -- Recursively adds the bit code first char in the remaining string
    -- to the beginning of the bit code array to preserve the characters order.
    buildBitEncoding :: String -> Htree -> [Integer]
    buildBitEncoding [] _        = []
    buildBitEncoding (x:xs) tree = getBitCode [] x tree ++ buildBitEncoding xs tree

    -- Traverses the Huffman tree to find leaf matching given 
    -- char. Stores bit code to that leaf in the accumulator.
    -- Input: Bit code accumulator, Char to encode, Huffman tree.
    -- test with: getBitCode [] 'e' (maketree $ statistics "aaaaaaaaaaeeeeeeeeeeeeeeeiiiiiiiiiiiisssttttpppppppppppppn")
    getBitCode :: [Integer] -> Char -> Htree -> [Integer]
    getBitCode acc char (Leaf c)
        | char == c = acc 
        | otherwise = []  

    getBitCode acc char (Branch lt rt)
        | not (null left)  = left 
        | not (null right) = right 
        | otherwise        = [] 
        where
            left  = getBitCode (acc ++ [0]) char lt
            right = getBitCode (acc ++ [1]) char rt



    -- =================== Sub-problem 5 ===================  --

    -- TODO: comment
    decode :: Htree -> [Integer] -> String
    decode tree []   = []
    decode tree bits = char ++ decode tree bits'
        where
            (char, bits') = decodeChar tree bits

    -- TODO: comment
    decodeChar :: Htree -> [Integer] -> (String, [Integer])
    decodeChar (Leaf c) bits = ([c], bits) 
    decodeChar (Branch lt rt) (x:xs) 
        | x == 0    =  decodeChar lt xs 
        | otherwise =  decodeChar rt xs
    --decodeChar _ []          = ("", [])   


    -- TODO: Clean up the code before handing it in!


    -- TODO: 
    -- Current limitation:
    --      one-char string get no bit code (and can therefore not be decoded as well)
    --      cannot print å, ä, ö, instead it prints s\299 for example which represents a swedish char