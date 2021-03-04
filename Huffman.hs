module Huffman (Htree (..), statistics, maketree, encode, decode) where
    import Data.List ( sort, nub )

    -- Alias for Integers used as weights in Huffman tree
    type Weight  = Integer
    -- Alias for bit codes used in en-/decoding
    type BitCode = [Integer]

    -- Data type: Htree
    -- Recursive data type representing a Huffman tree
    data Htree = Leaf Char | Branch Htree Htree 
        deriving (Show, Read)

    -- Implementation of type class Eq for data type Htree 
    instance Eq Htree where
        (Branch lt rt) == (Branch lt' rt') = lt == lt' && rt == rt'
        (Leaf c) == (Leaf c')              = c == c'
        _ == _                             = False
    
    -- Data type: Wtree
    -- A Weighted tree. L = leaf, B = branch , Weight = the weight
    data Wtree = L Weight Char | B Weight Wtree Wtree 
        deriving (Show, Read)

    -- Implementation of type class Eq for data type Wtree
    instance Eq Wtree where
        t1@(B _ lt rt) == t2@(B _ lt' rt') = 
            getWeight t1 == getWeight t2 
            && lt == lt' 
            && rt == rt'

        l1@(L _ c) == l2@(L _ c') = 
            getWeight l1 == getWeight l2 
            && c == c' 

        _ == _ = False

    -- Implementation of type class Ord data type for Wtree
    instance Ord Wtree where
        t1 `compare` t2 = compare (getWeight t1) (getWeight t2)

    -- Function: getWeight
    --
    -- Returns the weight of any Wtree.
    getWeight :: Wtree -> Weight
    getWeight (L w _)   = w
    getWeight (B w _ _) = w 

    -- Function: statistics
    --
    -- Returns tuple which is the unique Chars from the given String 
    -- and the number of occurrences for that Char in the String
    -- Uses buildFrequencyPairs as helper function.
    statistics :: String -> [(Weight, Char)]
    statistics ""  = []   
    statistics str = let 
        noDup = nub str
        in buildFrequencyPair noDup str

    -- Function: buildFrequencyPair
    --
    -- Maps every element in the the first array to tuples 
    -- with the element and its nr of occurrences in the second array
    buildFrequencyPair :: Eq b => [b] -> [b] -> [(Weight, b)]
    buildFrequencyPair cs str = map (\char -> (countElem char str, char)) cs

    -- Function: countElem
    --
    -- Counts all identical elements in given array
    countElem :: Eq a => a -> [a] -> Weight
    countElem elem arr = fromIntegral $ length [e | e <- arr, e == elem]

    -- Function: maketree
    --
    -- Makes a Huffman tree by creating ordered leafs from 
    -- given [(Integer, Char)]-array and uses those to build 
    -- a Wtree. Finally, converts weighted tree to Htree.
    -- Uses makeOrderedLeafs, makeWeightedTree and convertToHtree as 
    -- helper functions. 
    maketree :: [(Weight, Char)] -> Htree
    maketree []       = error "Input tuple array is empty."
    maketree [(_, c)] = Branch (Leaf c) (Leaf c) -- if only one "leaf"
    maketree (x:xs)   = let
        ordLeafs  = makeOrderedLeafs (x:xs)
        wtree     = makeWeightedTree ordLeafs 
        in convertToHtree wtree

    -- Function: makeOrderedLeafs
    --
    -- Maps tuple-list to Wtree leafs and returns the 
    -- leafs in ascending orderings based on weight.  
    makeOrderedLeafs :: [(Weight, Char)] -> [Wtree]
    makeOrderedLeafs = sort . map (uncurry L)

    -- Function: makeWeightedTree
    --
    -- Builds an array containing a single Wtree, spanning the
    -- entire Huffman tree based on combined node weights in new branches.  
    -- Needs ordered leafs as the original input. 
    -- Uses getWeight as helper function.
    makeWeightedTree :: [Wtree] -> Wtree
    makeWeightedTree []  = 
        error "Can't make weighted tree from empty input Wtree array"     
    makeWeightedTree [t] = t
    makeWeightedTree t@(t1:t2:xs) 
        | w1 > w2   = makeWeightedTree $ sort t
        | otherwise = makeWeightedTree $ B (w1+w2) t1 t2 : xs 
        where 
            w1 = getWeight t1
            w2 = getWeight t2

    -- Function: convertToHtree
    --
    -- Traverses recursively down the Wtree structure
    -- until a leaf is found and clones that structure
    -- to a Htree with leafs of the same char value. 
    convertToHtree :: Wtree -> Htree
    convertToHtree (L _ c)     = Leaf c
    convertToHtree (B _ lb rb) = Branch (convertToHtree lb) (convertToHtree rb) 

    -- Function: encode
    --
    -- Builds an Htree from a given string and uses 
    -- that tree to encode the string into a Huffman code. 
    -- Uses maketree, statistics and buildBitEncoding as 
    -- helper functions. 
    encode :: String -> (Htree, BitCode)
    encode str = let 
        htree  = maketree $ statistics str
        bitArr = buildBitEncoding str htree
        in (htree, bitArr) 

    -- Function: buildBitEncoding
    -- 
    -- Builds Huffman bit code from given String and Huffman tree
    -- Recursively adds the bit code first char in the remaining string
    -- to the beginning of the bit code array to preserve the characters order.
    -- Uses getBitCode as helper function. 
    buildBitEncoding :: String -> Htree -> BitCode
    buildBitEncoding [] _        = []
    buildBitEncoding (x:xs) tree = getBitCode [] x tree ++ buildBitEncoding xs tree

    -- Function: getBitCode
    --
    -- Traverses the Huffman tree to find leaf matching given 
    -- char. Stores bit code to that leaf in the accumulator.
    -- Input: Bit code accumulator, Char to encode, Huffman tree.
    getBitCode :: BitCode -> Char -> Htree -> BitCode
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

    -- Function: decode
    --
    -- Recursively decodes integer pattern based on given Htree.
    -- Appends found Char to beginning of String to maintain 
    -- original order of characters. 
    -- Uses decodeChar as helper function.
    decode :: Htree -> BitCode -> String
    decode tree []   = []
    decode tree bits = char ++ decode tree bits'
        where
            (char, bits') = decodeChar tree bits

    -- Function: decodeChar
    --
    -- Traverses down the Htree Branches until Leaf is found or bit code
    -- is empty. If next bit is 0 go to left subtree, else go
    -- to right subtree. Returns the leaf value and unused bit code.
    decodeChar :: Htree -> BitCode -> (String, BitCode)
    decodeChar (Branch _ _) [] = ("", [])
    decodeChar (Leaf c) bits   = ([c], bits) 
    decodeChar (Branch lt rt) (x:xs) 
        | x == 0    = decodeChar lt xs 
        | otherwise = decodeChar rt xs
