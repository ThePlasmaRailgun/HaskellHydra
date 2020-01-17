module Lib
    ( someFunc
    ) where

-- import Data.Tree

-- TREE() function modified from https://codegolf.stackexchange.com/a/146532/7577

someFunc :: IO ()


-- Two argument Ackermann function
ackermann :: Int -> Int -> Int

ackermann 0 n = n + 1

ackermann m 0 = ackermann (m - 1) 1

ackermann m n = ackermann (m - 1) (ackermann m (n - 1))


-- Conway's chained arrow notation
conway_ :: [Int] -> Int

conway_ [a] = a

conway_ [b, a] = a ^ b

conway_ (1:xs) = conway_ xs

conway_ (a:1:xs) = conway_ xs

conway_ (b:a:xs) = conway_ ([b - 1] ++ [conway_([b, a - 1] ++ xs)] ++ xs)

conway x = conway_ (reverse x)


-- Knuth's Up arrow Notation
uparrow :: Int -> Int -> Int -> Int

uparrow a 1 b = a ^ b

uparrow _ _ 0 = 1

uparrow a n b = uparrow a (n - 1) (uparrow a n (b - 1))


{--

Kruskal's tree theorem 
TREE() function on k-labeled trees

--}


data Tree = Tree [Tree] Int deriving Show

-- Number of nodes of a tree
tlength :: Tree -> Int
tlength (Tree n _) = 1 + sum(tlength <$> n)

-- Homeomorphic embeddability
-- x # y means that x is embeddable into y
(#) :: Tree -> Tree -> Bool
a @ (Tree n c) # Tree m d = any(a #) m || c == d && n ! m

-- Helper for embedding, don't use directly
(!) :: [Tree] -> [Tree] -> Bool
l @ (x:t) ! (y:u) = l ! u || x # y && t ! u
x ! _ = null x


treehelp :: Int -> Int -> [Tree]

treehelp nodes labels = (getTrees (nodes - 1) labels)


treeNodes nodes labels x = ((mapM ((\_ -> treehelp nodes labels)) [2..x]))

thing x = Tree <$> x

getTrees :: Int -> Int -> [Tree]

getTrees nodes labels = concat [ (
    (fmap Tree (treeNodes nodes labels x)
    ) <*> [1..labels]) 
                                 | x <- [1..nodes]]


{-- Lists all valid sequences of labeled trees

NOTE: This list is reversed, largest trees are at head of list!

From mathematical definition:

We have a sequence of k-labeled trees T_1, T_2, T_3... (T_1 will be at last index on Haskell list because sequence is reversed)

Such that:

* Each tree T_i has at most i vertices
* No tree is homeomorphically embeddable into any tree later in the sequence 
(in this case that means embeddable into any tree with a smaller index, see note)

`treeSeqs seqlen labels` returns all valid sequences of length `seqlen` with `labels` coloring, ie. T_1, T_2... T_seqlen.
--}

treeSeqs :: Int -> Int -> [[Tree]]


treeSeqs 0 _ = [[]]
treeSeqs seqlen labels = [tree:priors | priors <- (treeSeqs (seqlen - 1) labels), -- Recursively get the values for T_i-1, T_i-2...
                                tree <- (getTrees seqlen labels), -- The current tree with up to`seqlen` vertices
                                (tlength tree <= seqlen) -- T_i has at most i vertices
                                && not (any(# tree) priors)] -- None of the trees in `p` embed into t

-- Actual tree function
-- TREE(1) = 1, TREE(2) = 3, TREE(3) = BIG
-- the function parameter is actually the number of tree colors allowed

tree :: Int -> Int
tree n = [x - 1 | x <- [0..], -- For all possible sequence lengths, find the longest for which a valid sequence still exists
                  null $ (treeSeqs x n)] !! 0 
            


-- Replace commas with comma then newline
strrepl :: String -> String

strrepl [] = [];
strrepl (x:xs) = (if x == ',' then ",\n" else [x]) ++ (strrepl xs)


{--
t1 = Tree [ Tree [Tree [] 1, Tree [] 3 ] 2 ] 1
t2 = Tree [Tree [] 1, Tree [] 3 ] 2
-- t2 embeds into t1
--}

someFunc = do {
    print $ (map . map) tlength treeseq;
    putStrLn $ strrepl (show treeseq);    
    
    {--
    print $ map (map tlength) (treeseq2);
    putStrLn $ (strrepl (show treeseq2));
    --}

    {--  
    print $ t1;
    print $ t2;
    print $ t2 # t1; -- True
    print $ t1 # t2; -- False
    --}
} where treeseq = (treeSeqs 3 2); 

-- There are no valid 4 length sequences with only two colors
-- treeseq2 = (treeSeqs 4 2); 


--someFunc = print (conway [3, 3, 2])

--someFunc = print (uparrow 3 3 2)