module Lib
    ( someFunc
    ) where

import Data.List
import qualified Data.Set as Set
import Control.Applicative

-- Inverse map 
-- Applies list of functions to single arg
pam :: [a -> b] -> a -> [b]

pam fl x = map ($ x) fl
-- Also an option, though less clear
--pam fl x = fl <*> (return x)

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


data Tree = Tree [Tree] Int deriving (Show, Eq, Ord)

-- Number of nodes of a tree
tlength :: Tree -> Int
tlength (Tree nodes _) = 1 + sum(tlength <$> nodes)

-- Homeomorphic embeddability
-- x # y means that x is embeddable into y
(#) :: Tree -> Tree -> Bool
left @ (Tree n c) # right @ (Tree m d) = any(left #) m || c == d && n ! m

-- Helper for embedding, don't use directly
(!) :: [Tree] -> [Tree] -> Bool
l @ (x:t) ! (y:u) = l ! u || x # y && t ! u
x ! _ = null x

genTrees :: Int -> Int -> [Tree]

-- Generate a list of lists of Tree
-- map Tree [[Tree]] to create partials of each tree without labels

genTreeList_ :: Int -> Int -> [[Tree]] 


genTreeList_ nodes labels = concat [ 
    (mapM (\_ -> (genTrees (nodes - 1) labels)) [2..x]) | 
                                                        x <- [1..nodes]]
genTrees nodes labels = concat [ 
    pam (map Tree (genTreeList_ nodes labels)) label | 
                                                    label <- [1..labels]]


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
                                tree <- (genTrees seqlen labels), -- The current tree with up to`seqlen` vertices
                                (tlength tree <= seqlen) -- T_i has at most i vertices
                                && not (any(# tree) priors)] -- None of the trees in `p` embed into t

-- Actual tree function, TREE(1) = 1, TREE(2) = 3, TREE(3) = BIG

tree :: Int -> Int
-- For all possible sequence lengths, find the longest for which a valid sequence still exists
tree n = head [x - 1 | x <- [0..], null (treeSeqs x n)]


printTree :: Int -> IO ()
printTree n = do {
    putStr "TREE(";
    putStr $ show n; 
    putStr ") = ";
    putStrLn $ show $ tree n;
}

someFunc = do {
    printTree 1;
    printTree 2;
}