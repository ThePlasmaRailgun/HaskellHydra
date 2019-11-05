module Lib
    ( someFunc
    ) where

-- import Data.Tree


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
-- TREE() function modified from https://codegolf.stackexchange.com/a/146532/75773
data T = T [T] Int
l (T n _) = 1 + sum(l <$> n)
a @ (T n c) # T m d = any(a #) m || c == d && n ! m
l @ (x:t) ! (y:u) = l ! u || x # y && t ! u
x ! _ = null x
a n z = concat [ T <$> mapM(\_ -> (a (n - 1) z)) [2..x] <*> [1..z] | x <- [1..n]]
s 0 z = [[]]
s n z= [t:p | p <- (s (n - 1) z), t <- (a n z), (l t <= n) > any(# t) p]
-- TREE(1) = 1, TREE(2) = 3, TREE(3) = BIG
tree n = [x - 1 | x <- [0..], null $ (s x n)] !! 0
--}


-- BIGGEST OF BOIS
-- BuchHolz Hydra with only labels 1 and 0

data H = L Int | B [H] deriving (Show, Eq)

--lvo = B [B [B [L 1, L 1], L 1], L 0]

smol = B [L 1, L 0]

c :: H -> H -> H

c (B x) (B z) = B (x ++ z)
c (L x) (B z) = B (L x : z)

sb :: H -> H

sb (L x) = L 0
sb (B (x:xs)) = B (sb x : xs)
sb (B x) = L 0

r :: H -> H -> Int -> H

r t @ (B (u:v)) (b @ (B _)) n =
          if (u == (L 1))
            then (c (sb b) (B v))
          else if (u /= (L 1) || u /= (L 0))
            then B ((replicate n (rr u b n)) ++ v)
            else B v

rr (B t) b n = if (t !! 0) == (L 0)
  then (r (B t) b n)
  else (r (B t) b n)

rr t b n = B []

f t@(B x) n = if (length x == 1) then n else (f (rr t (B []) n) (n + 1))

someFunc = print $ f smol 1

--someFunc = print $ sb lvo

{-
def S(T):
  return 0 if T == 1 else [S(T[0])] + T[1:]

def R(T, c, B=None):
  if not B:
    B = []
  U=T[0]
  V=T[1:]
  if T[-1]==0:
    B=T
  if U==1:
      return [S(B)]+V
  return [R(U, c, B)]*c+V if U else V

def RR(T, c):
  return c if len(T) == 0 else RR(R(T, c), c + c)

A = [1, 0]

print(RR(A, 1))
-}




--someFunc = print $ tree 2

--someFunc = print (conway [3, 3, 2])

--someFunc = print (uparrow 3 3 2)