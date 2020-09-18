module Unsafeio where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)


data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Eq, Show)


tree1 :: Tree Int
tree1 = Leaf 1

tree2 :: Tree Int
tree2 = Node (Leaf 2) (Leaf 4)

tree3 :: Tree Int
tree3 = Node tree2 tree1

tree4 :: Tree Int
tree4 = Node tree2 tree3

-------------------------------------------------------------------------------------
-- Subtask 2.3.1
-- these function takes a tree and return the labelled version of a tree.
-- for these we used  IORef to save the counter number. and a helper function 
-- that takes a tree and and IORef and returns IO(Tree (a,Int))

relabelTree :: Tree a -> IO (Tree (a, Int))
relabelTree a = do 
    c <- newIORef 0
    relabelTree' a c

relabelTree' :: Tree a -> IORef Int-> IO (Tree (a, Int))
relabelTree' (Leaf t) i = do
    x <- readIORef i
    writeIORef i (x+1)
    return (Leaf (t, x))
relabelTree' (Node l r) i = do 
    l' <- relabelTree' l i
    r' <- relabelTree' r i
    return ( Node l' r')

-------------------------------------------------------------------------------------
-- Subtask 2.3.2
-- these function returns a IOref with t]a value of undefined


anything :: IORef a
anything = unsafePerformIO $ newIORef undefined

-------------------------------------------------------------------------------------
-- Subtask 2.3.3
-- these function takes a value of a and returns a value of b and we used an unsafePerformIO
-- and anything to define the function 
-- there are three case for these function
-- 1. the case that works
--      if we gave a correct type for a or b we get the correct result 
-- 2. the case that works but the encode type of the input type and the output type is different.
--    example  ((cast 'c'):: Int) the type of a is char but we specify the output type is an int 
--             we expect the output to be c but it will return 99
-- 3. the case that don't work
--      if we gave it a pair, it will crash because the memory space that 
--      is given for one value and a pair is different 
 

cast :: a -> b
cast a = unsafePerformIO $ do 
    writeIORef anything a
    b <- readIORef anything
    return b

test = do
    print (cast "fg":: String)
    print ((cast "Fg")::String)
    print ((cast 'c'):: Int)
    print ((cast 1)::String)
    print ((cast ["dfg"])::[String])