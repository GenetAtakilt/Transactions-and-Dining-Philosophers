import System.IO
import Control.Concurrent
import Control.Monad

-------------------------------------------------------------------------------------
-- W2.2 Dining Philosophers
-- Subtask 2.2.1

-- these function takes the number of Philosophers dining and in what 
-- order the Philosopher is sitting and returns the left and the right fork 

getForks:: Int -> Int ->(Int,Int)
getForks n 1= (0,n-1)
getForks _ i=(i-1,i-2)

-- Get the Mvar value and a string to display and it will display the string
-- gives each thread a chance to display out so that the output won't be garbled

logOut':: MVar ()-> String-> IO()
logOut' v s = do
   takeMVar v
   putStrLn s
   putMVar v ()

-- these function take a philosopher and the philosopher will try to take the left
-- fork first and the right fork. then the philosopher will put the right fork first 
-- and then the left fork next. 
  

runPhilosopher :: MVar()-> Forks -> Int -> IO()
runPhilosopher v fs i= do
    let n = length fs
        (l,r) = getForks n i
        vl = fs !! l
        vr = fs !! r
    forever $ do
        takeMVar vl
        logOut' v $ "Philosopher "++ show i ++"takes left ( "++ show l++" )forks."
        takeMVar vr
        logOut' v $ "Philosopher "++ show i ++"takes right ( "++ show r++" ) forks."
        logOut' v $"Philosopher "++ show i ++"is eating"
        putMVar vr ()
        logOut' v $ "Philosopher "++ show i ++"puts right ( "++ show r++" ) forks."
        putMVar vl ()
        logOut' v $ "Philosopher "++ show i ++"puts left ( "++ show l++" ) forks."

type Forks = [MVar ()]
 

main :: IO()
main = do
    putStrLn "Enter the number of dining philosophers"
    s  <- getLine
    let n = read s
    v  <- newMVar ()
    fs <- replicateM n $ newMVar ()
    mapM_ (forkIO.runPhilosopher v fs) [2 .. n]
    runPhilosopher v fs 1