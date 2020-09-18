module Transaction where
import qualified Data.Map as M
import Data.List       hiding (delete,map)
import Data.Tuple
import Prelude
import Control.Monad

data Transaction =
     Transaction
        { tId      :: Id
        , tInputs  :: [Input]
        , tOutputs :: [Output]
        }
        deriving (Show)
type Id     = Int


data Output =
     Output
       { oValue   :: Int
       , oAddress :: Address
       }
      deriving (Show)
type Address = String


data Input   =
     Input
      { iPrevious :: Id
      , iIndex    :: Index
      }
     deriving (Show, Eq, Ord)
type Index  = Int

type UTXOs  = M.Map Input Output

--initial UTXOs
genesis :: UTXOs
genesis    = M.fromList  [ (Input 0 (-1), Output 10000 "John") ]

--updated UTXOs after the first transaction
utxos1 :: UTXOs
utxos1 = 
   M.fromList
    [ (Input 1 0, Output 1500 "Genet")
    , (Input 1 1, Output 1500 "Meron")
    , (Input 1 2, Output 1500 "Faiza")
    , (Input 1 3, Output 5500 "John")
    ]

--updated UTXOs after the second transaction
utxos2 :: UTXOs
utxos2 =
    M.fromList
    [ (Input 1 0, Output 1500 "Genet")
    , (Input 1 3, Output 5500 "John")
    , (Input 2 0, Output 100 "Bethel's Fine Ethiopian Food")
    , (Input 2 1, Output 1440 "Meron")
    , (Input 2 2, Output 1440 "Faiza")
   ]
-- the firist transaction
perDiem :: Transaction
perDiem            = Transaction
       { tId       = 1
       , tInputs   = [Input 0 (-1)]
       , tOutputs  =
                  [ Output 1500 "Genet"
                  , Output 1500 "Meron"
                  , Output 1500 "Faiza"
                  , Output 5500 "John"
                  ]
       }
--the next transaction
lunch :: Transaction
lunch             = Transaction
     { tId        = 2
     , tInputs    = [Input 1 1, Input 1 2]
     , tOutputs   =
                 [ Output 100 "Bethel's Fine Ethiopian Food"
                 , Output 1440 "Meron"
                 , Output 1440 "Faiza"
                 ]
     }

-- check if the sum of inputs is greater than sum of outputs 
-- if it is greater then it check if the input is avalible then delete the old and insert the new value
processTransaction :: Transaction -> UTXOs -> Either String UTXOs
processTransaction tx u = if sumi >= sumo then  
                                           let d' = deleteI (tInputs tx)  u
                                               l' = insertI (tId tx) (tOutputs tx) d' 0
                                           in Right l'
                                           else Left "invalid transaction" 
   where
       unout  = map(flip M.lookup u) (tInputs tx)         
       sumi   = sum (map (\(Just o) -> oValue  o) unout)    --remove the just and add the inputs
       sumo   = sum (map (\a ->  oValue  a) (tOutputs tx))  -- add the output values

-- insert the new Unspent Transaction Outputs
insertI  :: Int -> [Output] -> UTXOs -> Int -> UTXOs
insertI  _   []     u  _    = u
insertI id  (x: xs) u  i    = insertI id xs (M.insert (Input id i) (Output (oValue x) (oAddress x)) u) (i + 1)

--delete the old Unspent Transaction Outputs
deleteI :: [Input] -> UTXOs -> UTXOs
deleteI [] u       = u
deleteI (x : xs) u = let 
         d'        = M.delete x u
         d''       = deleteI xs d' in d''

-- check a list of transaction
processTransactions :: [Transaction] -> UTXOs -> Either String UTXOs
processTransactions []  u      = Left "invalid transaction" 
processTransactions [xs , ys] u = case processTransaction xs u of
                                   Left _ -> Left ("No transaction to process!")
                                   Right x -> processTransactions [ys] u


 