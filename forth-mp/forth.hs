import Data.HashMap.Strict as H

-- Initial types

type ForthState = (IStack, CStack, Dictionary)

type IStack = [Integer]
initialIStack = []

type CStack = [[String]]
initialCStack = []

-- Type for the symbol dictionary

type Dictionary = H.HashMap String [Entry]

data Entry =
     Prim ([Integer] -> [Integer])
   | Def [String]
   | Num Integer
   | Unknown String

instance Show Entry where
  show (Prim f)    = "Prim"
  show (Def s)     = show s
  show (Num i)     = show i
  show (Unknown s) = "Unknown: " ++ s

-- Dictionary helpers

wrap2 f (x:y:xs) = (f x y):xs
wrap2 f _ = error "Value stack underflow!"

dlookup :: String -> Dictionary -> Entry
dlookup word dict =
  case H.lookup word dict of
    Nothing -> case reads word of
                 [(i,"")] -> Num i
                 _        -> Unknown word
    Just x  -> head x

dinsert :: String -> Entry -> Dictionary -> Dictionary
dinsert key val dict =
   case H.lookup key dict of
      Nothing -> H.insert key [val] dict
      Just x  -> H.insert key (val:x) dict

--helper

-- Initial Dictionary

dictionary  = dinsert "+" (Prim $ wrap2 (+)) H.empty
--dictionary2 = dinsert "sq1" (Def $ ["dup","*"]) dictionary

-- The Evaluator

eval :: [String] -> ForthState -> IO ForthState
eval []    (istack, [],     dict) = return (istack, [], dict)
eval words (istack, cstack, dict) =
  case dlookup (head words) dict of
    Num i          -> eval xs (i:istack, cstack, dict)
    Prim f         -> eval xs (f istack, cstack, dict)
    Def s	   -> do { putStrLn $ show s;
				eval (head s:last s:xs) (istack, cstack, dict)}
    Unknown "dup"  -> eval xs (hd:istack, cstack, dict)
    Unknown "drop" -> eval xs (td, cstack, dict)
    Unknown "rot"  -> eval xs (reverse istack, cstack, dict)
    Unknown "swap" -> eval xs ((head td : hd : tail td),cstack,dict)
    Unknown ".S"   -> do { putStrLn $ show (reverse istack);
			     eval xs (istack, cstack, dict)}
    Unknown "."    -> do { putStrLn $ show hd;
                             eval xs (td, cstack, dict)}
    Unknown "-"    -> eval xs ([head td - hd], cstack,dict)
    Unknown "*"    -> eval xs ([head td * hd], cstack,dict)
    Unknown "/"    -> eval xs ([head td `div` hd], cstack,dict)
    Unknown ";"    -> eval [] (istack, cstack, dinsert (head (head cstack)) (Def $ (reverse (last cstack))) dict)
    Unknown ":"    -> eval [last xs] (istack,[head xs]:tail (reverse (tail xs)):cstack,dict)
  where xs = tail words
	hd = head istack
	td = tail istack


repl :: ForthState -> IO ForthState
repl state =
  do putStr "> " ;
     input <- getLine
     nustate <- eval (words input) state
     repl nustate

main = do
  putStrLn "Welcome to your forth interpreter!"
  repl (initialIStack, initialCStack, dictionary)
