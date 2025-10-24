-----------------------------
-- HC10T1: ShowSimple Type Class
-----------------------------
data PaymentMethod = Cardano | Cash | Bank String
  deriving (Show)

class ShowSimple a where
  showSimple :: a -> String

instance ShowSimple PaymentMethod where
  showSimple Cardano   = "Cardano"
  showSimple Cash      = "Cash"
  showSimple (Bank s)  = "Bank(" ++ s ++ ")"

mainHC10T1 :: IO ()
mainHC10T1 = do
  putStrLn "=== HC10T1: ShowSimple ==="
  print (showSimple Cardano)
  print (showSimple (Bank "FNB"))
  putStrLn ""

-----------------------------
-- HC10T2: Summable Type Class
-----------------------------
class Summable a where
  sumUp :: [a] -> a

instance Summable Int where
  sumUp = sum

mainHC10T2 :: IO ()
mainHC10T2 = do
  putStrLn "=== HC10T2: Summable ==="
  print (sumUp [1,2,3,4,5] :: Int)
  putStrLn ""

-----------------------------
-- HC10T3: Comparable Type Class
-----------------------------
data Blockchain = Bitcoin Int | Ethereum Int
  deriving (Show)

class Comparable a where
  compareWith :: a -> a -> Ordering

instance Comparable Blockchain where
  compareWith (Bitcoin a) (Bitcoin b) = compare a b
  compareWith (Ethereum a) (Ethereum b) = compare a b
  compareWith (Bitcoin _) (Ethereum _) = LT
  compareWith (Ethereum _) (Bitcoin _) = GT

mainHC10T3 :: IO ()
mainHC10T3 = do
  putStrLn "=== HC10T3: Comparable ==="
  print (compareWith (Bitcoin 5) (Bitcoin 7))
  print (compareWith (Ethereum 9) (Bitcoin 3))
  putStrLn ""

-----------------------------
-- HC10T4: Eq Instance for Box
-----------------------------
data Box a = EmptyBox | Box a
  deriving (Show)

instance (Eq a) => Eq (Box a) where
  EmptyBox == EmptyBox = True
  Box a == Box b       = a == b
  _ == _               = False

mainHC10T4 :: IO ()
mainHC10T4 = do
  putStrLn "=== HC10T4: Eq Box ==="
  print (Box 5 == Box 5)
  print (Box 3 == Box 4)
  print (EmptyBox == (EmptyBox :: Box Int))
  putStrLn ""

-----------------------------
-- HC10T5: ShowDetailed Type Class
-----------------------------
data User = User { userName :: String, userAge :: Int }

class ShowDetailed a where
  showDetailed :: a -> String

instance ShowDetailed User where
  showDetailed (User name age) = "User: " ++ name ++ ", Age: " ++ show age

mainHC10T5 :: IO ()
mainHC10T5 = do
  putStrLn "=== HC10T5: ShowDetailed ==="
  print (showDetailed (User "Alice" 25))
  putStrLn ""

-----------------------------
-- HC10T6: Mutual Recursion in Eq for Blockchain
-----------------------------
instance Eq Blockchain where
  (==) a b = not (a /= b)
  (/=) (Bitcoin x) (Bitcoin y) = x /= y
  (/=) (Ethereum x) (Ethereum y) = x /= y
  (/=) (Bitcoin _) (Ethereum _) = True
  (/=) (Ethereum _) (Bitcoin _) = True

mainHC10T6 :: IO ()
mainHC10T6 = do
  putStrLn "=== HC10T6: Eq Blockchain (mutual recursion) ==="
  print (Bitcoin 5 == Bitcoin 5)
  print (Ethereum 2 /= Ethereum 3)
  print (Bitcoin 1 /= Ethereum 1)
  putStrLn ""

-----------------------------
-- HC10T7: Convertible Type Class
-----------------------------
class Convertible a b where
  convert :: a -> b

instance Convertible PaymentMethod String where
  convert Cardano   = "Cardano"
  convert Cash      = "Cash"
  convert (Bank s)  = "Bank:" ++ s

mainHC10T7 :: IO ()
mainHC10T7 = do
  putStrLn "=== HC10T7: Convertible ==="
  print (convert Cardano :: String)
  print (convert (Bank "Nedbank") :: String)
  putStrLn ""

-----------------------------
-- HC10T8: AdvancedEq Subclass of Eq
-----------------------------
class Eq a => AdvancedEq a where
  compareEquality :: a -> a -> Bool

instance AdvancedEq Blockchain where
  compareEquality (Bitcoin x) (Bitcoin y)   = x == y
  compareEquality (Ethereum x) (Ethereum y) = x == y
  compareEquality _ _                       = False

mainHC10T8 :: IO ()
mainHC10T8 = do
  putStrLn "=== HC10T8: AdvancedEq ==="
  print (compareEquality (Bitcoin 10) (Bitcoin 10))
  print (compareEquality (Ethereum 1) (Bitcoin 1))
  putStrLn ""

-----------------------------
-- HC10T9: MinMax Type Class
-----------------------------
class MinMax a where
  minValue :: a
  maxValue :: a

instance MinMax Int where
  minValue = -1000
  maxValue = 1000

mainHC10T9 :: IO ()
mainHC10T9 = do
  putStrLn "=== HC10T9: MinMax ==="
  print (minValue :: Int)
  print (maxValue :: Int)
  putStrLn ""

-----------------------------
-- HC10T10: Concatenatable Type Class
-----------------------------
class Concatenatable a where
  concatWith :: a -> a -> a

instance Concatenatable [Char] where
  concatWith = (++)

mainHC10T10 :: IO ()
mainHC10T10 = do
  putStrLn "=== HC10T10: Concatenatable ==="
  print (concatWith "Hello" " World")
  putStrLn ""

-----------------------------
-- Main to run all tasks
-----------------------------
main :: IO ()
main = do
  mainHC10T1
  mainHC10T2
  mainHC10T3
  mainHC10T4
  mainHC10T5
  mainHC10T6
  mainHC10T7
  mainHC10T8
  mainHC10T9
  mainHC10T10
  putStrLn "=== End of Chapter 10 Tests ==="
