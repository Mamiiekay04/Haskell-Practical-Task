import Data.List (sortBy)
import Data.Ord (comparing)

-- HC2T2
add :: Int -> Int -> Int
add x y = x + y

isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

concatStrings :: String -> String -> String
concatStrings s1 s2 = s1 ++ s2

-- HC2T3
myAge :: Int
myAge = 21

piValue :: Double
piValue = 3.14159

greeting :: String
greeting = "Hello, Haskell!"

isHaskellFun :: Bool
isHaskellFun = True

-- HC2T4
prefix1 = (+) 5 3
prefix2 = (*) 10 4
prefix3 = (&&) True False

infix1 = 7 + 2
infix2 = 6 * 5
infix3 = True && False

-- HC2T5
circleArea :: Float -> Float
circleArea r = pi * r ^ 2

maxOfThree :: Int -> Int -> Int -> Int
maxOfThree a b c = max a (max b c)

-- HC2T6
smallNumber :: Int
smallNumber = 262

bigNumber :: Integer
bigNumber = 2127

-- HC2T7
expr1 = True && True
expr2 = False || False
expr3 = not False
expr4 = 10 > 20

-- ========================
-- Main Function
-- ========================
main :: IO ()
main = do
    putStrLn "=== HC2T1: Checking Types ==="
    putStrLn "42 :: Int, 3.14 :: Fractional (Double), \"Haskell\" :: String, 'Z' :: Char, True && False :: Bool"

    putStrLn "\n=== HC2T2: Functions ==="
    putStrLn $ "add 5 7 = " ++ show (add 5 7)
    putStrLn $ "isEven 8 = " ++ show (isEven 8)
    putStrLn $ "concatStrings \"Hi \" \"there\" = " ++ concatStrings "Hi " "there"

    putStrLn "\n=== HC2T3: Immutable Variables ==="
    putStrLn $ "myAge = " ++ show myAge
    putStrLn $ "piValue = " ++ show piValue
    putStrLn $ "greeting = " ++ greeting
    putStrLn $ "isHaskellFun = " ++ show isHaskellFun

    putStrLn "\n=== HC2T4: Infix/Prefix Conversion ==="
    putStrLn $ "prefix (+) 5 3 = " ++ show prefix1
    putStrLn $ "prefix (*) 10 4 = " ++ show prefix2
    putStrLn $ "prefix (&&) True False = " ++ show prefix3
    putStrLn $ "infix 7 + 2 = " ++ show infix1
    putStrLn $ "infix 6 * 5 = " ++ show infix2
    putStrLn $ "infix True && False = " ++ show infix3

    putStrLn "\n=== HC2T5: Functions ==="
    putStrLn $ "circleArea 5 = " ++ show (circleArea 5)
    putStrLn $ "maxOfThree 7 3 9 = " ++ show (maxOfThree 7 3 9)

    putStrLn "\n=== HC2T6: Int vs Integer ==="
    putStrLn $ "smallNumber (Int) = " ++ show smallNumber
    putStrLn $ "bigNumber (Integer) = " ++ show bigNumber
    putStrLn "Try in GHCi: 2^64 :: Int  (will overflow)"

    putStrLn "\n=== HC2T7: Boolean Expressions ==="
    putStrLn $ "True && True = " ++ show expr1
    putStrLn $ "False || False = " ++ show expr2
    putStrLn $ "not False = " ++ show expr3
    putStrLn $ "10 > 20 = " ++ show expr4
