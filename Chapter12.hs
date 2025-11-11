import Data.List (sort)
import System.IO
import Control.Exception

-- ===== Task 1 =====
welcomeMessage :: IO ()
welcomeMessage = putStrLn "Welcome to Haskell Programming!"

-- ===== Task 2 =====
addTwoNumbers :: Int -> Int -> Int
addTwoNumbers x y = x + y

taskAddTwoNumbers :: IO ()
taskAddTwoNumbers = do
    putStrLn "Enter first number:"
    a <- readLn
    putStrLn "Enter second number:"
    b <- readLn
    putStrLn ("The sum is: " ++ show (addTwoNumbers a b))

-- ===== Task 3 =====
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

taskFactorial :: IO ()
taskFactorial = do
    putStrLn "Enter a number to find its factorial:"
    n <- readLn
    putStrLn ("Factorial of " ++ show n ++ " is " ++ show (factorial n))

-- ===== Task 4 =====
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

taskFibonacci :: IO ()
taskFibonacci = do
    let fibs = [fibonacci n | n <- [0..9]]
    putStrLn "First 10 Fibonacci numbers:"
    print fibs

-- ===== Task 5 =====
isPalindrome :: String -> Bool
isPalindrome str = str == reverse str

taskPalindrome :: IO ()
taskPalindrome = do
    putStrLn "Enter a word or phrase:"
    input <- getLine
    if isPalindrome (map toLowerCase (filter (/= ' ') input))
        then putStrLn "It's a palindrome!"
        else putStrLn "Not a palindrome."
  where
    toLowerCase c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise = c

-- ===== Task 6 =====
taskSortList :: IO ()
taskSortList = do
    putStrLn "Enter numbers separated by spaces:"
    input <- getLine
    let nums = map read (words input) :: [Int]
    putStrLn ("Sorted list: " ++ show (sort nums))

-- ===== Task 7 =====
calculateCircleArea :: Floating a => a -> a
calculateCircleArea r = pi * r * r

taskCircleArea :: IO ()
taskCircleArea = do
    putStrLn "Enter radius of the circle:"
    r <- readLn
    putStrLn ("Area of the circle is: " ++ show (calculateCircleArea r))

-- ===== Task 8 =====
mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists [] ys = ys
mergeLists xs [] = xs
mergeLists (x:xs) (y:ys)
    | x < y     = x : mergeLists xs (y:ys)
    | otherwise = y : mergeLists (x:xs) ys

taskMergeLists :: IO ()
taskMergeLists = do
    putStrLn "Enter first sorted list (space-separated):"
    list1 <- fmap (map read . words) getLine :: IO [Int]
    putStrLn "Enter second sorted list (space-separated):"
    list2 <- fmap (map read . words) getLine :: IO [Int]
    putStrLn ("Merged sorted list: " ++ show (mergeLists list1 list2))

-- ===== Task 9 =====
taskReadFile :: IO ()
taskReadFile = do
    putStrLn "Enter file name to read:"
    fileName <- getLine
    content <- try (readFile fileName) :: IO (Either IOError String)
    case content of
        Left _ -> putStrLn "Error: File not found or cannot be read."
        Right text -> putStrLn "\nFile content:\n" >> putStrLn text

-- ===== Task 10 =====
add :: Float -> Float -> Float
add a b = a + b

subtract' :: Float -> Float -> Float
subtract' a b = a - b

multiply :: Float -> Float -> Float
multiply a b = a * b

divide :: Float -> Float -> String
divide _ 0 = "Error: Cannot divide by zero"
divide a b = show (a / b)

taskMathOperations :: IO ()
taskMathOperations = do
    putStrLn "Enter first number:"
    a <- readLn
    putStrLn "Enter second number:"
    b <- readLn
    putStrLn ("\nSum: " ++ show (add a b))
    putStrLn ("Difference: " ++ show (subtract' a b))
    putStrLn ("Product: " ++ show (multiply a b))
    putStrLn ("Quotient: " ++ divide a b)

-- ===== Main Menu =====
main :: IO ()
main = do
    putStrLn "\n=== Haskell Chapter 12 Practical Tasks ==="
    putStrLn "1. Print a Welcome Message"
    putStrLn "2. Add Two Numbers"
    putStrLn "3. Factorial Function"
    putStrLn "4. First 10 Fibonacci Numbers"
    putStrLn "5. Palindrome Checker"
    putStrLn "6. Sort a List of Integers"
    putStrLn "7. Calculate Circle Area"
    putStrLn "8. Merge Two Sorted Lists"
    putStrLn "9. Read and Print File Content"
    putStrLn "10. Mathematical Operations Module"
    putStrLn "0. Exit"
    putStrLn "Choose a task number:"
    choice <- getLine
    case choice of
        "1"  -> welcomeMessage
        "2"  -> taskAddTwoNumbers
        "3"  -> taskFactorial
        "4"  -> taskFibonacci
        "5"  -> taskPalindrome
        "6"  -> taskSortList
        "7"  -> taskCircleArea
        "8"  -> taskMergeLists
        "9"  -> taskReadFile
        "10" -> taskMathOperations
        "0"  -> putStrLn "Exiting program. Goodbye!"
        _    -> putStrLn "Invalid option. Please try again."
