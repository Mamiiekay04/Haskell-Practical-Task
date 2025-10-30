import Data.Char (toUpper)

main :: IO ()
main = do
    putStrLn "=== Haskell Chapter 11 Practical Tasks ==="
    putStrLn "Running all tasks (HC11T1 to HC11T10)"
    putStrLn "------------------------------------------"

    putStrLn "\nHC11T1: Greet the User"
    task1

    putStrLn "\nHC11T2: Count Characters in a Line"
    task2

    putStrLn "\nHC11T3: Double a Number"
    task3

    putStrLn "\nHC11T4: Concatenate Two Lines"
    task4

    putStrLn "\nHC11T5: Repeat Until \"quit\" (Simulated)"
    task5

    putStrLn "\nHC11T6: Uppercase Converter"
    task6

    putStrLn "\nHC11T7: User Options"
    task7

    putStrLn "\nHC11T8: Even or Odd Checker"
    task8

    putStrLn "\nHC11T9: Sum Two Numbers"
    task9

    putStrLn "\nHC11T10: Reverse User Input"
    task10

    putStrLn "\n------------------------------------------"
    putStrLn "All tasks executed successfully!"
    putStrLn "=========================================="

------------------------------------------------------
-- HC11T1: Greet the User
------------------------------------------------------
task1 :: IO ()
task1 = do
    let name = "Maria"
    putStrLn ("Hello, " ++ name ++ "!")

------------------------------------------------------
-- HC11T2: Count Characters in a Line
------------------------------------------------------
task2 :: IO ()
task2 = do
    let line = "Hello Haskell"
    putStrLn ("Line: " ++ line)
    putStrLn ("The number of characters is: " ++ show (length line))

------------------------------------------------------
-- HC11T3: Double a Number
------------------------------------------------------
task3 :: IO ()
task3 = do
    let number = 7
    putStrLn ("Number: " ++ show number)
    putStrLn ("Double of that number is: " ++ show (number * 2))

------------------------------------------------------
-- HC11T4: Concatenate Two Lines
------------------------------------------------------
task4 :: IO ()
task4 = do
    let first = "Hello "
    let second = "World!"
    putStrLn ("First: " ++ first)
    putStrLn ("Second: " ++ second)
    putStrLn ("Concatenated result: " ++ first ++ second)

------------------------------------------------------
-- HC11T5: Repeat Until \"quit\" (Simulated)
------------------------------------------------------
task5 :: IO ()
task5 = do
    let inputs = ["Hi", "Haskell", "quit"]
    mapM_ process inputs
  where
    process "quit" = putStrLn "Goodbye!"
    process text   = putStrLn ("You entered: " ++ text)

------------------------------------------------------
-- HC11T6: Uppercase Converter
------------------------------------------------------
task6 :: IO ()
task6 = do
    let line = "Haskell is fun!"
    putStrLn ("Original: " ++ line)
    putStrLn ("Uppercase: " ++ map toUpper line)

------------------------------------------------------
-- HC11T7: User Options (Simulated choice)
------------------------------------------------------
task7 :: IO ()
task7 = do
    let choice = "2"
    putStrLn ("You chose option: " ++ choice)
    case choice of
        "1" -> putStrLn "Hello, User!"
        "2" -> do
            let a = 5
            let b = 8
            putStrLn ("The sum of " ++ show a ++ " and " ++ show b ++ " is " ++ show (a + b))
        "3" -> putStrLn "Exiting..."
        _   -> putStrLn "Invalid option!"

------------------------------------------------------
-- HC11T8: Even or Odd Checker
------------------------------------------------------
task8 :: IO ()
task8 = do
    let n = 9
    putStrLn ("Number: " ++ show n)
    if even n
        then putStrLn "The number is even."
        else putStrLn "The number is odd."

------------------------------------------------------
-- HC11T9: Sum Two Numbers
------------------------------------------------------
task9 :: IO ()
task9 = do
    let x = 10
    let y = 15
    putStrLn ("The sum of " ++ show x ++ " and " ++ show y ++ " is " ++ show (x + y))

------------------------------------------------------
-- HC11T10: Reverse User Input
------------------------------------------------------
task10 :: IO ()
task10 = do
    let input = "Haskell"
    putStrLn ("Original: " ++ input)
    putStrLn ("Reversed: " ++ reverse input)
