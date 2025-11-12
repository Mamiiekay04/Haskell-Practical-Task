import System.Directory (listDirectory)
import Data.List (isInfixOf, sort)
import qualified Data.List as DL
import qualified Data.Map as Map
import qualified Data.Map as DM
import Control.Exception (try, IOException)

sumNonEmpty :: [Int] -> Int
sumNonEmpty [] = error "Error: List cannot be empty!"
sumNonEmpty xs = sum xs

task1 :: IO ()
task1 = do
    putStrLn "\nFiles in current directory:"
    files <- listDirectory "."
    mapM_ putStrLn files

task2 :: IO ()
task2 = do
    putStrLn "Enter a substring to filter files:"
    sub <- getLine
    files <- listDirectory "."
    let filtered = filter (isInfixOf sub) files
    putStrLn "\nFiltered files:"
    mapM_ putStrLn filtered

task3 :: IO ()
task3 = do
    putStrLn "Enter substring to search in filenames:"
    sub <- getLine
    files <- listDirectory "."
    let filteredSorted = sort (filter (isInfixOf sub) files)
    putStrLn "\nSorted and filtered files:"
    mapM_ putStrLn filteredSorted

task4 :: IO ()
task4 = do
    putStrLn "Testing sumNonEmpty function:"
    print (sumNonEmpty [1,2,3,4])

task5 :: IO ()
task5 = do
    putStrLn "Restricted helper functions are not exported in real modules."
    print (sumNonEmpty [10,20,30])

task6 :: IO ()
task6 = do
    putStrLn "Enter substring to filter files:"
    sub <- getLine
    files <- listDirectory "."
    let filtered = filter (isInfixOf sub) files
    let fileMap = Map.fromList (zip [1..] filtered)
    putStrLn "\nFiles mapped to keys:"
    print fileMap

task7 :: IO ()
task7 = do
    putStrLn "Enter numbers separated by spaces:"
    input <- getLine
    let nums = map read (words input) :: [Int]
    putStrLn ("Sum of numbers: " ++ show (sumNonEmpty nums))

task8 :: IO ()
task8 = do
    let lst = [5,3,1,4,2]
    putStrLn ("\nSorted list using Data.List: " ++ show (DL.sort lst))
    let mp = Map.fromList [(1,"One"),(2,"Two")]
    putStrLn ("Value for key 1 from Data.Map: " ++ show (Map.lookup 1 mp))

task9 :: IO ()
task9 = do
    let fruits = ["banana", "apple", "cherry"]
    putStrLn ("\nSorted fruits using DL (renamed Data.List): " ++ show (DL.sort fruits))
    let fruitMap = DM.fromList [(1, "apple"), (2, "banana")]
    putStrLn ("Fruit map using DM (renamed Data.Map): " ++ show fruitMap)

task10 :: IO ()
task10 = do
    putStrLn "Enter a substring to search for files:"
    sub <- getLine
    files <- listDirectory "."
    let filtered = filter (isInfixOf sub) files
    let sortedFiles = sort filtered
    putStrLn "\nSearch results (sorted):"
    mapM_ putStrLn sortedFiles

main :: IO ()
main = do
    putStrLn "\n=== Haskell Chapter 13 Practical Tasks ==="
    putStrLn "1. List Files in Directory"
    putStrLn "2. Filter Files by Substring"
    putStrLn "3. Sort and Return Filtered Files"
    putStrLn "4. SumNonEmpty Module"
    putStrLn "5. Restrict Module Export List"
    putStrLn "6. File Names to Map"
    putStrLn "7. Use Custom Module in Main"
    putStrLn "8. Qualified Imports for Name Conflicts"
    putStrLn "9. Renaming Module Namespace"
    putStrLn "10. Multi-Module Main Function"
    putStrLn "0. Exit"
    putStrLn "Enter your choice:"
    choice <- getLine
    case choice of
        "1"  -> task1
        "2"  -> task2
        "3"  -> task3
        "4"  -> task4
        "5"  -> task5
        "6"  -> task6
        "7"  -> task7
        "8"  -> task8
        "9"  -> task9
        "10" -> task10
        "0"  -> putStrLn "Exiting program. Goodbye!"
        _    -> putStrLn "Invalid choice. Please try again."
