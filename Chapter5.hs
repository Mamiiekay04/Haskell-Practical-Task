-- HC5T1: Using applyTwice - apply a function three times
applyThrice :: (a -> a) -> a -> a
applyThrice f x = f (f (f x))

-- Example usage
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- HC5T2: Filtering odd numbers from 1 to 30
oddsFrom1to30 :: [Int]
oddsFrom1to30 = filter odd [1..30]

-- HC5T3: Checking if any word starts with uppercase
import Data.Char (isUpper)

anyStartsWithUpper :: [String] -> Bool
anyStartsWithUpper words = any (\w -> not (null w) && isUpper (head w)) words

-- HC5T4: Rewrite using lambda
biggerThan10 :: Int -> Bool
biggerThan10 = \x -> x > 10

-- HC5T5: Partial application to multiply by five
multiplyByFive :: Num a => a -> a
multiplyByFive = (*5)

-- HC5T6: Function composition to square numbers and filter even ones
square :: Num a => a -> a
square x = x * x

squaredEvens :: [Int] -> [Int]
squaredEvens = filter even . map square

-- HC5T7: Using $ operator
result :: Int
result = sum $ map (*2) $ filter (>3) [1..10]

-- HC5T8: Point-free style
addFive :: Num a => a -> a
addFive = (+5)

-- HC5T9: Higher-order function to apply a function twice to each list element
transformList :: (a -> a) -> [a] -> [a]
transformList f = map (f . f)

-- HC5T10: Combining filter, map, and any
checkAnySquaredGreaterThan50 :: [Int] -> Bool
checkAnySquaredGreaterThan50 xs = any (>50) (map (^2) xs)

-- Main function demonstrating all tasks
main :: IO ()
main = do
    -- HC5T1
    putStrLn "HC5T1 - applyThrice (increment by 1):"
    print $ applyThrice (+1) 0  -- Should print 3
    putStrLn ""

    -- HC5T2
    putStrLn "HC5T2 - Odd numbers from 1 to 30:"
    print oddsFrom1to30
    putStrLn ""

    -- HC5T3
    putStrLn "HC5T3 - Any word starts with uppercase:"
    print $ anyStartsWithUpper ["hello", "World", "test"]
    print $ anyStartsWithUpper ["hello", "world"]
    putStrLn ""

    -- HC5T4
    putStrLn "HC5T4 - biggerThan10 (lambda):"
    print $ biggerThan10 9
    print $ biggerThan10 15
    putStrLn ""

    -- HC5T5
    putStrLn "HC5T5 - multiplyByFive:"
    print $ multiplyByFive 4
    print $ multiplyByFive 10
    putStrLn ""

    -- HC5T6
    putStrLn "HC5T6 - squaredEvens:"
    print $ squaredEvens [1..10]
    print $ squaredEvens [11, 12, 13, 14]
    putStrLn ""

    -- HC5T7
    putStrLn "HC5T7 - Using $ operator:"
    print result
    putStrLn ""

    -- HC5T8
    putStrLn "HC5T8 - addFive in point-free style:"
    print $ addFive 10
    print $ addFive (-3)
    putStrLn ""

    -- HC5T9
    putStrLn "HC5T9 - transformList (apply f twice):"
    print $ transformList (+1) [1, 2, 3]
    print $ transformList (*2) [1, 2, 3]
    putStrLn ""

    -- HC5T10
    putStrLn "HC5T10 - Any squared value > 50:"
    print $ checkAnySquaredGreaterThan50 [5, 6, 7]
    print $ checkAnySquaredGreaterThan50 [3, 4]
    print $ checkAnySquaredGreaterThan50 [1, 2, 3]
