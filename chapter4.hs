-- HC4T1 - Task 1: weatherReport
weatherReport :: String -> String
weatherReport condition = case condition of
    "sunny"  -> "It's a bright and beautiful day!"
    "rainy"  -> "Don't forget your umbrella!"
    "cloudy" -> "A bit gloomy, but no rain yet!"
    _        -> "Weather unknown"

-- HC4T2 - Task 2: dayType
dayType :: String -> String
dayType day = case day of
    "Saturday" -> "It's a weekend!"
    "Sunday"   -> "It's a weekend!"
    "Monday"   -> "It's a weekday."
    "Tuesday"  -> "It's a weekday."
    "Wednesday"-> "It's a weekday."
    "Thursday" -> "It's a weekday."
    "Friday"   -> "It's a weekday."
    _          -> "Invalid day"

-- HC4T3 - Task 3: gradeComment
gradeComment :: Int -> String
gradeComment grade
    | grade >= 90 && grade <= 100 = "Excellent!"
    | grade >= 70 && grade <= 89  = "Good job!"
    | grade >= 50 && grade <= 69  = "You passed."
    | grade >= 0  && grade <= 49  = "Better luck next time."
    | otherwise                   = "Invalid grade"

-- HC4T4 - Task 4: specialBirthday using pattern matching
specialBirthday :: String -> String
specialBirthday "Alice" = "Happy birthday, Alice!"
specialBirthday "Bob"   = "Happy birthday, Bob!"
specialBirthday name    = "Happy birthday, " ++ name ++ "!"

-- HC4T5 - Task 5: enhanced specialBirthday with age
specialBirthdayEnhanced :: String -> Int -> String
specialBirthdayEnhanced name age = case (name, age) of
    ("Alice", 30) -> "Happy 30th birthday, Alice!"
    ("Bob", 25)   -> "Happy 25th birthday, Bob!"
    (_, ageVal)   -> "Happy birthday, " ++ name ++ "! You are now " ++ show ageVal ++ " years old."

-- HC4T6 - Task 6: whatsInsideThisList
whatsInsideThisList :: [a] -> String
whatsInsideThisList lst = case lst of
    []          -> "The list is empty."
    [_]         -> "The list has one element."
    [_, _]      -> "The list has two elements."
    _           -> "The list has many elements."

-- HC4T7 - Task 7: firstAndThird with Show constraint
firstAndThird :: Show a => [a] -> (Maybe a, Maybe a)
firstAndThird lst = case lst of
    (x:_:z:_) -> (Just x, Just z)
    (x:_)     -> (Just x, Nothing)
    _         -> (Nothing, Nothing)

-- HC4T8 - Task 8: describeTuple with Show constraints
describeTuple :: (Show a, Show b) => (a, b) -> String
describeTuple (x, y) = "First element: " ++ show x ++ ", Second element: " ++ show y

-- Main function demonstrating all tasks
main :: IO ()
main = do
    -- HC4T1
    putStrLn "HC4T1 - Weather Report:"
    putStrLn $ weatherReport "sunny"
    putStrLn $ weatherReport "rainy"
    putStrLn $ weatherReport "snowy"
    putStrLn ""

    -- HC4T2
    putStrLn "HC4T2 - Day Type:"
    putStrLn $ dayType "Monday"
    putStrLn $ dayType "Sunday"
    putStrLn $ dayType "Funday"
    putStrLn ""

    -- HC4T3
    putStrLn "HC4T3 - Grade Comments:"
    print $ gradeComment 95
    print $ gradeComment 75
    print $ gradeComment 55
    print $ gradeComment 30
    print $ gradeComment 110
    putStrLn ""

    -- HC4T4
    putStrLn "HC4T4 - Special Birthday (Pattern Matching):"
    putStrLn $ specialBirthday "Alice"
    putStrLn $ specialBirthday "Charlie"
    putStrLn ""

    -- HC4T5
    putStrLn "HC4T5 - Enhanced Special Birthday:"
    putStrLn $ specialBirthdayEnhanced "Alice" 30
    putStrLn $ specialBirthdayEnhanced "David" 40
    putStrLn ""

    -- HC4T6
    putStrLn "HC4T6 - List Contents:"
    putStrLn $ whatsInsideThisList []
    putStrLn $ whatsInsideThisList [1]
    putStrLn $ whatsInsideThisList [1, 2]
    putStrLn $ whatsInsideThisList [1, 2, 3]
    putStrLn ""

    -- HC4T7
    putStrLn "HC4T7 - First and Third Elements:"
    print $ firstAndThird [1, 2, 3, 4]
    print $ firstAndThird ["a", "b", "c"]
    print $ firstAndThird ([] :: [Int])
    putStrLn ""

    -- HC4T8
    putStrLn "HC4T8 - Describe Tuple:"
    putStrLn $ describeTuple (10, "apple")
    putStrLn $ describeTuple ("Hello", True)
