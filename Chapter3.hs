-- ============================
-- HC3 Combined Tasks
-- ============================

import Text.Printf (printf)

-- HC3T1: Check if a number is positive, negative, or zero
checkNumber :: Int -> String
checkNumber n =
    if n > 0 then "Positive"
    else if n < 0 then "Negative"
    else "Zero"

-- HC3T2: Determine grade using guards
grade :: Int -> String
grade n
    | n >= 90 = "A"
    | n >= 80 = "B"
    | n >= 70 = "C"
    | n >= 60 = "D"
    | otherwise = "F"

-- HC3T3: Convert RGB to Hex
rgbToHex :: (Int, Int, Int) -> String
rgbToHex (r,g,b) =
    let rh = printf "%02X" r
        gh = printf "%02X" g
        bh = printf "%02X" b
    in "#" ++ rh ++ gh ++ bh

-- HC3T4: Triangle Area (Heron's formula)
triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c =
    let s = (a+b+c) / 2
    in sqrt (s * (s-a) * (s-b) * (s-c))

-- HC3T5: Triangle Type
triangleType :: Float -> Float -> Float -> String
triangleType a b c
    | a == b && b == c = "Equilateral"
    | a == b || b == c || a == c = "Isosceles"
    | otherwise = "Scalene"

-- HC3T6: Leap Year
isLeapYear :: Int -> Bool
isLeapYear year =
    if year `mod` 400 == 0 then True
    else if year `mod` 100 == 0 then False
    else if year `mod` 4 == 0 then True
    else False

-- HC3T7: Season by month
season :: Int -> String
season m
    | m == 12 || m == 1 || m == 2 = "Winter"
    | m == 3 || m == 4 || m == 5 = "Spring"
    | m == 6 || m == 7 || m == 8 = "Summer"
    | m == 9 || m == 10 || m == 11 = "Autumn"
    | otherwise = "Invalid month"

-- HC3T8: BMI Category
bmiCategory :: Float -> Float -> String
bmiCategory weight height
    | bmi < 18.5 = "Underweight"
    | bmi < 25 = "Normal"
    | bmi < 30 = "Overweight"
    | otherwise = "Obese"
  where
    bmi = weight / (height ^ 2)

-- HC3T9: Max of Three
maxOfThree :: Int -> Int -> Int -> Int
maxOfThree a b c =
    let max1 = max a b
        max2 = max max1 c
    in max2

-- HC3T10: Palindrome
isPalindrome :: String -> Bool
isPalindrome str
    | length str <= 1 = True
    | head str == last str = isPalindrome (init (tail str))
    | otherwise = False

-- ============================
-- MAIN PROGRAM
-- ============================
main :: IO ()
main = do
    putStrLn "=== HC3T1: Check Number ==="
    print (checkNumber 5)
    print (checkNumber (-3))
    print (checkNumber 0)

    putStrLn "\n=== HC3T2: Grade ==="
    print (grade 95)
    print (grade 72)
    print (grade 50)

    putStrLn "\n=== HC3T3: RGB to Hex ==="
    print (rgbToHex (255,0,127))
    print (rgbToHex (0,255,64))

    putStrLn "\n=== HC3T4: Triangle Area ==="
    print (triangleArea 3 4 5)
    print (triangleArea 7 8 9)

    putStrLn "\n=== HC3T5: Triangle Type ==="
    print (triangleType 3 3 3)
    print (triangleType 5 5 8)
    print (triangleType 6 7 8)

    putStrLn "\n=== HC3T6: Leap Year ==="
    print (isLeapYear 2000)
    print (isLeapYear 1900)
    print (isLeapYear 2024)

    putStrLn "\n=== HC3T7: Season ==="
    print (season 3)
    print (season 7)
    print (season 11)

    putStrLn "\n=== HC3T8: BMI Category ==="
    print (bmiCategory 70 1.75)
    print (bmiCategory 90 1.8)

    putStrLn "\n=== HC3T9: Max of Three ==="
    print (maxOfThree 10 20 15)
    print (maxOfThree 5 25 10)

    putStrLn "\n=== HC3T10: Palindrome ==="
    print (isPalindrome "racecar")
    print (isPalindrome "haskell")
    print (isPalindrome "madam")
