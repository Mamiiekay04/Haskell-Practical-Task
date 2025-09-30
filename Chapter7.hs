{-# LANGUAGE FlexibleInstances #-}

import Text.Read (readMaybe)

-- HC7T1: Eq instance for Color
data Color = Red | Green | Blue deriving (Show)

instance Eq Color where
    Red   == Red   = True
    Green == Green = True
    Blue  == Blue  = True
    _     == _     = False

-- HC7T2: Ord instance for Color
instance Ord Color where
    compare Red Red = EQ
    compare Red _   = LT
    compare Green Red = GT
    compare Green Green = EQ
    compare Green Blue = LT
    compare Blue Blue = EQ
    compare Blue _ = GT

-- HC7T7: Bounded and Enum for Color
instance Bounded Color where
    minBound = Red
    maxBound = Blue

instance Enum Color where
    toEnum 0 = Red
    toEnum 1 = Green
    toEnum 2 = Blue
    toEnum _ = error "Invalid color index"

    fromEnum Red = 0
    fromEnum Green = 1
    fromEnum Blue = 2

nextColor :: Color -> Color
nextColor c
    | c == maxBound = minBound
    | otherwise     = succ c

-- HC7T3: compareValues function
compareValues :: (Eq a, Ord a) => a -> a -> a
compareValues x y = if x >= y then x else y

-- HC7T4: Shape type with Show, Read, Eq, and Ord
data Shape = Circle Double | Rectangle Double Double deriving (Show, Read, Eq)

-- HC7T4: Ord instance for Shape based on area
instance Ord Shape where
    compare s1 s2 = compare (area s1) (area s2)
      where
        area (Circle r) = pi * r * r
        area (Rectangle w h) = w * h

-- parseShape function
parseShape :: String -> Maybe Shape
parseShape str = readMaybe str

-- HC7T5: Square area
squareArea :: Num a => a -> a
squareArea side = side * side

-- HC7T6: circleCircumference
circleCircumference :: (Floating a) => a -> a
circleCircumference r = 2 * pi * r

-- HC7T8: Describable class and instances
class Describable a where
    describe :: a -> String

instance Describable Bool where
    describe True  = "It's True!"
    describe False = "It's False!"

instance Describable Shape where
    describe (Circle r) = "Circle with radius " ++ show r
    describe (Rectangle w h) = "Rectangle " ++ show w ++ " x " ++ show h

-- HC7T9: Wrapper for Int to implement Describable
newtype DescrInt = DescrInt Int deriving (Show, Eq, Ord)

instance Describable DescrInt where
    describe (DescrInt n) = show n

-- HC7T10: Function with multiple constraints
describeAndCompare :: (Describable a, Ord a) => a -> a -> String
describeAndCompare x y =
    let bigger = if x >= y then x else y
    in "Bigger: " ++ describe bigger

-- MAIN: Demonstration of all features
main :: IO ()
main = do
    -- HC7T1 & HC7T2: Color equality and comparison
    putStrLn $ "HC7T1: Color equality (Red == Red): " ++ show (Red == Red)
    putStrLn $ "HC7T2: Is Red < Green? " ++ show (Red < Green)
    putStrLn $ "HC7T2: Compare Green and Blue: " ++ show (compare Green Blue)

    -- HC7T7: nextColor
    putStrLn $ "HC7T7: Next color after Red: " ++ show (nextColor Red)
    putStrLn $ "HC7T7: Next color after Blue: " ++ show (nextColor Blue)

    -- HC7T3: compareValues
    putStrLn $ "HC7T3: Larger of 5 and 10: " ++ show (compareValues (5 :: Int) 10)

    -- HC7T4: Shapes and parsing
    let shapeStr1 = "Circle 3.5"
        shapeStr2 = "Rectangle 4.5 6.0"
        shapeStr3 = "InvalidShape"
    putStrLn $ "HC7T4: Parse '" ++ shapeStr1 ++ "': " ++ show (parseShape shapeStr1)
    putStrLn $ "HC7T4: Parse '" ++ shapeStr2 ++ "': " ++ show (parseShape shapeStr2)
    putStrLn $ "HC7T4: Parse '" ++ shapeStr3 ++ "': " ++ show (parseShape shapeStr3)

    -- HC7T5: Square area
    putStrLn $ "HC7T5: Area of square with side 4: " ++ show (squareArea 4)

    -- HC7T6: Circumference
    putStrLn $ "HC7T6: Circumference of circle with radius 5: " ++ show (circleCircumference 5)

    -- HC7T8: Describable instances
    putStrLn $ "HC7T8: Boolean True: " ++ describe True
    putStrLn $ "HC7T8: Shape: " ++ describe (Circle 5.0)

    -- HC7T9: Comparing Int wrapped in DescrInt
    let a = DescrInt 7
        b = DescrInt 3
    putStrLn $ "HC7T9: Comparing Ints 7 and 3: " ++ describeAndCompare a b

    -- HC7T10: Comparing Shapes based on area
    let shape1 = Circle 4
        shape2 = Circle 5
    putStrLn $ "HC7T10: Comparing Shapes: " ++ describeAndCompare shape1 shape2
