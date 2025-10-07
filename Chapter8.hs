-- Combined HC8T1 to HC8T10 Code

-- HC8T1: Type synonyms and main function
type Address = String
type Value = Int

generateTx :: Address -> Address -> Value -> String
generateTx addr1 addr2 val = "From: " ++ addr1 ++ ", To: " ++ addr2 ++ ", Value: " ++ show val

-- HC8T2: New types and data constructors
data PaymentMethod = Cash | Card | Cryptocurrency deriving (Show)

data Person = Person
  { name :: String
  , address :: (String, Int)
  , paymentMethod :: PaymentMethod
  } deriving (Show)

bob :: Person
bob = Person { name = "Bob", address = ("123 Elm St", 456), paymentMethod = Cash }

-- HC8T3: Algebraic Data Types and functions
data Shape = Circle Float | Rectangle Float Float deriving (Show)

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h

circleRadius :: Float
circleRadius = 5

circleArea :: Float
circleArea = area (Circle circleRadius)

rectangleWidth :: Float
rectangleWidth = 10

rectangleHeight :: Float
rectangleHeight = 5

rectangleArea :: Float
rectangleArea = area (Rectangle rectangleWidth rectangleHeight)

-- HC8T4: Record syntax for Employee
data Employee = Employee
  { employeeName :: String
  , experienceInYears :: Float
  } deriving (Show)

richard :: Employee
richard = Employee { employeeName = "Richard", experienceInYears = 7.5 }

-- HC8T5: Record syntax for Person with employment status
data PersonHC8T5 = PersonHC8T5
  { personName :: String
  , age :: Int
  , isEmployed :: Bool
  } deriving (Show)

person1 :: PersonHC8T5
person1 = PersonHC8T5 { personName = "Alice", age = 30, isEmployed = True }

person2 :: PersonHC8T5
person2 = PersonHC8T5 { personName = "Bob", age = 45, isEmployed = False }

-- HC8T6: Record syntax for Shape variants
data ShapeHC8T6
  = ShapeCircle { center :: (Float, Float), color :: String, radius :: Float }
  | ShapeRectangle { topLeft :: (Float, Float), width :: Float, height :: Float, rectColor :: String }
  deriving (Show)

circleShape :: ShapeHC8T6
circleShape = ShapeCircle { center = (0, 0), color = "red", radius = 10 }

rectangleShape :: ShapeHC8T6
rectangleShape = ShapeRectangle { topLeft = (0, 0), width = 20, height = 10, rectColor = "blue" }

-- HC8T7: Data types and describing animals
data Animal = Dog String | Cat String deriving (Show)

describeAnimal :: Animal -> String
describeAnimal (Dog n) = "Dog named " ++ n
describeAnimal (Cat n) = "Cat named " ++ n

dog :: Animal
dog = Dog "Buddy"

cat :: Animal
cat = Cat "Whiskers"

-- HC8T8: Type synonyms and greeting function
type Name = String
type Age = Int

greet :: Name -> Age -> String
greet n a = "Hello, " ++ n ++ "! You are " ++ show a ++ " years old."

-- HC8T9: Record type Transaction
type TxAddress = String
type TxValue = Int

data Transaction = Transaction
  { from :: TxAddress
  , to :: TxAddress
  , amount :: TxValue
  , transactionId :: String
  } deriving (Show)

createTransaction :: TxAddress -> TxAddress -> TxValue -> String -> String
createTransaction fromAddr toAddr val txId =
  let tx = Transaction { from = fromAddr, to = toAddr, amount = val, transactionId = txId }
  in "Transaction ID: " ++ transactionId tx

-- HC8T10: Algebraic Data Types for Vehicle
data Vehicle = Car { make :: String, year :: Int }
             | Bike { brand :: String, isElectric :: Bool }
             deriving (Show)

showVehicle :: Vehicle -> String
showVehicle (Car make year) = "Car: " ++ make ++ ", Year: " ++ show year
showVehicle (Bike brand electric) =
  "Bike: " ++ brand ++ ", Electric: " ++ show electric

myCar :: Vehicle
myCar = Car { make = "Toyota", year = 2020 }

myBike :: Vehicle
myBike = Bike { brand = "Tesla Bike", isElectric = True }

-- Main function to demonstrate all HC8T snippets
main :: IO ()
main = do
  -- HC8T1 output
  putStrLn "HC8T1: Generate Transaction String:"
  putStrLn $ generateTx "Alice" "Bob" 100

  -- HC8T2 output
  putStrLn "\nHC8T2: Person Bob:"
  print bob

  -- HC8T3 output
  putStrLn "\nHC8T3: Areas of Shapes:"
  putStrLn $ "Circle with radius 5: " ++ show circleArea
  putStrLn $ "Rectangle 10x5: " ++ show rectangleArea

  -- HC8T4 output
  putStrLn "\nHC8T4: Employee Richard:"
  print richard

  -- HC8T5 output
  putStrLn "\nHC8T5: Person1 and Person2:"
  print person1
  print person2

  -- HC8T6 output
  putStrLn "\nHC8T6: Shape Instances:"
  print circleShape
  print rectangleShape

  -- HC8T7 output
  putStrLn "\nHC8T7: Animal Descriptions:"
  putStrLn $ describeAnimal dog
  putStrLn $ describeAnimal cat

  -- HC8T8 output
  putStrLn "\nHC8T8: Greeting:"
  putStrLn $ greet "John" 28

  -- HC8T9 output
  putStrLn "\nHC8T9: Transaction String:"
  putStrLn $ createTransaction "Addr1" "Addr2" 250 "TX12345"

  -- HC8T10 output
  putStrLn "\nHC8T10: Vehicle Info:"
  putStrLn $ showVehicle myCar
  putStrLn $ showVehicle myBike
