-- Student Academic Performance Report
-- (StudentName, Mark)

type Student = (String, Int)

-- Sample data
sampleStudents :: [Student]
sampleStudents =
  [("Alice", 35),
   ("Bob", 55),
   ("Charlie", 65),
   ("David", 82),
   ("Eve", 40)]

-- Classify a mark using guards
classify :: Int -> String
classify m
  | m < 40    = "Fail"
  | m < 60    = "Pass"
  | m < 80    = "Merit"
  | otherwise = "Distinction"

-- Build categorized list (Name, Mark, Category) recursively
categorize :: [Student] -> [(String, Int, String)]
categorize [] = []
categorize ((n,m):rest) =
  (n, m, classify m) : categorize rest

-- Count how many students passed (mark >= 40), recursive
countPassed :: [Student] -> Int
countPassed [] = 0
countPassed ((_,m):rest)
  | m >= 40   = 1 + countPassed rest
  | otherwise = countPassed rest

-- Split helper (to parse input "Name:Mark")
split :: Char -> String -> [String]
split _ "" = [""]
split delim (c:cs)
  | c == delim = "" : split delim cs
  | otherwise  = (c : head parts) : tail parts
  where parts = split delim cs

-- Parse input "Name:Mark"
parseStudent :: String -> Student
parseStudent str =
  let [n,m] = split ':' str
  in (n, read m)

-- Read user input until empty line
readStudents :: IO [Student]
readStudents = do
  line <- getLine
  if null line
    then return []
    else do
      rest <- readStudents
      return (parseStudent line : rest)

-- Display categorized list nicely
printCategorized :: [(String, Int, String)] -> IO ()
printCategorized [] = return ()
printCategorized ((n,m,c):rest) = do
  putStrLn (n ++ " - " ++ show m ++ " - " ++ c)
  printCategorized rest

-- Main program
main :: IO ()
main = do
  putStrLn "Enter student records (Name:Mark), one per line. Empty line to stop."
  students <- readStudents
  let records = if null students then sampleStudents else students
  let categorized = categorize records
  let passCount = countPassed records

  putStrLn "\n--- Student Academic Performance Report ---"
  printCategorized categorized
  putStrLn ("Number of students passed: " ++ show passCount)
