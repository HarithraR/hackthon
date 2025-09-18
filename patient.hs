-- Hospital Patient Records Analysis
-- (Name, Age, ReasonCode)

type Patient = (String, Int, Int)

-- Sample data
samplePatients :: [Patient]
samplePatients =
  [("Alice", 30, 1),
   ("Bob", 45, 2),
   ("Charlie", 17, 1),
   ("David", 60, 3),
   ("Eve", 25, 2)]

-- Count patients in a given ReasonCode (recursive)
countReason :: Int -> [Patient] -> Int
countReason _ [] = 0
countReason code ((_,_,r):rest)
  | r == code = 1 + countReason code rest
  | otherwise = countReason code rest

-- Count adults (recursive)
countAdults :: [Patient] -> Int
countAdults [] = 0
countAdults ((_,age,_):rest)
  | age >= 18 = 1 + countAdults rest
  | otherwise = countAdults rest

-- Split helper (no libraries)
split :: Char -> String -> [String]
split _ "" = [""]
split delim (c:cs)
  | c == delim = "" : split delim cs
  | otherwise  = (c : head parts) : tail parts
  where parts = split delim cs

-- Parse input like "John:34:2"
parsePatient :: String -> Patient
parsePatient str =
  let [n,a,r] = split ':' str
  in (n, read a, read r)

-- Count until empty input
readPatients :: IO [Patient]
readPatients = do
  line <- getLine
  if null line
    then return []
    else do
      rest <- readPatients
      return (parsePatient line : rest)

-- Main program
main :: IO ()
main = do
  putStrLn "Enter patient records (Name:Age:Reason), one per line. Empty line to stop."
  patients <- readPatients
  let records = if null patients then samplePatients else patients

  putStrLn "\n--- Hospital Report ---"
  putStrLn ("General Checkup: " ++ show (countReason 1 records))
  putStrLn ("Emergency: "       ++ show (countReason 2 records))
  putStrLn ("Surgery: "         ++ show (countReason 3 records))
  putStrLn ("Total Adults: "    ++ show (countAdults records))
