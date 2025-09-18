-- Cinema Ticket Sales Report
-- (Category, Quantity)

type Sale = (String, Int)

-- Sample data (used if user enters nothing)
sampleSales :: [Sale]
sampleSales =
  [("Adult", 3),
   ("Child", 5),
   ("Senior", 2),
   ("Adult", 1),
   ("Child", 2)]

-- Ticket prices
ticketPrice :: String -> Int
ticketPrice "Adult"  = 12
ticketPrice "Child"  = 8
ticketPrice "Senior" = 10
ticketPrice _        = 0   -- fallback

-- Count total tickets for a given category (recursion)
countCategory :: String -> [Sale] -> Int
countCategory _ [] = 0
countCategory cat ((c,q):rest)
  | c == cat  = q + countCategory cat rest
  | otherwise = countCategory cat rest

-- Calculate total revenue (recursion)
totalRevenue :: [Sale] -> Int
totalRevenue [] = 0
totalRevenue ((c,q):rest) =
  (ticketPrice c * q) + totalRevenue rest

-- Split helper (to parse input like "Adult:3")
split :: Char -> String -> [String]
split _ "" = [""]
split delim (c:cs)
  | c == delim = "" : split delim cs
  | otherwise  = (c : head parts) : tail parts
  where parts = split delim cs

-- Parse input "Category:Quantity"
parseSale :: String -> Sale
parseSale str =
  let [c,q] = split ':' str
  in (c, read q)

-- Read user input until empty line
readSales :: IO [Sale]
readSales = do
  line <- getLine
  if null line
    then return []
    else do
      rest <- readSales
      return (parseSale line : rest)

-- Main program
main :: IO ()
main = do
  putStrLn "Enter sales (Category:Quantity), one per line. Empty line to stop."
  sales <- readSales
  let records = if null sales then sampleSales else sales

  putStrLn "\n--- Cinema Ticket Sales Report ---"
  putStrLn ("Adult tickets:  " ++ show (countCategory "Adult" records))
  putStrLn ("Child tickets:  " ++ show (countCategory "Child" records))
  putStrLn ("Senior tickets: " ++ show (countCategory "Senior" records))
  putStrLn ("Total revenue: $" ++ show (totalRevenue records))
