-- | had to install cabal split package
import Data.List.Split

-- | returns the first line for column values
getColumns :: String -> String
getColumns =  head . lines

-- | returns everything after columns for values
getValues :: String -> [String]
getValues = tail . lines

-- | recursive function each call writes a json row
writeObject :: [String] -> [String] -> Int -> IO ()
writeObject xs xy i =
  if i > 0
    then
      do
        let colLen = length xy
        appendFile "practice.json" ("  {\n")
        printTuple (splitOn "," $ xs!!(i - 1)) xy colLen
        if i > 1
          then 
            appendFile "practice.json" ",\n"
          else
            appendFile "practice.json" "\n"
        writeObject xs xy (i - 1)
    else
      appendFile "practice.json" "]"

-- | recursive function writes tuples in the row
printTuple :: [String] -> [String] -> Int -> IO ()
printTuple xs xy i =
  if i > 0
    then
      do
        appendFile "practice.json" ("    " ++ xy!!(i - 1))
        appendFile "practice.json" (": " ++ xs!!(i - 1) ++ "\n")
        printTuple xs xy (i - 1)
    else
      appendFile "practice.json" ("  }")

main = do
  -- | bind file to string
  contents <- readFile "User.csv"
  -- | separate columns from values
  let columns = splitOn "," $ getColumns contents
  let values = getValues contents
 -- | take length of values for iteration
  let valLen = length values
 -- | begin writing the file
  writeFile "practice.json" "[\n"
  writeObject values columns valLen