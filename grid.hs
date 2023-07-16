-- Function to update the grid based on the current position
updateGrid :: [[String]] -> (Int, Int) -> [[String]]
updateGrid grid (x, y) = [[if (i, j) == (x, y) then "X" else "_" | (j, cell) <- zip [0..] row] | (i, row) <- zip [0..] grid]

-- Function to print the grid
printGrid :: [[String]] -> IO ()
printGrid grid = mapM_ (putStrLn . unwords) grid

-- Function to check if a position is within the grid boundaries
isValidPosition :: (Int, Int) -> Bool
isValidPosition (x, y) = x >= 0 && x < 5 && y >= 0 && y < 5

-- Function to move the point based on the user's input
movePoint :: (Int, Int) -> String -> (Int, Int)
movePoint (x, y) direction
  | direction == "left"    = if isValidPosition (x, y - 1) then (x, y - 1) else (x, y)
  | direction == "right"  = if isValidPosition (x, y + 1) then (x, y + 1) else (x, y)
  | direction == "up"  = if isValidPosition (x - 1, y) then (x - 1, y) else (x, y)
  | direction == "down" = if isValidPosition (x + 1, y) then (x + 1, y) else (x, y)
  | otherwise            = (x, y)

-- Main program
main :: IO ()
main = do
  let initialGrid = replicate 5 (replicate 5 "_")
      initialPosition = (2, 2)
  putStrLn "Welcome to the grid point movement program!"
  putStrLn "Enter 'up', 'down', 'left', or 'right' to move the point."
  putStrLn "Enter 'quit' to exit the program."
  loop initialGrid initialPosition

-- Recursive loop for user interaction
loop :: [[String]] -> (Int, Int) -> IO ()
loop grid pos = do
  putStrLn "Current grid:"
  printGrid grid
  putStrLn "Enter direction ('up', 'down', 'left', 'right') or 'quit':"
  direction <- getLine
  let newPos = movePoint pos direction
      newGrid = updateGrid grid newPos
  if direction == "quit"
    then putStrLn "Exiting the program. Goodbye!"
    else loop newGrid newPos
