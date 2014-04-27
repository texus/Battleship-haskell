import Data.Char (ord)
import Data.List (permutations)


type Coordinate = (Int, Int)
type Ship = [Coordinate]
type Field = [[Bool]]
type Player = String


fieldSize = 10
minShipSize = 2
maxShipSize = 5


-- Select the n-th element in a list
select :: Int -> [a] -> a
select n xs = head (drop (n-1) (take n xs))

-- Change n-th element in a list
replace :: Int -> [a] -> a -> [a]
replace n xs x = take (n-1) xs ++ [x] ++ drop n xs

-- Initialize the 10x10 field
initField :: Field
initField = take fieldSize (repeat (take fieldSize (repeat False)))

-- Extract the coordinate from the string
-- Also immediately convert the coordinate from range [0,10[ to [1,10]
-- An invalid coordinate is returned when the string isn't of the correct style.
convertStringToCoordinates :: String -> Coordinate
convertStringToCoordinates ['(', x, ',', y, ')'] = ((ord x) - (ord '0') + 1, (ord y) - (ord '0') + 1)
convertStringToCoordinates _ = (-1, -1)

-- Split a string containing coordinates seperated by semi-colons into a list of (unchecked) coordinates.
-- You must still call convertStringToCoordinates on every element in the returned list.
splitCoordinatesInString :: String -> [String]
splitCoordinatesInString [] = [[]]
splitCoordinatesInString (x:xs) = if x == ';' then
                                      [] : splitCoordinatesInString xs
                                  else
                                      (x : head (splitCoordinatesInString xs)) : tail (splitCoordinatesInString xs)

-- Check if a coordinate lies inside the field
validateCoordinate :: Coordinate -> Bool
validateCoordinate coord = and [ fst coord >= 1,
                                 snd coord >= 1,
                                 fst coord <= fieldSize,
                                 snd coord <= fieldSize
                               ]

-- Make sure that the ship is given valid coordinates
validateShipCoordinates :: [Ship] -> Ship -> Int -> Bool
validateShipCoordinates placedShips ship shipLength
    | length ship /= shipLength = False -- Check if ship was given enough coordinates
    | or [coord1 == coord2 | ship2 <- placedShips, coord1 <- ship, coord2 <- ship2] = False -- The coordinates may not overlap with another ship
    | not (and [validateCoordinate coord | coord <- ship]) = False -- Check if coordinates lie in the field
    | and (map (==0) [abs ((fst coord1) - (fst coord2)) | coord1 <- ship, coord2 <- ship]) -- Check if  coordinates are neighbors (vertical)
        = (sum [abs ((snd coord1) - (snd coord2)) | coord1 <- ship, coord2 <- ship]) * 3 == (shipLength-1) * (shipLength^2 + shipLength)
    | and (map (==0) [abs ((snd coord1) - (snd coord2)) | coord1 <- ship, coord2 <- ship]) -- Check if  coordinates are neighbors (horizontal)
        = (sum [abs ((fst coord1) - (fst coord2)) | coord1 <- ship, coord2 <- ship]) * 3 == (shipLength-1) * (shipLength^2 + shipLength)
    | otherwise = False -- Coordinates are not on the same line

-- Convert the field into a printable string
convertFieldToString :: Field -> [Ship] -> Coordinate -> String
convertFieldToString field ships coordinate
        | fst coordinate <= fieldSize
          && snd coordinate <= fieldSize = if select (fst coordinate) (select (snd coordinate) field) == True then
                                               if or [coordinate == coord | ship <- ships, coord <- ship] then 'o' : convertFieldToString field ships (fst coordinate + 1, snd coordinate)
                                                   else 'x' : convertFieldToString field ships (fst coordinate + 1, snd coordinate)
                                           else ' ' : convertFieldToString field ships (fst coordinate + 1, snd coordinate)
                                        
        | snd coordinate <= fieldSize = "H\nH" ++ convertFieldToString field ships (1, snd coordinate + 1)
        | otherwise = []

-- Output the field in the terminal
printField :: String -> Field -> [Ship] -> IO ()
printField playerName field ships = do
                                      putStrLn (playerName ++ "'s field:")
                                      putStrLn (take (fieldSize+2) (repeat 'H') ++ "\nH" ++ convertFieldToString field ships (1, 1) ++ take (fieldSize+1) (repeat 'H') )
                                      putStrLn ""

-- Mark a cell on the field as shot
markShot :: Field -> Int -> Int -> Field
markShot field x y = replace x field (replace y (select x field) True)

-- Remove the ships from the list when they are destroyed
removeDestroyedShips :: [Ship] -> [Ship]
removeDestroyedShips [] = []
removeDestroyedShips (x:xs) | null x    = removeDestroyedShips xs
                            | otherwise = x : removeDestroyedShips xs

-- Check if the ship has been destroyed and remove it from the game when it is
--
-- Input:
--    field:      The field on which the ship is located
--    ship:       The ship that we should check the coordinate against
--    coordinate: The coordinate that is being shot at
--
-- Output:
--    Tuple of the ship that was given as input and a boolean that indicates if the shot was a hit or miss.
--    When the ship is sunk, an empty list will be returned instead of the ship that was given as input.
--
checkShipDestroyed :: Field -> Ship -> Coordinate -> (Ship, Bool)
checkShipDestroyed field ship coordinate = if or [coordinate == coord | coord <- ship] == False then do
                                               (ship, False)    -- Miss
                                           else do
                                               if and [select (fst coord) (select (snd coord) field) == True | coord <- ship, coord /= coordinate] == False then
                                                   (ship, True) -- Hit, but not sunk
                                               else
                                                   ([], True)   -- Hit and sunk


-- Fire a shot at a given coordinate
--
-- Input:
--    enemyField: The 10x10 field of the opponent
--    enemyShips: A list of all the opponent ships
--    coordinate: The position that we are shooting at
--
-- Output:
--    Tuple with the updated enemyField, enemyShips and a boolean to indicate a hit or miss
--
fire :: (Field, [Ship]) -> Coordinate -> (Field, [Ship], Bool)
fire (enemyField, enemyShips) coordinate = (markShot enemyField (snd coordinate) (fst coordinate),
                                            removeDestroyedShips [fst (checkShipDestroyed enemyField ship coordinate) | ship <- enemyShips],
                                            or [snd (checkShipDestroyed enemyField ship coordinate) | ship <- enemyShips])


-- Fire at the opponent once for every ship you have left
--
-- Input:
--    enemyField: Current field of the opponent
--    enemyShips: The list of all ships from the opponent
--    ourShips:   List of ship that we have left that can still fire
--
-- Output:
--    Tuple containing the updated field and ships of the opponent
--
fireWithEveryShip :: (Field, [Ship]) -> [Ship] -> IO (Field, [Ship])
fireWithEveryShip (enemyField, enemyShips) [] = return (enemyField, enemyShips)
fireWithEveryShip (enemyField, enemyShips) ourShips = do
                                                        putStrLn ("Enter the coordinates to fire shot (" ++ show (length ourShips) ++ " shots left)")
                                                        string <- getLine
                                                        let coord = convertStringToCoordinates string
                                                        if validateCoordinate coord then
                                                            do
                                                              let (newEnemyField, newEnemyShips, hit) = fire (enemyField, enemyShips) coord

                                                              if hit then
                                                                  putStrLn ("Firing at coordinate (" ++ show ((fst coord) - 1) ++ "," ++ show ((snd coord) - 1) ++ "), Hit")
                                                              else
                                                                  putStrLn ("Firing at coordinate (" ++ show ((fst coord) - 1) ++ "," ++ show ((snd coord) - 1) ++ "), Miss")

                                                              if length newEnemyShips < length enemyShips then
                                                                  do
                                                                    putStrLn "You sunk my battleship!"
                                                                    fireWithEveryShip (newEnemyField, newEnemyShips) (tail ourShips)
                                                              else
                                                                  fireWithEveryShip (newEnemyField, newEnemyShips) (tail ourShips)
                                                        else
                                                            fireWithEveryShip (enemyField, enemyShips) ourShips

-- Play the game, one turn at a time
--
-- Input:
--    names:  List of player names
--    fields: List of fields belonging to the players
--    ships:  List of ships belonging to the player
--
-- The first element in the lists, are from the player whose turn it currently is
--
play :: [String] -> [Field] -> [[Ship]] -> IO ()
play names fields ships = do
                            putStrLn ("\n" ++ head names ++ "'s turn")
                            printField (last names) (last fields) (last ships)
                            (newField, newShipList) <- fireWithEveryShip (last fields, last ships) (head ships)
                            if length newShipList == 0 then
                                do
                                  putStrLn ("\n" ++ head names ++ " won!\n")
                                  printField (last names) newField newShipList
                                  printField (head names) (head fields) (head ships)
                            else
                                play [last names, head names] [newField, head fields] [newShipList, head ships]

-- Input one ship with a given length
inputShip :: [Ship] -> Int -> IO Ship
inputShip placedShips len = do
                              putStrLn ("Enter the coordinates of the ship of length " ++ show len ++ "?")
                              string <- getLine
                              let stringCoords = splitCoordinatesInString string
                              let coords = map convertStringToCoordinates stringCoords
                              if validateShipCoordinates placedShips coords len then
                                  return coords
                              else
                                  inputShip placedShips len

-- Input all the ships for a player
inputShips :: Int -> [Ship] -> IO [Ship]
inputShips shipSize placedShips = if shipSize <= maxShipSize then
                                      do
                                        ship <- inputShip placedShips shipSize
                                        shipList <- inputShips (shipSize + 1) (ship : placedShips)
                                        return (ship : shipList)
                                  else
                                      return []

-- Input the names of the players
inputNames :: IO [String]
inputNames = do
               putStrLn "What is the name of the first player?"
               name1 <- getLine
               putStrLn "What is the name of the second player?"
               name2 <- getLine
               return [name1, name2]

-- The entry point of the program
main :: IO ()
main = do
         names <- inputNames

         putStrLn (head names ++ ", enter your ships")
         shipsPlayer1 <- inputShips minShipSize []

         putStrLn (last names ++ ", enter your ships")
         shipsPlayer2 <- inputShips minShipSize []

         play names [initField, initField] [shipsPlayer1, shipsPlayer2]

