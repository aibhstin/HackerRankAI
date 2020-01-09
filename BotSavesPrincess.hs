module Main where
getList :: Int -> IO[String]
getList n = if n==0 then return [] else do i <- getLine; is <- getList(n-1); return (i:is)

getColOf c n (x:xs)
    | c == x = n
    | otherwise = getColOf c (n+1) xs

getRowOf c n (x:xs)
    | elem c x = n
    | otherwise = getRowOf c (n+1) xs

rowDir x = if x < 0 then "DOWN\n" else "UP\n"
colDir x = if x < 0 then "RIGHT\n" else "LEFT\n"

displayPathtoPrincess :: Int -> [String] -> String
displayPathtoPrincess _ coords = 
    concat $ (take (abs rowDelta) (repeat $ rowDir rowDelta)) ++ (take (abs colDelta) (repeat $ colDir colDelta))
    where
        playerRow = getRowOf 'm' 0 coords
        playerCol = getColOf 'm' 0 (coords !! playerRow)
        princessRow = getRowOf 'p' 0 coords
        princessCol = getColOf 'p' 0 (coords !! princessRow)
        rowDelta = playerRow - princessRow
        colDelta = playerCol - princessCol
    

main = do
    n <- getLine
    let i = read n
    grid <- getList i
    putStrLn.displayPathtoPrincess i $ grid
