data Base = Decimal | Hexadecimal | Binary 
    deriving (Show, Eq)

decToBin :: Int -> String
decToBin 0 = "0"
decToBin x = decBinHelper x
    where
        decBinHelper 0 = ""
        decBinHelper x  
            | mod x 2 == 0 = decBinHelper (div x 2) ++ "0" 
            | otherwise = decBinHelper (div x 2) ++ "1"

binToDec :: String -> String
binToDec xs = show (decHelper xs)
    where
        decHelper [] = 0 -- Bad length calculation down here, but no reverse. Actually, bad code everywhere :)
        decHelper (x:xs) = (read [x]) * (2 ^ (length xs)) + decHelper xs 

decToHex :: Int -> String
decToHex 0 = "0"
decToHex x = decHexHelper x
    where
        decHexHelper 0 = ""
        decHexHelper x = decHexHelper (div x 16) ++ hexChar (mod x 16)
        hexChar n
            | n < 10 = show n
            | n == 10 = "A"
            | n == 11 = "B"
            | n == 12 = "C"
            | n == 13 = "D"
            | n == 14 = "E"
            | n == 15 = "F"

hexToDec :: String -> String
hexToDec xs = show (hexToDecHelper xs)
    where 
        hexToDecHelper [] = 0
        hexToDecHelper (x:xs) = (hexBinChar x) * (16 ^ (length xs)) + hexToDecHelper xs
        hexBinChar '0' = 0
        hexBinChar '1' = 1
        hexBinChar '2' = 2
        hexBinChar '3' = 3
        hexBinChar '4' = 4
        hexBinChar '5' = 5
        hexBinChar '6' = 6
        hexBinChar '7' = 7
        hexBinChar '8' = 8
        hexBinChar '9' = 9
        hexBinChar 'A' = 10
        hexBinChar 'B' = 11
        hexBinChar 'C' = 12
        hexBinChar 'D' = 13
        hexBinChar 'E' = 14
        hexBinChar 'F' = 15
        hexBinChar 'a' = 10
        hexBinChar 'b' = 11
        hexBinChar 'c' = 12
        hexBinChar 'd' = 13
        hexBinChar 'e' = 14
        hexBinChar 'f' = 15

convert :: (Base, Base) -> String -> String
convert (Decimal, Binary) str = decToBin (read str)
convert (Binary, Decimal) str = binToDec str
convert (Decimal, Hexadecimal) str = decToHex (read str) 
convert (Hexadecimal, Decimal) str = hexToDec str
convert (Binary, Hexadecimal) str = decToHex (read (binToDec str))
convert (Hexadecimal, Binary) str = decToBin (read (hexToDec str))

validateInput :: Char -> Maybe (Base, Base)
validateInput '1' = Just (Decimal, Binary)
validateInput '2' = Just (Binary, Decimal)
validateInput '3' = Just (Decimal, Hexadecimal)
validateInput '4' = Just (Hexadecimal, Decimal)
validateInput '5' = Just (Binary, Hexadecimal)
validateInput '6' = Just (Hexadecimal, Binary)
validateInput _ = Nothing

main :: IO ()
main = do
    
    putStr "\ESC[2J\ESC[H"   
    putStrLn "| 1 (Int to Binary) | 2 (Binary to Int) | 3 (Int to Hex) | 4 (Hex to Int) | 5 (Binary to Hex) | 6 (Hex to Binary) |"
    putStr "Choose number: "
    input <- getLine
    putStr "\n"

    case input of 
        [c] ->
            case validateInput c of
                
                Just (from, to) -> do
                    putStrLn "Enter number to convert: "
                    number <- getLine
                    putStr "\n"
                    putStrLn "Result:"
                    putStrLn ("(" ++ show from ++ " to " ++ show to ++ ") " ++ "Number: " ++ show number ++ " | " ++ convert (from, to) number) 
                    putStr "\n"

                Nothing -> do
                        putStrLn "Incorrect character"
        _ ->
            putStrLn "Incorrect character"
    
    putStrLn "Press Enter to continue..."   
    _ <- getLine
    putStr "\ESC[2J\ESC[H"   
    main
