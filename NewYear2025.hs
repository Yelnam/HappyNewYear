-- Custom type to represent different kinds of years
data YearType = LeapYear | OlympicYear | RegularYear
    deriving Show

-- Function to determine year type using pattern matching
getYearType :: Int -> YearType
getYearType year
    | isLeapYear = LeapYear
    | isOlympicYear = OlympicYear
    | otherwise = RegularYear
    where
        isLeapYear = (year `mod` 4 == 0 && year `mod` 100 /= 0) 
                     || year `mod` 400 == 0
        isOlympicYear = year `mod` 4 == 0

-- Safe function to check if a year is in the future
-- Demonstrates Maybe monad usage
isFutureYear :: Int -> Int -> Maybe Bool
isFutureYear currentYear inputYear
    | inputYear < 0 = Nothing  -- Invalid year
    | otherwise = Just (inputYear > currentYear)

-- Generate message based on year type
yearMessage :: YearType -> String
yearMessage LeapYear = "Happy New Year! That's a leap year - you get an extra day!"
yearMessage OlympicYear = "Happy New Year! An Olympic year - let the games begin!"
yearMessage RegularYear = "Happy New Year! A regular year, but still full of possibilities!"

-- Main function to test our code
main :: IO ()
main = do
    let testYear = 2025
        yearType = getYearType testYear
    putStrLn $ "Analysis of year " ++ show testYear ++ ":"
    putStrLn $ yearMessage yearType
