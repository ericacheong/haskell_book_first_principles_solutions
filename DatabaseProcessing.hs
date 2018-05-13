import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = 
    [ DbDate (UTCTime 
               (fromGregorian 1911 5 1)
               (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime
                (fromGregorian 1921 5 1)
                (secondsToDiffTime 34123)        
               )
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate [] = []
filterDbDate (DbDate time:xs) = time : filterDbDate xs
filterDbDate (_:xs) = filterDbDate xs

filterDbDbNumber :: [DatabaseItem] -> [Integer]
filterDbDbNumber [] = []
filterDbDbNumber (DbNumber i:xs) = i : filterDbDbNumber xs
filterDbDbNumber (_:xs) = filterDbDbNumber xs

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent x = maximum $ filterDbDate x

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb x = fromIntegral (sumDb x `div` (fromIntegral (length x) :: Integer)) :: Double