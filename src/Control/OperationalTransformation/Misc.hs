-- This is some random code I found on the internet
-- data JSONNumberOperation = Add Path Int
--                          deriving Show

-- data JSONStringOperation = StringInsert Path Position Text
--                          | StringDelete Path Position Text
--                          deriving Show

-- data JSONArrayOperation = ArrayInsert Path Position Value
--                         | ArrayDelete Path Position Value
--                         | ArrayReplace Path Position Value Value
--                         | ArrayMove Path Position Int
--                         deriving Show

-- data JSONObjectOperation = Insert Path Property Value
--                          | Delete Path Property
--                          | Replace Path Property Value Value
--                          deriving Show

-- instance FromJSON JSONNumberOperation where
--   parseJSON (Object v) = Add <$> (v .: "p") <*> (v .: "na")
--   parseJSON _          = fail "Not an Object"
