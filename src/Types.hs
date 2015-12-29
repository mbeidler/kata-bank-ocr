{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types
    ( Digit
    , zero
    , one
    , two
    , three
    , four
    , five
    , six
    , seven
    , eight
    , nine
    , Account
    , fromList
    , Verified
    , verify
    , verified
    ) where

newtype Digit = Digit { unDigit :: Int } deriving (Enum, Eq, Ord)

instance Show Digit where
    show = show . unDigit

zero, one, two, three, four, five, six, seven, eight, nine :: Digit
zero  = Digit 0
one   = Digit 1
two   = Digit 2
three = Digit 3
four  = Digit 4
five  = Digit 5
six   = Digit 6
seven = Digit 7
eight = Digit 8
nine  = Digit 9

newtype Account = Account { account :: [Digit] } deriving (Eq)

instance Show Account where
    show = concatMap show . account

fromList :: [Digit] -> Maybe Account
fromList ds | length ds == 9 = Just $ Account ds
            | otherwise      = Nothing

newtype Verified = Verified { verified :: Account } deriving (Eq)

instance Show Verified where
    show = show . verified

verify :: Account -> Maybe Verified
verify a | isValid a = Just $ Verified a
         | otherwise = Nothing

isValid :: Account -> Bool
isValid (Account ds) =
    (sum $ zipWith (*) [9,8..1] (map unDigit ds)) `mod` 11 == 0
