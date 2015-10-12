module Reader
    ( readAccount
    , Result
    ) where

import           Control.Lens ((&), (.~), (^.), at, ix)
import           Data.Either  (isRight, rights)
import           Data.List    (transpose)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe   (catMaybes, fromJust)
import           Types

prepare :: String -> [String]
prepare = map concat . transpose . map triples . init . lines
  where
    triples (a:b:c:xs) = [a,b,c] : triples xs
    triples _          = []

digitMap :: [(String, Digit)]
digitMap =
  [ (" _ | ||_|", zero)
  , ("     |  |", one)
  , (" _  _||_ ", two)
  , (" _  _| _|", three)
  , ("   |_|  |", four)
  , (" _ |_  _|", five)
  , (" _ |_ |_|", six)
  , (" _   |  |", seven)
  , (" _ |_||_|", eight)
  , (" _ |_| _|", nine) ]

readAccount :: String -> Result
readAccount = run . readDigits

readDigits :: String -> [DigitR]
readDigits = map readDigit . prepare

type DigitR = Either [Digit] Digit

readDigit :: String -> DigitR
readDigit s = maybe (Left $ near s) Right $ lookup s digitMap

report :: [DigitR] -> String
report = mapEither (const '?') (head . show)

near :: String -> [Digit]
near str = [ d | (s,d) <- digitMap, diffSum str s == 1]

diffSum :: Eq a => [a] -> [a] -> Int
diffSum xs = length . filter not . zipWith (==) xs

kin :: Map Digit [Digit]
kin = M.fromList $
  [ (zero,  [zero, eight])
  , (one,   [one, seven])
  , (two,   [two])
  , (three, [three, nine])
  , (four,  [four])
  , (five,  [five, six, nine])
  , (six,   [five, six, eight])
  , (seven, [one, seven])
  , (eight, [zero, six, eight, nine])
  , (nine,  [three, five, eight, nine])
  ]

expand :: [[Digit]] -> [Verified]
expand =  catMaybes . map ((=<<) verify . fromList) . sequence

mapEither :: (a -> c) -> (b -> c) -> [Either a b] -> [c]
mapEither f g = map (either f g)

collect :: [DigitR] -> [[Digit]]
collect = mapEither id pure

data Result = Success (Either ([DigitR], [Verified]) Verified)
            | Miss [DigitR] [Verified]

instance Show Result where
    show (Success (Right v))       = show v
    show (Success (Left (rs, vs))) = showRes rs vs
    show (Miss rs vs)              = showRes rs vs

showRes :: [DigitR] -> [Verified] -> String
showRes rs []  = report rs ++ " ILL"
showRes _  [v] = show v
showRes rs vs  = concat [report rs, " AMB ", show vs]

run :: [DigitR] -> Result
run reads
     | all isRight reads =
         Success $ case accnt >>= verify of
           Just v  -> Right v
           Nothing ->
               Left (reads, concatMap expand $ map f [0..8])
     | otherwise = Miss reads $ expand space
  where
    space = collect reads
    ds = rights reads
    accnt = fromList ds
    f i = space & ix i .~ (fromJust $ kin ^.at (ds !! i))
