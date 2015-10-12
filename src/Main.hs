import Reader (readAccount, Result)

main :: IO ()
main = getContents >>= mapM_ (print . readAccount) . quads . lines
  where
    quads (a:b:c:d:xs) = unlines [a,b,c,d] : quads xs
    quads _            = []
