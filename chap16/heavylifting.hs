

-- (fmap . fmap) ((return '1' ++) . show) (\x -> [x, 1..3]) 0
-- print (fmap (+1) $ read "[1]" :: [Int])
-- print ((fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"]))
-- print $ fmap (*2) ((\x -> x - 2) :: (Int -> Int)) 88 -- NOTE the result is 172


e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read . ("123"++) . show) ioi
    in fmap (*3) changed

