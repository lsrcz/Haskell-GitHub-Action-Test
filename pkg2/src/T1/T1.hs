module T1.T1 where

f1 :: Int -> Int -> Int
f1 = (*)

f2 :: (a -> b) -> IO a -> IO b
f2 f x = x >>= return . f
