import Data.Array
import System.Environment

main :: IO ()
main = getArgs >>= readFile . head >>= putStr . showMatrix . go

go s = floodFill (0,4) 
                 'o' 
                 '-' 
                 (array ((0,0),(xmax,ymax)) $ zip [(y,x) | x <- [0..xmax], y <- [0..ymax]] pixels)
  where matrix = init . map words . lines $ s
        pixels = map head . concat $ matrix
        xmax = (length matrix) - 1
        ymax = (length . head $ matrix) - 1

type Point = (Int, Int)

lo, hi :: Array Point a -> Point
lo arr = fst $ bounds arr
hi arr = snd $ bounds arr

outOfBounds :: Array Point a -> Point -> Bool
outOfBounds a (x,y)
  | x < fst (lo a) || x > fst (hi a) = True
  | y < snd (lo a) || y > snd (hi a) = True
  | otherwise = False

isTarget :: Eq a => Array Point a -> Point -> a -> Bool
isTarget a p c
  | a ! p == c = True
  | otherwise = False

floodFill :: Eq a => Point -> a -> a -> Array Point a -> Array Point a
floodFill p t r a
  | outOfBounds a p = a
  | isTarget a p r = a
  | isTarget a p t = floodFill (x+1,y) t r .
      floodFill (x-1,y) t r .
      floodFill (x,y+1) t r $
      floodFill (x,y-1) t r a'
  | otherwise = a
  where (x,y) = p
        a' = a // [(p,r)]


showMatrix :: Show a => Array Point a -> String
showMatrix a = unlines [unwords [show $ a ! (x,y) | x <- [xmin..xmax]] | y <- [ymin..ymax]]
 where ((xmin,ymin),(xmax,ymax)) = bounds a
