import System.IO
import System.Environment
import System.Directory
import Data.Numbers.Primes
import Color
import Codec.Picture

-- prime factors

pfact :: Int -> [Int]
pfact n = findf $ takeWhile (<=n) primes where
   findf pp@(p:ps)
      | p == n = [n]
      | mod n p == 0 = p : (pfact $ div n p)
      | otherwise = findf ps

-- number of occurrences of n on ascending list

count :: [Int] -> Int -> Int
count list n = length $ filter (== n) $ takeWhile (<= n) list

-- prime powers

factors :: Int -> [Int]
factors n = map (count (pfact n)) $ takeWhile (<=n) primes

-- pixels


white :: PixelRGB8
white = PixelRGB8 255 255 255

light_grey :: PixelRGB8
light_grey = PixelRGB8 200 200 200

grey :: PixelRGB8
grey = PixelRGB8 100 100 100

dark_grey :: PixelRGB8
dark_grey = PixelRGB8 50 50 50

black :: PixelRGB8
black = PixelRGB8 0 0 0

--

yellow :: PixelRGB8
yellow = PixelRGB8 255 255 0

green :: PixelRGB8
green = PixelRGB8 0 255 0

cyan :: PixelRGB8
cyan = PixelRGB8 0 255 255

blue :: PixelRGB8
blue = PixelRGB8 0 0 255

magenta :: PixelRGB8
magenta = PixelRGB8 255 0 255

red :: PixelRGB8
red = PixelRGB8 255 0 0

-- pixel power representation

pwr_pixel :: Int -> PixelRGB8
pwr_pixel 0 = black
pwr_pixel 1 = light_grey
pwr_pixel 2 = grey
pwr_pixel 3 = dark_grey
pwr_pixel 4 = green
pwr_pixel 5 = yellow
pwr_pixel 6 = red
pwr_pixel _ = blue

-- dimensions

type Dimensions = (Int,Int)

-- draw

draw :: Dimensions -> IO()
draw d = do
   createDirectoryIfMissing True "io"
   done <- doesFileExist file
   if done then 
      putClrLn W (file ++ " skipped") -- print existing file to console
   else do 
      savePngImage file $ ImageRGB8 $ generateImage (\x y -> let fs = factors y in if x < length fs then pwr_pixel $ fs !! x else pwr_pixel 0) w h
      putClrLn G file -- print drawn file to console
   where
      (w,h) = d
      file = "io/" ++ show w ++ "x" ++ show h ++ ".png"

main :: IO ()
main = do 
   args <- getArgs
   if length args == 2 then let
      (w:h:[]) = map read args
      in
      draw (w,h)
   else do
      putClr R "args:"
      putClrLn W "width height"
      pure ()
