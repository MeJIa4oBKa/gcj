import Data.List
import Text.Parsec
import GCJ

-- some matrix functions
vflip = reverse
hflip = map reverse
low = tail
up = init
left = map init
right = map tail
rotate = hflip . transpose -- rotate ccw by 90 degrees
rotations = take 4 . iterate rotate -- all 4 rotations of a matrix, ccw order
dimension = length
symmetric xss = transpose xss == xss

-- subdiamonds
subdiamonds d = 
  take (dimension d) $ iterate (right . up) d

-- make a matrix given diagonals (tl-br/bl-tr format)
-- ex:
-- given a diamond such as
--   7
--  4 8
-- 1 5 9
--  2 6
--   3
--
-- (i.e. the diagonals are [[7], [4,8], [1,5,9], [2,6], [3]])
--
-- matrixFromDiags will spit out
--
--  [['1','2','3'],['4','5','6'],['7','8','9']]
matrixFromDiags [] = []
matrixFromDiags xss = 
  let (ls,r) = splitAt (length xss `div` 2) (map head xss)
      yss = matrixFromDiags $ filter (/= []) (map tail xss)
  in r:(zipWith (:) (reverse ls) yss)

parseDiamond = do
  k <- natural -- diamond size: k by k matrix thus 2k-1 diagonals to read
  newline
  dss <- count (2*k-1) line
  return $ matrixFromDiags dss
  where
  line = do
    many (char ' ') -- initial whitespace
    ds <- digit `sepBy` (char ' ') 
    newline
    return ds

solveDiamond d = 
  let k = dimension d -- original diamond dimension
      (dkh,dkv) = (k - max ur bl, k - max ul br) -- horizontal,vertical size changes
      k' = k + dkh + dkv -- minimal dimension for elegant diamond containing d
  in k'^2 - k^2 -- cost
  where
  -- u=up, b=bottom, l=left, r=right
  [ur,ul,bl,br] = map (dimension . head . filter symmetric . subdiamonds) (rotations d)
  

runDiamond = runGCJ parseDiamond solveDiamond
