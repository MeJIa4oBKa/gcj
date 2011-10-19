module GCJ where

import Text.Parsec
import Control.Monad

-- parse a natural number (zero is ok), without sign
natural = do
  ds <- many1 digit
  return $ read ds

gcjInput tcp = do
  ntc <- natural -- number of test cases
  newline
  tcs <- count ntc tcp
  return tcs

runGCJ tcParse tcSolve ifile ofile = do
  input <- readFile ifile
  case parse (gcjInput tcParse) "error" input of
    Left perr ->
      putStrLn $ "runGCJ: parse error: " ++ show perr     -- parsing error!
    Right tcs ->
      do let sols = map tcSolve tcs
             gcjOutput = unlines ["Case #" ++ show i ++ ": " ++ show sol
                                 |(sol,i) <- zip sols [1 ..]]
             testOutput = unlines ["testcase #" ++ show i ++ ": " ++ "\n" ++
                                      show tc ++ "\n" ++
                                      "solution: " ++ "\n" ++
                                      show sol ++ "\n"
                                  |(tc,sol,i) <- zip3 tcs sols [1 ..]]
         unless (ofile == "") $ writeFile ofile gcjOutput
         when (ofile == "") $ putStrLn testOutput
