-- by Partick Yoon yeyoon and Mark Smyda msmyda

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-orphans #-} 
{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, NamedFieldPuns, 
TupleSections,  FlexibleContexts #-}

module Main where

import Test.QuickCheck
import Text.Printf (printf)
import Control.Monad.Random
import Test.QuickCheck.Monadic hiding (stop)
import System.Console.GetOpt
import System (getArgs)
import Chrom
import Pop
import Helper
import GAConfig

-- | Data for Flags for options
data Flag = InitPopSize Int
          | ElitismRatio Double
          | MutRatio Double
          | XOverRatio Double
          | TournaSize Int
          | MaxGen Int
          | XOverOption Int
          | MutOption Int
          | SelOption Int
          | BinaryTarget
          | TargetStr String
          deriving (Show, Read)

instance Eq Flag where
  InitPopSize _  == InitPopSize _   = True
  ElitismRatio _ == ElitismRatio _  = True
  MutRatio _     == MutRatio _      = True
  XOverRatio _   == XOverRatio _    = True
  BinaryTarget   == BinaryTarget    = True
  TournaSize _   == TournaSize _    = True
  MaxGen _       == MaxGen _        = True
  XOverOption _  == XOverOption _   = True
  MutOption _    == MutOption _     = True
  SelOption _    == SelOption _     = True
  TargetStr _    == TargetStr _     = True
  _              == _               = False

-- | list of options for Genetic Algorithm parameters
options :: [OptDescr Flag]
options =
    [ Option "i" ["initpopsize"] (ReqArg (InitPopSize . read)
      "<initial population size>" )
      "How many candidate strings should be initialized in the population"
    , Option "e" ["eliRatio"] (ReqArg (ElitismRatio . read)
      "<elitism ratio>" )
      "What proportion of candidates continues to next gen without change"
    , Option "m" ["mutRatio"] (ReqArg (MutRatio . read)
      "<mutation ratio>" )
      "What proportion of candidates is chosen for random mutation"
    , Option "x" ["xoverRatio"] (ReqArg (XOverRatio . read)
      "<crossover ratio>" )
      "The proportion of strings that undergoes crossover"
    , Option "t" ["tournaSize"] (ReqArg (TournaSize . read)
      "<tournament size>" )
      "The size of the tournament in selecting parent genes from population"
    , Option "g" ["maxGen"] (ReqArg (MaxGen . read)
      "<maximum generations>" )
      "The maximum number of generations to attempt before quitting"
    , Option "v" ["xoverOption"] (ReqArg (XOverOption . read)
      "<crossover option>" )
      "Crossover Method Option. Read README file for list of options."
    , Option "u" ["mutOption"] (ReqArg (MutOption . read)
      "<mutation option>" )
      "Mutation Method Option. Read README file for list of options."
    , Option "s" ["selOption"] (ReqArg (SelOption . read)
                                "<selection option>" )
      "Selection Method Option. Read README file for list of options."
    , Option "b" ["BinTar"] (NoArg BinaryTarget)
      "Whether or not to use binary representation for target"
    , Option "tg" ["target"] (ReqArg (TargetStr . read)
      "<target>" )
      "The string to which we should attempt convergence" ] 

-- 
getOption :: Flag -> [Flag] -> Flag
getOption flag (f:flags) 
  | flag == f = f
  | otherwise = getOption flag flags
getOption flag [] = flag

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv = 
    case getOpt Permute options argv of
        (o, n, []  ) -> return (o,n)
        (_, _, errs) -> ioError (userError (concat errs ++ 
                                            usageInfo header options))
    where header = "Usage: Main [OPTION...]"
                                                                
-- | Execute the genetic algorithm and print the result to stdout.
main :: IO() 
main = do 
  args <- getArgs
  (opts, _) <- compilerOpts args
  let (InitPopSize ps) = getOption (InitPopSize 1000) opts
  let (ElitismRatio er) = getOption (ElitismRatio 0.5) opts
  let (MutRatio mr) = getOption (MutRatio 0.01) opts
  let (XOverRatio xr) = getOption (XOverRatio 0.2) opts
  let (TournaSize ts) = getOption (TournaSize 3) opts
  let (MaxGen mg) = getOption (MaxGen 10000) opts
  let (XOverOption xo) = getOption (XOverOption 1) opts
  let (MutOption mo) = getOption (MutOption 1) opts
  let (SelOption so) = getOption (SelOption 1) opts              
  let (TargetStr str) = getOption (TargetStr "Haskell is fun.") opts
  let popConfig = PopInfo ps er xr mr ts
  let algoConfig = GAConfig xo mo so
  if xo == 2 then putStrLn "Error"
    else do chrom <- evalRandIO (randomPop popConfig 
                              >>= repeatWhile (terminate mg) (evolve algoConfig))
            printResult mg algoConfig chrom

-- | Termination condition for a population
terminate :: Int -> Pop Chrom -> Bool
terminate maxGen (Pop _ (ch:_::[Chrom])) = fitness ch == 0 || (gen ch) == maxGen
terminate _ (Pop _ []) = True

-- | Print the output to stdout
printResult :: Int -> GAConfig -> Pop Chrom -> IO ()
printResult maxGen config (Pop info (ch:_ :: [Chrom]))
  | gen ch == maxGen = 
    printf "Max gen reached without reaching target: %s\n%s\n" 
    (show info) (show config)
  | fitness ch == 0 = 
    printf "Reached target at gen %d: %s\n%s\n%s\n" 
    (gen ch) (show ch) (show info) (show config)
  | otherwise = putStrLn "Error"
printResult _ _ (Pop _ []) = putStrLn "Error : Population is Empty"

-- | Run QuickCheck for all properties.
runQuickChecks :: IO ()
runQuickChecks = do mapM_ (quickCheck . monadicIO) 
                      [prop_xoverGetOne, prop_mutateRand, prop_mutateSwap,
                       prop_xoverGetTwo, prop_genRandom, prop_randomPop,
                       prop_xoverTwoPivot]
