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
    , Option "r" ["target"] (ReqArg (TargetStr . read)
      "<target>" )
      "The string to which we should attempt convergence" ] 
    
-- | Get Flag value from parsed list of flags
-- if the flag does not exists in the list, returns the default value
-- passed in as the first argument
getFlag :: Flag -> [Flag] -> Flag
getFlag flag (f:flags) 
  | flag == f = f
  | otherwise = getFlag flag flags
getFlag flag [] = flag

-- | Parse command line arguments into list of Flags and list of other arguments
cmdlineOpts :: [String] -> IO ([Flag], [String])
cmdlineOpts argv = 
    case getOpt Permute options argv of
        (o, n, []  ) -> return (o,n)
        (_, _, errs) -> ioError (userError (concat errs ++ 
                                            usageInfo header options))
    where header = "Usage: Main [OPTION...]"
                                                                
-- | Execute the genetic algorithm and print the result to stdout.
main :: IO() 
main = do 
  args <- getArgs
  (opts, _) <- cmdlineOpts args
  let (InitPopSize popSize) = getFlag (InitPopSize 1000) opts
  let (ElitismRatio eliRatio) = getFlag (ElitismRatio 0.5) opts
  let (MutRatio mutRatio) = getFlag (MutRatio 0.01) opts
  let (XOverRatio xoRatio) = getFlag (XOverRatio 0.2) opts
  let (TournaSize tourSize) = getFlag (TournaSize 3) opts
  let (MaxGen maxGen) = getFlag (MaxGen 1000) opts
  let (XOverOption xOption) = getFlag (XOverOption 1) opts
  let (MutOption mOption) = getFlag (MutOption 1) opts
  let (SelOption sOption) = getFlag (SelOption 1) opts              
  let (TargetStr str) = getFlag (TargetStr "Haskell is fun.") opts
  let targ = if BinaryTarget `elem` opts then toBinRep str else str
  let popConfig = PopInfo popSize eliRatio xoRatio mutRatio tourSize
  let algoConfig = GAConfig xOption mOption sOption
  chrom <- evalRandIO (randomPop targ popConfig 
                       >>= repeatWhile (terminate maxGen) 
                       (evolve targ algoConfig))
  printResult maxGen algoConfig chrom
  
-- | Termination condition for a population
terminate :: Int -> Pop Chrom -> Bool
terminate maxGen (Pop _ (ch:_::[Chrom])) = fit ch == 0 || (gen ch) == maxGen
terminate _ (Pop _ []) = True

-- | Print the output to stdout
printResult :: Int -> GAConfig -> Pop Chrom -> IO ()
printResult maxGen config (Pop info (ch:_ :: [Chrom]))
  | gen ch == maxGen = 
    printf "Max gen %d reached without reaching target: %s\n%s\n. Closest is: %s\n"
    maxGen (show info) (show config) (show ch)
  | fit ch == 0 = 
    printf "Reached target at gen %d: %s\n%s\n%s\nMax Gen : %d" 
    (gen ch) (show ch) (show info) (show config) maxGen
  | otherwise = putStrLn "Error"
printResult _ _ (Pop _ []) = putStrLn "Error : Population is Empty"

-- | Run QuickCheck for all properties.
runQuickChecks :: IO ()
runQuickChecks = do mapM_ (quickCheck . monadicIO) 
                      [ prop_xoverTwoPivot, prop_xoverGetOne, 
                        prop_xoverRandomPick, prop_xoverGetTwo, 
                        prop_mutateRand, prop_mutateSwap,
                        prop_genRandom, prop_randomPop ]

