-- by Partick Yoon yeyoon and Mark Smyda msmyda

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-orphans #-} 
{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, NamedFieldPuns, 
TupleSections,  FlexibleContexts #-}

module GAConfig (GAConfig(..)
                ,) where

-- | Genetic Algorithm configuration 
-- Crossover Option : {xoverGetTwo = 1, xoverTwoPivot =3,
-- xoverRandomPick = 4}
-- Mutation Option : {mutate = 1, mutateSwap =2}
-- Selection Option : {tournaSelect = 1, fitPropSelect = 2}
data GAConfig = GAConfig 
                  Int  --  Crossover Option
                  Int  --  Mutation Option
                  Int  --  Selection Option
                  deriving (Eq)

instance Show GAConfig where
  show (GAConfig xOption muOption selOption) = 
    "Crossover Method : " ++ (getxoverOptionStr xOption) ++
    "\nMutation Method : " ++ (getmuOptionStr muOption) ++
    "\nSelection Method : " ++ (getselOptionStr selOption) 
    where getxoverOptionStr :: Int -> String
          getxoverOptionStr option 
            | option == 1 = "One Pivot Crossover, Use Two Children"
            | option == 2 = "One Pivot Crossover, Use One Child"
            | option == 3 = "Two Pivot Crossover, Use Two Children"
            | option == 4 = "Multiple Pivot Crossover, Use Two Children"
            | otherwise = "Invalid Crossover Option"
          getmuOptionStr :: Int -> String 
          getmuOptionStr option 
            | option == 1 = "Random Character Switch Mutation"
            | option == 2 = "Swap Two Genes Mutation"
            | otherwise = "Invalid Mutation Option"
          getselOptionStr :: Int -> String
          getselOptionStr option
            | option == 1 = "Tournament Selection"
            | option == 2 = "Fitness Ranking Based Selection"
            | otherwise = "Invalid Selection Option"

