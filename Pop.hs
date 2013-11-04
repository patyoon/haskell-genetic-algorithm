-- by Partick Yoon yeyoon and Mark Smyda msmyda

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-orphans #-} 
{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, NamedFieldPuns, 
TupleSections,  FlexibleContexts #-}

module Pop (Pop(..),
            PopInfo(..),
            getPopSize,
            randomPop,
            prop_randomPop,
            evolve,
            ) where

import Control.Applicative
import Control.Monad.State
import Data.List (minimumBy, sortBy)
import Data.Ord (comparing)
import Control.Monad.Random
import Test.QuickCheck.Monadic hiding (stop)
import Helper
import Chrom
import GAConfig 

-- | The Population has information about the population
-- and a list of Chroms with pre-defined size. 
data Pop ch = Pop { info :: PopInfo, 
                 chs  :: [ch] }
             deriving Show
             
-- | Population information data for evolution.
-- Initial Population Size : Size of Initial population
-- Elitism ratio : The ratio of population sorted by fitness is 
-- directly passed to the next generation without any manipulation.
-- Crossover ratio : The proportion of the rest of the population 
-- that goes through crossoever. The reminder is copied over directly.
-- Mutation ratio : In either case of crossover or direcy copy, 
-- each of next generation chromosomes goes random mutation based on the 
-- mutation ratio.
-- Tournament Size : Tournament size of tournament selection method.
data PopInfo = PopInfo 
               Int      -- Initial Population Size
               Double   -- Elitism ratio
               Double   -- Crossover ratio
               Double   -- Mutation ratio
               Int      -- Tournament Size

instance Show PopInfo where
  show (PopInfo size er xr mr ts) = 
    "Initial Population Size : " ++ (show size) ++
    "\nElitism Ratio : " ++ (show er) ++
    "\nCrossover Ratio : " ++ (show xr) ++
    "\nMutation Ratio : " ++ (show mr) ++
    "\nTournament Size : " ++ (show ts)

getPopSize :: PopInfo -> Int
getPopSize (PopInfo size _ _ _ _) = size

getTournaSize :: PopInfo -> Int
getTournaSize (PopInfo _ _ _ _ size) = size

-- | Default population configuration
-- Used for QuickCheck
defaultPopConfig :: PopInfo
defaultPopConfig = PopInfo 1000 0.5 0.2 0.01 3

-- | generate random population of size initPopSize
randomPop :: RandomGen g => String -> PopInfo -> Rand g (Pop Chrom)
randomPop targ info = do chs <- replicateM (getPopSize info) (genRandom targ)
                         return $ Pop info 
                           (sortBy (comparing (fitness targ . gene)) chs)
                    
-- | QuickCheck Property for genRandom. 
-- Checks if property on size, sorted
prop_randomPop :: PropertyM IO ()
prop_randomPop = run ((evalRandIO . (randomPop defTarget)) 
                      defaultPopConfig) >>= assert . check
  where check :: Pop Chrom -> Bool
        check pop = (length . chs) pop == (getPopSize . info) pop &&
                isOrdered (fitness defTarget . gene) (chs pop)
                
-- | Implements Deterministic tournament selection algorithm.
-- Choose k individuals from the population at random
-- choose the best individual from pool/tournament 
tournaSelect :: RandomGen g => String -> Pop Chrom -> Rand g Chrom
tournaSelect targ (Pop info chs) 
  =  minimumBy (comparing (fitness targ . gene)) . map
     (chs !!) <$> replicateM (getTournaSize info)
     (getRandomR (0, (getPopSize info) - 1))

-- | Fitness proportionate selection 
-- Choose k individuals from the population at random
-- After picking k elements, make the probability of selection 
-- be proportional to fitness ranking
fitPropSelect :: RandomGen g => String -> Pop Chrom -> Rand g Chrom
fitPropSelect targ (Pop info chs) = do chroms <- map (chs !!) <$> 
                                                 replicateM (getTournaSize info)
                                                 (getRandomR 
                                                  (0,getPopSize info - 1))
                                       let pool = makePool targ chroms
                                       index <- getRandomR (0, length pool - 1)
                                       return $ pool !! index

-- | Helper function : Make a pool of chromosome where the number 
-- of the same chromosome is inversely proportional to its rank in the pool.
makePool :: String -> [Chrom] -> [Chrom]
makePool targ chroms = foldr (\(x,y) zs -> replicate x y ++ zs) 
                       [] $ zip (reverse [1..(length chroms)]) 
                       (sortBy (comparing (fitness targ . gene)) chroms)

-- | Select two parents to crossover. 
selParents :: RandomGen g => Int -> String -> Pop Chrom -> Rand g (Chrom, Chrom)
selParents option targ
  | option == 1 = twoTupleM . tournaSelect targ
  | option == 2 = twoTupleM . fitPropSelect targ
  | otherwise = error "Invalid Option for Selection method"
                                                        
-- | Evolve a population 
evolve :: RandomGen g => String -> GAConfig -> Pop Chrom-> Rand g (Pop Chrom)
evolve targ (GAConfig xo mo so) p@(Pop info@(PopInfo size elr xor mur _) chs) =
  do nextGens <- (replicateM (size - idx)) getChildren
     return $ Pop info (sortBy (comparing (fitness targ . gene)) 
                        (take idx chs ++ concat nextGens))
       where idx = round (fromIntegral size * elr)
             addChild :: RandomGen g => Double -> Chrom -> Rand g Chrom
             addChild m
               | m <= mur = mutate mo targ
               | otherwise = return
             addMutation :: RandomGen g => Double -> Rand g Chrom
             addMutation m
               | m <= mur = (mutate mo targ) . (chs !!) 
                            =<< getRandomR (idx, size - 1)
               | otherwise = (chs !!) <$> getRandomR (idx, size - 1)
             getChildren :: RandomGen g => Rand g [Chrom]
             getChildren = do (r, m1, m2) <- threeTupleM getRandom
                              if r <= xor
                                then do (p1, p2) <- selParents so targ p
                                        -- if crossover option is 2, 
                                        -- use only one child from crossover
                                        if xo == 2 then
                                          do c1 <- xoverGetOne targ p1 p2
                                             c1'<- addChild m1 c1
                                             return [c1']
                                             else 
                                          do (c1, c2) <- xover xo targ p1 p2
                                             c1' <- addChild m1 c1
                                             c2' <- addChild m2 c2
                                             return $ c1' : [c2']
                                else do c1 <- addMutation m1
                                        return [c1]
