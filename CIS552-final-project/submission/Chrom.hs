-- by Partick Yoon yeyoon and Mark Smyda msmyda

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-orphans #-} 
{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, NamedFieldPuns, 
TupleSections,  FlexibleContexts #-}

module Chrom (Chrom(..), 
              target,
              charTarget,
              toBinTarget,
              xover,
              xoverGetOne,
              prop_xoverGetOne,
              prop_xoverGetTwo,
              prop_xoverTwoPivot,
              xoverRandomPick,
              mutate,
              prop_mutateRand,
              prop_mutateSwap,
              fitness,
              test_fitness,
              genRandom,
              prop_genRandom,
              ) where

import Control.Applicative
import Control.Monad.State
import Data.List (nub, (\\), findIndex)
import Test.QuickCheck
import Data.Function (on)
import Control.Monad.Random
import Test.QuickCheck.Monadic hiding (stop)
import Test.HUnit hiding (assert)
import Helper

-- | The type of Gene is a string of ASCII characters or 
-- its binary representation.
data Chrom = Chrom { gene :: String,
                     gen  :: Int }
                deriving (Eq, Ord, Show)

-- | QuickCheck Arbitrary instance for Chrom
instance Arbitrary (Chrom) where
  arbitrary = do gene <- replicateM (length (gene target))
                          $ (arbitrary :: Gen Char)
                 return $ Chrom gene 0
                 
-- target :: Bool -> Chrom
-- target option
--  option = charTarget
--  otherwise = toBinTar
                 
-- | Default target chromosome 
target :: Chrom
target = Chrom "Haskell is fun." (-1)

-- | Pre-defined target chromosome with target string.
charTarget :: Chrom
charTarget = Chrom "Haskell is fun." (-1)

-- | Convert to binary version of the target.
toBinTarget :: Chrom -> Chrom
toBinTarget targ = Chrom (concat binChars) (-1)
  where binChars = map char2bin (gene targ)

-- | Crossever function selector
xover :: RandomGen g => Int -> Chrom -> Chrom -> Rand g (Chrom, Chrom)
xover option 
  | option == 1 = xoverGetTwo
  | option == 3 = xoverTwoPivot
  | option == 4 = xoverRandomPick
  | otherwise = error "Invalid Option for Crossover"

-- | Function for doing crossever of two chromosomes
-- Here we select a random pivot for the crossoever.
-- For the first child, select the characters with index i < pivot from the 
-- first parent, then the remaining characters from the second parent with 
-- index i such that pivot <= i < length.
-- For the second child, we select the characters that are not included in 
-- the first child. (Using the same pivot point)
-- This is crossover function that returns only one child created.
xoverGetOne :: RandomGen g => Chrom -> Chrom -> Rand g Chrom
xoverGetOne ch1 ch2 = do pivot <- getRandomR (0, length (gene ch1) - 1)
                         let child = take pivot (gene ch1) ++ 
                                     drop pivot (gene ch2) 
                         return $ Chrom child ((gen ch1)+1)        


-- | QuickCheck property for crossover
prop_xoverGetOne :: PropertyM IO ()
prop_xoverGetOne = run (evalRandIO $ twoTupleM genRandom >>= makeTuple) 
                   >>= assert . check
          where check :: (Chrom, Chrom, Chrom) -> Bool
                check (p1,p2,c) = length (gene c) == length (gene p1) && 
                        (and . map (\(_, y, z) -> y == z) . 
                         dropWhile (\(x, _, z) -> x == z) 
                         $ zip3 (gene p1) (gene p2) (gene c))
                makeTuple :: RandomGen g => (Chrom, Chrom) -> 
                             Rand g (Chrom, Chrom, Chrom)
                makeTuple (p1,p2) = (p1,p2,) <$> xoverGetOne p1 p2
                                                                   
-- | Mate function that returns both children results from crossover
xoverGetTwo :: RandomGen g => Chrom -> Chrom -> 
             Rand g (Chrom, Chrom)
xoverGetTwo ch1 ch2 = do pivot <- getRandomR (0, length (gene ch1) - 1)
                         let child1 = take pivot (gene ch1) ++ 
                                      drop pivot (gene ch2)
                         let child2 = take pivot (gene ch2) ++ 
                                      drop pivot (gene ch1)
                         return $ (Chrom child1 ((gen ch1)+1),
                                   Chrom child2 ((gen ch1)+1))
                       
-- | QuickCheck property for xoverChTwo
prop_xoverGetTwo :: PropertyM IO ()
prop_xoverGetTwo = run (evalRandIO $ twoTupleM genRandom >>= makeTuple) 
                   >>= assert . check
  where check :: (Chrom, Chrom, (Chrom, Chrom)) -> Bool
        check (p1, p2, (c1, c2)) = length (gene c1) == length (gene p1) && 
                              length (gene c2) == length (gene p1) && 
                              checkAllFromParent p1 p2 c1 &&
                              checkAllFromParent p2 p1 c2 &&
                              checkRebuildParent p1 p2 c1 c2
        makeTuple :: RandomGen g => (Chrom, Chrom) -> 
                     Rand g (Chrom, Chrom, (Chrom, Chrom))
        makeTuple (p1,p2) = (p1,p2,) <$> xoverGetTwo p1 p2
        
-- | Check whether the p1 is equal to String c1[:pivot] ++ c2[pivot+1:]
-- and p2 is equal to String c2[pivot+1:] ++ c2[:pivot]
checkRebuildParent :: Chrom -> Chrom -> Chrom -> Chrom -> Bool
checkRebuildParent p1 p2 c1 c2 = gene p1 == take pivot (gene c1) ++ 
                              drop pivot (gene c2) &&
                              gene p2 == take pivot (gene c2) 
                              ++ drop pivot (gene c1)
                                where pivot = case findIndex (\(x,y) -> x/=y) 
                                                   $ zip (gene p1) (gene c1) of 
                                                Just x -> x
                                                Nothing -> (length . gene) p1
                                                  
-- | Check if all characters of a child is from either one of its parents.
checkAllFromParent :: Chrom -> Chrom -> Chrom -> Bool
checkAllFromParent p1 p2 c = (and . map (\(_, y, z) -> y == z) .
                              dropWhile (\(x, _, z) -> x == z)
                              $ zip3 (gene p1) (gene p2) (gene c))
                             
-- | Crossoever function with two random crossever points. 
xoverTwoPivot :: RandomGen g => Chrom -> Chrom -> 
                  Rand g (Chrom, Chrom)
xoverTwoPivot ch1 ch2 = 
  do pivot1 <- getRandomR (0, length (gene ch1) - 2)
     pivot2 <- getRandomR (pivot1 + 1, length (gene ch1) - 1)
     let child1 = (take pivot1 (gene ch1)) ++
                  ((drop pivot1) . (take pivot2)) (gene ch2) ++
                  (drop pivot2 (gene ch1))
     let child2 = (take pivot1 (gene ch2)) ++
                  ((drop pivot1) . (take pivot2)) (gene ch1) ++
                  (drop pivot2 (gene ch2))
     return $ (Chrom child1 ((gen ch1) + 1),
               Chrom child2 ((gen ch1) + 1))
                                                          
-- | QuickCheck property for xoverChTwo
prop_xoverTwoPivot :: PropertyM IO ()
prop_xoverTwoPivot = run (evalRandIO $ twoTupleM genRandom >>= makeTuple) 
                     >>= assert . check
  where check :: (Chrom, Chrom, (Chrom, Chrom)) -> Bool
        check (p1, p2, (c1, c2)) = length (gene c1) == length (gene p1) && 
                              length (gene c2) == length (gene p1) && 
                              checkAllFromPTwoPivot p1 p2 c1 &&
                              checkAllFromPTwoPivot p2 p1 c2 &&
                              ckRebuildPaTwoP p1 p2 c1 c2
        makeTuple :: RandomGen g => (Chrom, Chrom) -> 
                     Rand g (Chrom, Chrom, (Chrom, Chrom))
        makeTuple (p1,p2) = (p1,p2,) <$> xoverTwoPivot p1 p2

-- | Check if all characters of a child is from either one of its parents.
checkAllFromPTwoPivot :: Chrom -> Chrom -> Chrom -> Bool
checkAllFromPTwoPivot p1 p2 c = (and . map (\(x, _, z) -> x == z) .
                                 dropWhile (\(_, y, z) -> y == z) .
                                 dropWhile (\(x, _, z) -> x == z)
                                 $ zip3 (gene p1) (gene p2) (gene c))
                                
-- | Check whether p1 = c1[:pivot1] ++ c2[pivot1+1:pivot2] ++ c1[pivot2+1:]
-- and p2 = c2[:pivot1] ++ c1[pivot1+1:pivot2] ++ c2[pivot2+1:]
ckRebuildPaTwoP :: Chrom -> Chrom -> Chrom -> Chrom -> Bool
ckRebuildPaTwoP p1 p2 c1 c2 = gene p1 == take pivot1 (gene c1) ++ 
                              ((drop pivot1) . (take pivot2)) (gene c2) ++
                              drop pivot2 (gene c1) &&
                              gene p2 == take pivot1 (gene c2) ++ 
                              ((drop pivot1) . (take pivot2)) (gene c1) ++
                              drop pivot2 (gene c2)
                                where pivot1 = case findIndex (\(x,y) -> x/=y) 
                                                   $ zip (gene p1) (gene c1) of
                                                Just x -> x
                                                Nothing -> (length . gene) p1
                                      pivot2 = case findIndex (\(x,y) -> x/=y) 
                                                   $ zip ((reverse . gene) p1) 
                                                    ((reverse . gene) c1) of 
                                                Just x -> (length . gene) p1 - x
                                                Nothing -> 0

-- | Crossoever function with random multiple crossever points. 
-- child1 is created randomly at each point from either parent1 or 
-- parent2, and child2 has untaken genes.
xoverRandomPick :: RandomGen g => Chrom -> Chrom -> 
                   Rand g (Chrom, Chrom)
xoverRandomPick c1 c2 = do r :: [Int] <- replicateM (length (gene target))
                                                    (getRandomR (0,1))
                           let combined = zip3 (gene c1) (gene c2) r
                           let child1 = map (\(f,s,n) ->
                                             if n == 0 then f else s) combined
                           let child2 = map (\(f,s,n) -> 
                                             if n == 1 then f else s) combined
                           return $ (Chrom child1 ((gen c1) + 1), 
                                     Chrom child2 ((gen c1) + 1))
                   
-- | QuickCheck property for xoverRandomPick
prop_xoverRandomPick :: PropertyM IO ()
prop_xoverRandomPick = run (evalRandIO $ twoTupleM genRandom >>= makeTuple) 
                     >>= assert . check
  where check :: (Chrom, Chrom, (Chrom, Chrom)) -> Bool
        check (p1, p2, (c1, c2)) = length (gene c1) == length (gene p1) && 
                              length (gene c2) == length (gene p1) && 
                              checkFromEitherOfTwoPa p1 p2 c1 c2
        makeTuple :: RandomGen g => (Chrom, Chrom) -> 
                     Rand g (Chrom, Chrom, (Chrom, Chrom))
        makeTuple (p1,p2) = (p1,p2,) <$> xoverRandomPick p1 p2

checkFromEitherOfTwoPa :: Chrom -> Chrom -> Chrom -> Chrom -> Bool
checkFromEitherOfTwoPa p1 p2 c1 c2  = undefined

-- | get mutuation character
getMutchar :: RandomGen g => Rand g Char
getMutchar = getRandomR (' ', 'z')
-- getMutchar = getRandomR ('0', '1')

-- | Selector function for mutate function.
mutate :: RandomGen g => Int -> Chrom -> Rand g Chrom
mutate option 
  | option == 1 = mutateRand
  | option == 2 = mutateSwap
  | otherwise = error "Invalid Option"

-- | Mutate the string at random position with random character.
mutateRand :: RandomGen g => Chrom -> Rand g Chrom
mutateRand ch = do index <- getRandomR (0, length (gene ch) - 1)
                   let (split1, split2) = splitAt index (gene ch)
                   mutChar <- getMutchar 
                   return $ Chrom (split1 ++ (mutChar : tail split2)) (gen ch)
                 
-- | QuickCheck Property for mutateRandCh function
prop_mutateRand :: PropertyM IO ()
prop_mutateRand = run (evalRandIO $ genRandom >>= makeMutTuple) >>= assert . check
                where check :: (Chrom, Chrom) -> Bool
                      check (orig, mut) = length (nub (gene orig) 
                                                  \\ nub (gene mut)) <= 1 &&
                                          length (gene orig) 
                                          == length (gene mut)
                      makeMutTuple :: RandomGen g => Chrom -> 
                                      Rand g (Chrom, Chrom)
                      makeMutTuple orig = (orig,) <$> mutateRand orig
                      
-- | Mutate by randomly swapping two characters
mutateSwap :: RandomGen g => Chrom -> Rand g Chrom
mutateSwap ch = do idx1 <- getRandomR (0, length (gene ch) - 2)
                   idx2 <- getRandomR (idx1 + 1, length (gene ch) - 1)
                   return $ Chrom (swap idx1 idx2 (gene ch)) (gen ch)
                     
-- | QuickCheck Property for mutateChSwap function
prop_mutateSwap :: PropertyM IO ()
prop_mutateSwap = run (evalRandIO $ genRandom >>= makeMutTuple) 
                  >>= assert . check
                where check :: (Chrom, Chrom) -> Bool
                      check (orig, mut) = (nub (gene orig) \\ 
                                           nub (gene mut) == []) &&
                                          length (gene mut) == length (gene orig)
                      makeMutTuple orig = (orig,) <$> mutateSwap orig
                                          
-- | Fitness function. Works only for chromosomes with the same length. 
-- If the chromosome is longer and have the target as prefix e.g. 
-- "Haskell is fun..." it will score zero.
fitness :: Chrom -> Int
fitness ch = sum $ map abs (zipWith ((-) `on` fromEnum) 
                                 (gene target) (gene ch))

-- | unit test for fitness
test_fitness :: Test
test_fitness = test [ fitness (Chrom "Haskell is fun." 0)  ~=? 0,
                      fitness (Chrom "Python is better" 0) ~=? 419,
                      fitness (Chrom "Java is messy" 0)    ~=? 212,
                      fitness (Chrom "Haskell is fun?" 0)  ~=? 17 ]

-- | Generate a random chromosome with the same length as the target. 
genRandom :: RandomGen g => Rand g Chrom
genRandom = do genes <- replicateM (length (gene target)) 
                        $ getRandomChar
               return $ Chrom genes 0
              
getRandomChar :: RandomGen g => Rand g Char
getRandomChar = getRandomR (' ', 'z')
-- getRandomR ('0', '1')
               
-- | QuickCheck Property for genRandom
prop_genRandom :: PropertyM IO ()
prop_genRandom = run (evalRandIO genRandom) >>= assert . check
  where check :: Chrom -> Bool
        check ch = and $ map ($ ch) [all (between (fromEnum ' ') 
                                          (fromEnum 'z') . fromEnum) . gene,
                                  (== length (gene target)) . length . gene,
                                  (>= 0) . fitness]
        between :: Int -> Int -> Int -> Bool
        between l r x = l <= x && x <= r
        
        
