-- by Partick Yoon yeyoon and Mark Smyda msmyda

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-orphans #-} 
{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, NamedFieldPuns, 
TupleSections,  FlexibleContexts #-}

module Chrom (Chrom(..), 
              xover,
              defTarget,
              xoverGetOne,
              prop_xoverGetOne,
              prop_xoverGetTwo,
              prop_xoverTwoPivot,
              prop_xoverRandomPick,
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
data Chrom = Chrom { gen  :: Int, 
                     fit  :: Int, 
                     gene :: String }
                deriving (Eq, Ord, Show)

-- | QuickCheck Arbitrary instance for Chrom
instance Arbitrary (Chrom) where
  arbitrary = do gene <- replicateM (length defTarget)
                          $ (arbitrary :: Gen Char)
                 return $ Chrom 0 (fitness defTarget gene) gene

-- | Pre-defined target string used for QuickChecks.
defTarget :: String
defTarget = "Haskell is fun."

-- | Crossever function selector
xover :: RandomGen g => Int -> String -> Chrom -> Chrom -> Rand g (Chrom, Chrom)
xover option targ ch ch2
  | option == 1 = xoverGetTwo ch ch2     >>= makeChrom
  | option == 3 = xoverTwoPivot ch ch2   >>= makeChrom
  | option == 4 = xoverRandomPick ch ch2 >>= makeChrom
  | otherwise = error "Invalid Option for Crossover"
    where makeChrom :: (String, String) -> Rand g (Chrom, Chrom)
          makeChrom = \(x,y) -> return $ (Chrom (gen ch + 1) (fitness targ x) x,
                                          Chrom (gen ch + 1) (fitness targ y) y)



-- | Function for doing crossever of two chromosomes
-- Here we select a random pivot for the crossoever.
-- For the first child, select the characters with index i < pivot from the 
-- first parent, then the remaining characters from the second parent with 
-- index i such that pivot <= i < length.
-- For the second child, we select the characters that are not included in 
-- the first child. (Using the same pivot point)
-- This is crossover function that returns only one child created.
xoverGetOne :: RandomGen g => String -> Chrom -> Chrom -> Rand g Chrom
xoverGetOne targ ch1 ch2 = do pivot <- getRandomR (0, length (gene ch1) - 1)
                              let child = take pivot (gene ch1) ++ 
                                          drop pivot (gene ch2) 
                              return $ Chrom (gen ch1 + 1) 
                                 (fitness targ child) child

-- | QuickCheck property for crossover
prop_xoverGetOne :: PropertyM IO ()
prop_xoverGetOne = run (evalRandIO $ twoTupleM (genRandom defTarget) 
                        >>= makeTuple) 
                   >>= assert . check
          where check :: (Chrom, Chrom, Chrom) -> Bool
                check (p1,p2,c) = (length . gene) c == length (gene p1) && 
                        (and . map (\(_, y, z) -> y == z) . 
                         dropWhile (\(x, _, z) -> x == z) 
                         $ zip3 (gene p1) (gene p2) (gene c))
                makeTuple :: RandomGen g => (Chrom, Chrom) -> 
                             Rand g (Chrom, Chrom, Chrom)
                makeTuple (p1,p2) = (p1,p2,) <$> xoverGetOne defTarget p1 p2
                                                                   
-- | Mate function that returns both children results from crossover
xoverGetTwo :: RandomGen g => Chrom -> Chrom -> 
             Rand g (String, String)
xoverGetTwo ch1 ch2 = do pivot <- getRandomR (0, length (gene ch1) - 1)
                         let child1 = take pivot (gene ch1) ++ 
                                      drop pivot (gene ch2)
                         let child2 = take pivot (gene ch2) ++ 
                                      drop pivot (gene ch1)
                         return (child1, child2)
                       
-- | QuickCheck property for xoverChTwo
prop_xoverGetTwo :: PropertyM IO ()
prop_xoverGetTwo = run (evalRandIO $ twoTupleM (genRandom defTarget) 
                        >>= makeTuple) 
                   >>= assert . check
  where check :: (Chrom, Chrom, (String, String)) -> Bool
        check (p1, p2, (c1, c2)) = length c1 == length (gene p1) && 
                              length c2 == length (gene p1) && 
                              checkAllFromParent p1 p2 c1 &&
                              checkAllFromParent p2 p1 c2 &&
                              checkRebuildParent p1 p2 c1 c2
        makeTuple :: RandomGen g => (Chrom, Chrom) -> 
                     Rand g (Chrom, Chrom, (String, String))
        makeTuple (p1,p2) = (p1,p2,) <$> xoverGetTwo p1 p2
        
-- | Check whether the p1 is equal to String c1[:pivot] ++ c2[pivot+1:]
-- and p2 is equal to String c2[pivot+1:] ++ c2[:pivot]
checkRebuildParent :: Chrom -> Chrom -> String -> String -> Bool
checkRebuildParent p1 p2 c1 c2 = gene p1 == take pivot c1 ++ 
                              drop pivot c2 &&
                              gene p2 == take pivot c2 
                              ++ drop pivot c1
                                where pivot = case findIndex (\(x,y) -> x/=y) 
                                                   $ zip (gene p1) c1 of 
                                                Just x -> x
                                                Nothing -> (length . gene) p1
                                                  
-- | Check if all characters of a child is from either one of its parents.
checkAllFromParent :: Chrom -> Chrom -> String -> Bool
checkAllFromParent p1 p2 c = (and . map (\(_, y, z) -> y == z) .
                              dropWhile (\(x, _, z) -> x == z)
                              $ zip3 (gene p1) (gene p2) c)
                             
-- | Crossoever function with two random crossever points. 
xoverTwoPivot :: RandomGen g => Chrom -> Chrom -> 
                  Rand g (String, String)
xoverTwoPivot ch1 ch2 = 
  do pivot1 <- getRandomR (0, length (gene ch1) - 1)
     pivot2 <- getRandomR (pivot1 + 1, length (gene ch1) )
     let child1 = (take pivot1 (gene ch1)) ++
                  ((drop pivot1) . (take pivot2)) (gene ch2) ++
                  (drop pivot2 (gene ch1))
     let child2 = (take pivot1 (gene ch2)) ++
                  ((drop pivot1) . (take pivot2)) (gene ch1) ++
                  (drop pivot2 (gene ch2))
     return (child1, child2)
                                                          
-- | QuickCheck property for xoverChTwo
prop_xoverTwoPivot :: PropertyM IO ()
prop_xoverTwoPivot = run (evalRandIO $ twoTupleM (genRandom defTarget)
                          >>= makeTuple) 
                     >>= assert . check
  where check :: (Chrom, Chrom, (String, String)) -> Bool
        check (p1, p2, (c1, c2)) = length c1 == length (gene p1) && 
                                   length c2 == length (gene p1) && 
                                   checkAllFromPTwoPivot p1 p2 c1 &&
                                   checkAllFromPTwoPivot p2 p1 c2 &&
                                   checkFromEitherOfTwoPa p1 p2 c1 c2
        makeTuple :: RandomGen g => (Chrom, Chrom) -> 
                     Rand g (Chrom, Chrom, (String, String))
        makeTuple (p1,p2) = (p1,p2,) <$> xoverTwoPivot p1 p2

-- | Check if all characters of a child is from either one of its parents.
-- and number of pivots is 2.
checkAllFromPTwoPivot :: Chrom -> Chrom -> String -> Bool
checkAllFromPTwoPivot p1 p2 c = (and . map (\(x, _, z) -> x == z) .
                                 dropWhile (\(_, y, z) -> y == z) .
                                 dropWhile (\(x, _, z) -> x == z)
                                 $ zip3 (gene p1) (gene p2) c)
                                
-- | Crossoever function with random multiple crossever points. 
-- child1 is created randomly at each point from either parent1 or 
-- parent2, and child2 has untaken genes.
xoverRandomPick :: RandomGen g => Chrom -> Chrom -> 
                   Rand g (String, String)
xoverRandomPick p1 p2 = do r :: [Int] <- replicateM ((length . gene) p1)
                                                    (getRandomR (0,1))
                           let combined = zip3 (gene p1) (gene p2) r
                           let child1 = map (\(f,s,n) ->
                                             if n == 0 then f else s) combined
                           let child2 = map (\(f,s,n) -> 
                                             if n == 1 then f else s) combined
                           return (child1, child2)
                   
-- | QuickCheck property for xoverRandomPick
prop_xoverRandomPick :: PropertyM IO ()
prop_xoverRandomPick = run (evalRandIO $ twoTupleM (genRandom defTarget)
                            >>= makeTuple) 
                     >>= assert . check
  where check :: (Chrom, Chrom, (String, String)) -> Bool
        check (p1, p2, (c1, c2)) = length c1 == length (gene p1) && 
                              length c2 == length (gene p1) && 
                              checkFromEitherOfTwoPa p1 p2 c1 c2
        makeTuple :: RandomGen g => (Chrom, Chrom) -> 
                     Rand g (Chrom, Chrom, (String, String))
        makeTuple (p1,p2) = (p1,p2,) <$> xoverRandomPick p1 p2

-- | check if the children has genes from strictly either from p1 or p2
-- | but not from both of them.
checkFromEitherOfTwoPa :: Chrom -> Chrom -> String -> String -> Bool
checkFromEitherOfTwoPa p1 p2 c1 c2  = and $ map2 (\(a,b) (c,d) ->
                                     if a == c then b == d
                                     else a==d && b==c )
                                     (zip (gene p1) (gene p2)) 
                                     (zip c1 c2)

-- | Selector function for mutate function.
mutate :: RandomGen g => Int -> String -> Chrom -> Rand g Chrom 
mutate option targ ch
  | option == 1 = mutateRand ch >>= makeChrom
  | option == 2 = mutateSwap ch >>= makeChrom
  | otherwise = error "Invalid Option"
    where makeChrom :: String -> Rand g Chrom
          makeChrom = \x -> return $ Chrom (gen ch + 1) (fitness targ x) x

-- | Mutate the string at random position with random character.
mutateRand :: RandomGen g => Chrom -> Rand g String
mutateRand ch = do index <- getRandomR (0, length (gene ch) - 1)
                   let (split1, split2) = splitAt index (gene ch)
                   mutChar <- genRandomChar (gene ch)
                   return $ split1 ++ (mutChar : tail split2)
                 
-- | QuickCheck Property for mutateRandCh function
prop_mutateRand :: PropertyM IO ()
prop_mutateRand = run (evalRandIO $ (genRandom defTarget)
                       >>= makeMutTuple) >>= assert . check
                where check :: (Chrom, String) -> Bool
                      check (orig, mut) = length (nub (gene orig) 
                                                  \\ nub mut) <= 1 &&
                                          length (gene orig) 
                                          == length mut
                      makeMutTuple :: RandomGen g => Chrom -> 
                                      Rand g (Chrom, String)
                      makeMutTuple orig = (orig,) <$> mutateRand orig
                      
-- | Mutate by randomly swapping two characters
mutateSwap :: RandomGen g => Chrom -> Rand g String
mutateSwap ch = do idx1 <- getRandomR (0, length (gene ch) - 2)
                   idx2 <- getRandomR (idx1 + 1, length (gene ch) - 1)
                   return $ swap idx1 idx2 (gene ch)
                     
-- | QuickCheck Property for mutateChSwap function
prop_mutateSwap :: PropertyM IO ()
prop_mutateSwap = run (evalRandIO $ (genRandom defTarget)
                       >>= makeMutTuple) 
                  >>= assert . check
                where check :: (Chrom, String) -> Bool
                      check (orig, mut) = (nub (gene orig) \\ 
                                           nub mut == []) &&
                                          length mut == length (gene orig)
                      makeMutTuple orig = (orig,) <$> mutateSwap orig
                                          
-- | Fitness function. Works only for genes(strings) with the same length. 
-- If the gene is longer and have the target as prefix e.g. 
-- "Haskell is fun..." it will score zero.
fitness :: String -> String -> Int
fitness targ str = sum $ map abs (zipWith ((-) `on` fromEnum) 
                                   targ str)
             
-- | unit test for fitness
test_fitness :: Test
test_fitness = test [ fitness defTarget "Haskell is fun."  ~=? 0,
                      fitness defTarget "Python is better" ~=? 419,
                      fitness defTarget "Java is messy"    ~=? 212,
                      fitness defTarget "Haskell is fun?"  ~=? 17 ]

-- | Generate a random chromosome with the same length as the target. 
genRandom :: RandomGen g => String -> Rand g Chrom
genRandom targ = do genes <- replicateM (length targ) 
                             $ genRandomChar targ
                    return $ Chrom 1 (fitness targ genes) genes 
              
-- | generate random character for mutation or population initialization
-- if the input gene is binary, only generates binary 0/1.
genRandomChar :: RandomGen g => String -> Rand g Char
genRandomChar genes 
  | nub genes == ['0','1'] =  getRandomR ('0', '1')
  | otherwise              =  getRandomR (' ', 'z')
               
-- | QuickCheck Property for genRandom
prop_genRandom :: PropertyM IO ()
prop_genRandom = run (evalRandIO (genRandom defTarget)) >>= assert . check
  where check :: Chrom -> Bool
        check ch = all (between (fromEnum ' ') 
                        (fromEnum 'z') . fromEnum) (gene ch) &&  
                   length defTarget == (length . gene) ch &&
                   fit ch >= 0

        between :: Int -> Int -> Int -> Bool
        between l r x = l <= x && x <= r
        
        
