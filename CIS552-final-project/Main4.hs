-- by Partick Yoon yeyoon and Mark Smyda msmyda

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, NamedFieldPuns, 
             TupleSections,  FlexibleContexts #-}

module Main2 where

import Control.Applicative
import Control.Monad.State
import Data.List (minimumBy, sortBy, nub, (\\), findIndex)
import Test.QuickCheck
import Data.Ord (comparing)
import Text.Printf (printf)
import Data.Function (on)
import Control.Monad.Random
import Test.QuickCheck.Monadic hiding (stop)
import System.Console.GetOpt

import Test.HUnit hiding (assert)

-- | General TODO(patrick) (in order of priority): 
-- -2) Fix naming of functions & Add more comments 
-- -1) Fix Compiler warning for TwoM and ThreeM
-- 0) Write README, prepare answers for questions.
-- 1) Enable options for selecting mutation, crossover, and selection methods.
-- 2) Enable profiling during evolution
-- 3) Write, possibly visualize distribution of fitness over generations.

data Flag = InitPopSize Int
          | ElitismRatio Double
          | MutRatio Double
          | XOverRatio Double
          | TournaSize Int
          | MaxGen Int
          | XOverOption Int
          | MutOption Int
          | SelOption Int
          | Target String
          deriving (Eq, Show, Read)

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
    , Option "xo" ["xoverOption"] (ReqArg (XOverOption . read)
      "<crossover option>" )
      "Whether or not to enable crossing over in the simulation"
    , Option "mo" ["mutOption"] (ReqArg (MutOption . read)
      "<mutation option>" )
      "Whether or not to enable random mutation of candidates"
    , Option "s" ["selOption"] (ReqArg (SelOption . read)
      "<selection option>" )
      "Which method to use for selecting candidates for next generation"
    , Option "tg" ["target"] (ReqArg (Target . read)
      "<target>" )
      "The string to which we should attempt convergence" ]

-- | The type of Gene is a string of ASCII characters.
-- TODO(patrick) : we can try to use bit string as well?
-- TODO(patrick) : better to have fitness information as well?
data Chrom = Chrom { gene :: String,
                               gen :: Int }
                deriving (Eq, Ord, Show)

instance Arbitrary (Chrom) where
  arbitrary = do gene <- replicateM (length (gene target))
                          $ (arbitrary :: Gen Char)
                 return $ Chrom gene 0
       
-- | Pre-defined target chromosome with target string.
-- TODO(patrick) : enable user to input target string using IO input.
target :: Chrom
target = Chrom "Haskell is fun." (-1)

-- | Function for doing crossover of two chromosomes
-- Here we select a random pivot for the crossoever.
-- For the first child, select the characters with index i < pivot from the
-- first parent, then the remaining characters from the second parent with
-- index i such that pivot <= i < length.
-- For the second child, we select the characters that are not included in 
-- the first child. (Using the same pivot point)
-- This is crossover function that returns only one child created.
xoverGetOne :: RandomGen g => Chrom -> Chrom -> Rand g Chrom
xoverGetOne ch1 ch2 = do pivot <- getRandomR (0, length (gene ch1) - 1)
                         let child = take pivot (gene ch1) 
                                     ++ drop pivot (gene ch2)
                         return $ Chrom child ((gen ch1)+1)        

-- | QuickCheck property for crossover
prop_xoverGetOne :: PropertyM IO ()
prop_xoverGetOne = run (evalRandIO $ twoM genRandom >>= makeTuple)
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
                         let child1 = take pivot (gene ch1) 
                                      ++ drop pivot (gene ch2)
                         let child2 = take pivot (gene ch2) 
                                      ++ drop pivot (gene ch1)
                         return $ (Chrom child1 ((gen ch1)+1),
                                   Chrom child2 ((gen ch1)+1))
                       
-- | QuickCheck property for xoverChTwo
prop_xoverGetTwo :: PropertyM IO ()
prop_xoverGetTwo = run (evalRandIO $ twoM genRandom >>= makeTuple)
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
                                where pivot :: Int
                                      pivot = case findIndex (\(x,y) -> x/=y)
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
prop_xoverTwoPivot = run (evalRandIO $ twoM genRandom >>= makeTuple) 
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
                                where pivot1 :: Int
                                      pivot1 = case findIndex (\(x,y) -> x/=y)
                                                   $ zip (gene p1) (gene c1) of
                                                Just x -> x
                                                Nothing -> (length . gene) p1
                                      pivot2 :: Int
                                      pivot2 = case findIndex (\(x,y) -> x/=y)
                                                   $ zip ((reverse . gene) p1)
                                                    ((reverse . gene) c1) of
                                                Just x -> (length . gene) p1
                                                           - x
                                                Nothing -> 0

-- | Mutate the string at random position with random character.
mutate :: RandomGen g => Chrom -> Rand g Chrom
mutate ch = do index <- getRandomR (0, length (gene ch) - 1)
               let (split1, split2) = splitAt index (gene ch)
               mutChar <- getRandomR (' ', 'z')
               return $ Chrom (split1 ++ (mutChar : tail split2)) (gen ch)
                 
-- | QuickCheck Property for mutateCh function
prop_mutate :: PropertyM IO ()
prop_mutate = run (evalRandIO $ genRandom >>= makeMutTuple) >>= assert . check
                where check :: (Chrom, Chrom) -> Bool
                      check (orig, mut) = length (nub (gene orig) 
                                                  \\ nub (gene mut)) <= 1 &&
                                          length (gene orig) 
                                          == length (gene mut)
                      makeMutTuple :: RandomGen g => Chrom -> 
                                      Rand g (Chrom, Chrom)
                      makeMutTuple orig = (orig,) <$> mutate orig

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
                                          length (gene mut) == 
                                          length (gene orig)
                      makeMutTuple :: RandomGen g => Chrom -> 
                                      Rand g (Chrom, Chrom)
                      makeMutTuple orig = (orig,) <$> mutateSwap orig

-- | Helper swap function for mutateChSwap. 
-- Swaps element of a list at two indices.
swap :: Int -> Int -> [a] -> [a]
swap i j xs 
  |  i == j   = xs
  | otherwise = take l xs ++ [xs !! u] ++ 
                (take (u-l-1) $ drop (l+1) xs) ++ [xs !! l] ++ drop (u+1) xs
                where l :: Int
                      l = if i < j then i else j
                      u :: Int
                      u = if i > j then i else j
                                          
-- | Fitness function. Works only for chromosomes with the same length.
-- If the chromosome is longer and have the target as prefix e.g. 
-- "Haskell is fun..." it will score zero.
fitness :: Chrom -> Int
fitness ch = sum $ 
             map abs (zipWith ((-) `on` fromEnum) (gene target) (gene ch))

-- | unit test for fitness
unittest_fitness :: Test
unittest_fitness = test [ fitness (Chrom "Haskell is fun." 0)  ~=? 0, 
                          fitness (Chrom "Python is better" 0) ~=? 419,
                          fitness (Chrom "Java is messy" 0)    ~=? 212,
                          fitness (Chrom "Haskell is fun?" 0)  ~=? 17 ]

-- | Generate a random chromosome with the same length as the target.
genRandom :: RandomGen g => Rand g Chrom
genRandom = do genes <- replicateM (length (gene target)) 
                        $ getRandomR (' ', 'z')
               return $ Chrom genes 0
              
-- | QuickCheck Property for genRandom
prop_genRandom :: PropertyM IO ()
prop_genRandom = run (evalRandIO genRandom) >>= assert . check
  where check :: Chrom -> Bool
        check ch = and $ map ($ ch) [all (between (fromEnum ' ') 
                                        (fromEnum 'z') . fromEnum) . gene,
                                  (== length (gene target)) . length . gene,
                                  (>= 0) . fitness]
        between :: Ord a => a -> a -> a -> Bool
        between l r x = l <= x && x <= r
           
-- | Select two parents to crossover. 
selParents :: RandomGen g => Pop Chrom -> Rand g (Chrom, Chrom)
selParents = twoM . tournaSelect

-- | The Population has information about the population
-- and a list of Chroms with pre-defined size. 
data Pop ch = Pop { info :: PopInfo, 
                 chs  :: [ch] }
             deriving Show
             
-- | Population information data for evolution.
-- initPopSize : Size of population
-- eliRatio (Elitism ratio) : This ratio of population sorted by fitness is
-- directly passed to the next generation without any manipulation.
-- xoverRatio (Crossover ratio) : The proportion of the rest of the population
-- that goes through crossoever. The reminder is copied over directly.
-- muRatio (Mutation ratio) : In either case of crossover or direcy copy,
-- each of next generation chromosomes goes random mutation based on the
-- mutation ratio.
-- tournaSize (Tournament Size) : Tournament size of tournament selection
-- method.
data PopInfo = PopInfo { initPopSize  :: Int, 
                         eliRatio     :: Double,
                         xoverRatio   :: Double,
                         muRatio      :: Double, 
                         tournaSize   :: Int }
               deriving Show

-- | Maximum number of generations
-- TODO(patrick): Make this as user input as well?
maxGen :: Int
maxGen = 10000

getSize :: Pop ch -> Int
getSize (Pop info _) = initPopSize info
               
-- | Default population information.
defaultConfig :: PopInfo
defaultConfig = PopInfo 1000 0.5 0.2 0.01 3

-- | generate random population of size initPopSize
randomPop :: RandomGen g => PopInfo -> Rand g (Pop Chrom)
randomPop info = do chs <- replicateM (initPopSize info) genRandom 
                    return $ Pop info (sortBy (comparing fitness) chs)
                    
-- | TODO(patrick) : Write QuickCheck property for randomPop
-- | QuickCheck Property for genRandom.. property on size, sorted
prop_randomPop :: PropertyM IO ()
prop_randomPop = run (evalRandIO genRandom) >>= assert . check
  where check :: Chrom -> Bool
        check g = and $ map ($ g) [ (>= 0) . fitness
                                  , (== length (gene target)) . length . gene
                                  , all (between 32 122 . fromEnum) . gene ]
        between :: Ord a => a -> a -> a -> Bool
        between l r x = l <= x && x <= r
                 
-- | Implements Deterministic tournament selection algorithm.
-- Choose k individuals from the population at random
-- choose the best individual from pool/tournament 
tournaSelect :: RandomGen g => Pop Chrom -> Rand g Chrom
tournaSelect p@(Pop info chs) =  minimumBy (comparing fitness). map 
                                 (chs !!) <$> replicateM (tournaSize info)
                                 (getRandomR (0, getSize p - 1))

-- | TODO(patrick) : Write QuickCheck property for tournaSelect

-- | Fitness proportionate selection (FPS)
-- TODO(patrick) : Choose k individuals from the population at random
-- After picking k elements, make the probability of selection 
-- be proportional to fitness value
fitPropSelect :: RandomGen g => Pop Chrom -> Rand g Chrom
fitPropSelect p@(Pop info chs) =  minimumBy (comparing fitness) .  
                                   map (chs !!) <$>   
                         replicateM (tournaSize info) 
                                    (getRandomR (0, getSize p - 1))

-- | Helper function : make a two-tuple by replicating a monad instance twice
twoM :: Monad m => m a -> m (a, a)
twoM = liftM toTwoTuple . replicateM 2 
  where toTwoTuple :: [a] -> (a,a)
        toTwoTuple [x,y] = (x,y)
        toTwoTuple _     = error "Impossible case"

-- | Helper function : make a three-tuple by replicating a monad instance
-- three times
threeM :: Monad m => m a -> m (a, a, a)
threeM = liftM toThreeTuple . replicateM 3
         where toThreeTuple :: [a] -> (a,a,a)
               toThreeTuple [x, y, z] = (x, y, z)
               toThreeTuple _         = error "Impossible case."

-- | Evolve a population using just one child from a crossover.
evolveOne :: RandomGen g => Pop Chrom -> Rand g (Pop Chrom)
evolveOne p@(Pop info@(PopInfo size el xover mu _ ) chs) = 
  do nextGens <- (replicateM (size - idx)) getChild
     return $ Pop info (take idx chs ++ nextGens)
       where idx :: Int
             idx = round (fromIntegral size * el)
             addChild :: RandomGen g => Double -> Chrom -> Rand g Chrom
             addChild m
               | m <= mu = mutate
               | otherwise = return
             addMutation :: RandomGen g => Double -> Rand g Chrom
             addMutation m
               | m <= mu = mutate . (chs !!) =<< getRandomR (idx, size - 1)
               | otherwise = (chs !!) <$> getRandomR (idx, size - 1)
             getChild :: RandomGen g => Rand g Chrom
             getChild = do (r, m) <- twoM getRandom
                           if r <= xover 
                             then do (p1, p2) <- selParents p
                                     c1 <- xoverGetOne p1 p2
                                     c1' <- addChild m c1
                                     return c1'
                             else do c1 <- addMutation m
                                     return c1
                                        
-- | Evolve a population using both resultant child of crossover.
evolve :: RandomGen g => Pop Chrom-> Rand g (Pop Chrom)
evolve p@(Pop info@(PopInfo size el xover mu _ ) chs) = 
  do nextGens <- (replicateM (size - idx)) getChildren
     return $ Pop info (sortBy (comparing fitness) 
                        (take idx chs ++ concat nextGens))
       where idx :: Int
             idx = round (fromIntegral size * el)
             addChild :: RandomGen g => Double -> Chrom -> Rand g Chrom
             addChild m
               | m <= mu = mutate
               | otherwise = return
             addMutation :: RandomGen g => Double -> Rand g Chrom
             addMutation m
               | m <= mu = mutate . (chs !!) =<< getRandomR (idx, size - 1)
               | otherwise = (chs !!) <$> getRandomR (idx, size - 1)
             getChildren :: RandomGen g => Rand g [Chrom]
             getChildren = do (r, m1, m2) <- threeM getRandom
                              if r <= xover
                                then do (p1, p2) <- selParents p
                                        (c1, c2) <- xoverGetTwo p1 p2
                                        c1' <- addChild m1 c1
                                        c2' <- addChild m2 c2
                                        return $ c1' : [c2']
                                else do c1 <- addMutation m1
                                        return [c1]

-- | repeat until a condition is met. Used for 
-- repeatedly evolving the population.
repeatWhile :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
repeatWhile stop f = go
  where go x | stop x = return x
             | otherwise = f x >>= go
                           
-- | Execute the genetic algorithm and print the result to stdout.
main :: IO ()
main = evalRandIO (randomPop defaultConfig >>= repeatWhile terminate evolve)
       >>= printResult
  
-- | Terminate condition for a population
terminate :: Pop Chrom -> Bool
terminate (Pop _ (ch:_::[Chrom])) = fitness ch == 0 || (gen ch) == maxGen
terminate (Pop _ []) = True

-- | Print the output to stdout
printResult :: Pop Chrom -> IO ()
printResult (Pop _ (ch:_ :: [Chrom]))
  | fitness ch == 0 = printf "Reached target string at gen %d: %s\n"
                             (gen ch) $ show ch
  | gen ch == maxGen = putStrLn 
                       "Max generation reached without reaching target"
  | otherwise = putStrLn "Error"
printResult (Pop _ []) = putStrLn "Error : Population is Empty"

-- | Run QuickCheck for all properties.
runQuickChecks :: IO ()
runQuickChecks = do mapM_ (quickCheck . monadicIO)
                      [prop_xoverGetOne, prop_mutate, prop_mutateSwap,
                       prop_xoverGetTwo, prop_genRandom,
                       prop_xoverTwoPivot]
