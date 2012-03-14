-- by Partick Yoon yeyoon and Mark Smyda msmyda

{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-orphans #-} 
{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, NamedFieldPuns, 
TupleSections,  FlexibleContexts #-}

module Main where

import Control.Applicative
import Control.Monad.State
import Data.List (minimumBy, sortBy, nub, (\\), findIndex, unfoldr)
import Test.QuickCheck
import Data.Ord (comparing)
import Text.Printf (printf)
import Data.Function (on)
import Control.Monad.Random
import Test.QuickCheck.Monadic hiding (stop)
import System.Console.GetOpt
import System (getArgs)
import Test.HUnit hiding (assert)

-- | General TODO(patrick) (in order of priority): 
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
          | BinaryTarget
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
    , Option "r" ["xoverOption"] (ReqArg (XOverOption . read)
      "<crossover option>" )
      "Whether or not to enable crossing over in the simulation"
    , Option "u" ["mutOption"] (ReqArg (MutOption . read)
      "<mutation option>" )
      "Whether or not to enable random mutation of candidates"
    , Option "s" ["selOption"] (ReqArg (SelOption . read)
                                "<selection option>" )
      "Which method to use for selecting candidates for next generation"
    , Option "b" ["BinTar"] (NoArg BinaryTarget)
      "Whether or not to use binary representation for target"]

sameFlag :: Flag -> Flag -> Bool
sameFlag (InitPopSize _) (InitPopSize _) = True
sameFlag (ElitismRatio _) (ElitismRatio _) = True
sameFlag (MutRatio _) (MutRatio _) = True
sameFlag (XOverRatio _) (XOverRatio _) = True
sameFlag BinaryTarget BinaryTarget = True
sameFlag (TournaSize _) (TournaSize _) = True
sameFlag (MaxGen _) (MaxGen _) = True
sameFlag (MutOption _) (MutOption _) = True
sameFlag (SelOption _) (SelOption _) = True
sameFlag _ _= False

tryOpt :: Flag -> [Flag] -> Flag
tryOpt flag (f:flags) | sameFlag flag f = f
                      | otherwise = tryOpt flag flags
tryOpt flag [] = flag

tryOpt' :: Flag -> [Flag] -> Maybe Flag
tryOpt' flag (f:flags) | sameFlag flag f = Just f
                       | otherwise = tryOpt' flag flags
tryOpt' _ [] = Nothing

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv = 
    case getOpt Permute options argv of
        (o, n, []  ) -> return (o,n)
        (_ ,_, errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: Main [OPTION...]"

-- | Genetic Algorithm configuration 
-- xoverOption : {xoverGetTwo = 1, xoverTwoPivot =3,
-- xoverRandomPick = 4}
-- muOption : {mutate = 1, mutateSwap =2}
-- selOption : {tournaSelect = 1, fitPropSelect = 2}
data AlgoConfig = AlgoConfig { xoverOption  :: Int,
                               muOption     :: Int,
                               selOption    :: Int }

instance Show AlgoConfig where
  show (AlgoConfig xOption muOption selOption) = 
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
                 
-- | Converts a character into its binary represenation
-- reference : PLEAC-Haskell
char2bin :: Char -> [Char]
char2bin = map i2c . reverse . unfoldr decomp . fromEnum
  where decomp :: Int -> Maybe (Int, Int)
        decomp n = if n == 0 then Nothing else Just(n `mod` 2, n `div` 2)
        i2c :: Int -> Char
        i2c i = if i == 0 then '0' else '1'

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

-- | helper swap function for mutateChSwap. 
-- Swaps element of a list at two indices.
swap :: Int -> Int -> [a] -> [a]
swap i j xs 
  |  i == j   = xs
  | otherwise = take l xs ++ [xs !! u] ++ 
                (take (u-l-1) $ drop (l+1) xs) ++ [xs !! l] ++ drop (u+1) xs
                where l = if i < j then i else j
                      u = if i > j then i else j
                                          
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
-- binaryTarget : True to use binary representation of target
data PopInfo = PopInfo { initPopSize  :: Int, 
                         eliRatio     :: Double,
                         xoverRatio   :: Double,
                         muRatio      :: Double, 
                         tournaSize   :: Int}

instance Show PopInfo where
  show (PopInfo size er xr mr ts) = 
    "Initial Population Size : " ++ (show size) ++
    "\nElitism Ratio : " ++ (show er) ++
    "\nCrossover Ratio : " ++ (show xr) ++
    "\nMutation Ratio : " ++ (show mr) ++
    "\nTournament Size : " ++ (show ts)

getSize :: Pop ch -> Int
getSize (Pop info _) = initPopSize info
               
-- | Default population configuration
-- Used for QuickCheck
defaultPopConfig :: PopInfo
defaultPopConfig = PopInfo 1000 0.5 0.2 0.01 3

-- | generate random population of size initPopSize
randomPop :: RandomGen g => PopInfo -> Rand g (Pop Chrom)
randomPop info = do chs <- replicateM (initPopSize info) genRandom 
                    return $ Pop info (sortBy (comparing fitness) chs)
                    
-- | QuickCheck Property for genRandom. 
-- Checks if property on size, sorted
prop_randomPop :: PropertyM IO ()
prop_randomPop = run ((evalRandIO . (randomPop)) 
                      defaultPopConfig) >>= assert . check
  where check :: Pop Chrom -> Bool
        check pop = (length . chs) pop == (initPopSize . info) pop &&
                isOrdered fitness (chs pop)
                
-- | Helper function for checking if a list is ordered
isOrdered :: Ord b => (a -> b) -> [a] -> Bool
isOrdered f (x1:x2:xs) = f x1 <= f x2 && isOrdered f (x2:xs)
isOrdered _ _          = True

-- | Implements Deterministic tournament selection algorithm.
-- Choose k individuals from the population at random
-- choose the best individual from pool/tournament 
tournaSelect :: RandomGen g => Pop Chrom -> Rand g Chrom
tournaSelect p@(Pop info chs) =  minimumBy (comparing fitness) . map
                                 (chs !!) <$> replicateM (tournaSize info) 
                                 (getRandomR (0, getSize p - 1))

-- | Fitness proportionate selection 
-- Choose k individuals from the population at random
-- After picking k elements, make the probability of selection 
-- be proportional to fitness ranking
fitPropSelect :: RandomGen g => Pop Chrom -> Rand g Chrom
fitPropSelect p@(Pop info chs) =     
  do chroms <- map (chs !!) <$> 
               replicateM (tournaSize info) (getRandomR (0, getSize p - 1))
     let pool = makePool chroms
     index <- getRandomR (0, length pool - 1)
     return $ pool !! index

-- | Helper function : Make a pool of chromosome where the number 
-- of the same chromosome is inversely proportional to its rank in the pool.
makePool :: [Chrom] -> [Chrom]
makePool chroms = foldr (\(x,y) zs -> replicate x y ++ zs) 
                  [] $ zip (reverse [1..(length chroms)]) 
                  (sortBy (comparing fitness) chroms)

-- | Helper function : make a two-tuple by replicating a monad instance twice
twoTupleM :: Monad m => m a -> m (a, a)
twoTupleM = liftM toTwoTuple . replicateM 2 
  where toTwoTuple :: [a] -> (a, a)
        toTwoTuple [x,y] = (x,y)
        toTwoTuple _     = error "Impossible case"

-- | Helper function : make a three-tuple by replicating a monad instance 
-- three times
threeTupleM :: Monad m => m a -> m (a, a, a)
threeTupleM = liftM toThreeTuple . replicateM 3
         where toThreeTuple :: [a] -> (a, a, a)
               toThreeTuple [x, y, z] = (x, y, z)
               toThreeTuple _         = error "Impossible case."

-- | Select two parents to crossover. 
selParents :: RandomGen g => Int -> Pop Chrom -> Rand g (Chrom, Chrom)
selParents option
  | option == 1 = twoTupleM . tournaSelect
  | option == 2 = twoTupleM . fitPropSelect
  | otherwise = error "Invalid Option for Selection method"
                
-- | Evolve a population using just one child from a crossover.
evolveOne :: RandomGen g => AlgoConfig -> Pop Chrom -> Rand g (Pop Chrom)
evolveOne (AlgoConfig _ mo so) p@(Pop info@(PopInfo size elr xor mur _) chs) =
  do nextGens <- (replicateM (size - idx)) getChild
     return $ Pop info (take idx chs ++ nextGens)
       where idx = round (fromIntegral size * elr)
             addChild :: RandomGen g => Double -> Chrom -> Rand g Chrom
             addChild m
               | m <= mur = mutate mo
               | otherwise = return
             addMutation :: RandomGen g => Double -> Rand g Chrom
             addMutation m
               | m <= mur = (mutate mo) . (chs !!) =<< getRandomR (idx, size - 1)
               | otherwise = (chs !!) <$> getRandomR (idx, size - 1)
             getChild :: RandomGen g => Rand g Chrom
             getChild = do (r, m) <- twoTupleM getRandom
                           if r <= xor 
                             then do (p1, p2) <- (selParents so) p
                                     c1 <- xoverGetOne p1 p2
                                     c1' <- addChild m c1
                                     return c1'
                             else do c1 <- addMutation m
                                     return c1
                                        
-- | Evolve a population using both resultant child of crossover.
evolve :: RandomGen g => AlgoConfig -> Pop Chrom-> Rand g (Pop Chrom)
evolve (AlgoConfig xo mo so) p@(Pop info@(PopInfo size elr xor mur _) chs) = 
  do nextGens <- (replicateM (size - idx)) getChildren
     return $ Pop info (sortBy (comparing fitness) 
                        (take idx chs ++ concat nextGens))
       where idx = round (fromIntegral size * elr)
             addChild :: RandomGen g => Double -> Chrom -> Rand g Chrom
             addChild m
               | m <= mur = mutate mo
               | otherwise = return
             addMutation :: RandomGen g => Double -> Rand g Chrom
             addMutation m
               | m <= mur = (mutate mo) . (chs !!) =<< getRandomR (idx, size - 1)
               | otherwise = (chs !!) <$> getRandomR (idx, size - 1)
             getChildren :: RandomGen g => Rand g [Chrom]
             getChildren = do (r, m1, m2) <- threeTupleM getRandom
                              if r <= xor
                                then do (p1, p2) <- selParents so p
                                        (c1, c2) <- xover xo p1 p2
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
main :: IO() 
main = do 
  args <- getArgs
  (opts, _) <- compilerOpts args
  let (InitPopSize ps) = tryOpt (InitPopSize 1000) opts
  let (ElitismRatio er) = tryOpt (ElitismRatio 0.5) opts
  let (MutRatio mr) = tryOpt (MutRatio 0.01) opts
  let (XOverRatio xr) = tryOpt (XOverRatio 0.2) opts
  let (TournaSize ts) = tryOpt (TournaSize 3) opts
  let (MaxGen mg) = tryOpt (MaxGen 10000) opts
  let (XOverOption xo) = tryOpt (XOverOption 1) opts
  let (MutOption mo) = tryOpt (MutOption 1) opts
  let (SelOption so) = tryOpt (SelOption 1) opts              
  let popConfig = PopInfo ps er xr mr ts
  let algoConfig = AlgoConfig xo mo so
  chrom <- evalRandIO (randomPop popConfig 
                       >>= repeatWhile (terminate mg) (evolve algoConfig))
  printResult mg algoConfig chrom
  
-- | Terminate condition for a population
terminate :: Int -> Pop Chrom -> Bool
terminate maxGen (Pop _ (ch:_::[Chrom])) = fitness ch == 0 || (gen ch) == maxGen
terminate _ (Pop _ []) = True

-- | Print the output to stdout
printResult :: Int -> AlgoConfig -> Pop Chrom -> IO ()
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
