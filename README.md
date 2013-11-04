Genetic Algorithm Program - By Patrick Yeo Ho Yoon & Mark Smyda [CIS 552 Final Project]
=================================================================

### Additional Libraries that the projects depends on.

 - GetOpt
 - MonadRandom (Control.Monad.Random)
 - QuickCheck, Monadic QuickCheck (Test.QuickCheck.Monadic)
 - HUnit (System.Console.GetOpt)
 - Text.Printf
 - Control.Applicative

### List of source files

1. Chrom.hs

     1. Definition of Chromosome Data
     2. Crossover Methods for Chromosome. Numbers indicate option number for execution (will be explained below).
        - One Pivot Crossover, Use Two Children (Default) : Cross-over on one pivot. Use both resulting children for next generation.
        - One Pivot Crossover, Use One Child : Cross-over on one pivot. Use only one resulting child for next generation.
        - Two Pivot Crossover, Use Two Children : Same as 1., but crossing over on two pivots.
        - Random Pivot Crossover : Multiple Pivots are chosen randomly. Use both children for next generation.
                                    Each two genes from two parents are assigned randomly to two children.
     3. Mutation Methods for Chromosome. Numbers indicate option number for execution (will be explained below).
       - Random Character Switch Mutation : One character is switched to a random character in a random position.
        - Swap Two Genes Mutation : Two randomly chosen genes are swapped in a chromosome
     4. Fitness function : Defined as closeness to the target (Sum of differences of ASCII numbers)
     5. QuickCheck Property for mutation & crossover functions
        Random chromosome generation function and QuickCheck property

2. GAConfig.hs

     1.  Contains data for Genetic algorithm configurations and show instance.
     2.  Crossover Option : {xoverGetTwo = 1, xoverGetOne = 2, xoverTwoPivot =3, xoverRandomPick = 4}
     3.  Mutation Option : {mutate = 1, mutateSwap =2}
     4.  Selection Option : {tournaSelect = 1, fitPropSelect = 2}

3. Helper.hs : Contains all helper functions for all other modules.

4. Pop.hs

     1. Definition of Population data with parameters:
       - Elitism Ratio : ratio of % of fittest population carried over to the next generation without any manipulation.
       - Crossover Ratio : The proportion of the rest of the population that goes through crossoever.
                            The reminder is copied over directly.
       - Mutation Ratio : Mutation ratio : In either case of crossover or direcy copy, each of next generation chromosomes
                           goes random mutation based on the mutation ratio.
       - Initial Population Size
       - Maximum number of Generation.
    2. Random population initialization function, QuickCheck Property
    3. Parents Selection Methds : Numbers indicate option number for execution (will be explained below).
       - Tournament Selection : Pick k elements randomly from population and pick the fittest one.
       - Fitness Ranking Based Selection : Pick k elements randomly from population and pick one with probability inversely proportional to the ranking in the k-pool.
    4. Population evolution method.

5. Main.hs

    1. Contains logics for specifying options for Genetic Algorithm parameters and printing the result.
    2. has Main function to execute the program and runQuickChecks function to run all QuickChecks.
    3. The options are

  -i <initial population size>  --initpopsize=<initial population size>  How many candidate strings should be initialized in the population
  -e <elitism ratio>            --eliRatio=<elitism ratio>               What proportion of candidates continues to next gen without change
  -m <mutation ratio>           --mutRatio=<mutation ratio>              What proportion of candidates is chosen for random mutation
  -x <crossover ratio>          --xoverRatio=<crossover ratio>           The proportion of strings that undergoes crossover
  -t <tournament size>          --tournaSize=<tournament size>           The size of the tournament in selecting parent genes from population
  -g <maximum generations>      --maxGen=<maximum generations>           The maximum number of generations to attempt before quitting
  -v <crossover option>         --xoverOption=<crossover option>         Crossover Method Option. Read README file for list of options.
  -u <mutation option>          --mutOption=<mutation option>            Mutation Method Option. Read README file for list of options.
  -s <selection option>         --selOption=<selection option>           Selection Method Option. Read README file for list of options.
  -b                            --BinTar                                 Whether or not to use binary representation for target
  -r <target>                   --target=<target>                        The string to which we should attempt convergence

Sample Output
--------------

For example the command "./Main -b -i 10" will give the result,

Reached target at gen 1315: Chrom {gen = 1315, fit = 0, gene = "100100011000011110011110101111001011101100110110010000011010011110011100000110011011101011101110101110"}
Initial Population Size : 10
Elitism Ratio : 0.5
Crossover Ratio : 0.2
Mutation Ratio : 1.0e-2
Tournament Size : 3
Crossover Method : One Pivot Crossover, Use Two Children
Mutation Method : Random Character Switch Mutation
Selection Method : Tournament Selection
Genetic Algorithm Program - By Patrick Yeo Ho Yoon yeyoon & Mark Smyda msmyda
