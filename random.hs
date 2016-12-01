import System.Random

finiteRandoms :: (RandomGen g, Random a) => Int -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen = (value:restOfList, finalGen)
  where (value, newGen)        = random gen
        (restOfList, finalGen) = finiteRandoms (n-1) newGen
