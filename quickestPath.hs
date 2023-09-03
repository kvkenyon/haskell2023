import System.Directory.Internal.Prelude (getArgs)

data RoadSystem = Section
  { roadA :: Int,
    roadB :: Int,
    crossRoad :: Int,
    nextSection :: RoadSystem
  }
  deriving (Show, Read)

type PathCost = Int

parseInput :: [String] -> [Int]
parseInput = map read

buildRoadSystem :: [Int] -> RoadSystem
buildRoadSystem (a : b : c : sections) = Section a b c $ buildRoadSystem sections

main :: IO () =
  do
    (fileName : _) <- getArgs
    contents <- readFile fileName
    let input = parseInput $ words contents
    let roads = buildRoadSystem input
    print roads