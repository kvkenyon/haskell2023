import System.Directory.Internal.Prelude (getArgs)

data RoadSystem
  = Section
      { roadA :: Int,
        roadB :: Int,
        crossRoad :: Int,
        nextSection :: RoadSystem
      }
  | Destination
  deriving (Show, Read)

type PathCost = Int

type Path = [String]

parseInput :: [String] -> [Int]
parseInput = map read

buildRoadSystem :: [Int] -> RoadSystem
buildRoadSystem [] = Destination
buildRoadSystem (a : b : c : sections) = Section a b c $ buildRoadSystem sections

computeSectionCost :: RoadSystem -> (PathCost, PathCost) -> Path -> Path -> (PathCost, PathCost, Path, Path)
computeSectionCost Destination (a, b) pathA pathB = (a, b, pathA, pathB)
computeSectionCost (Section roadA roadB crossRoad rest) (a, b) pathA pathB =
  computeSectionCost
    rest
    ( min (a + roadA) (b + roadB + crossRoad),
      min (b + roadB) (a + roadA + crossRoad)
    )
    ( pathA
        ++ [pathToA]
    )
    ( pathB
        ++ [pathToB]
    )
  where
    pathToA = if a + roadA < b + roadB + crossRoad then "A" else "BC"
    pathToB = if b + roadB < a + roadA + crossRoad then "B" else "AC"

main :: IO () =
  do
    (fileName : _) <- getArgs
    contents <- readFile fileName
    let input = parseInput $ words contents
    let roads = buildRoadSystem input
    let (costA, costB, pathA, pathB) = computeSectionCost roads (0, 0) [] []
    let cost = min costA costB
    let path = if costA < costB then pathA else pathB
    print cost
    print path