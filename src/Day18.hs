import Data.List
import Debug.Trace
import Data.List.Split

import Data.Set (Set)
import qualified Data.Set as Set

parseCube :: String -> (Float, Float, Float)
parseCube str = tuplify3 $ map read (splitOn "," str)

parseCubeInt :: String -> (Int, Int, Int)
parseCubeInt str = tuplify3 $ map read (splitOn "," str)

tuplify3 [x,y,z] = (x,y,z)

cubeFaces :: (Float, Float, Float) -> Set (Float, Float, Float)
cubeFaces (x, y, z) = Set.fromList [(x+(1/2), y, z), (x, y+(1/2), z), (x, y, z+(1/2)),
                                    (x-(1/2), y, z), (x, y-(1/2), z), (x, y, z-(1/2))]

faceToCubes :: (Float, Float, Float) -> Set (Float, Float, Float)
faceToCubes (x, y, z)
    | fromIntegral (floor x) /= x = Set.fromList [(x+0.5, y, z), (x-0.5, y, z)]
    | fromIntegral (floor y) /= y = Set.fromList [(x, y+0.5, z), (x, y-0.5, z)]
    | fromIntegral (floor z) /= z = Set.fromList [(x, y, z+0.5), (x, y, z-0.5)]

getSurface :: Set (Int, Int, Int) -> [(Float, Float, Float)] -> Set (Int, Int, Int) -> [(Int, Int, Int)] -> [(Float, Float, Float)]
getSurface dropletCubes dropletSurface explored [] = dropletSurface
getSurface dropletCubes dropletSurface explored (cube:toExplore)
    | Set.member cube explored = getSurface dropletCubes dropletSurface explored toExplore
    | otherwise = dropletSurface ++ surfaces cube dropletSurfaceNeighbors ++ getSurface dropletCubes dropletSurface newExplored newToExplore
    where cubeNeighbors = filter (`Set.notMember` explored) (neighbors cube)
          (dropletSurfaceNeighbors, foundToExplore) = partition (`Set.member` dropletCubes) cubeNeighbors
          newExplored = Set.insert cube explored
          newToExplore = toExplore ++ foundToExplore

surfaces :: (Int, Int, Int) -> [(Int, Int, Int)] -> [(Float, Float, Float)]
surfaces _ [] = []      -- adjacentDropletCubes
surfaces airCube@(x, y, z) ((x1, y1, z1):rst) =
    (fromIntegral x + (xDist/2), fromIntegral y +(yDist/2), fromIntegral z + (zDist/2)):surfaces airCube rst
    where (xDist, yDist, zDist) :: (Float, Float, Float) = (fromIntegral (x1-x), fromIntegral (y1-y), fromIntegral (z1-z))

neighbors :: (Int, Int, Int) -> [(Int, Int, Int)]
neighbors (x, y, z) =
    filter (\(a, b, c) -> (a >= -1 && b >= -1 && c >= -1) && (a <= 20 && b <= 20 && c <= 20)) 
    [(x+1, y, z), (x, y+1, z), (x, y, z+1), (x-1, y, z), (x, y-1, z), (x, y, z-1)]


main = do
    file <- readFile "input\\day_18.txt"
    let cubes = map parseCube (lines file)
    let allFaces = foldr (Set.union . cubeFaces) Set.empty cubes
    let numFaces = length allFaces
    let expecedFaces = length cubes * 6
    let gap = expecedFaces - numFaces
    print (numFaces - gap) -- p1

    let intCubes = map parseCubeInt (lines file)
    let surfaceFaces = getSurface (Set.fromList intCubes) [] Set.empty [(0, 0, 0)]
    print (length surfaceFaces) -- p2



