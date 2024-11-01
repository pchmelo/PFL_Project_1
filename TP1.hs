import qualified Data.List
import qualified Data.Array
import qualified Data.Bits 

-- PFL 2024/2025 Practical assignment 1

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

-- iterates through an array of cities and returns a list of cities without duplicates
-- time complexity: O(n)
deduplicator :: [City] -> [City]
deduplicator [] = []
deduplicator [x] = [x]
deduplicator (x:xs) = if x == head xs 
    then deduplicator xs 
    else x : deduplicator xs

-- returns a sorted list of all cities in the roadmap
-- time complexity: O(nlog(n))
cities :: RoadMap -> [City]
cities roadmap = deduplicator (Data.List.sort [city | (city1, city2, _) <- roadmap, city <- [city1, city2]])

-- returns true if the two cities in a roadmap are adjacent and false otherwise
-- time complexity: O(n)
areAdjacent :: RoadMap -> City -> City -> Bool

areAdjacent roadmap city1 city2 = if city1 /= city2 
    then [] /= [city | (c1, c2, _) <- roadmap, city <- [c1, c2], (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)] 
    else True
    

-- returns the distance between two cities in a roadmap if they are adjacent and Nothing otherwise
-- time complexity: O(n)
distance :: RoadMap -> City -> City -> Maybe Distance
distance roadmap city1 city2
    | city1 == city2 = Just 0
    | areAdjacent roadmap city1 city2 = Just (head [dist | (c1, c2, dist) <- roadmap, (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)])
    | otherwise = Nothing

-- returns an array of cities adjacent to a given city in a roadmap and the distance between them
-- time complexity: O(n)
adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent roadmap city = [(c, dist) | (c1, c2, dist) <- roadmap, c <- [c1, c2], (c1 == city || c2 == city) && c /= city]

-- returns the sum of the distances between consecutive pairs of cities of a path in a roadmap if it is valid and Nothing otherwise
-- time complexity: O(m * n) where m is the length of the path and n is the number of cities in the roadmap
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Just 0
pathDistance _ [_] = Just 0
pathDistance roadmap (x:y:xs)
    | areAdjacent roadmap x y = do
        dist <- distance roadmap x y
        dist' <- pathDistance roadmap (y:xs)
        return (dist + dist')
    | otherwise = Nothing

-- returns an array of cities with the highest number of roads connecting to them
-- time complexity: O(n^2)
romeAux::RoadMap -> [City] -> [City] -> Int -> [City]
romeAux _ [] res_acc _ = res_acc
romeAux roadmap (x:xs) res_acc degree_acc = romeAux roadmap xs new_res_acc new_degree_acc
    where 
        l_degree = adjacent roadmap x
        new_degree_acc = if length l_degree > degree_acc 
            then length l_degree 
            else degree_acc
        new_res_acc = if length l_degree > degree_acc 
            then [x] 
            else if length l_degree == degree_acc 
                then x : res_acc 
                else res_acc
        

--returns an array of cities with the highest number of roads connecting to them
-- time complexity: O(n^2)
rome :: RoadMap -> [City]
rome roadmap = romeAux roadmap list_cities [] 0
    where list_cities = cities roadmap

-- returns a list of cities reachable from a given city in a roadmap
-- time complexity: O(V + E) where V is the number of cities and E is the number of roads
dfs :: RoadMap -> City -> [City] -> [City]
dfs roadmap city visited
    | city `elem` visited = visited
    | otherwise = foldl (\acc (city', _) -> dfs roadmap city' acc) (city : visited) (adjacent roadmap city)

-- returns true if every city in a roadmap is reachable from every other city and false otherwise
-- time complexity: O(V + E) where V is the number of cities and E is the number of roads
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected roadmap = length(dfs roadmap (head list_cities) []) == length list_cities
    where list_cities = cities roadmap

-- returns all shortest paths connecting two cities using a bfs approach
-- time complexity: O(V + E) where V is the number of cities and E is the number of roads
shortestPathAux :: RoadMap -> [(City, Path, Distance)] -> [Path] -> Distance -> City -> [Path]
shortestPathAux _ [] shortest_paths _ _ = shortest_paths
shortestPathAux roadmap ((cur_city, cur_path, cur_distance) : queue) shortest_paths shortest_distance end_city
    | cur_city == end_city =
        if cur_distance < shortest_distance
           then shortestPathAux roadmap queue [cur_path] cur_distance end_city
           else if cur_distance == shortest_distance
                then shortestPathAux roadmap queue (cur_path : shortest_paths) shortest_distance end_city
                else shortestPathAux roadmap queue shortest_paths shortest_distance end_city
    | cur_distance >= shortest_distance = shortestPathAux roadmap queue shortest_paths shortest_distance end_city
    | otherwise = shortestPathAux roadmap new_queue shortest_paths shortest_distance end_city
    where new_queue = queue ++ [(adjacentCity, cur_path ++ [adjacentCity], cur_distance + d) | (adjacentCity, d) <- adjacent roadmap cur_city, adjacentCity `notElem` cur_path]

-- returns all shortest paths connecting two cities
-- time complexity: O(V + E) where V is the number of cities and E is the number of roads
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadmap start_city end_city
    | start_city == end_city = [[start_city]]
    | otherwise = shortestPathAux roadmap [(start_city, [start_city], 0)] [] maxBound end_city


type DPIndex = (Int, BitMask)
type BitMask = Int

data DPCell = DPCell {
    minDistance :: !Distance,     -- Minimum distance found so far (strict evaluation)
    visitedMask :: !BitMask,     -- Bit mask for visited cities (strict evaluation)
    nextCity :: !(Maybe City)    -- Next city in the optimal path (strict evaluation)
} deriving (Show, Eq)

-- Dynamic Programming Matrix type
data DPMatrix = DPMatrix {
    dpArray :: !(Data.Array.Array DPIndex DPCell),  -- The actual DP table
    indexToCity :: !(Data.Array.Array Int City)     -- Mapping from indices to city names
} deriving (Show)

createDPMatrix :: RoadMap -> [City] -> Int -> DPMatrix
createDPMatrix roadMap list_cities num_cities =  fillDPMatrix roadMap (addDistancesDPMatrix roadMap dataMatrix) num_cities
    where
        dataMatrix = DPMatrix{
            dpArray = createDPArray list_cities num_cities, 
            indexToCity = createIndexToCity list_cities num_cities
        }

createIndexToCity :: [City] -> Int -> (Data.Array.Array Int City)
createIndexToCity list_cities num_cities =
    Data.Array.array (0, num_cities - 1) (zip [0..num_cities - 1] list_cities)

createDPArray :: [City] -> Int -> (Data.Array.Array DPIndex DPCell)
createDPArray list_cities num_cities = Data.Array.array bounds [((i, mask), initCell i mask) | i <- [0..num_cities - 1], mask <- [0..fullBitMask]]
    where
        fullBitMask = Data.Bits.bit num_cities - 1
        bounds = ((0, 0), (num_cities - 1, fullBitMask))
        initCell i mask
            | mask == Data.Bits.bit i = DPCell {
                minDistance = 0,
                visitedMask = mask,
                nextCity = Nothing
            }
            | otherwise = DPCell {
                minDistance = maxBound,
                visitedMask = mask,
                nextCity = Nothing
            }

updateDPMatricCell :: Data.Array.Array DPIndex DPCell -> DPIndex -> DPCell -> Data.Array.Array DPIndex DPCell
updateDPMatricCell dpArray index newCell = dpArray Data.Array.// [(index, newCell)]

calculateMask :: [City] -> Data.Array.Array Int City -> BitMask
calculateMask cities indexToCityArray = foldl (Data.Bits..|.) 0 (map (Data.Bits.bit . cityIndex indexToCityArray) cities)

cityIndex :: Data.Array.Array Int City -> City -> Int
cityIndex indexToCityArray city = safeCityIndex
    where 
        cityIndex = Data.List.elemIndex city (Data.Array.elems indexToCityArray)
        safeCityIndex = case cityIndex of
            Just idx -> idx
            Nothing -> error $ "City not found: " ++ city 

addDistancesDPMatrix :: RoadMap -> DPMatrix -> DPMatrix
addDistancesDPMatrix roadMap dpMatrix = foldl addDistance dpMatrix roadMap
    where
        addDistance dpMatrix (city1, city2, dist) = 
            let
                city1Index = cityIndex (indexToCity dpMatrix) city1
                city2Index = cityIndex (indexToCity dpMatrix) city2
                mask = calculateMask [city1, city2] (indexToCity dpMatrix)
                index_1 = (city1Index, mask)
                index_2 = (city2Index, mask)
                newCell_1 = DPCell {
                    minDistance = dist,
                    visitedMask = mask,
                    nextCity = Just city2
                }
                newCell_2 = DPCell {
                    minDistance = dist,
                    visitedMask = mask,
                    nextCity = Just city1
                }
            in
                dpMatrix { dpArray = updateDPMatricCell (updateDPMatricCell (dpArray dpMatrix) index_1 newCell_1) index_2 newCell_2 }




fillDPMatrix :: RoadMap -> DPMatrix -> Int -> DPMatrix
fillDPMatrix roadMap dpMatrix num_cities = 
    foldl (processSubset roadMap) dpMatrix subsetSizes
  where
    -- Generate all subset sizes from 2 to numCities
    subsetSizes = [2..num_cities]
    
    -- Process each subset size
    processSubset :: RoadMap -> DPMatrix -> Int -> DPMatrix
    processSubset roadMap matrix size = 
        foldl (\m mask -> processSubsetMask roadMap size m mask num_cities) matrix allMasks
      where
        -- Generate all possible masks for current size
        allMasks = filter ((== size) . popCount) [0..fullMask]
        fullMask = Data.Bits.bit num_cities - 1
        
        -- Count set bits in mask
        popCount :: BitMask -> Int
        popCount mask = length (filter id [Data.Bits.testBit mask i | i <- [0..num_cities-1]])

-- Process a specific subset mask
processSubsetMask :: RoadMap -> Int -> DPMatrix -> BitMask -> Int -> DPMatrix
processSubsetMask roadMap size matrix mask num_cities= 
    foldl (processCity roadMap mask num_cities) matrix possibleEndCities 
  where
    -- Find all possible end cities in current mask
    possibleEndCities = 
        [j | j <- [0..num_cities-1], Data.Bits.testBit mask j]

-- Process paths ending at a specific city
processCity :: RoadMap -> BitMask -> Int -> DPMatrix -> Int -> DPMatrix
processCity roadMap mask num_cities matrix endCity = 
    foldl (tryPath roadMap) matrix possiblePrevCities
  where
    -- Find all possible previous cities in the path
    possiblePrevCities = 
        [i | i <- [0..num_cities-1], 
         i /= endCity && Data.Bits.testBit mask i]

    -- Try a specific path through prevCity to endCity
    tryPath :: RoadMap -> DPMatrix -> Int -> DPMatrix
    tryPath roadMap dpMat prevCity =
        let prevMask = Data.Bits.clearBit mask endCity
            prevCell = dpArray dpMat Data.Array.! (prevCity, prevMask)
            currCell = dpArray dpMat Data.Array.! (endCity, mask)
            dis = distance roadMap 
                        (indexToCity dpMat Data.Array.! prevCity) 
                        (indexToCity dpMat Data.Array.! endCity)
        in case dis of
            Nothing -> dpMat  -- No direct path exists
            Just dist ->
                if minDistance prevCell /= maxBound && 
                   minDistance prevCell + dist < minDistance currCell
                then
                    let newCell = DPCell {
                            minDistance = minDistance prevCell + dist,
                            visitedMask = mask,
                            nextCity = Just (indexToCity dpMat Data.Array.! prevCity)
                        }
                    in dpMat { 
                        dpArray = updateDPMatricCell 
                            (dpArray dpMat) 
                            (endCity, mask) 
                            newCell 
                    }
                else dpMat

--------------------------

travelSales :: RoadMap -> Path
travelSales roadMap 
    | null validPaths = []  -- No valid TSP path exists
    | otherwise = reconstructPath dpMatrix minPath
  where

    list_cities = cities roadMap

    num_cities = length list_cities
    -- Create initial DP matrix
    dpMatrix = createDPMatrix roadMap list_cities num_cities
    
    -- Full bitmask (all cities visited)
    fullMask = Data.Bits.bit num_cities - 1
    
    -- Find the minimum cost path
    minPath = findMinPath dpMatrix num_cities fullMask
    
    -- Find valid paths
    validPaths = [path | path <- [minPath], not (null path)]

-- Find the minimum cost path through dynamic programming
findMinPath :: DPMatrix -> Int -> BitMask -> DPIndex
findMinPath dpMatrix numCities fullMask = 
    -- Find the minimum cost final state
    fst (foldl1 minPathState 
        [finalState | finalState <- candidates, isValidFinalState finalState] )
  where
    -- Candidate final states (each city as potential end point)
    candidates = 
        [((endCity, mask), cell) 
        | endCity <- [0..numCities-1],
          let mask = Data.Bits.setBit fullMask endCity,
          let cell = dpArray dpMatrix Data.Array.! (endCity, mask)
        ]
    
    -- Check if the final state is valid (has a complete tour)
    isValidFinalState ((endCity, mask), cell) = 
        mask == fullMask &&  -- All cities visited
        minDistance cell /= maxBound  -- Finite distance

    -- Compare path states to find the minimum
    minPathState ((city1, mask1), cell1) ((city2, mask2), cell2)
        | minDistance cell1 <= minDistance cell2 = ((city1, mask1), cell1)
        | otherwise = ((city2, mask2), cell2)

-- Reconstruct the actual path from the DP matrix
reconstructPath :: DPMatrix -> DPIndex -> Path
reconstructPath dpMatrix (endCityIdx, finalMask)
    | null pathCities = []  -- No valid path
    | otherwise = reverse pathCities
  where
    -- Translate index back to city name
    indexToCity' = indexToCity dpMatrix
    
    -- Recursive path reconstruction
    pathCities = reconstructPathHelper endCityIdx finalMask []
    
    reconstructPathHelper :: Int -> BitMask -> Path -> Path
    reconstructPathHelper currCityIdx currMask acc
        | currMask == Data.Bits.bit currCityIdx = 
            -- Start city reached
            indexToCity' Data.Array.! currCityIdx : acc
        | otherwise = 
            -- Continue path reconstruction
            case nextCity nextCityCell of
                Just nextCityName -> 
                    let nextCityIdx = cityIndex (indexToCity dpMatrix) nextCityName
                        nextMask = Data.Bits.clearBit currMask currCityIdx
                    in reconstructPathHelper nextCityIdx nextMask 
                       (indexToCity' Data.Array.! currCityIdx : acc)
                Nothing -> []  -- No valid next city
      where
        nextCityCell = dpArray dpMatrix Data.Array.! (currCityIdx, currMask)



tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]

gTest4 :: RoadMap
gTest4 = [("0", "2", 1),("0", "4", 1),("0", "5", 1),("1", "4", 1),("1", "5", 1),("2", "3", 1),("2", "4", 1),("4", "5", 1),("6", "7", 1)]

gTest5 :: RoadMap
gTest5 = [("0", "1", 1),("0", "2", 3),("1", "2", 2),("1", "3", 5),("2", "3", 1),("2", "4", 6),("3", "4", 3)]

gTest6 :: RoadMap
gTest6 = [("1", "2", 1), ("1", "3", 1), ("3", "2", 4)]

gTest7 :: RoadMap
gTest7 = [("0", "1", 2), ("0", "2", 9), ("0", "3", 10), ("0", "4", 7),
          ("1", "2", 6), ("1", "3", 4), ("1", "4", 3),
          ("2", "3", 8), ("2", "4", 5),
          ("3", "4", 1)]
