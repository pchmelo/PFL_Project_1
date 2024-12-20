import qualified Data.List
import qualified Data.Array
import qualified Data.Bits 

-- PFL 2024/2025 Practical assignment 1

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

-- iterates through an array of cities and returns a list of cities without duplicates
{-
    [City] - list of cities to be deduplicated

    return:
        [City] - list of cities without duplicates
-}
-- time complexity: O(n)
deduplicator :: [City] -> [City]
deduplicator [] = []
deduplicator [x] = [x]
deduplicator (x:xs) = if x == head xs 
    then deduplicator xs 
    else x : deduplicator xs

-- returns a sorted list of all cities in the roadmap
{-
    Roadmap - roadmap(Graph) containing all the cities(Vertexes) and roads(Edges) connecting them

    return:
        [City] - list of cities without duplicates
-}
-- time complexity: O(nlog(n))
cities :: RoadMap -> [City]
cities roadmap = deduplicator (Data.List.sort [city | (city1, city2, _) <- roadmap, city <- [city1, city2]])

-- returns true if the two cities in a roadmap are adjacent and false otherwise
{-
    Roadmap - roadmap(Graph) containing all the cities(Vertexes) and roads(Edges) connecting them
    City - first city
    City - second city

    return:
        Bool - true if the two cities are adjacent, false otherwise
-}
-- time complexity: O(n)
areAdjacent :: RoadMap -> City -> City -> Bool

areAdjacent roadmap city1 city2 = if city1 /= city2 
    then [] /= [city | (c1, c2, _) <- roadmap, city <- [c1, c2], (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)] 
    else True
    

-- returns the distance between two cities in a roadmap if they are adjacent and Nothing otherwise
{-
    Roadmap - roadmap(Graph) containing all the cities(Vertexes) and roads(Edges) connecting them
    City - first city
    City - second city

    return:
        Maybe Distance - distance between the two cities if they are adjacent, Nothing otherwise
-}
-- time complexity: O(n)
distance :: RoadMap -> City -> City -> Maybe Distance
distance roadmap city1 city2
    | city1 == city2 = Just 0
    | areAdjacent roadmap city1 city2 = Just (head [dist | (c1, c2, dist) <- roadmap, (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)])
    | otherwise = Nothing

-- returns an array of cities adjacent to a given city in a roadmap and the distance between them
{-
    Roadmap - roadmap(Graph) containing all the cities(Vertexes) and roads(Edges) connecting them
    City - city to find the adjacent cities

    return:
        [(City, Distance)] - list of tupples of adjacent cities and the distance between them
-}
-- time complexity: O(n)
adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent roadmap city = [(c, dist) | (c1, c2, dist) <- roadmap, c <- [c1, c2], (c1 == city || c2 == city) && c /= city]

-- returns the sum of the distances between consecutive pairs of cities of a path in a roadmap if it is valid and Nothing otherwise
{-
    Roadmap - roadmap(Graph) containing all the cities(Vertexes) and roads(Edges) connecting them
    Path - path to calculate the distance

    return:
        Maybe Distance - sum of the distances between consecutive pairs of cities of a path if it is valid, Nothing otherwise
-}
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
{-
    Roadmap - roadmap(Graph) containing all the cities(Vertexes) and roads(Edges) connecting them
    [City] - list of cities in the roadmap
    [City] - accumulator for the result
    Int - accumulator for the degree of the cities in the roadmap

    return:
        [City] - list of cities with the highest number of roads connecting to them
-}
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
{-
    Roadmap - roadmap(Graph) containing all the cities(Vertexes) and roads(Edges) connecting them

    return:
        [City] - list of cities with the highest number of roads connecting to them
-}
-- time complexity: O(n^2)
rome :: RoadMap -> [City]
rome roadmap = romeAux roadmap list_cities [] 0
    where list_cities = cities roadmap

-- returns a list of cities reachable from a given city in a roadmap
{-
    Roadmap - roadmap(Graph) containing all the cities(Vertexes) and roads(Edges) connecting them
    City - city to start the search
    [City] - list of visited cities

    return:
        [City] - list of cities reachable from the given city
-}
-- time complexity: O(V + E) where V is the number of cities and E is the number of roads
dfs :: RoadMap -> City -> [City] -> [City]
dfs roadmap city visited
    | city `elem` visited = visited
    | otherwise = foldl (\acc (city', _) -> dfs roadmap city' acc) (city : visited) (adjacent roadmap city)

-- returns true if every city in a roadmap is reachable from every other city and false otherwise
{-
    Roadmap - roadmap(Graph) containing all the cities(Vertexes) and roads(Edges) connecting them

    return:
        Bool - true if every city in the roadmap is reachable from every other city, false otherwise
-}
-- time complexity: O(V + E) where V is the number of cities and E is the number of roads
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected roadmap = length(dfs roadmap (head list_cities) []) == length list_cities
    where list_cities = cities roadmap

-- returns all shortest paths connecting two cities using a bfs approach
{-
    Roadmap - roadmap(Graph) containing all the cities(Vertexes) and roads(Edges) connecting them
    [(City, Path, Distance)] - queue of cities to visit with their path and distance
    [Path] - list of shortest paths
    Distance - shortest distance
    City - end city

    return:
        [Path] - list of shortest paths connecting two cities
-}
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
{-
    Roadmap - roadmap(Graph) containing all the cities(Vertexes) and roads(Edges) connecting them
    City - start city
    City - end city

    return:
        [Path] - list of shortest paths connecting two cities
-}
-- time complexity: O(V + E) where V is the number of cities and E is the number of roads
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadmap start_city end_city
    | start_city == end_city = [[start_city]]
    | end_city `notElem` (cities roadmap) = []
    | otherwise = shortestPathAux roadmap [(start_city, [start_city], 0)] [] maxBound end_city

-- type for the dynamic programming matrix index
type DPIndex = (Int, BitMask)

-- type for the bitmask
type BitMask = Int

-- Dynamic Programming Cell type
data DPCell = DPCell {
    minDistance :: !Distance,       
    nextCity :: !(Maybe City)    
} deriving (Show, Eq)

-- Dynamic Programming Matrix type with DPIndex as the index type and DPCell as the cell type and an array of cities to index mapping
data DPMatrix = DPMatrix {
    dpArray :: !(Data.Array.Array DPIndex DPCell), 
    indexToCity :: !(Data.Array.Array Int City)     
} deriving (Show)

-- PathDist type with a path and its distance
data PathDist = PathDist {
    path :: Path,
    distancePath :: Distance
} deriving (Show, Eq)

-- arguments: a roadmap of the cities and the distances between them
-- returns the minimum distance path from a RoadMap
-- time complexity: O(n^3 * 2^n)
travelSales :: RoadMap -> Path
travelSales roadMap = minPath
  where
    list_cities = cities roadMap
    num_cities = length list_cities
    fullMask = Data.Bits.bit num_cities - 1
    
    dpMatrix = createDPMatrix roadMap list_cities num_cities
    minPath = case (findMinPath roadMap dpMatrix num_cities fullMask) of
        Nothing -> []
        Just p -> p
    

-- arguments: a roadmap of the cities and the distances between them, a list of unique cities and the number of unique cities
-- returns a DP matrix from a roadmap and a list of cities
-- time complexity: O(n^3 * 2^n) 
createDPMatrix :: RoadMap -> [City] -> Int -> DPMatrix
createDPMatrix roadMap list_cities num_cities =  dataMatrixFilled
    where
        dataMatrix = DPMatrix{
            dpArray = createDPArray list_cities num_cities, 
            indexToCity = createIndexToCity list_cities num_cities
        }

        dataMatrixRoadMapValues = addDistancesDPMatrix roadMap dataMatrix
        dataMatrixFilled = fillDPMatrix roadMap dataMatrixRoadMapValues num_cities

-- arguments: a list of unique cities and the number of unique cities
-- returns an index to city array (0 .. n - 1) (City_1 .. City_n)
-- time complexity: O(n) 
createIndexToCity :: [City] -> Int -> (Data.Array.Array Int City)
createIndexToCity list_cities num_cities =
    Data.Array.array (0, num_cities - 1) (zip [0..num_cities - 1] list_cities)

-- arguments: a list of unique cities and the number of unique cities
-- returns a DP matrix array with all cells initialized to maxBound except for the diagonal cells
-- time complexity: O(n * n^2) 
createDPArray :: [City] -> Int -> (Data.Array.Array DPIndex DPCell)
createDPArray list_cities num_cities = Data.Array.array bounds [((i, mask), initCell i mask) | i <- [0..num_cities - 1], mask <- [0..fullBitMask]]
    where
        fullBitMask = Data.Bits.bit num_cities - 1
        bounds = ((0, 0), (num_cities - 1, fullBitMask))
        initCell i mask
            | mask == Data.Bits.bit i = DPCell {
                minDistance = 0,
                nextCity = Nothing
            }
            | otherwise = DPCell {
                minDistance = maxBound,
                nextCity = Nothing
            }

-- arguments: a DPMatrix, a city index (end city) and a bitmask (mask of cities)
-- returns a DPCell at a given index
-- time complexity: O(1) 
getDPCell :: DPMatrix -> Int -> BitMask -> DPCell
getDPCell dpMatrix cityIndex mask = dpArray dpMatrix Data.Array.! (cityIndex, mask)

-- arguments: a DPMatrix, a DPIndex (index of the matrix to update) and a DPCell (the updated cell)
-- returns a DPMatrix with an updated cell at a given index
-- time complexity: O(1)
updateDPMatricCell :: Data.Array.Array DPIndex DPCell -> DPIndex -> DPCell -> Data.Array.Array DPIndex DPCell
updateDPMatricCell dpArray index newCell = dpArray Data.Array.// [(index, newCell)]

-- arguments: a list of cities to calcule the mask and the array of cities indexed
-- returns a bitmask for a list of cities
-- time complexity: O(n) 
calculateMask :: [City] -> Data.Array.Array Int City -> BitMask
calculateMask cities indexToCityArray = foldl combineBits initialMask bitPositions
  where
    initialMask = 0
    bitPositions = map (Data.Bits.bit . cityIndex indexToCityArray) cities
    combineBits acc bitPos = acc Data.Bits..|. bitPos

-- arguments: an array of cities indexed and a city to get the index
-- returns the index of the city
-- time complexity: O(n) 
cityIndex :: Data.Array.Array Int City -> City -> Int
cityIndex indexToCityArray city = safeCityIndex
    where 
        cityIndex = Data.List.elemIndex city (Data.Array.elems indexToCityArray)
        safeCityIndex = case cityIndex of
            Just idx -> idx
            Nothing -> error $ "City not found: " ++ city 

-- arguments: a RoadMap and a DPMatrix to be updated with the distances between cities of the RoadMap
-- returns a DPMatrix with the distances between cities (from the RoadMap) added
-- time complexity: O(n*R) where R is the number of roads in the RoadMap and n is the number of cities 
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
                    nextCity = Just city2
                }
                newCell_2 = DPCell {
                    minDistance = dist,
                    nextCity = Just city1
                }
                updatedArray1 = updateDPMatricCell (dpArray dpMatrix) index_1 newCell_1
                updatedArray2 = updateDPMatricCell updatedArray1 index_2 newCell_2
            in
                dpMatrix { dpArray = updatedArray2 }


-- arguments: a RoadMap, a DPMatrix to be updated witl dynamic programming approach and the number of unique cities
-- returns a DPMatrix with the dynamic programming approach applied
-- time complexity: O(n^3 * 2^n) 
fillDPMatrix :: RoadMap -> DPMatrix -> Int -> DPMatrix
fillDPMatrix roadMap dpMatrix num_cities = 
    foldl (processSubset roadMap num_cities) dpMatrix subsetSizes 
  where
    subsetSizes = [2..num_cities]
    
-- arguments: a RoadMap, the number of unique cities, a DPMatrix and the size of the subset of cities
-- returns a DPMatrix with the dynamic programming approach applied to a subset of cities
-- time complexity: O(2^n)
processSubset :: RoadMap -> Int -> DPMatrix -> Int -> DPMatrix
processSubset roadMap num_cities matrix size = 
    foldl processMask matrix allMasks
  where
    allMasks = filter isValidMask [0..fullMask]
    fullMask = Data.Bits.bit num_cities - 1
    isValidMask mask = popCount num_cities mask == size
    
    processMask :: DPMatrix -> BitMask -> DPMatrix
    processMask m mask = processSubsetMask roadMap size m mask num_cities

-- arguments: the number of unique cities and a bitmask to count the number of set bits
-- returns the number of set bits in a bitmask
-- time complexity: O(n)
popCount :: Int -> BitMask -> Int
popCount num_cities mask = length (filter id [Data.Bits.testBit mask i | i <- [0..num_cities-1]])

-- arguments: a RoadMap, the number of unique cities, a DPMatrix, a size of the subset of cities and a bitmask
-- return a DPMatrix with the dynamic programming approach applied to a subset of cities with a given mask
-- time complexity: O(n^2)
processSubsetMask :: RoadMap -> Int -> DPMatrix -> BitMask -> Int -> DPMatrix
processSubsetMask roadMap size matrix mask num_cities = 
    foldl processCityWithMask matrix possibleEndCities
  where
    possibleEndCities = findPossibleEndCities mask num_cities
    
    processCityWithMask :: DPMatrix -> Int -> DPMatrix
    processCityWithMask m endCity = processCity roadMap mask num_cities m endCity
    
    -- arguments: a bitmask and the number of unique cities
    -- returns a list of possible end cities in a path
    -- time complexity: O(n)
    findPossibleEndCities :: BitMask -> Int -> [Int]
    findPossibleEndCities mask num_cities = [j | j <- [0..num_cities-1], Data.Bits.testBit mask j]

-- arguments: a RoadMap, a bitmask, the number of unique cities, a DPMatrix and an end city
-- returns a DPMatrix with the dynamic programming approach applied to a specific city
-- time complexity: O(n)
processCity :: RoadMap -> BitMask -> Int -> DPMatrix -> Int -> DPMatrix
processCity roadMap mask num_cities matrix endCity = 
    foldl (tryPath roadMap) matrix possiblePrevCities
  where
    possiblePrevCities = 
        [i | i <- [0..num_cities-1], 
         i /= endCity && Data.Bits.testBit mask i]

    -- arguments: a RoadMap, a DPMatrix, a previous city and an end city
    -- returns a DPMatrix with the dynamic programming approach applied to a specific path
    -- time complexity: O(1)
    tryPath :: RoadMap -> DPMatrix -> Int -> DPMatrix
    tryPath roadMap dpMat prevCity =
        let prevMask = Data.Bits.clearBit mask endCity
            prevCell = dpArray dpMat Data.Array.! (prevCity, prevMask)
            currCell = dpArray dpMat Data.Array.! (endCity, mask)
            dis = distance roadMap 
                        (indexToCity dpMat Data.Array.! prevCity) 
                        (indexToCity dpMat Data.Array.! endCity)
        in case dis of
            Nothing -> dpMat  
            Just dist ->
                if minDistance prevCell /= maxBound && 
                   minDistance prevCell + dist < minDistance currCell
                then
                    let newCell = DPCell {
                            minDistance = minDistance prevCell + dist,
                            nextCity = Just (indexToCity dpMat Data.Array.! prevCity)
                        }
                    in dpMat { 
                        dpArray = updateDPMatricCell 
                            (dpArray dpMat) 
                            (endCity, mask) 
                            newCell 
                    }
                else dpMat

-- arguments: a RoadMap, a DPMatrix, the number of unique cities and the full bitmask
-- returns the minimum distance path from a DPMatrix, return Nothing if no path is found
-- time complexity: O(n^3) 
findMinPath :: RoadMap -> DPMatrix -> Int -> BitMask -> Maybe Path
findMinPath roadMap dpMatrix numCities fullMask = 
    if null validCandidates
    then Nothing
    else Just (fst (foldl1 minPathState validPaths))
  where
    candidates = 
        [((endCity, mask), cell) 
        | endCity <- [0..numCities-1],
          let mask = Data.Bits.setBit fullMask endCity,
          let cell = dpArray dpMatrix Data.Array.! (endCity, mask)
        ]

    validCandidates = filter isValidFinalState candidates 
    isValidFinalState ((endCity, mask), cell) = (mask == fullMask && minDistance cell /= maxBound) 
    
    candidatesPaths = concatMap (\((endCity, mask), cell) -> reconstructPath dpMatrix (endCity, mask)) validCandidates    
    
    validPaths = map addStartingCity (filter isValidPath candidatesPaths)
    
    isValidPath (path, _) = case distance roadMap (head path) (last path) of
        Just _  -> True
        Nothing -> False

    addStartingCity :: ([City], Distance) -> ([City], Distance)
    addStartingCity (path, dist) = 
        let newPath = path ++ [head path]
            newDist = case distance roadMap (head path) (last path) of
                Just d  -> dist + d
                Nothing -> dist
        in (newPath, newDist)  
    
    minPathState (path_1, dist_1) (path_2, dist_2)
        | dist_1 <= dist_2 = (path_1, dist_1)
        | otherwise = (path_2, dist_2)


-- arguments: a DPMatrix and a DPIndex of the final city of the path
-- returns the path from a DPMatrix by reconstructing the path from the final city 
-- time complexity: O(n^2) 
reconstructPath :: DPMatrix -> DPIndex -> [([City], Distance)]
reconstructPath dpMatrix (endCityIdx, finalMask) =
    [ (reverse pathCities, minDistance (getDPCell dpMatrix endCityIdx finalMask)) ]
  where
    city_index = indexToCity dpMatrix 
    pathCities = reconstructPathAux endCityIdx finalMask []
    
    -- auxiliary function to reconstruct the path from the final city with recursive call 
    -- arguments: the current city index, the current mask, and the accumulator of the path
    -- returns when the start city is reached
    -- time complexity: O(n)
    reconstructPathAux :: Int -> BitMask -> Path -> Path
    reconstructPathAux currCityIdx currMask acc
        | currMask == Data.Bits.bit currCityIdx =  city_index Data.Array.! currCityIdx : acc
        | otherwise = 
            case nextCity nextCityCell of
                Just nextCityName -> 
                    let nextCityIdx = cityIndex (indexToCity dpMatrix) nextCityName
                        nextMask = Data.Bits.clearBit currMask currCityIdx
                    in reconstructPathAux nextCityIdx nextMask 
                       (city_index Data.Array.! currCityIdx : acc)
                Nothing -> [] 
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

{-
gTest4 :: RoadMap
gTest4 = [("0", "2", 1),("0", "4", 1),("0", "5", 1),("1", "4", 1),("1", "5", 1),("2", "3", 1),("2", "4", 1),("4", "5", 1),("6", "7", 1)]

gTest5 :: RoadMap
gTest5 = [("0", "1", 1),("0", "2", 3),("1", "2", 2),("1", "3", 5),("2", "3", 1),("2", "4", 6),("3", "4", 3)]

gTest6 :: RoadMap
gTest6 = [("1", "2", 1), ("3", "2", 4)]

gTest7 :: RoadMap
gTest7 = [("0", "1", 2), ("0", "2", 9), ("0", "3", 10), ("0", "4", 7),
          ("1", "2", 6), ("1", "3", 4), ("1", "4", 3),
          ("2", "3", 8), ("2", "4", 5),
          ("3", "4", 1)]

gTest8 :: RoadMap
gTest8 = [("A", "B", 2), ("A", "C", 9), ("A", "D", 10), ("A", "E", 7),
          ("B", "C", 6), ("B", "D", 4), ("B", "E", 3),
          ("C", "D", 8), ("C", "E", 5),
          ("D", "E", 1)]

gTest9 :: RoadMap
gTest9 = [("0", "1", 2), ("0", "2", 9), ("0", "3", 10), ("0", "4", 7)]
-}
