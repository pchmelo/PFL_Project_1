-- Fill the DP matrix with all possible subpaths
fillDPMatrix :: DPMatrix -> Int -> DPMatrix
fillDPMatrix dpMatrix num_cities = 
    foldl processSubset dpMatrix subsetSizes
  where
    -- Generate all subset sizes from 2 to numCities
    subsetSizes = [2..num_Cities]
    
    -- Process each subset size
    processSubset :: DPMatrix -> Int -> DPMatrix
    processSubset matrix size = 
        foldl (processSubsetMask size) matrix allMasks
      where
        -- Generate all possible masks for current size
        allMasks = filter ((== size) . popCount) [0..fullMask]
        fullMask = Data.Bits.bit num_Cities - 1
        
        -- Count set bits in mask
        popCount :: BitMask -> Int
        popCount mask = length (filter id [Data.Bits.testBit mask i | i <- [0..num_Cities-1]])

-- Process a specific subset mask
processSubsetMask :: Int -> DPMatrix -> BitMask -> Int -> DPMatrix
processSubsetMask size matrix mask num_Cities= 
    foldl (processCity mask num_Cities) matrix possibleEndCities 
  where
    -- Find all possible end cities in current mask
    possibleEndCities = 
        [j | j <- [0..num_Cities-1], Data.Bits.testBit mask j]

-- Process paths ending at a specific city
processCity :: BitMask -> Int -> DPMatrix -> Int -> DPMatrix
processCity mask num_Cities matrix endCity = 
    foldl tryPath matrix possiblePrevCities
  where
    -- Find all possible previous cities in the path
    possiblePrevCities = 
        [i | i <- [0..num_Cities-1], 
         i /= endCity && Data.Bits.testBit mask i]

    -- Try a specific path through prevCity to endCity
    tryPath :: DPMatrix -> Int -> DPMatrix
    tryPath dpMat prevCity =
        let prevMask = Data.Bits.clearBit mask endCity
            prevCell = dpArray dpMat Data.Array.! (prevCity, prevMask)
            currCell = dpArray dpMat Data.Array.! (endCity, mask)
            distance = getDistance roadMap 
                        (indexToCity dpMat Data.Array.! prevCity) 
                        (indexToCity dpMat Data.Array.! endCity)
        in case distance of
            Nothing -> dpMat  -- No direct path exists
            Just dist ->
                if prevCell.minDistance /= maxBound && 
                   prevCell.minDistance + dist < currCell.minDistance
                then
                    let newCell = DPCell {
                            minDistance = prevCell.minDistance + dist,
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

-- Get distance between two cities from roadmap
getDistance :: RoadMap -> City -> City -> Maybe Distance
getDistance roadMap city1 city2 = 
    case [(d) | (c1, c2, d) <- roadMap, 
               (c1 == city1 && c2 == city2) || 
               (c1 == city2 && c2 == city1)] of
        (d:_) -> Just d
        [] -> Nothing


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
    fullMask = Data.Bits.bit numCities - 1
    
    -- Find the minimum cost path
    minPath = findMinPath dpMatrix numCities fullMask
    
    -- Find valid paths
    validPaths = [path | path <- [minPath], not (null path)]

-- Find the minimum cost path through dynamic programming
findMinPath :: DPMatrix -> Int -> BitMask -> DPIndex
findMinPath dpMatrix numCities fullMask = 
    -- Find the minimum cost final state
    foldl1 minPathState 
        [finalState | finalState <- candidates, isValidFinalState finalState]
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
        cell.minDistance /= maxBound  -- Finite distance

    -- Compare path states to find the minimum
    minPathState ((city1, mask1), cell1) ((city2, mask2), cell2)
        | cell1.minDistance <= cell2.minDistance = ((city1, mask1), cell1)
        | otherwise = ((city2, mask2), cell2)

-- Reconstruct the actual path from the DP matrix
reconstructPath :: DPMatrix -> DPIndex -> Path
reconstructPath dpMatrix ((endCityIdx, finalMask), finalCell)
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
            case nextCityCell.nextCity of
                Just nextCityName -> 
                    let nextCityIdx = cityIndex (indexToCity dpMatrix) nextCityName
                        nextMask = Data.Bits.clearBit currMask currCityIdx
                    in reconstructPathHelper nextCityIdx nextMask 
                       (indexToCity' Data.Array.! currCityIdx : acc)
                Nothing -> []  -- No valid next city
      where
        nextCityCell = dpArray dpMatrix Data.Array.! (currCityIdx, currMask)

-- Helper function to find city index
cityIndex :: Data.Array.Array Int City -> City -> Int
cityIndex indexToCityArray city = 
    case Data.List.elemIndex city (Data.Array.elems indexToCityArray) of
        Just idx -> idx
        Nothing -> error $ "City not found: " ++ city