import qualified Data.List as L
import qualified Data.Array as A
import qualified Data.Bits as B

type City = String
type Distance = Int
type Path = [City]
type RoadMap = [(City, City, Distance)]

-- Main function
travelSales :: RoadMap -> Maybe Path
travelSales roadMap 
    | null cities = Nothing    -- Empty roadmap
    | not (isConnected cities roadMap) = Nothing  -- Graph not connected
    | otherwise = Just $ snd $ dp ! (fullMask, 0)
    where
        -- Step 1: Process cities and create mappings
        cities = L.nub $ concatMap (\(c1, c2, _) -> [c1, c2]) roadMap
        n = length cities
        startCity = head cities
        
        -- Create bidirectional array of distances for O(1) lookup
        cityToIndex = A.listArray (0, n-1) cities
        indexToCity = A.listArray (0, n-1) [0..]
        distances = A.listArray ((0,0), (n-1,n-1))
            [findDistance (cityToIndex A.! i) (cityToIndex A.! j) | i <- [0..n-1], j <- [0..n-1]]
        
        -- Bit manipulation helpers
        fullMask = (1 `B.shiftL` n) - 1
        
        -- Step 2: Create dynamic programming array
        dp = A.listArray ((0,0), (fullMask,n-1))
            [solve mask pos | mask <- [0..fullMask], pos <- [0..n-1]]
        
        -- Step 3: Main DP function
        solve :: Int -> Int -> (Distance, Path)
        solve mask pos
            -- Base case: visited all cities, return to start
            | mask == fullMask = (dist pos 0, [cityToIndex A.! pos, cityToIndex A.! 0])
            -- Invalid state
            | not (B.testBit mask pos) = (maxBound, [])
            | otherwise = minimum
                [(d + nextCost, cityToIndex A.! pos : nextPath) |
                    next <- [0..n-1],
                    not (B.testBit mask next),
                    let d = dist pos next,
                    d /= maxBound,
                    let (nextCost, nextPath) = dp ! (B.setBit mask next, next)]
            where
                dist x y = distances A.! (x,y)

-- Step 4: Helper Functions
-- Find distance between cities in roadMap
findDistance :: City -> City -> Distance
findDistance c1 c2 roadMap = case L.find isEdge roadMap of
    Just (_, _, d) -> d
    Nothing -> maxBound
    where isEdge (x, y, _) = (x == c1 && y == c2) || (x == c2 && y == c1)

-- Check if graph is connected using DFS
isConnected :: [City] -> RoadMap -> Bool
isConnected [] _ = True
isConnected (c:cs) roadMap = length (dfs [c] [c]) == length (c:cs)
    where
        dfs [] visited = visited
        dfs (current:stack) visited = 
            let neighbors = [next | 
                    (c1, c2, _) <- roadMap,
                    let next = if c1 == current then c2 else if c2 == current then c1 else "",
                    next /= "",
                    next `notElem` visited]
            in dfs (neighbors ++ stack) (visited ++ neighbors)
