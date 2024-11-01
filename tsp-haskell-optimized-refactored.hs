{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

import qualified Data.List as L
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Bits as B
import GHC.Generics

type City = String
type Distance = Int
type Path = [City]
type RoadMap = [(City, City, Distance)]

-- Data types for efficient lookups
newtype CityIndex = CityIndex Int deriving (Eq, Ord, Show, Generic)
newtype BitMask = BitMask Int deriving (Eq, Ord, Show, Generic)

instance Hashable CityIndex
instance Hashable BitMask

data DP = DP {
    distances :: V.Vector (V.Vector Distance),
    paths :: HM.HashMap (BitMask, CityIndex) Path
} deriving (Generic)

-- Main function
travelSales :: RoadMap -> Maybe Path
travelSales roadMap = do
    cities <- validateRoadMap roadMap
    let startCity = head cities
        dp = initDP cities
    fmap snd $ HM.lookup (fullMask dp, CityIndex 0) $ paths dp
  where
    validateRoadMap :: RoadMap -> Maybe [City]
    validateRoadMap roadMap
        | null cities = Nothing
        | not (isConnected cities roadMap) = Nothing
        | otherwise = Just cities
      where
        cities = L.nub $ concatMap (\(c1, c2, _) -> [c1, c2]) roadMap

    initDP :: [City] -> DP
    initDP cities = DP {
        distances = V.generate (length cities) $ \i ->
            V.generate (length cities) $ \j ->
                findDistance cities i j roadMap,
        paths = HM.fromList [(fullMask dp, CityIndex 0, [head cities])]
    }
      where
        n = length cities
        dp = BitMask $ (1 `B.shiftL` n) - 1

    -- Dynamic programming function
    solve :: BitMask -> CityIndex -> DP -> Maybe (Distance, Path)
    solve mask pos dp = do
        -- Base case: visited all cities, return to start
        if mask == fullMask dp
            then Just (dist pos 0 dp, head cities : [indexToCity dp pos])
        -- Invalid state
        else if not (B.testBit mask $ cityIndex pos)
            then Nothing
        else do
            minimum [(d + nextCost, pos : nextPath) |
                next <- [0..(length (indexToCity dp) - 1)],
                not (B.testBit mask $ CityIndex next),
                let d = dist pos (CityIndex next) dp,
                d /= maxBound,
                let (nextCost, nextPath) = fromMaybe (maxBound, []) $ HM.lookup (B.setBit mask $ CityIndex next, CityIndex next) (paths dp)]

    -- Helper functions
    fullMask :: DP -> BitMask
    fullMask (DP _ _) = BitMask $ (1 `B.shiftL` (length $ indexToCity dp)) - 1

    cityIndex :: CityIndex -> Int
    cityIndex (CityIndex i) = i

    indexToCity :: DP -> CityIndex -> City
    indexToCity (DP _ _) (CityIndex i) = cities !! i

    dist :: CityIndex -> CityIndex -> DP -> Distance
    dist (CityIndex i) (CityIndex j) (DP ds _) = ds V.! i V.! j

    isConnected :: [City] -> RoadMap -> Bool
    isConnected [] _ = True
    isConnected (c:cs) roadMap = length (dfs [c] [c] roadMap) == length (c:cs)
      where
        dfs [] visited _ = visited
        dfs (current:stack) visited roadMap =
            let neighbors = [next |
                    (c1, c2, _) <- roadMap,
                    let next = if c1 == current then c2 else if c2 == current then c1 else "",
                    next /= "",
                    next `notElem` visited]
            in dfs (neighbors ++ stack) (visited ++ neighbors) roadMap

    findDistance :: [City] -> Int -> Int -> RoadMap -> Distance
    findDistance cities i j roadMap = case L.find isEdge roadMap of
        Just (_, _, d) -> d
        Nothing -> maxBound
      where
        isEdge (x, y, _) = (x == cities !! i && y == cities !! j) || (x == cities !! j && y == cities !! i)
