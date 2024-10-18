import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

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

rome :: RoadMap -> [City]
rome = undefined

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected = undefined

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined

travelSales :: RoadMap -> Path
travelSales = undefined

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]
