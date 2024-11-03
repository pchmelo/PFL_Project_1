# PFL - Haskell Coursework
## Group Members:

- Gabriel Carvalho (up202208939@up.pt)
- Vasco Melo (up202207564@up.pt)

## Contribution Scores:
### Gabriel Carvalho:

> Worked on:
> 
> - cities and related function
> - areAdjacent
> - distance
> - adjacent
> - pathDistance
> - isStronglyConnected and related function
> - shortestPath and related function
> - readme section for shortestPath
> 
> Overall Contribution - 50%

### Vasco Melo:

> Worked on:
>
> - rome and related function
> - travelSales and related functions
> - readme section for travelSales
> 
> Overall Contribution - 50%

## Shortest Path Explanation:

The `shortestPath` function is designed to find all shortest paths connecting two cities in a bidirectional, positively weighted graph representing a roadmap. This implementation is a variant of Dijkstra's algorithm, adapted to explore all possible shortest paths without the use of a priority queue due to some restrictions we faced during development.



## Travel Sales Explanation:

### Overview
The `travelSales` function is a implementation of the dynamic programming with a bitmask approach
to solves the `Traveling Salesman Problem (TSP)`. The algorithm tries to find the shortest possible route that visits each city exactly once and returns to the starting city. In case of no existance of a valid route, it returns a empety list.

### Core Concept

The algorithm is based on the `Held-Karp algorithm`, which is a dynamic programming solution for TSP. It builds solutions incrementally by considering subsets of cities and finding optimal paths through those subsets. The developed algorithm is based on the following formula:

```
C(S, i) = min{C(S-{i}, j) + d[j,i]} for j ∈ S, j ≠ i
```
Where:

- C(S, i) = minimum cost path visiting all vertices in set S exactly once, ending at vertex i
- S = subset of vertices including vertex i and vertex 1 (starting vertex)
- d[j,i] = distance/cost from vertex j to vertex i

### Key Data Structures
#### DPMatrix
The DPMatrix is the core data structure consisting of:
```haskell
data DPMatrix = DPMatrix {
    dpArray :: !(Data.Array.Array DPIndex DPCell), 
    indexToCity :: !(Data.Array.Array Int City)     
}
```


Where:
- `dpArray`: A 2D array indexed by (currentCity, visitedCities)
- `indexToCity`: Maps numeric indices to actual city names

#### DPCell
Each cell in the matrix contains:
```haskell
data DPCell = DPCell {
    minDistance :: !Distance,       
    nextCity :: !(Maybe City)    
}
```
- `minDistance`: Minimum distance to complete the path from this state
- `nextCity`: Next city in the optimal path

#### DPIndex and BitMask
Each index of the matrix contains:
```Haskell
type DPIndex = (Int, BitMask)
```
- `Int`: Index of the ending city
- `BitMask`: A BitMask (type BotMask = Int) which each bit represents a city. It serves to represent subsets of cities. 
```Text
01101 -> Subset that represents the cities of index 0, 2 and 3
``` 

#### PathDist
```Haskell
data PathDist = PathDist {
    path :: Path,
    distancePath :: Distance
} deriving (Show, Eq)
```
- `path`: Represents a path a cities
- `distancePath`: Represents the total distance of the path


### Algorithm Steps

1. **Initialization**
   - Create DPMatrix with all cells set to infinity
   - Set distance to 0 for single-city paths
   - Fill initial distances between directly connected cities

2. **Main DP Loop**
   - Process subsets of increasing sizes (3 to n)
   - For each subset size:
     - Generate all valid bitmasks representing that many cities
     - For each valid ending city in the subset:
       - Creates a new Subset without the ending city and search in DPMatrix for that subset.
       - Calcule the minimum between all the possibilities of subset and update the DPMatrix with the new subset calculated

3. **Find Min Path**
   - Generate all candidates of minimum path  using the `nextCity` pointers to backtrack
   - Reverse the path to get the correct order and add the starting city in the final of the path, if possivel
   - From all the possivel solutions, search for the minimum one

### Key Functions

#### createDPMatrix
```haskell
createDPMatrix :: RoadMap -> [City] -> Int -> DPMatrix
```
- Creates the DP matrix with all values filled
- It calls `createDPArray, addDistancesDPMatrix and fillDPMatrix` functions

- Time complexity: O(n^2 * 2^n)

#### createDPArray 
```haskell
createDPArray :: [City] -> Int -> (Data.Array.Array DPIndex DPCell)
```
`Purpose`
- Creates and initializes a dynamic programming array used for solving TSP
- Time complexity: O(n^2)

`Base Case`
```haskell
| mask == Data.Bits.bit i = DPCell {
    minDistance = 0,
    nextCity = Nothing
}
```
- When mask only contains current city
- Distance is 0 (starting point)
- No next city defined

Default Case

```haskell
| otherwise = DPCell {
    minDistance = maxBound,
    nextCity = Nothing
}
```

- For all other combinations
- Distance set to maximum possible value
- No next city defined

#### addDistancesDPMatrix
```haskell
addDistancesDPMatrix :: RoadMap -> DPMatrix -> DPMatrix
```
`Purpose`

- Adds road distances to the DP matrix
- Updates the matrix with direct connections between cities
- Processes each road in the map and updates corresponding cells (subsets of 2 bits)
- Time complexity: O(R) where R is the number of roads in the RoadMap

`Cell Creation`

```haskell
newCell_1 = DPCell {
    minDistance = dist,
    nextCity = Just city2
}
newCell_2 = DPCell {
    minDistance = dist,
    nextCity = Just city1
}
```

- Creates cells for both directions
- Stores distance and next city information

#### fillDPMatrix
```haskell
fillDPMatrix :: RoadMap -> DPMatrix -> Int -> DPMatrix
```
`Purpose`
- Implements the core dynamic programming algorithm for TSP
- Fills the DP matrix with optimal paths for all possible subsets of cities
- Uses bottom-up approach starting from smaller subsets to larger ones
- Time complexity: 

`Main Function (fillDPMatrix)`
```haskell
fillDPMatrix :: RoadMap -> DPMatrix -> Int -> DPMatrix
```
- Processes subsets of increasing sizes (3 to num_cities)
- Uses foldl to accumulate updates through all subset sizes

`Subset Processing (processSubset)` 
```haskell
processSubset :: RoadMap -> Int -> DPMatrix -> Int -> DPMatrix
```
- Processes all valid masks of given size
- Filters masks based on proper bit count

`processSubsetMask`
```haskell
processSubsetMask :: RoadMap -> Int -> DPMatrix -> BitMask -> Int -> DPMatrix
```

- Processes each valid end city for a mask
- Coordinates path calculations

`processCity`
```haskell
processCity :: RoadMap -> BitMask -> Int -> DPMatrix -> Int -> DPMatrix
```
- Processes paths ending at specific city
- Evaluates all possible previous cities

`tryPath`
```haskell
tryPath :: RoadMap -> DPMatrix -> Int -> DPMatrix
```
- Calculate previous mask
- Get relevant cells
- Check distance between cities
- Update if better path found

#### findMinPath
`Purpose and Overview`
```haskell
findMinPath :: RoadMap -> DPMatrix -> Int -> BitMask -> Maybe Path
```
- Function finds the minimum cost path that visits all cities and returns to start
- Input: RoadMap, DPMatrix (with computed solutions), number of cities, full bitmask
- Time Complexity:

`Candidates generation`

```haskell
candidates = [((endCity, mask), cell)
    | endCity <- [0..numCities-1],
    let mask = Data.Bits.setBit fullMask endCity,
    let cell = dpArray dpMatrix Data.Array.! (endCity, mask)]
```

- Iterates through all possible end cities (0 to numCities-1)
For each city:

- Creates a mask including that city
Retrieves corresponding cell from DPMatrix


- Creates list of candidates with their states

`Candidate Validation`

```haskell
validCandidates = filter isValidFinalState candidates
isValidFinalState ((endCity, mask), cell) = 
    (mask == fullMask && minDistance cell /= maxBound)
```

- Filters candidates based on two criteria:

  - Mask must be full (all cities included)
  - Distance must be valid (not maxBound)


- Removes invalid or impossible paths

`Path Reconstruction`
```haskell
candidatesPaths = concatMap 
    (\((endCity, mask), cell) -> reconstructPath dpMatrix (endCity, mask)) 
    validCandidates
```
- For each valid candidate:

  - Reconstructs complete path from DPMatrix
  - Maintains both path and distance information


- Creates list of all possible complete paths

`Path Validation`
```haskell
validPaths = map addStartingCity (filter isValidPath candidatesPaths)
isValidPath (path, _) = case distance roadMap (head path) (last path) of
    Just _ -> True
    Nothing -> False
```

 - Checks if each path is valid:

   - Verifies connection between first and last city exists


 - Filters out invalid paths
 - Adds starting city to complete the cycle

`Final Result Processing`

```haskell
if null validCandidates
    then Nothing
    else Just (fst (foldl1 minPathState validPaths))
```

 - Checks if any valid candidates exist
 - If no valid candidates:

   - Returns Nothing


 - If valid candidates exist:

   - Finds minimum distance path
   - Returns Just path

#### reconstructPath
```haskell
reconstructPath :: DPMatrix -> DPIndex -> Path
```
`Purpose`

- Reconstructs the optimal path from a filled dynamic programming matrix
- Returns a list containing a tuple of (path of cities, minimum distance)
- Time complexity: 

`reconstructPathAux`

- Auxiliary recursive function that does the actual path reconstruction
 - Parameters:

   - currCityIdx: Current city's index
   - currMask: Current state bitmask
   - acc: Accumulator for the path

`reconstructPathAux recursive base case` 
```haskell
-- When only current city is in mask
| currMask == Data.Bits.bit currCityIdx = 
    city_index Data.Array.! currCityIdx : acc
```
`reconstructPathAux recursive case` 

 - Get next city from DP cell
 - Clear current city from mask
 - Recursively build path

### Time Complexity Analysis

- Time Complexity: O(n^2 * 2^n)
  - For each subset size (n iterations)
  - For each valid subset of that size (approximately 2^n subsets)
  - For each possible end city (n iterations)
