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

### Implementation:

The `shortestPath` function has two main components, the function in itself that returns the shortest paths between two cities in a roadmap and also analyses if the end_city s equal to the starting city or not part of the given roadmap, and the helper function `shortestPathAux` that recursively computes all the shortest paths using a breadth-first-search(BFS) approach.

#### Function signatures:

> shortestPath :: RoadMap -> City -> City -> [Path]
> ****
> **Atributes**<br>
> Roadmap - Graph that shows all cities and the roads connecting them
> Start City - The city from which to start the search
> End City - The city to which paths are being searched
>
> **Output**<br>
> Shortest Paths - List of the shortest paths connecting Start City and End City

> shortestPathAux :: RoadMap -> [(City, Path, Distance)] -> [Path] -> Distance -> City -> [Path]
> ****
> **Atributes**<br>
> Roadmap - Graph that shows all cities and the roads connecting them
> Queue - Queue used for the BFS search
> Shortest Paths - List where the shortest paths are stored throughout the recursion
> Shortest Distance - Shortest distance found so far
> End City - The goal city
>
> **Output**<br>
> Shortest Paths - List of the shortest paths connecting Start City and End City

#### Data Structures:

The queue is a fundamental data structure for the BFS, allowing for the first-in-first-out processing of cities to explore. For this function, the queue stores tuples containing the current city, the path taken to reach it, and the distance traveled so far in the current path.

### Algorithm Steps:

1. **Initialization**:
   - If `start_city` is the same as `end_city`, return a list containing just `[start_city]`.
   - If `end_city` is not an element in the roadmap 
   - Initialize a queue with the tuple `(start_city, [start_city], 0)`:
     - `start_city`: Current city.
     - `[start_city]`: Current path taken.
     - `0`: Current distance.
   - Initialize an empty list `shortest_paths` to store valid paths.
   - Set `shortest_distance` to a large value (`maxBound`).

2. **Check for Destination**:
   - If `cur_city` is equal to `end_city`:
     - If `cur_distance` is less than `shortest_distance`:
       - Set `shortest_paths` to `cur_path`.
     - Else if `cur_distance` is equal to `shortest_distance`:
       - Append `cur_path` to `shortest_paths`.
     - Else ignore and continue processing the queue

3. **Continue Exploration**:
   - If `cur_distance` is greater than or equal to `shortest_distance`, skip further exploration.
   - Otherwise, for each adjacent city `(adjacentCity, d)` obtained from `adjacent(roadmap cur_city)`:
     - If `adjacentCity` is not already in `cur_path`:
       - Construct a new path as `cur_path ++ [adjacentCity]`.
       - Calculate the new distance as `cur_distance + d`.
       - Enqueue `(adjacentCity, new_path, new_distance)`.

4. **Return Result**:
   - After processing the queue, return `shortest_paths`.

### Used Algorithms:

#### Dijkstra's Algorithm:

The `shortestPath` function takes strong inspiration from Dijkstra's algorithm by adopting its foundational principles of systematic exploration, distance tracking, and iterative refinement of paths. Although we trade the priority queue for a simpler queue mechanism, it retains the essence of finding the shortest paths in a bidirectional, positively weighted graph. This approach provides a more readable and understandable way to implement the algorithm while still effectively addressing the problem of identifying multiple shortest paths.

#### Breadth-First-Search:

The BFS is a part of the Dijkstra's algorithm and it is used to explore all neighboring vertices at the present depth prior to moving on to vertices at the next depth level.

### Time Complexity:

Since the `shortestPath` function was made with Dijkstra's algorithm in mind it shares the same time complexity as one without a priority queue which is **O(V^2)** for the absolute worst case scenario where the graph is extremely dense leading to many paths having to be explored.

However, in practice, it will usually perform much better due to:

- The shortest distance pruning cuts off many paths
- Most real road networks are sparse (E ≈ V) rather than complete
- The `notElem` check prevents cycles

If the graph is not too dense, we can often consider it approximately **O(V+E+P)** or even **O(V+E)**, where V is the number of cities, E is the number of roads and P is the number of generated paths.

#### Possible Improvements:

We could have implemented a priority queue using a minHeap to turn our functions into a true Dijkstra's algorithm which would have improved the time complexity to O((V+E)log(V)), although we choose not to, as we deemed this iprovement not worthy due to the added complexity and worse readibility in Haskell compared with other programming languages.

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

- Time complexity: O(n^3 * 2^n)

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
- Time complexity: O(n * R) where R is the number of roads in the RoadMap and n is the number of cities

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
- Time complexity: O(n^3 * 2^n) 

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
- Time Complexity: O(n^3) 

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
- Time complexity: O(n^2) 

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

- Time Complexity: O(n^3 * 2^n):
  - The `2^n` factor comes from the number of possible masks.
  - One `n` factor comes from iterating over the cities for each mask.
  - Another `n` factor comes from generating the list of previous cities.
  - The third `n` factor comes from the foldl tryPath operation iterating over the previous cities.

#### Possible Improvements
The original Keld-Karp algorithm has a time complexity of `O(n^2 * 2^n)`, so a great improvement for our algorithm would be reducing the time complexity. After some research we had come with the solution; However, we handn't the time to aplly it. Our algorithm calcule every single path starting from each city, so knowing that we could only make ir for one path. 
