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

### Algorithm Steps

1. **Initialization**
   - Create DPMatrix with all cells set to infinity
   - Set distance to 0 for single-city paths
   - Fill initial distances between directly connected cities

2. **Main DP Loop**
   - Process subsets of increasing sizes (2 to n)
   - For each subset size:
     - Generate all valid bitmasks representing that many cities
     - For each valid ending city in the subset:
       - Try all possible previous cities
       - Update minimum distance if a better path is found

3. **Path Reconstruction**
   - Start from the optimal final state
   - Backtrack using the `nextCity` pointers 
   - Reverse the path to get the correct order

...

### Pseudocode

### Time Complexity Analysis

- Space Complexity: O(n * 2^n)
  - Stores a value for each combination of:
    - Visited cities subset (2^n possibilities)
    - Current city (n possibilities)

- Time Complexity: O(n^2 * 2^n)
  - For each subset size (n iterations)
  - For each valid subset of that size (approximately 2^n subsets)
  - For each possible end city (n iterations)
  - For each possible previous city (n iterations)

### Key Functions

#### createDPMatrix
```haskell
createDPMatrix :: RoadMap -> [City] -> Int -> DPMatrix
```
- Creates the initial DP matrix
- Initializes base cases
- Time complexity: O(n^2)

#### fillDPMatrix
```haskell
fillDPMatrix :: RoadMap -> DPMatrix -> Int -> DPMatrix
```
- Main function that implements the dynamic programming logic
- Processes subsets of increasing size
- Updates optimal paths and distances
- Time complexity: O(n^2 * 2^n)

#### reconstructPath
```haskell
reconstructPath :: DPMatrix -> DPIndex -> Path
```
- Builds the final path from the DP table
- Starts from optimal end state and follows next city pointers
- Time complexity: O(n)
