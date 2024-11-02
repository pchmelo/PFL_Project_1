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

### Time Complexity

Since the `shortestPath` function was made with Dijkstra's algorithm in mind it shares the same time complexity as one without a priority queue which is **O(V^2)** for the absolute worst case scenario where the graph is extremely dense leading to many paths having to be explored.

However, in practice, it will usually perform much better due to:

- The shortest distance pruning cuts off many paths
- Most real road networks are sparse (E ≈ V) rather than complete
- The `notElem` check prevents cycles

If the graph is not too dense, we can often consider it approximately **O(V+E+P)** or even **O(V+E)**, where V is the number of cities, E is the number of roads and P is the number of generated paths.

## Travel Sales Explanation:

### Overview
The `travelSales` function is a implementation of the dynamic programming with a bitmask approach
to solves the `Traveling Salesman Problem (TSP)`. The algorithm tries to find the shortest possible route that visits each city exactly once and returns to the starting city. In case of no existance of a valid route, it returns a empety list.

### Core Concept

The algorithm is based on the `Held-Karp algorithm`, which is a dynamic programming solution for TSP. It builds solutions incrementally by considering subsets of cities and finding optimal paths through those subsets.

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
- `Int`: Index of the starting city
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
