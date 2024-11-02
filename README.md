# PFL - Haskell Coursework
## Group Members:

- Gabriel Carvalho (up202208939@up.pt)
- Vasco Melo (up2022xxxxx@up.pt)

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

...
