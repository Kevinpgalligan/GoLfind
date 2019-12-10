## Evolving Faces
Using evolutionary algorithms to evolve Game of Life states that eventually turn into faces.

\<Insert demonstration gif here>

#### TO-DO
1. display life (step by step; play N states; reset; save to image; save to gif).
2. fitness plotting.
3. use these tools to debug the smiley face.
   suspicions: converging to local max, which is just empty cells everywhere.
   need to think about how to change fitness fn to get away from that.
   play with mutation rate, amount of mutation in population, initial pop size,
   proportion of elitism vs others, etc.
   Try evolving from one state to another, see if it's even possible.
   Then use GA to search for lives that transition to THOSE lives.
   (Just so as not to throw away all the work I've done so far. The problem
   is that even a slight imperfection throws off further states).
   Actually smart approach: SAT solver. Gardens of Eden seem rare / weird-looking
   enough that it should be possible. The resulting SAT might be too big for SAT,
   or might not. Probably not.
4. start write-up.

