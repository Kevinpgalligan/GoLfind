## Evolving Faces
Using evolutionary algorithms to evolve Game of Life states that eventually turn into faces.

\<Insert demonstration gif here>

#### TO-DO
1. load b/w image, convert to life (initial version looks wrong).
2. display life (step by step; play N states; reset; save to image; save to gif).
3. fitness plotting.
4. use these tools to debug the smiley face.
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
5. start write-up.

