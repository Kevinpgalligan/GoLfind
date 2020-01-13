## Mona Lisa Game of Life
Searching for Game of Life states that eventually turn into a given picture, such as the Mona Lisa.

\<Insert demonstration gif here>

#### TO-DO
It seems that the backsearch ain't gonna fly on large instances of Life. Two issues: takes fucking forever, and heap fills up.

Possible next steps:

1. Optimise the backsearch by using different SAT solver (glucose) and/or generating smaller, easier-to-solve SAT equations (see: The Art of Computer Programming, Volume 4, Fascicle 6).
2. Find smaller Life instances that look good and run it on those instead. Candidates: my face, Mona Lisa, draw a bike, the moon, Steve Buscemi, etc. This is my preference, since I have already sunk a lot of time into this. Just note that even 50x20 takes forever to run. That's 1000 cells. 18x24=432. Need to run some benchmarks to see how it takes for a given number of cells. Anyway, it should still look cool at that size, as long as the original & pixelated versions are displayed along-side it for comparison.

Given the above blabbering, here's the new to-do:

1. Benchmark backsearch, find maximum number of cells that can be backsearched in a reasonable amount of time.
2. Brainstorm images to use, gather them (and pixelate so that they're as close to the max size as possible).
3. Run the backsearch on collected images.
4. Start write-up.
