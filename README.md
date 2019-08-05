# Conway's Game of Life
[Conway's Game of Life](https://wikipedia.org/wiki/Conway%27s_Game_of_Life),
written in Elm.

## Rules
This implementation of Conway's Game of Life takes place on a finite
two-dimensional grid. Coordinates wrap around for the sake of determining
neighbors.

The board is initially populated from a random seed. Each cycle, cells change
between the live and dead states based upon the following rules:
1. Any live cell with fewer than two living neighbors dies.
2. Any live cell with two or three living neighbors stays alive.
3. Any live cell with more than three living neighbors dies.
4. Any dead cell with exactly three living neighbors becomes alive.

## License
This software and associated documentation files are licensed under the
[MIT License](https://opensource.org/licenses/MIT).

This file may not be copied, modified, or distributed except according to
those terms.
