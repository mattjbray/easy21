Hello
  $ dune exec -- test --help
  Easy 21
    -n <int>     Number of episodes
    --seed <int> Random seed
    -v           Be verbose
    -f <fpath>   GNUPlot data file prefix
    -help        Display this list of options
    --help       Display this list of options

  $ dune exec -- test --seed 0 -n 3 -v
  ==== Episode 1 ====
  * dealer draws +3
  * player draws +1
      dealer: 3 player: 1 (playing)
  * stick -> player sticks
      dealer: 3 player: 1 (sticking)
  * hit -> dealer draws +9
      dealer: 12 player: 1 (sticking)
  * hit -> dealer draws +5
      dealer: 17 player: 1 (sticking)
  * stick -> dealer sticks
      dealer: 17 player: 1 (loss)
  ==== Episode 2 ====
  * dealer draws +8
  * player draws +9
      dealer: 8 player: 9 (playing)
  * hit -> player draws +9
      dealer: 8 player: 18 (playing)
  * stick -> player sticks
      dealer: 8 player: 18 (sticking)
  * hit -> dealer draws +8
      dealer: 16 player: 18 (sticking)
  * hit -> dealer draws -3
      dealer: 13 player: 18 (sticking)
  * hit -> dealer draws +9
      dealer: 22 player: 18 (win)
  ==== Episode 3 ====
  * dealer draws +9
  * player draws +9
      dealer: 9 player: 9 (playing)
  * stick -> player sticks
      dealer: 9 player: 9 (sticking)
  * hit -> dealer draws +2
      dealer: 11 player: 9 (sticking)
  * hit -> dealer draws -9
      dealer: 2 player: 9 (sticking)
  * hit -> dealer draws +9
      dealer: 11 player: 9 (sticking)
  * hit -> dealer draws +6
      dealer: 17 player: 9 (sticking)
  * stick -> dealer sticks
      dealer: 17 player: 9 (loss)
