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
  * hit -> dealer draws +4
      dealer: 7 player: 1 (sticking)
  * hit -> dealer draws -3
      dealer: 4 player: 1 (sticking)
  * hit -> dealer draws +8
      dealer: 12 player: 1 (sticking)
  * hit -> dealer draws +4
      dealer: 16 player: 1 (sticking)
  * hit -> dealer draws +6
      dealer: 22 player: 1 (win)
  ==== Episode 2 ====
  * dealer draws +10
  * player draws +9
      dealer: 10 player: 9 (playing)
  * stick -> player sticks
      dealer: 10 player: 9 (sticking)
  * hit -> dealer draws +9
      dealer: 19 player: 9 (sticking)
  * stick -> dealer sticks
      dealer: 19 player: 9 (loss)
  ==== Episode 3 ====
  * dealer draws +8
  * player draws +1
      dealer: 8 player: 1 (playing)
  * stick -> player sticks
      dealer: 8 player: 1 (sticking)
  * hit -> dealer draws +9
      dealer: 17 player: 1 (sticking)
  * stick -> dealer sticks
      dealer: 17 player: 1 (loss)
