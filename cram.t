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
  * hit -> player draws +3
      dealer: 3 player: 4 (playing)
  * hit -> player draws -3
      dealer: 3 player: 1 (playing)
  * hit -> player draws -9
      dealer: 3 player: -8 (loss)
  ==== Episode 2 ====
  * dealer draws +4
  * player draws +9
      dealer: 4 player: 9 (playing)
  * hit -> player draws +9
      dealer: 4 player: 18 (playing)
  * hit -> player draws +7
      dealer: 4 player: 25 (loss)
  ==== Episode 3 ====
  * dealer draws +9
  * player draws +8
      dealer: 9 player: 8 (playing)
  * stick -> player sticks
      dealer: 9 player: 8 (sticking)
  * hit -> dealer draws +2
      dealer: 11 player: 8 (sticking)
  * hit -> dealer draws +9
      dealer: 20 player: 8 (sticking)
  * stick -> dealer sticks
      dealer: 20 player: 8 (loss)
