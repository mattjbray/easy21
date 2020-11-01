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
  + choose random (ε 1.000)
  * stick -> player sticks
      dealer: 3 player: 1 (sticking)
  * hit -> dealer draws +9
      dealer: 12 player: 1 (sticking)
  * hit -> dealer draws +5
      dealer: 17 player: 1 (sticking)
  * stick -> dealer sticks
      dealer: 17 player: 1 (loss)
  + choose random (ε 1.000)
  win rate: 0.0% (after 1 episodes)
  ==== Episode 2 ====
  * dealer draws +9
  * player draws +6
      dealer: 9 player: 6 (playing)
  + choose random (ε 1.000)
  * stick -> player sticks
      dealer: 9 player: 6 (sticking)
  * hit -> dealer draws +7
      dealer: 16 player: 6 (sticking)
  * hit -> dealer draws +8
      dealer: 24 player: 6 (win)
  + choose random (ε 1.000)
  win rate: 50.0% (after 2 episodes)
  ==== Episode 3 ====
  * dealer draws +9
  * player draws +9
      dealer: 9 player: 9 (playing)
  + choose random (ε 1.000)
  * hit -> player draws +1
      dealer: 9 player: 10 (playing)
  + choose random (ε 1.000)
  * stick -> player sticks
      dealer: 9 player: 10 (sticking)
  * hit -> dealer draws +9
      dealer: 18 player: 10 (sticking)
  * stick -> dealer sticks
      dealer: 18 player: 10 (loss)
  + choose random (ε 0.990)
  win rate: 33.3% (after 3 episodes)
