Hello
  $ dune exec -- test --help
  Easy 21
    -n <int>     Number of episodes
    --seed <int> Random seed
    -help        Display this list of options
    --help       Display this list of options

  $ dune exec -- test --seed 0 -n 2
  ==== Episode 1 ====
  * dealer draws +3
      dealer: 3 player: 0 (playing)
  * player draws +1
      dealer: 3 player: 1 (playing)
  * player draws +3
      dealer: 3 player: 4 (playing)
  * player draws -3
      dealer: 3 player: 1 (playing)
  * player draws -9
      dealer: 3 player: -8 (loss)
  values:
    dealer: 3 player: -8 (loss) -> r:-1., n:1
    dealer: 3 player: 1 (playing) -> r:-1., n:1
    dealer: 3 player: 4 (playing) -> r:-1., n:1
  ==== Episode 2 ====
  * dealer draws +4
      dealer: 4 player: 0 (playing)
  * player draws +9
      dealer: 4 player: 9 (playing)
  * player draws +9
      dealer: 4 player: 18 (playing)
  * player draws +7
      dealer: 4 player: 25 (loss)
  values:
    dealer: 3 player: -8 (loss) -> r:-1., n:1
    dealer: 3 player: 1 (playing) -> r:-1., n:1
    dealer: 3 player: 4 (playing) -> r:-1., n:1
    dealer: 4 player: 18 (playing) -> r:-1., n:1
    dealer: 4 player: 25 (loss) -> r:-1., n:1
