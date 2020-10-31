open Easy21.Main

let seed = ref None

let n = ref 1

let verbose = ref false

let () =
  Arg.parse
    (Arg.align
       [
         ("-n", Set_int n, "<int> Number of episodes");
         ("--seed", Int (fun i -> seed := Some i), "<int> Random seed");
         ("-v", Set verbose, " Be verbose");
       ])
    (fun _ -> raise (Arg.Bad "no positional args please."))
    "Easy 21"

let () =
  let st =
    !seed
    |> CCOpt.map_lazy Random.State.make_self_init (fun seed ->
           Random.State.make [| seed |])
  in
  let vs = iter ~st ~log:!verbose ~n:!n Agent.player_policy in
  let open CCFormat in
  let () =
    CCIO.with_out "hit_values.xyz" (fun out_chan ->
        let fmt = of_chan out_chan in
        vs
        |> M.iter (fun (s, a) (r, _n) ->
               if a = Agent.Hit then
                 fprintf fmt "%i %i %a@." s.dealer_showing s.player_sum float r))
  in
  let () =
    CCIO.with_out "stick_values.xyz" (fun out_chan ->
        let fmt = of_chan out_chan in
        vs
        |> M.iter (fun (s, a) (r, _n) ->
               if a = Agent.Stick then
                 fprintf fmt "%i %i %a@." s.dealer_showing s.player_sum float r))
  in
  let p = printf in
  p "set hidden3d@.";
  p "set dgrid3d 50,50 qnorm 2@.";
  p "set xlabel 'Dealer showing'@.";
  p "set ylabel 'Player sum'@.";
  p "set zlabel 'Reward'@.";
  p "splot 'hit_values.xyz' with lines, 'stick_values.xyz' with lines@."
