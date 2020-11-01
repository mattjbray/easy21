open Easy21.Main

let seed = ref None

let n = ref 1

let verbose = ref false

let data_prefix = ref None

let gnuplot prefix vs =
  let open CCFormat in
  let hit_fname = sprintf "%s_hit.xyz" prefix in
  let stick_fname = sprintf "%s_stick.xyz" prefix in
  let () =
    CCIO.with_out hit_fname (fun hit_chan ->
        CCIO.with_out stick_fname (fun stick_chan ->
            let fmt_hit = of_chan hit_chan in
            let fmt_stick = of_chan stick_chan in
            vs
            |> Agent.V.iter (fun s (v : Agent.v) ->
                   v.qs
                   |> Agent.Q.iter (fun a (q : Agent.q) ->
                          let fmt =
                            if a = Agent.Hit then fmt_hit else fmt_stick
                          in
                          fprintf fmt "%i %i %a@." s.other_player_showing
                            s.my_sum float q.reward))))
  in
  let p = printf in
  p "set hidden3d@.";
  p "set dgrid3d 50,50 qnorm 2@.";
  p "set xlabel 'Dealer showing'@.";
  p "set ylabel 'Player sum'@.";
  p "set zlabel 'Reward'@.";
  p "splot '%s' with lines title 'Hit', '%s' with lines title 'Stick'@."
    hit_fname stick_fname

let () =
  Arg.parse
    (Arg.align
       [
         ("-n", Set_int n, "<int> Number of episodes");
         ("--seed", Int (fun i -> seed := Some i), "<int> Random seed");
         ("-v", Set verbose, " Be verbose");
         ( "-f",
           String (fun s -> data_prefix := Some s),
           "<fpath> GNUPlot data file prefix" );
       ])
    (fun _ -> raise (Arg.Bad "no positional args please."))
    "Easy 21"

let () =
  let st =
    !seed
    |> CCOpt.map_lazy Random.State.make_self_init (fun seed ->
           Random.State.make [| seed |])
  in
  let policy = Agent.epsilon_greedy ~n_0:100 (* Agent.rand_policy *) in
  let vs = evaluate ~st ~log:!verbose ~n:!n policy in
  match !data_prefix with None -> () | Some pre -> gnuplot pre vs
