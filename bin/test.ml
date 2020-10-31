open Easy21.Main

let seed = ref None

let n = ref 1

let () =
  Arg.parse
    (Arg.align
       [
         ("-n", Set_int n, "<int> Number of episodes");
         ("--seed", Int (fun i -> seed := Some i), "<int> Random seed");
       ])
    (fun _ -> raise (Arg.Bad "no positional args please."))
    "Easy 21"

let () =
  let st =
    !seed
    |> CCOpt.map_lazy Random.State.make_self_init (fun seed ->
           Random.State.make [| seed |])
  in
  let _ = iter ~st ~n:!n Agent.player_policy in
  ()
