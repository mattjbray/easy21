open Easy21.Main

let () =
  let st = Random.State.make [||] in
  episode ~st Agent.player_policy |> ignore
