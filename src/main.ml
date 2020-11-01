module Event = struct
  type color = Red | Black

  type draw = { color : color; values : int }

  let pp_draw fmt d =
    let open CCFormat in
    let c = match d.color with Red -> "Red" | Black -> "Black" in
    let op = match d.color with Red -> "-" | Black -> "+" in
    fprintf fmt "%s%a" op (with_color c int) d.values

  (** Environment events *)
  type t =
    | Player_draws of draw
    | Player_sticks
    | Dealer_draws of draw
    | Dealer_sticks
    | No_event

  let pp fmt e =
    let open CCFormat in
    match e with
    | Player_draws draw -> fprintf fmt "player draws %a" pp_draw draw
    | Player_sticks -> fprintf fmt "player sticks"
    | Dealer_draws draw -> fprintf fmt "dealer draws %a" pp_draw draw
    | Dealer_sticks -> fprintf fmt "dealer sticks"
    | No_event -> fprintf fmt "no event"
end

module State = struct
  type outcome = Win | Draw | Loss

  let outcome_to_string = function
    | Win -> "win"
    | Draw -> "draw"
    | Loss -> "loss"

  let pp_outcome = CCFormat.of_to_string outcome_to_string

  type mode = Playing | Sticking | Finished of outcome

  let pp_mode fmt =
    let open CCFormat in
    function
    | Playing -> fprintf fmt "playing"
    | Sticking -> fprintf fmt "sticking"
    | Finished outcome -> fprintf fmt "%a" pp_outcome outcome

  type t = { dealer : int; player : int; mode : mode }

  let make = { dealer = 0; player = 0; mode = Playing }

  let pp fmt s =
    let open CCFormat in
    fprintf fmt "@[";
    fprintf fmt "dealer: %a@ " int s.dealer;
    fprintf fmt "player: %a (%a)" int s.player pp_mode s.mode;
    fprintf fmt "@]"

  let mode s = s.mode

  let player s = s.player

  let dealer s = s.dealer

  let map_dealer f s = { s with dealer = f s.dealer }

  let map_player f s = { s with player = f s.player }

  let draw (draw : Event.draw) sum =
    let op = match draw.color with Black -> ( + ) | Red -> ( - ) in
    op sum draw.values

  let is_bust sum = sum > 21 || sum < 1

  let finish_with o s = { s with mode = Finished o }

  let outcome state =
    if state.player = state.dealer then Draw
    else if state.player > state.dealer then Win
    else Loss

  let update event state =
    match event with
    | Event.Player_draws d ->
        let state = state |> map_player (draw d) in
        if is_bust state.player then finish_with Loss state else state
    | Event.Player_sticks -> { state with mode = Sticking }
    | Event.Dealer_draws d ->
        let state = state |> map_dealer (draw d) in
        if is_bust state.dealer then finish_with Win state else state
    | Event.Dealer_sticks -> finish_with (outcome state) state
    | Event.No_event -> state
end

module Agent = struct
  (** Agent actions *)
  type action = Stick | Hit

  let all_actions = [ Stick; Hit ]

  let pp_action fmt =
    let open CCFormat in
    function Stick -> fprintf fmt "stick" | Hit -> fprintf fmt "hit"

  type state = { other_player_showing : int; my_sum : int }

  let observe_dealer s =
    { other_player_showing = State.player s; my_sum = State.dealer s }

  let observe_player s =
    { other_player_showing = State.dealer s; my_sum = State.player s }

  module V = CCMap.Make (struct
    type t = state

    let compare = Stdlib.compare
  end)

  module Q = CCMap.Make (struct
    type t = action

    let compare = Stdlib.compare
  end)

  type q = { visited : int; reward : float }

  type v = { visited : int; qs : q Q.t }

  type values = v V.t

  let update_q reward = function
    | None -> Some { visited = 1; reward }
    | Some { visited; reward = prev_estimate } ->
        let visited = visited + 1 in
        Some
          {
            visited;
            reward =
              prev_estimate
              +. ((reward -. prev_estimate) /. float_of_int visited);
          }

  let update_v action reward = function
    | None ->
        Some { visited = 1; qs = Q.singleton action { visited = 1; reward } }
    | Some v ->
        Some
          {
            visited = v.visited + 1;
            qs = v.qs |> Q.update action (update_q reward);
          }

  let update state action reward vs =
    vs |> V.update state (update_v action reward)

  type policy = ?st:CCRandom.state -> values -> state -> action

  let dealer_policy : policy =
   fun ?st:_ _values state -> if state.my_sum >= 17 then Stick else Hit

  let rand_policy ?st _values _state =
    CCRandom.(
      (let+ i = int 3 in
       if i = 0 then Stick else Hit)
      |> run ?st)

  let epsilon_greedy ~n_0 : policy =
   fun ?st (vs : values) (state : state) ->
    let v = vs |> V.get_or ~default:{ visited = 0; qs = Q.empty } state in
    let n_0 = float_of_int n_0 in
    let epsilon = n_0 /. (n_0 +. float_of_int v.visited) in
    let r =
      CCRandom.(
        let* f = float 1. in
        if f < epsilon then choose_exn (all_actions |> CCList.map pure)
        else
          let action_rewards =
            all_actions
            |> CCList.map (fun action ->
                   ( action,
                     v.qs
                     |> Q.get_or ~default:{ visited = 0; reward = 0.0 } action
                   ))
          in
          action_rewards
          |> CCList.sort (fun (_, q1) (_, q2) -> compare q1.reward q2.reward)
          |> CCList.hd |> fst |> pure)
    in
    CCRandom.run ?st r
end

module Environment = struct
  (** 1/3 chance of Red; 2/3 chance of Black. *)
  let r_color =
    CCRandom.(
      let+ i = int 3 in
      if i = 0 then Event.Red else Black)

  (** Value between 1 and 10 inclusive. *)
  let r_value =
    CCRandom.(
      let+ i = int 10 in
      i + 1)

  let r_draw =
    CCRandom.(
      let* color = r_color in
      let* values = r_value in
      pure { Event.color; values })

  (** To start, both players draw a Black card. *)
  let r_draw_black =
    CCRandom.(
      let* values = r_value in
      pure { Event.color = Black; values })

  let start ?st ?(log = false) state =
    let dealer_draw = CCRandom.run ?st r_draw_black in
    let event = Event.Dealer_draws dealer_draw in
    let () = if log then CCFormat.(eprintf "* %a@." Event.pp event) in
    let state = state |> State.update event in

    let player_draw = CCRandom.run ?st r_draw_black in
    let event = Event.Player_draws player_draw in
    let () = if log then CCFormat.(eprintf "* %a@." Event.pp event) in
    let state = state |> State.update event in
    let () = if log then CCFormat.eprintf "    %a@." State.pp state in
    state

  (** When the agent performs an action, what event do we actually get? *)
  let event_of_action ?st action state =
    match State.mode state with
    | Finished _ -> Event.No_event
    | Playing -> (
        match action with
        | Agent.Hit ->
            let draw = CCRandom.run ?st r_draw in
            Player_draws draw
        | Stick -> Player_sticks )
    | Sticking -> (
        match action with
        | Hit ->
            let draw = CCRandom.run ?st r_draw in
            Dealer_draws draw
        | Stick -> Dealer_sticks )

  let step ?st ?(log = false) action state =
    let event = event_of_action ?st action state in
    let () =
      if log then
        CCFormat.(eprintf "* %a -> %a@." Agent.pp_action action Event.pp event)
    in
    let state = State.update event state in
    let () = if log then CCFormat.eprintf "    %a@." State.pp state in
    state

  let reward state =
    match State.mode state with
    | Finished Win -> 1.0
    | Finished Draw -> 0.0
    | Finished Loss -> -1.0
    | _ -> 0.0
end

let pp_state_value fmt (reward, visits) =
  CCFormat.(fprintf fmt "@[r:%a, n:%i@]" float reward visits)

let pp_action_event fmt (action, event) =
  let open CCFormat in
  match (action, event) with
  | Agent.Hit, (Event.Player_draws d | Dealer_draws d) ->
      fprintf fmt "hits and draws %a" Event.pp_draw d
  | Agent.Stick, (Event.Player_sticks | Dealer_sticks) -> fprintf fmt "sticks"
  | _ -> fprintf fmt "%a -> %a" Agent.pp_action action Event.pp event

let rec play_out_dealer ?st ?log values state =
  match State.mode state with
  | Sticking ->
      let agent_state = Agent.observe_dealer state in
      let action = Agent.dealer_policy ?st values agent_state in
      let state = Environment.step ?st ?log action state in
      play_out_dealer ?st ?log values state
  | Playing | Finished _ -> state

let step ?st ?log action values state =
  let state = Environment.step ?st ?log action state in
  match action with
  | Agent.Hit -> state
  | Stick -> play_out_dealer ?st values state ?log

let episode ?st ?(log = false) ?(values = Agent.V.empty) (policy : Agent.policy)
    =
  let state = Environment.start ?st ~log State.make in
  (* let other_player_showing = state.dealer in *)
  let rec go values state =
    match State.mode state with
    | Finished _ -> (values, Environment.reward state)
    | _ ->
        let agent_state = Agent.observe_player state in
        let action = policy ?st values agent_state in
        let state = step ?st ~log action values state in
        let values, return = go values state in
        let values = values |> Agent.update agent_state action return in
        (values, return)
  in
  go values state

(** Evaluate the action-value function q for this policy. *)
let evaluate ?st ?(log = false) ?(values = Agent.V.empty) ~n policy =
  let rec go i values =
    if i <= 0 then values
    else
      let () =
        if log then CCFormat.(eprintf "==== Episode %i ====@." (n - i + 1))
      in
      let values, _ = episode ?st ~log ~values policy in
      go (i - 1) values
  in
  go n values
