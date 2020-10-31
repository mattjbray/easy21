module Event = struct
  type color = Red | Black

  type draw = { color : color; value : int }

  let pp_draw fmt d =
    let open CCFormat in
    let c = match d.color with Red -> "Red" | Black -> "Black" in
    let op = match d.color with Red -> "-" | Black -> "+" in
    fprintf fmt "%s%a" op (with_color c int) d.value

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
    fprintf fmt "@[<v>";
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
    op sum draw.value

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

  let update_log event state =
    let () = CCFormat.printf "* %a@." Event.pp event in
    let state = update event state in
    let () = CCFormat.printf "    %a@." pp state in
    state
end

module Agent = struct
  (** Agent actions *)
  type action = Stick | Hit

  type state = State.t
  (** In this model, agents can view the full environment state *)

  let dealer_policy ?st:_ state =
    if State.dealer state >= 17 then Stick else Hit

  let player_policy ?st _state =
    CCRandom.(
      (let+ i = int 3 in
       if i = 0 then Stick else Hit)
      |> run ?st)
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
      let* value = r_value in
      pure { Event.color; value })

  (** To start, both players draw a Black card. *)
  let r_draw_black =
    CCRandom.(
      let* value = r_value in
      pure { Event.color = Black; value })

  let start ?st state =
    let dealer_draw = CCRandom.run ?st r_draw_black in
    let state = state |> State.update_log (Dealer_draws dealer_draw) in
    let player_draw = CCRandom.run ?st r_draw_black in
    let state = state |> State.update_log (Player_draws player_draw) in
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

  let step ?st action state =
    let event = event_of_action ?st action state in
    State.update_log event state

  let reward state =
    match State.mode state with
    | Finished Win -> Some 1
    | Finished Draw -> Some 0
    | Finished Loss -> Some (-1)
    | _ -> None
end

module State_map = CCMap.Make (struct
  type t = State.t

  let compare = Stdlib.compare
end)

let rec play_out_dealer ?st state =
  match State.mode state with
  | Sticking ->
      let act = Agent.dealer_policy ?st state in
      let evt = Environment.event_of_action ?st act state in
      let state = State.update_log evt state in
      play_out_dealer ?st state
  | Playing | Finished _ -> state

let step ?st policy state =
  match State.mode state with
  | Playing ->
      let act = policy ?st state in
      let evt = Environment.event_of_action ?st act state in
      State.update_log evt state
  | Sticking -> play_out_dealer ?st state
  | Finished _ -> state

let episode ?st policy =
  let state = Environment.start ?st State.make in
  let rec go state =
    match State.mode state with
    | Finished _ -> state
    | _ ->
        let state = step ?st policy state in
        go state
  in
  go state
