open! Realm.Core
open Realm.React

module App = App(struct
  type model =
    { originalA : string option
    ; originalB : string option
    ; combined : string option
    }

  type action =
    | Start
    | InitA of string
    | InitB of string

  let init () =
    Future.const 
      { originalA = None
      ; originalB = None
      ; combined = None
      }
    
  let update action =
    let open Effect in
    let tryInit model =
      match model.originalA, model.originalB with
      | Some a, Some b -> { model with combined = Some (a ^ ", " ^ b ^ "!")}
      | _              -> model
      in
    match action with
    | Start ->
      let fakeJob name delay =
        Future.make (fun resolve -> Js.log ("start " ^ name) |> resolve)
        |> Future.andThen (fun () -> Time.delay delay)
        |> Future.map (fun () -> Js.log ("done " ^ name); name)
        in
      do_ (fakeJob "Hello" 1000.) (fun value -> InitA value)
      |> andThen (do_ (fakeJob "world" 500.) (fun value -> InitB value))

    | InitA value ->
      update (fun model -> tryInit { model with originalA = Some value })

    | InitB value ->
      update (fun model -> tryInit { model with originalB = Some value })

  let subs _ = []

  let view model =
    let open Html in
    let open Attr in
    div
      [ button ~attrs:[ onClick Start ] [ text "Start" ]
      ; text model
      ]
end)