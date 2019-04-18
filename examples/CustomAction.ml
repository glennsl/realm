open! Realm.Core
open Realm.React

module App = App(struct
  type model =
    { count: int }

  type action =
    | Increment
    | Decrement

  let init () =
    Future.const { count = 0 }

  let update action =
    Effect.(
      match action with
      | Increment ->
        update (fun model  -> { count = (model.count + 1) })
      | Decrement ->
        update (fun model  -> { count = (model.count - 1) })
    )

  let subs _ = []

  let view model =
    let open Html in
    let open Attr in
    div
      [ button ~attrs:[ onClick Increment ] [ text "+" ]
      ; text (model.count |> String.fromInt)
      ; button ~attrs:[ onClick Decrement ] [ text "-" ]
      ]
end)