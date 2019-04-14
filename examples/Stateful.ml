open! Realm.Core
open Realm.React

module App = SimpleApp(struct
  type model =
    { count: int }

  let init () =
    { count = 0 }

  let click =
    Effect.update (fun model  -> { count = (model.count + 1) })

  let view model =
    let open Html in
    let open Attr in
    let message =
      "You've clicked this " ^ String.fromInt model.count ^ " times(s)"
      in
    div
      [ button ~attrs:[ onClick click ] [ text message ]
      ]
end)