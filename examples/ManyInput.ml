open! Realm.Core
open Realm.React

let textInput value =
  Html.input
    ~value:(`Text value)
    ~attrs:[ Html.Attr.onInput Effect.const ]
    []


module App = SimpleApp(struct
  type model =
    { a : string
    ; b : string
    ; c : string
    ; d : string
    }

  let init () =
    { a = ""
    ; b = ""
    ; c = ""
    ; d = ""
    }

  let view model =
    let open Html in
    div
      [ textInput model.a |> map (fun m -> m.a ) (fun m v -> { m with a = v })
      ; textInput model.b |> map (fun m -> m.b ) (fun m v -> { m with b = v })
      ; textInput model.c |> map (fun m -> m.c ) (fun m v -> { m with c = v })
      ; textInput model.d |> map (fun m -> m.d ) (fun m v -> { m with d = v })
      ]
end)