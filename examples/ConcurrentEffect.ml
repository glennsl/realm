open! Realm.Core
open Realm.React

module App = SimpleApp(struct
  type model =
    string

  let init () =
    ""

  let run =
    let fakeJob name delay =
      Future.make (fun resolve -> Js.log ("start " ^ name) |> resolve)
      |> Future.andThen (fun () -> Time.delay delay)
      |> Future.map (fun () -> Js.log ("done " ^ name))
      in
    Effect.do_ (fun _ -> Future.all2 (fakeJob "1" 1000.) (fakeJob "2" 500.)) (fun ((), ()) _ -> "done")

  let view model =
    let open Html in
    let open Attr in
    div
      [ button ~attrs:[ onClick run ] [ text "Start" ]
      ; text model
      ]
end)