open! Realm.Core
open Realm.React


module App = App(struct
  type model =
    { time: Time.t
    ; running: bool
    }

  type action =
    model Effect.t


  let init () =
    Time.now
      |> Future.map (fun time -> { time; running = true })

  let update = Fn.id

  let subs model =
    let tick () =
      Effect.do_ (fun _ -> Time.now) (fun time model -> { model with time })
      in
    if model.running then
      [ Time.every "fast" 1000. tick ]
    else
      []


  let enable =
    Effect.update (fun model -> { model with running = true })

  let disable =
    Effect.update (fun model -> { model with running = false })


  let view model =
    let open Html in
    let open Attr in
    div
      [ text (Time.toString model.time)
      ; div
        [ button ~attrs:[ onClick disable ] [ text "Off" ]
        ; button ~attrs:[ onClick enable ] [ text "On" ]
        ]
      ]
end)