open Realm
open! Core

type mode = Off | Slow | Fast | Both

type model =
  { count: int
  ; mode : mode
  }

module Html = MakeHtml (struct type nonrec model = model end)

let init () =
  { count = 0
  ; mode = Fast
  }

let subs model =
  let tick () = Effect.update (fun model -> { model with count = (model.count + 1) }) in
  let slow = Time.every "slow" 2000. tick in
  let fast = Time.every "fast" 1000. tick in
  match model.mode with
  | Off -> []
  | Slow -> [ slow ]
  | Fast -> [ fast ]
  | Both -> [ slow; fast ]

let setMode mode =
  Effect.update (fun model -> { model with mode })

let view model =
  let open Html in
  let open Attr in
  let message = String.fromInt model.count ^ " seconds since page load" in
  div
    [ div
      [ button ~attrs:[onClick (setMode Off)] [ text "Off" ]
      ; button ~attrs:[onClick (setMode Slow)] [ text "Slow" ]
      ; button ~attrs:[onClick (setMode Fast)] [ text "Fast" ]
      ; button ~attrs:[onClick (setMode Both)] [ text "Both" ]
      ]
    ; text message
    ]

let mount: at:string -> unit =
  fun ~at -> mountHtml ~at ~init ~subs ~view ()