open! Realm.Core
open Realm.React


module Clicker = struct
  type model =
    { count: int }

  let init () =
    { count = 0 }

  let click =
    Effect.update (fun model -> { count = model.count + 1 })

  let view model =
    let open Html in
    let open Attr in
    let message =
      "You've clicked this " ^ String.fromInt model.count ^ " times(s)"
      in
    div
      [ button ~attrs:[ onClick click ] [ text message ]
      ]
end


module Toggler = struct
  type model =
    { show: bool
    ; n: int
    }

  let init () =
    { show = true; n = 0 }

  let toggle =
    Effect.do_
      (fun _ -> Future.randomInt 0 10)
      (fun n model -> { n; show = not model.show })

  let view ~greeting  model =
    let open Html in
    let open Attr in
      div
        [ button
            ~attrs:[ onClick toggle ]
            [ text ("Toggle greeting " ^ String.fromInt model.n) ]
        ; begin match model.show with
          | true  -> text greeting
          | false  -> null
          end
        ]
end


module App = SimpleApp(struct
  type model =
    { clicker: Clicker.model
    ; toggler: Toggler.model
    }

  let init () =
    { clicker = Clicker.init ()
    ; toggler = Toggler.init ()
    }

  module Components = struct
      open Html

      let clicker model =
        Clicker.view model.clicker
          |> map
              ( Effect.map
                  (fun model -> model.clicker)
                  (fun model clickerModel -> { model with clicker = clickerModel })
              )

      let toggler ~greeting  model =
        Toggler.view ~greeting model.toggler
          |> map
              ( Effect.map
                  (fun model -> model.toggler)
                  (fun model togglerModel -> { model with toggler = togglerModel })
              )
  end

  let view model =
    let open Html in
    div
      [ Components.clicker model
      ; Components.toggler ~greeting:"Hello" model
      ]
end)