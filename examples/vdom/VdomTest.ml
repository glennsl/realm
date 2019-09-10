open Vdom.Node

let log patches =
  Js.log "--";
  patches
    |> List.map Vdom.pp_patch
    |> List.iter Js.log

let domNode = Vdom.Dom.getElementById "root"

type state =
  | First
  | Second
  
let current =
  ref (text "")

let rec render state =
  match state with
    | First ->
      element "strong"
        [ Attribute (Vdom.Attribute.make "class" "blue")
        ; Event ("click", onClick state)
        ]
        [ text "Hi"
        ; element "em" [] [ text " world" ]
        ; element "span" [] [ text "!" ]
        ]

    | Second ->
      element "strong"
        [ Attribute (Vdom.Attribute.make "class" "red")
        ; Event ("click", onClick state)
        ]
        [ text "Hello"
        ; element "em" [] [ text " world" ]
        ]

and onClick state _ =
  let next =
    match state with
    | First ->
      render Second

    | Second ->
      render First
  in
  let patches = Vdom.diff ~rootDomNode:domNode ~oldVTree:!current ~newVTree:next in
  log patches;
  Vdom.patch patches;
  current := next

let () =
  current := render First;
  Vdom.Dom.appendChild (Vdom.render !current) domNode
  