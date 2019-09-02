open Vdom.Node

let oldNode =
  element "strong"
    [ Vdom.Attribute.make "class" "test-class" ]
    [ text "Hello"
    ; element "em" [] [ text " world" ]
    ]
let node =
  element "strong"
    [ Vdom.Attribute.make "class" "test-class" ]
    [ text "Hello"
    ; text "!"
    ; element "em" [] [ text " world" ]
    ]

let domNode =
  Vdom.render node "root"

let () =
  Vdom.diff ~rootDomNode:domNode ~oldVTree:oldNode ~newVTree:node
    |> List.map Vdom.pp_patch
    |> List.iter Js.log
  
  