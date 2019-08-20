open Vdom.Node

let node =
  element "strong"
    [ Vdom.Attribute.make "class" "test-class" ]
    [ text "Hello"
    ; element "em" [] [ text " world" ]
    ]

let () =
  Vdom.render node "root"