open! Realm.Core;
open Realm.React;
open Model

module App = SimpleApp({
  type model = Model.t

  let init = () => {
    { entries:
      [ Todo.make("Item 1")
      , Todo.make("Item 2")
      ]
    , visibility: "All"
    }
  };

  open Html;

  let infoFooter =
    footer(
      ~className="info",
      [
        p([text("Double-click to edit a todo")]),
        p([
          text("Written by "),
          a(~href="https://github.com/glennsl", [text("Glenn Slotte")]),
        ]),
        p([
          text("Part of "),
          a(~href="http://todomvc.com", [text("TodoMVC")]),
        ]),
      ],
    );

  let view = ({ entries, visibility }) => {
    div([
      TodoApp.view(~entries, ~visibility),
      infoFooter
    ]);
  };
});