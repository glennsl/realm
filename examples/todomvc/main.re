open Realm;
open! Core;
open Model;

let init = () =>
  Task.const(
    { entries:
      [ Todo.make("Item 1")
      , Todo.make("Item 2")
      ]
    , visibility: "All"
    }
  );

open Model.Html;

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

mountHtml(~at="todoapp", ~init, ~view, ())