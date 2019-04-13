open! Realm.Core;
open Realm.React;

module App = SimpleApp({
  type model = {
    count: int
  };

  let init = () => {
    count: 0
  };

  let click =
    Effect.update(model => { count: model.count + 1 })

  let view = model => {
    open Html;
    open Attr;

    let message =
      "You've clicked this " ++ String.fromInt(model.count) ++ " times(s)";

    div([
      button(~attrs=[ onClick(click) ], [
        text(message)
      ])
    ]);
  };
})