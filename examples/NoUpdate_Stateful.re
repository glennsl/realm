open RealmNoUpdate;
open! Core;

type model = {
  count: int
};

module Html = MakeHtml({
  type nonrec model = model
})

let init = () => {
  count: 0
};

let click =
  Cmd.make(model => { count: model.count + 1 })

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

let mount 
  : (~at: string) => unit
  = (~at) =>
    mountHtml(~at, ~init, ~view, ());