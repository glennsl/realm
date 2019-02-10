open RealmNoUpdate;

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

  let message =
    "You've clicked this " ++ string_of_int(model.count) ++ " times(s)";

  div([
    button(~attrs=[ onClick(click) ], [
      text(message)
    ])
  ]);
};

let mount 
  : (~at: string) => unit
  = (~at) =>
    mountHtml(~at, ~init, ~view);