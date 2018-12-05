
type model = {
  count: int
};

module Html = Realm.NoUpdate.Html({
  type nonrec model = model
})

let init = () => {
  count: 0
};

let click =
  Realm.NoUpdate.SetState(model => { count: model.count + 1 })

let view = model => {
  open Html;

  let message =
    "You've clicked this " ++ string_of_int(model.count) ++ " times(s)";

  div([], [
    button([ onClick(click) ], [
      text(message)
    ])
  ]);
};

let mount 
  : (~at: string) => unit
  = (~at) =>
    Realm.NoUpdate.mountHtml(~at, ~init, ~view);