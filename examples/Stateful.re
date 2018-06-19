
type model = {
  count: int
};

type msg =
  | Click;

module Html = Realm.App.Html({ type nonrec msg = msg; })

let init = () => {
  count: 0
};


let update = (msg, model) =>
  switch (msg) {
  | Click => { count: model.count + 1 }
  };

let view = model => {
  open Html;

  let message =
    "You've clicked this " ++ string_of_int(model.count) ++ " times(s)";

  div([], [
    button([ onClick(Click) ], [
      text(message)
    ])
  ]);
};

let mount = (~at) =>
  Realm.App.mount(~at, ~init, ~update, ~view);