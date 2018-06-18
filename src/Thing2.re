
type model = {
  count: int,
  show: bool,
};

type msg =
  | Click
  | Toggle;

module Html = Realm.App.Html({ type nonrec msg = msg; })
open Html;

let init = () => {
  count: 0,
  show: true
};


let update = (msg, model) =>
  switch (msg) {
  | Click   => { ...model, count: model.count + 1 }
  | Toggle  => { ...model, show: !model.show }
  };


let view = (~greeting, model) => {
  let message =
    "You've clicked this " ++ string_of_int(model.count) ++ " times(s)";

  div([], [
    button([ onClick(Click) ], [
      text(message)
    ]),
    button([ onClick(Toggle) ], [
      text("Toggle greeting")
    ]),
    model.show ? text(greeting) : null
  ]);
};