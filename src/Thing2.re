open Realm.Html;

type model = {
  count: int,
  show: bool,
};

type msg =
  | Click
  | Toggle;


let init = () => {
  count: 0,
  show: true
};


let update = (msg, model) =>
  switch (msg) {
  | Click   => { ...model, count: model.count + 1 }
  | Toggle  => { ...model, show: !model.show }
  };


let view = (~greeting, model, dispatch) => {
  let message =
    "You've clicked this " ++ string_of_int(model.count) ++ " times(s)";

  div([
    button(~onClick=_event => dispatch(Click), [
      text(message)
    ]),
    button(~onClick=_event => dispatch(Toggle), [
      text("Toggle greeting")
    ]),
    model.show ? text(greeting) : null
  ]);
};