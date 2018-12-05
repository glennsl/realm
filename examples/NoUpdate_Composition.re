
module Clicker = {
  type model = {
    count: int
  };

  let init = () => {
    count: 0
  };

  let click =
    Realm.NoUpdate.SetState(model => { count: model.count + 1 })

  module Html = Realm.NoUpdate.Html({ type nonrec model = model; })

  let view = model => {
    open Html;

    let message =
      "You've clicked this " ++ string_of_int(model.count) ++ " times(s)";

    div([], [
      button([ onClick(click) ], [
        text(message)
      ]),
    ]);
  };
};


module Toggler = {
  type model = {
    show: bool
  };

  let init = () => {
    show: true
  };

  let toggle =
    Realm.NoUpdate.SetState(model => { show: !model.show })

  module Html = Realm.NoUpdate.Html({ type nonrec model = model; })

  let view = (~greeting, model) => {
    open Html;

    div([], [
      button([ onClick(toggle) ], [
        text("Toggle greeting")
      ]),
      model.show ? text(greeting) : null
    ]);
  };
};


type model = {
  clicker: Clicker.model,
  toggler: Toggler.model
};


let init = () => {
  clicker: Clicker.init(),
  toggler: Toggler.init()
};


module Html = Realm.NoUpdate.Html({ type nonrec model = model; })

let view = (~greeting, model) => {
  open Html;

  div([], [
    Clicker.view(model.clicker) |> Realm.NoUpdate.map(model => model.clicker, (clickerModel, model) => { ...model, clicker: clickerModel }),
    Toggler.view(~greeting, model.toggler) |> Realm.NoUpdate.map(model => model.toggler, (togglerModel, model) => { ...model, toggler: togglerModel })
  ])

};


let mount = (~at) =>
  Realm.NoUpdate.mountHtml(~at, ~init, ~view=view(~greeting="hello"));