
module Clicker = {
  type msg =
    | Click;

  type model = {
    count: int
  };

  let init = () => {
    count: 0
  };

  let update = (msg, model) =>
    switch (msg) {
    | Click => { count: model.count + 1 }
    };

  module Html = Realm.React.Html({ type nonrec msg = msg; })

  let view = model => {
    open Html;

    let message =
      "You've clicked this " ++ string_of_int(model.count) ++ " times(s)";

    div([], [
      button([ onClick(Click) ], [
        text(message)
      ]),
    ]);
  };
};


module Toggler = {
  type msg =
    | Toggle;

  type model = {
    show: bool
  };

  let init = () => {
    show: true
  };

  let update = (msg, model) =>
    switch (msg) {
    | Toggle => { show: !model.show }
    };

  module Html = Realm.React.Html({ type nonrec msg = msg; })

  let view = (~greeting, model) => {
    open Html;

    div([], [
      button([ onClick(Toggle) ], [
        text("Toggle greeting")
      ]),
      model.show ? text(greeting) : null
    ]);
  };
};


type msg =
  | ClickerMsg(Clicker.msg)
  | TogglerMsg(Toggler.msg);

type model = {
  clicker: Clicker.model,
  toggler: Toggler.model
};


let init = () => {
  clicker: Clicker.init(),
  toggler: Toggler.init()
};


let update = (msg, model) =>
  switch (msg) {
  | ClickerMsg(msg) => { ...model, clicker: Clicker.update(msg, model.clicker) }
  | TogglerMsg(msg) => { ...model, toggler: Toggler.update(msg, model.toggler) }
  };

module Html = Realm.React.Html({ type nonrec msg = msg; })

let view = (~greeting, model) => {
  open Html;

  div([], [
    Clicker.view(model.clicker) |> Realm.React.map(msg => ClickerMsg(msg)),
    Toggler.view(~greeting, model.toggler) |> Realm.React.map(msg => TogglerMsg(msg))
  ])

};


let mount = (~at) =>
  Realm.React.mount(~at, ~init, ~update, ~view=view(~greeting="hello"));