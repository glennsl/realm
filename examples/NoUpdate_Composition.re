open Realm;
open! Core;

module Clicker = {
  type model = {
    count: int
  };

  let init = () => Task.const({
    count: 0
  });

  let click =
    Effect.update(model => { count: model.count + 1 })

  module Html = MakeHtml({ type nonrec model = model; })

  let view = model => {
    open Html;
    open Attr;

    let message =
      "You've clicked this " ++ String.fromInt(model.count) ++ " times(s)";

    div([
      button(~attrs=[ onClick(click) ], [
        text(message)
      ]),
    ]);
  };
};


module Toggler = {
  type model = {
    show: bool,
    n: int
  };

  let init = () => Task.const({
    show: true,
    n: 0
  });

  let toggle =
    Effect.do_(
      _ => Task.randomInt(0, 10),
      (n, model) => { n, show: !model.show }
    )

  module Html = MakeHtml({ type nonrec model = model; })

  let view = (~greeting, model) => {
    open Html;
    open Attr;

    div([
      button(~attrs=[ onClick(toggle) ], [
        text("Toggle greeting " ++ String.fromInt(model.n))
      ]),
      model.show ? text(greeting) : null
    ]);
  };
};


type model = {
  clicker: Clicker.model,
  toggler: Toggler.model
};


let init = () =>
  Task.map2(
    (clicker, toggler) => {
      clicker: clicker,
      toggler: toggler
    }, Clicker.init(), Toggler.init()
  );


module Html = MakeHtml({ type nonrec model = model; })

module Components = {
  let clicker = model => 
    Clicker.view(model.clicker)
      |> map( model => model.clicker,
              (model, clickerModel) => { ...model, clicker: clickerModel });
  
  let toggler = (~greeting, model) =>
    Toggler.view(~greeting, model.toggler)
      |> map( model => model.toggler,
              (model, togglerModel) => { ...model, toggler: togglerModel });
}

let view = (~greeting, model) => {
  open Html;

  div([
    Components.clicker(model),
    Components.toggler(~greeting, model)
  ])
};


let mount = (~at) =>
  mountHtml(~at, ~init, ~view=view(~greeting="hello"), ());