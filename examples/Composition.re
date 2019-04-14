open! Realm.Core;
open Realm.React;

module Clicker = {
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
      ]),
    ]);
  };
};


module Toggler = {
  type model = {
    show: bool,
    n: int
  };

  let init = () => {
    show: true,
    n: 0
  };

  let toggle =
    Effect.do_(
      _ => Future.randomInt(0, 10),
      (n, model) => { n, show: !model.show }
    )

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


module App = SimpleApp({
  type model = {
    clicker: Clicker.model,
    toggler: Toggler.model
  };


  let init = () => {
    clicker: Clicker.init(),
    toggler: Toggler.init()
  };

  module Components = {
    open Html;

    let clicker = model => 
      Clicker.view(model.clicker)
        |> map( model => model.clicker,
                (model, clickerModel) => { ...model, clicker: clickerModel });
    
    let toggler = (~greeting, model) =>
      Toggler.view(~greeting, model.toggler)
        |> map( model => model.toggler,
                (model, togglerModel) => { ...model, toggler: togglerModel });
  }

  let view = model => {
    open Html;

    div([
      Components.clicker(model),
      Components.toggler(~greeting="Hello", model)
    ])
  };
})
