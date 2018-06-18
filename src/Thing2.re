
type model = {
  count: int,
  show: bool,
};

type msg =
  | Click
  | Toggle
  | Foo(string);

module Html = Realm.App.Html({ type nonrec msg = msg; })

let init = () => {
  count: 0,
  show: true
};


let update = (msg, model) =>
  switch (msg) {
  | Click   => { ...model, count: model.count + 1 }
  | Toggle  => { ...model, show: !model.show }
  | Foo(s)  => { Js.log(s); model }
  };

module FooHtml = Realm.App.Html({ type nonrec msg = string; })
let wrapFooMsg =
  Realm.App.map(s => Foo(s));

let fooView =
  FooHtml.(
    button([ onClick("foo") ], [
      text("Set foo")
    ])
  );

let view = (~greeting, model) => {
  open Html;

  let message =
    "You've clicked this " ++ string_of_int(model.count) ++ " times(s)";

  div([], [
    button([ onClick(Click) ], [
      text(message)
    ]),
    button([ onClick(Toggle) ], [
      text("Toggle greeting")
    ]),
    fooView |> wrapFooMsg,
    model.show ? text(greeting) : null
  ]);
};