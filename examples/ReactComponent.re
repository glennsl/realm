open! Realm.Core;
open Realm.React

module TextInput = {
  let component = ReasonReact.reducerComponent("Greeting");

  let make = (~onEnter, ~default, _children) => {
    ...component,
    initialState: () => default,
    reducer: (action, state) =>
      switch (action) {
      | `ValueChanged(value) =>
        ReasonReact.Update(value)

      | `KeyDown(key) => {
          if (key == 13) {
            onEnter(state);
            ReasonReact.Update(default)
          } else {
            ReasonReact.NoUpdate
          }
        }
      },
    render: self =>
      <input value       = self.state
             onChange    = (e => self.send(`ValueChanged(Obj.magic(e)##target##value)))
             onKeyDown   = (e => self.send(`KeyDown(Obj.magic(e)##keyCode))) />
  };
}

let textInput = default =>
  Html.raw(dispatch => <TextInput default onEnter=(Effect.const >> dispatch) />)

module App = SimpleApp({
  type model =
    string

  let init () =
    ""

  let view = model => {
    open Html

    div([
      textInput(""),
      text("In model:" ++ model)
    ])
  }
})