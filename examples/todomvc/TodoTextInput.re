open! Realm.Core;
open Realm.React

module TextInput = {
  let component = ReasonReact.reducerComponent("Greeting");

  let make = (~className, ~placeholder=?, ~onEnter, ~initialValue, _children) => {
    ...component,
    initialState: () => initialValue,
    reducer: (action, state) =>
      switch (action) {
      | `ValueChanged(value) => ReasonReact.Update(value)
      | `KeyDown(key) => {
          if (key == 13) {
            onEnter(state);
            ReasonReact.Update("")
          } else {
            ReasonReact.NoUpdate
          }
        }
      },
    render: self =>
      <input className
             ?placeholder
             value       = self.state
             onChange    = (e => self.send(`ValueChanged(Obj.magic(e)##target##value)))
             onKeyDown   = (e => self.send(`KeyDown(Obj.magic(e)##keyCode))) />
  };
}

let view = (~className, ~placeholder=?, initialValue) =>
  Html.raw(dispatch => <TextInput className ?placeholder initialValue onEnter=(value => dispatch(Effect.const(value))) />)