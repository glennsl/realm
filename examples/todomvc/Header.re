open! Realm.Core;
open Realm.React;
open Model;

let addTextInput =
  TodoTextInput.view(~className="new-todo", ~placeholder="What needs to be done?", "")
    |> Html.map(Effect.map(_ => "", (model, name) => { ...model, entries: model.entries @ [Todo.make(name)] }))

let view = () => {
  open Html;

  header(
    [ h1([text("todos")])
    , addTextInput
    ])
};