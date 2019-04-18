open Realm.React;
open! Realm.Core;
open Model;

let editTextInput = initialValue =>
  TodoTextInput.view(~className="edit", initialValue)
    |> Html.map(Effect.map(_ => "", (model, name) => { ...model, entries: model.entries @ [Todo.make(name)] }))

let view = (entry: Todo.t) => {
  open Html;
  open Attr;

  li(~className=entry.editing ? "editing" : "", [
    div(~className="view", [
      Html.input(
        ~className="toggle",
        ~attrs=[onChange(Actions.toggle(entry))], 
        ~value=`Checkbox(entry.completed),
        []),
      label(~attrs=[onDoubleClick(Actions.edit(entry, ~editing=true))], [text(entry.title)]),
      button(~className="destroy", ~attrs=[Attr.onClick(Actions.remove(entry))], [])
    ]),
    entry.editing ? editTextInput(entry.title) : null
    /* div(~attrs=Attr.[className("created")], [text(Js.Date.toLocaleString(item.created))]) */
  ])
};