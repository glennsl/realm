open RealmNoUpdate;
open! Core;
open Model;

let editTextInput = initialValue =>
  TodoTextInput.view(~className="edit", initialValue)
    |> map(_ => "", (model, name) => { ...model, entries: model.entries @ [Todo.make(name)] })

let view = (entry: Todo.t) => {
  open Model.Html;
  open Attr;

  li(~className=entry.editing ? "editing" : "", [
    div(~className="view", [
      Model.Html.input(
        ~className="toggle",
        ~attrs=[onChange(Actions.toggle(entry))], 
        ~value=`Checkbox(entry.completed)),
      label(~attrs=[onDoubleClick(Actions.edit(entry, ~editing=true))], [text(entry.title)]),
      button(~className="destroy", ~attrs=[Attr.onClick(Actions.remove(entry))], [])
    ]),
    entry.editing ? editTextInput(entry.title) : null
    /* div(~attrs=Attr.[className("created")], [text(Js.Date.toLocaleString(item.created))]) */
  ])
};