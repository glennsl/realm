open RealmNoUpdate;
open Model;

let editTextInput = initialValue =>
  TodoTextInput.view(~className="edit", initialValue)
    |> map(_ => "", (model, name) => { ...model, items: model.items @ [Todo.make(name)] })

let view = (~item: Todo.t, ~isEditing) => {
  open Model.Html;
  open Attr;

  li(~className=isEditing ? "editing" : "", [
    div(~className="view", [
      Model.Html.input(
        ~className="toggle",
        ~attrs=[onChange(Actions.toggle(item))], 
        ~value=`Checkbox(item.completed)),
      label(~attrs=[onDoubleClick(Actions.edit(item))], [text(item.title)]),
      button(~className="destroy", ~attrs=[Attr.onClick(Actions.remove(item))], [])
    ]),
    isEditing ? editTextInput(item.title) : null
    /* div(~attrs=Attr.[className("created")], [text(Js.Date.toLocaleString(item.created))]) */
  ])
};