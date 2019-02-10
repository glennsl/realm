open RealmNoUpdate;
open Model;

let addTextInput =
  TodoTextInput.view(~className="new-todo", ~placeholder="What needs to be done?", "")
    |> map(_ => "", (model, name) => { ...model, items: model.items @ [Todo.make(name)] })

let view = () => {
  open Model.Html;

  header(
    [ h1([text("todos")])
    , addTextInput
    ])
};