open RealmNoUpdate;
open Model;

let toggle = item =>
  Cmd.make(model => { ...model, items: List.map(it => it.Todo.id == item.Todo.id ? { ...item, completed: !item.completed } : it, model.items) })

let edit = item =>
  Cmd.make(model => { Js.log("edit"); { ...model, editing: Some(item.Todo.id) }});

let remove = item =>
  Cmd.make(model => { ...model, items: List.filter(it => it.Todo.id != item.Todo.id, model.items) })

let clearCompleted =
  Cmd.make(model => { ...model, items: List.filter(it => !it.Todo.completed, model.items) })
