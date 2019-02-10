open Model;

let view = (~items, ~editing) => {
  open Html;

  let itemViews =
    items |> List.map(item =>
    TodoItem.view(
      ~item,
      ~isEditing=(Some(item.id) == editing)))

 ul(
   ~className="todo-list",
   itemViews)
};