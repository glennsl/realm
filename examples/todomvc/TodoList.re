open! Realm.Core;
open Model;

let view = (~entries, ~visibility) => {
  open Html;

  let
    isVisible = entry =>
      switch (visibility) {
      | "Completed" => entry.Todo.completed
      | "Active"    => !entry.completed
      | _           => true
      }

  ul(
    ~className="todo-list",
    entries
      |> List.filter(isVisible)
      |> List.map(TodoItem.view)
  )
};