open! Realm.Core;
open Model;
open Html;
open Attr;

let visibilityButton = (uri, visibility, actualVisibility) =>
  li(
    ~attrs=[onClick(Actions.changeVisibility(visibility))],
    [ 
      a(
        ~className = visibility == actualVisibility ? "selected" : "",
        ~href=uri,
        [text(visibility)]
      )
    ],
  );

let clearButton = completedCount =>
  button(
    ~className = "clear-completed",
    ~attrs = [
      hidden(completedCount == 0),
      onClick(Actions.clearCompleted)
    ],
    [text("Clear completed")]);

let filters = visibility =>
  ul(
    ~className="filters",
    [
      visibilityButton("#/", "All", visibility),
      text(" "),
      visibilityButton("#/active", "Active", visibility),
      text(" "),
      visibilityButton("#/completed", "Completed", visibility),
    ],
  );


let view = (~entries, ~visibility) => {
  let completedCount =
    entries |> List.filter(entry => Todo.(entry.completed)) |> List.length;

  let activeCount =
    List.length(entries) - completedCount; 

  footer(
    ~className = "footer",
    ~attrs = [hidden(entries == [])],
    [ span(~className="todo-count", [
        strong([text(activeCount |> String.fromInt)]),
        text(" items")
      ])
    , filters(visibility)
    , clearButton(completedCount)
    ])
};