open Model;
open Html;

let clearButton = 
  button(~className="clear-completed", ~attrs=[onClick(Actions.clearCompleted)], [text("Clear completed")])

let view = (~items) => {
  let completedCount = items |> List.filter(item => Todo.(item.completed)) |> List.length;
  let activeCount = List.length(items) - completedCount; 

  footer(
    ~className="footer",
    [ span(~className="todo-count", [
        strong([text(activeCount |> string_of_int)]),
        text(" items")
      ])
    , completedCount > 0 ? clearButton : null
    ])
};