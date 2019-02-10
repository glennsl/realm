open Model;

let toggleAllCheckbox = (~items as _) =>
  Html.input(
    ~className="toggle-all",
    ~value=`Checkbox(false));

let view = (~items, ~editing) => {
  open Html;

  section(
    [ toggleAllCheckbox(~items)
    , TodoList.view(~items, ~editing)
    ])
};