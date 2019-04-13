open! Realm.Core;
open Realm.React;

let toggleAllCheckbox = (~entries as _) =>
  Html.input(
    ~className="toggle-all",
    ~value=`Checkbox(false),
    []);

let view = (~entries, ~visibility) => {
  open Html;

  section(
    [ toggleAllCheckbox(~entries)
    , TodoList.view(~entries, ~visibility)
    ])
};