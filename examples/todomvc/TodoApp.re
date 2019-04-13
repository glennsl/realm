open! Realm.Core;
open Realm.React;

let view = (~entries, ~visibility) =>
  Html.div(
    ~className="todoapp",
    [ Header.view()
    , MainSection.view(~entries, ~visibility)
    , TodoFooter.view(~entries, ~visibility)
    ])