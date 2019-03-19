open! RealmNoUpdate.Core;
open Model;

let view = (~entries, ~visibility) =>
  Html.div(
    ~className="todoapp",
    [ Header.view()
    , MainSection.view(~entries, ~visibility)
    , TodoFooter.view(~entries, ~visibility)
    ])