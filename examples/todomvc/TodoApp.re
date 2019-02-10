open Model;

let view = (~items, ~editing) =>
  Html.div(
    ~className="todoapp",
    [ Header.view()
    , MainSection.view(~items, ~editing)
    , TodoFooter.view(~items)
    ])