module Html = Realm.App.Html({ type msg = unit });
open Html;

let view = (~message, _) =>
  div([], [
    text(message)
  ]);