open Realm.Html;

let view = (~message, _, _) =>
  div(~onClick=(_) => Js.log("clicky!"), [
    text(message)
  ]);