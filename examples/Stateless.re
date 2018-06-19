module Html = Realm.App.Html({ type msg = unit });
open Html;

let view = (~message, _) =>
  div([], [
    text(message)
  ]);

let mount = (~at) =>
  Realm.App.mount(~at, ~init=() => (), ~update=(_, ()) => (), ~view=view(~message="hello"));