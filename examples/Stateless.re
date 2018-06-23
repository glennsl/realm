module Html = Realm.React.Html({ type msg = unit });
open Html;

let view = (~message, _) =>
  div([], [
    text(message)
  ]);

let mount = (~at) =>
  Realm.React.mount(~at, ~init=() => (), ~update=(_, ()) => (), ~view=view(~message="hello"));