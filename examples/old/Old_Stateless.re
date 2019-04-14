module Html = RealmOld.React.Html({ type msg = unit });
open Html;

let view = (~message, _) =>
  div([], [
    text(message)
  ]);

let mount = (~at) =>
  RealmOld.React.mount(~at, ~init=() => (), ~update=(_, ()) => (), ~view=view(~message="hello"));