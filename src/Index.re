ReactDOMRe.renderToElementWithId(<Component1 message="Hello!" />, "index1");

ReactDOMRe.renderToElementWithId(<Component2 greeting="Hello!" />, "index2");

Realm.mount(
  ~at="thing1",
  ~init=() => (),
  ~update=(_, ()) => (),
  ~view=Thing1.view(~message="hello")
);

Realm.mount(
  ~at="thing2",
  ~init=Thing2.init,
  ~update=Thing2.update,
  ~view=Thing2.view(~greeting="hello")
);
