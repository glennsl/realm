
type dispatcher('msg)
  = 'msg => unit;
type element('msg)
  = dispatcher('msg) => ReasonReact.reactElement;
type htmlElement('msg)
  = (~onClick: 'msg=?, list(element('msg))) => element('msg);

module Helpers = {
  let element = (
      elementName: string,
      ~onClick: option('msg)=?,
      children: list(element('msg))
    ): element('msg) => dispatch =>
    ReasonReact.createDomElement(
      elementName,
      ~props={
        "onClick": Belt.Option.map(onClick, msg => _event => dispatch(msg)) |> Js.Undefined.fromOption
      },
      children |> List.map(e => e(dispatch))
               |> Array.of_list
    );

};


module Html = {
  let null = _dispatch =>
    ReasonReact.null;

  let text = (str, _dispatch) =>
    ReasonReact.string(str);

  let div: htmlElement('msg) =
    Helpers.element("div");

  let button =
    Helpers.element("button");
};


let mount = (
    ~at: string,
    ~init: unit => 'model,
    ~update: ('msg, 'model) => 'model,
    ~view: 'model => element('msg)
  ): unit => {

  let model = ref(init());

  let render = component =>
    ReactDOMRe.renderToElementWithId(component, at);

  let rec dispatch = msg => {
    model := update(msg, model^);
    render(view(model^, dispatch));
  };

  render(view(model^, dispatch))
}