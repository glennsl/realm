
module Helpers = {
  let element = (
      elementName,
      ~onClick: option(Dom.event => unit)=?,
      children
    ) =>
    ReasonReact.createDomElement(
      elementName,
      ~props={
        "onClick": onClick |> Js.Undefined.fromOption
      },
      children |> Array.of_list
    );

};

module Html = {
  let null =
    ReasonReact.null;

  let text =
    ReasonReact.string;

  let div =
    Helpers.element("div");

  let button =
    Helpers.element("button");
};

type dispatcher('msg) = 'msg => unit;

let mount = (
    ~at: string,
    ~init: unit => 'model,
    ~update: ('msg, 'model) => 'model,
    ~view: ('model, dispatcher('msg)) => ReasonReact.reactElement
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