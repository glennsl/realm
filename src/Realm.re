module type Conf = {
  type msg;
};

module App: {
  type prop('msg);

  type dispatcher('msg) = 'msg => unit;
  type element('msg);
  type htmlElement('msg) = list(prop('msg)) => list(element('msg)) => element('msg);

  module Html(T: Conf): {
    let onClick: T.msg => prop(T.msg);

    let null: element(T.msg);
    let text: string => element(T.msg);
    let div: htmlElement(T.msg);
    let button: htmlElement(T.msg);
  };

  let mount: (
      ~at: string,
      ~init: unit => 'model,
      ~update: ('msg, 'model) => 'model,
      ~view: 'model => element('msg)
    ) => unit;

} = {

  type prop('msg) =
    | Raw(string, string)
    | Event(string, 'msg);

  type dispatcher('msg) = 'msg => unit;
  type element('msg) = dispatcher('msg) => ReasonReact.reactElement;
  type htmlElement('msg) = list(prop('msg)) => list(element('msg)) => element('msg);

  module Html(T: Conf) = {

    let onClick = msg =>
      Event("onClick", msg);

    [@bs.set_index] external _addProp : (Js.t({..}), string, 'a) => unit = "";

    let _element = (elementName, props, children) =>
      (dispatch: _ => unit) => {
        let props: Js.t({..}) = props |> List.fold_left(props =>
                                          fun | Raw(key, value) => { _addProp(props, key, value); props }
                                              | Event(name, msg) =>  { _addProp(props, name, _event => dispatch(msg)); props }, Js.Obj.empty());
        ReasonReact.createDomElement(
          elementName,
          ~props,
          children |> List.map(el => el(dispatch)) |> Array.of_list
        )
      };

    let null = _dispatch =>
      ReasonReact.null;

    let text = (text, _dispatch) =>
      ReasonReact.string(text);

    let div =
      _element("div");

    let button =
      _element("button");
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
}