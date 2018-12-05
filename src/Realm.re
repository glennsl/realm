module type CoreConfig = {
  type element;
};

module type MessageConfig = {
  type msg;
};


module Core(Config: CoreConfig) = {
  type dispatcher('msg) = 'msg => unit;
  type element('msg) = dispatcher('msg) => Config.element;

  let run = (
      ~mount: Config.element => unit,
      ~render: Config.element => unit,
      ~init: unit => 'model,
      ~update: ('msg, 'model) => 'model,
      ~view: 'model => element('msg)
    ): unit => {

    let model = ref(init());

    let rec dispatch = msg => {
      model := update(msg, model^);
      render(view(model^, dispatch));
    };

    mount(view(model^, dispatch))
  };

  let map = (f, element) =>
    dispatch =>
      element(msg => dispatch(f(msg)));
};


module React: {
  type dispatcher('msg) = 'msg => unit;
  type element('msg);

  module Html(T: MessageConfig): {
    type prop;
    type htmlElement = list(prop) => list(element(T.msg)) => element(T.msg);

    let onClick: T.msg => prop;

    let null: element(T.msg);
    let text: string => element(T.msg);
    let div: htmlElement;
    let button: htmlElement;

    let fromReact: (dispatcher(T.msg) => ReasonReact.reactElement) => element(T.msg);
  };

  let mount: (
      ~at: string,
      ~init: unit => 'model,
      ~update: ('msg, 'model) => 'model,
      ~view: 'model => element('msg)
    ) => unit;

  let map: ('a => 'b, element('a)) => element('b);

} = {
  include Core({ type element = ReasonReact.reactElement });
  
  let mount = (~at: string) => {

    let render = component =>
      ReactDOMRe.renderToElementWithId(component, at);

    run(~mount=render, ~render)
  };

  module Html(T: MessageConfig) = {
    type prop =
      | Raw(string, string)
      | Event(string, T.msg);
    type htmlElement = list(prop) => list(element(T.msg)) => element(T.msg);

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


    let fromReact = f => f;
  };
};

module NoUpdate = {
  type command('model) =
    | SetState('model => 'model);
  type action('arg, 'model) = 'arg => command('model);
  type dispatcher('model) = command('model) => unit;
  type element('model) = dispatcher('model) => htmlElement
  and htmlElement = ReasonReact.reactElement;

  let run = (
      ~mount: htmlElement => unit,
      ~render: htmlElement => unit,
      ~init: unit => 'model,
      ~view: 'model => element('model)
    ): unit => {

    let model = ref(init());

    let rec dispatch = command => {
      switch (command) {
        | SetState(update) =>
          model := update(model^);
          render(view(model^, dispatch));
      }
    };

    mount(view(model^, dispatch))
  };

   let map
    : ('b => 'a, ('a, 'b) => 'b, element('a)) => element('b)
    = (getter, setter, element) =>
      dispatch =>
        element(command => {
          let mapped =
            switch (command) {
            | SetState(updater) =>
              SetState(b => {
                let a =
                  b |> getter |> updater;

                setter(a, b)
              }) 
            };
          dispatch(mapped)
        });


  /* ---- */

  let mountHtml = (~at: string) => {
    let render = component =>
      ReactDOMRe.renderToElementWithId(component, at);

    run(~mount=render, ~render)
  };

  module Html(T: { type model }) = {
    type prop =
      | Raw(string, string)
      | Event(string, command(T.model));

    let onClick = command =>
      Event("onClick", command);

    [@bs.set_index] external _addProp : (Js.t({..}), string, 'a) => unit = "";

    let _element = (elementName, props, children) =>
      (dispatch: _ => unit) => {
        let props: Js.t({..}) = props |> List.fold_left(props =>
                                          fun | Raw(key, value) => { _addProp(props, key, value); props }
                                              | Event(name, command) =>  { _addProp(props, name, _event => dispatch(command)); props }, Js.Obj.empty());
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
  }
}