module Task : {
  type t('a);
  let make : (('a => unit) => unit) => t('a);
  let run : (('a => unit), t('a)) => unit;
  let map : (('a => 'b), t('a)) => t('b);

  let randomInt : (int, int) => t(int);
} = {
  type t('a) = ('a => unit) => unit;
  let make = f => f;
  let run = (receiver, task) =>
    task(receiver);
  let map = (f, task) =>
    resolve => task(a => resolve(f(a)));

  let randomInt = (l, h) =>
    f => f(Random.int(h) + l);
}

module Cmd : {
  type t('model);
  let make : ('model => 'model) => t('model);
  let fromTask : Task.t('model => 'model) => t('model);
  let run : (('model => unit), 'model, t('model)) => unit;
  let map : (('b => 'a), (('b, 'a) => 'b), t('a)) => t('b)
} = {
  type t('model) = Task.t('model => 'model);
  let make = updater => 
    Task.make(resolve => resolve(updater));
  let fromTask = task =>
    task;
  let run = (receiver, model, command) =>
    command |> Task.run(updater => model |> updater |> receiver);
  let map = (getter, setter, command) =>
    command |> Task.map(updater => b => b |> getter |> updater |> setter(b));
}

/* type action('arg, 'model) = 'arg => command('model); */
type dispatcher('model) = Cmd.t('model) => unit;
type element('model) = dispatcher('model) => htmlElement
and htmlElement = ReasonReact.reactElement;

let _log = value => value |> Obj.magic |. Js.Json.stringifyWithSpace(2) |> Js.log;

let run = (
    ~mount: htmlElement => unit,
    ~render: htmlElement => unit,
    ~init: 'arg => 'model,
    ~update: 'action => Cmd.t('model) = x => x,
    ~view: 'model => element('model),
    arg: 'arg
  ): unit => {

  let model = ref(init(arg));

  let rec dispatch = action => {
    action
      |> update
      |> Cmd.run(newModel => {
          _log(newModel);
          model := newModel;
          render(view(model^, dispatch));
        }, model^);
      };

  mount(view(model^, dispatch))
};

  let map
  : ('b => 'a, ('b, 'a) => 'b, element('a)) => element('b)
  = (getter, setter, element) =>
    dispatch =>
      element(command =>
        dispatch(
          command |> Cmd.map(getter, setter)
        )
      );


/* ---- */

let mountHtml = (~at: string) => {
  let render = component =>
    ReactDOMRe.renderToElementWithId(component, at);

  run(~mount=render, ~render)
};

module MakeHtml(T: { type model }) = {
  type event;

  type attr =
    | Raw(string, string)
    | Event(string, event => Cmd.t(T.model));

  module Attr = {
    let className = name => Raw("className", name);
    let autofocus = value => Raw("autoFocus", Obj.magic(value));
    let hidden = value => Raw("hidden", Obj.magic(value));
    let name = name => Raw("name", name);
    let onClick = command =>
      Event("onClick", _ => command);
    let onDoubleClick = command =>
      Event("onDoubleClick", _ => command);
    let onChange = command =>
      Event("onChange", _ => command);
    let onBlur = command =>
      Event("onBlur", _ => command);
    let onInput = callback =>
      Event("onInput", event => callback(Obj.magic(event)##target##value));
    let onKeyDown = callback =>
      Event("onKeyDown", event => callback(Obj.magic(event)##keyCode));
  };

  [@bs.set_index] external _setProp : (Js.t({..}), string, 'a) => unit = "";

  let _element = (elementName, ~id="", ~className="", ~attrs=[], children) =>
    (dispatch: _ => unit) => {
      let attrs = [Raw("id", id), Raw("className", className), ...attrs];
      let attrs: Js.t({..}) = attrs |> List.fold_left(attrs =>
                                        fun | Raw(key, value) => { _setProp(attrs, key, value); attrs }
                                            | Event(name, callback) =>  { _setProp(attrs, name, event => dispatch(callback(event))); attrs }, Js.Obj.empty());
      ReasonReact.createDomElement(
        elementName,
        ~props=attrs,
        children |> List.map(el => el(dispatch)) |> Array.of_list
      )
    };

  let null = _dispatch =>
    ReasonReact.null;

  let text = (text, _dispatch) =>
    ReasonReact.string(text);

  let button = _element("button");
  let footer = _element("footer");
  let div = _element("div");
  let header = _element("header");
  let h1 = _element("h1");
  let section = _element("section");
  let span = _element("span");
  let ul = _element("ul");
  let li = _element("li");
  let strong = _element("strong");
  let p = _element("p");
  
  let a = (~href="", ~attrs=[]) =>
      _element("a", ~attrs=[Raw("href", href)] @ attrs);

  let label = (~for_="", ~attrs=[]) =>
      _element("label", ~attrs=[Raw("htmlFor", for_)] @ attrs);

  let input = (~placeholder="", ~id=?, ~className=?, ~attrs=[], ~value) =>
    switch (value) {
    | `Text(value) =>
      _element("input", ~id=?id, ~className=?className, ~attrs=[Raw("placeholder", placeholder), Raw("value", value)] @ attrs, []);
    | `Checkbox(checked) =>
      _element("input", ~id=?id, ~className=?className, ~attrs=[Raw("type", "checkbox"), Raw("checked", Obj.magic(checked))] @ attrs, []);
    };
}

module Html = MakeHtml({
  type nonrec model = unit
});

module Core = Realm__Core