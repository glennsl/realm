module Task : {
  type t('a);
  let make : (('a => unit) => unit) => t('a);
  let run : (('a => unit), t('a)) => unit;
  let map : (('a => 'b), t('a)) => t('b);
  let const : 'a => t('a);

  let randomInt : (int, int) => t(int);
} = {
  type t('a) = ('a => unit) => unit;
  let make = f => f;
  let run = (receiver, task) =>
    task(receiver);
  let map = (f, task) =>
    resolve => task(a => resolve(f(a)));
  let const = value =>
    make (f => f(value));

  let randomInt = (l, h) =>
    f => f(Random.int(h) + l);
}

module EffectImpl : {
  type t('model)
  let none : t('model)
  let const : 'model => t('model)
  let update : ('model => 'model) => t('model)
  let do_ : ('model => Task.t('result), ('result, 'model) => 'model) => t('model)
  let andThen : (t('model), t('model)) => t('model)
  let map : ('b => 'a, ('b, 'a) => 'b, t('a)) => t('b)

  let step : ('model, t('model)) => (option('model), option(Task.t(t('model))))
} = {
  type t('model) =
  | Update('model => 'model, t('model))
  | Task('model => Task.t('model => 'model), t('model))
  | End

  let none =
    End
  let const = value =>
    Update(_ => value, End)
  let update = updater => 
    Update(updater, End)
  let do_ = (action, mapper) =>
    Task(model => action(model) |> Task.map(mapper), End)
  let rec andThen = last => 
    fun | End => last
        | Update(f, next) =>
          Update(f, andThen(last, next))
        | Task(f, next) =>
          Task(f, andThen(last, next))
  let rec map = (getter, setter) =>
    fun | End => End
        | Update(f, next) =>
          Update(model => model |> getter |> f |> setter(model), map(getter, setter, next))
        | Task(f, next) =>
          Task(model => model |> getter |> f |> Task.map(f => model => model |> getter |> f |> setter(model)), map(getter, setter, next))

  let step = model =>
    fun | End =>
          (None, None)
        | Update(f, next) =>
          (Some(f(model)),
           next == End ? None : Some(Task.const(next)))
        | Task(f, next) =>
          (None,
           Some(f(model) |> Task.map(updater => Update(updater, next))))

  // let mapModel = (mapper, command) =>
  //   model => command(model) |> Task.map(updater => value => updater |> mapper);
  // let mapResult = 
  //   model => command(model) |> Task.map(updater => b => b |> getter |> updater |> setter(b));
  // let map : (('b => 'a), (('b, 'a) => 'b), t('model, 'a)) => t('model, 'b)
  //         = (getter, setter, command) =>
  //   model => command(model) |> Task.map(updater => b => b |> getter |> updater |> setter(b));
  // let effect; let do_
  // let update
  // let andThen
  // let then_
}

module Effect : ({
  type t('model)
  let none : t('model)
  let const : 'model => t('model)
  let update : ('model => 'model) => t('model)
  let do_ : ('model => Task.t('result), ('result, 'model) => 'model) => t('model)
  let andThen : (t('model), t('model)) => t('model)
  let map : ('b => 'a, ('b, 'a) => 'b, t('a)) => t('b)

  let step : ('model, t('model)) => (option('model), option(Task.t(t('model))))
} with type t('model) = EffectImpl.t('model)) = EffectImpl;

module EventSource = {
  type t;
}

module Sub : {
  type t('action);
  type unsubber = unit => unit;
  type callback('a) = 'a => unit;
  let make : (string, 'a => 'action, callback('a) => unsubber) => t('action);
  let run : ('action => unit, t('action)) => unsubber;
  let unsub : unsubber => unit;
  let id : t(_) => string;
} = {
  type unsubber = unit => unit;
  type callback('a) = 'a => unit;
  type dispatcher('a) = 'a => unit
  type t('action) = { id: string, spawner: dispatcher('action) => unsubber };
  let make = (id, action, spawner) =>
    { id, spawner: dispatch => spawner(value => value |> action |> dispatch) }
  let run = (dispatch, sub) =>
    sub.spawner(dispatch);
  let unsub = unsubber =>
    unsubber();
  let id = sub =>
    sub.id
}

module Time : {
  let every : (string, float, unit => 'action) => Sub.t('action)
} = {
  let every = (id, ms, action) =>
    Sub.make(id, action,
      callback => {
        let intervalId = Js.Global.setIntervalFloat(callback, ms);
        () => Js.Global.clearInterval(intervalId)
      });
}

/* type action('arg, 'model) = 'arg => command('model); */
type dispatcher('model) = Effect.t('model) => unit;
type element('model) = dispatcher('model) => htmlElement
and htmlElement = ReasonReact.reactElement;

// let _log = value => value |> Obj.magic |. Js.Json.stringifyWithSpace(2) |> Js.log;
let _log = value => Js.log2("model updated", value);

module SubMap = Belt.Map.String;

let run = (
    ~mount: htmlElement => unit,
    ~render: htmlElement => unit,
    ~init: 'arg => 'model,
    ~update: 'action => Effect.t('model) = x => x,
    ~subs: 'model => list(Sub.t('action)) = _ => [],
    ~view: 'model => element('model),
    arg: 'arg
  ): unit => {

  let activeSubs = ref(SubMap.empty);
  let model = ref(init(arg));

  let rec updateSubs = () => {
    let newSubs = subs(model^) |> List.fold_left((subs, sub) => SubMap.set(subs, Sub.id(sub), sub), SubMap.empty);
    let spawns = SubMap.keep(newSubs, (key, _) => !SubMap.has(activeSubs^, key));
    let existing = SubMap.keep(activeSubs^, (key, _) => SubMap.has(newSubs, key));
    let kills = SubMap.keep(activeSubs^, (key, _) => !SubMap.has(newSubs, key));
    SubMap.forEach(kills, _ => Sub.unsub);
    activeSubs := SubMap.reduce(spawns, existing, (subs, id, sub) => SubMap.set(subs, id, Sub.run(dispatch, sub)));
    Js.log2("updateSubs", activeSubs^);
  }

  and dispatch = action => {
    let rec step = newModel => {
      _log(newModel);
      model := newModel;
      updateSubs();
      render(view(model^, dispatch));
    }
    and runEffect = effect => {
      let (maybeModel, nextEffect) = EffectImpl.step(model^, effect);
      switch maybeModel {
      | Some(newModel) => step(newModel)
      | None => ();
      };
      switch nextEffect {
      | Some(task) => Task.run(runEffect, task)
      | None => ()
      }
    }
    action |> update |> runEffect
  };

  updateSubs();
  mount(view(model^, dispatch))
};

  let map
  : ('b => 'a, ('b, 'a) => 'b, element('a)) => element('b)
  = (getter, setter, element) =>
    dispatch =>
      element(effect =>
        dispatch(
          effect |> Effect.map(getter, setter)
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
    | Event(string, event => Effect.t(T.model));

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