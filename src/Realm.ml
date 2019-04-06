module Task
: sig
    type 'a t
    val make : (('a -> unit) -> unit) -> 'a t
    val run : ('a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val const : 'a -> 'a t

    val randomInt : int -> int -> int t
  end =
  struct
    type 'a t = ('a -> unit) -> unit
    let make f =
      f
    let run receiver task =
      task receiver
    let map f task resolve =
      task (fun a  -> resolve (f a))
    let const value =
      make (fun f  -> f value)

    let randomInt l h f =
      f ((Random.int h) + l)
  end 

module EffectImpl
: sig
    type 'model t

    val none : 'model t
    val const : 'model -> 'model t
    val update : ('model -> 'model) -> 'model t
    val do_ : ('model -> 'result Task.t) -> ('result -> 'model -> 'model) -> 'model t
    val andThen : 'model t -> 'model t -> 'model t
    val map : ('b -> 'a) -> ('b -> 'a -> 'b) -> 'a t -> 'b t

    val step : 'model -> 'model t -> ('model option* 'model t Task.t option)
  end
  = struct
    type 'model t =
      'model node list
    and 'model node =
      | Update of ('model -> 'model)
      | Task   of ('model -> ('model -> 'model) Task.t)

    let none =
      []

    let const value =
      [ Update (fun _ -> value) ]

    let update updater =
      [ Update updater ]

    let do_ action mapper =
      [ Task (fun model -> action model |> Task.map mapper) ]

    let rec andThen last =
      function
      | []               -> last
      | Update f :: rest -> Update f :: andThen last rest
      | Task f   :: rest -> Task   f :: andThen last rest

    let rec map get set =
      function
      | [] -> []

      | Update f :: rest ->
        Update (fun model -> model |> get |> f |> set model)
          :: map get set rest

      | Task f :: rest ->
        Task (fun model -> model |> get |> f |> Task.map (fun f model -> model |> get |> f |> set model))
          :: map get set rest


    let step model =
      function
      | []               -> ( None, None )
      | Update f :: []   -> ( Some (f model), None )
      | Update f :: rest -> ( Some (f model), Some (Task.const rest) )
      | Task f   :: rest ->
        let next = f model |> Task.map (fun f' -> Update f' :: rest) in
        ( None, Some next )
  end

module Effect
: sig
    type 'model t

    val none : 'model t
    val const : 'model -> 'model t
    val update : ('model -> 'model) -> 'model t
    val do_ : ('model -> 'result Task.t) -> ('result -> 'model -> 'model) -> 'model t
    val andThen : 'model t -> 'model t -> 'model t
    val map : ('b -> 'a) -> ('b -> 'a -> 'b) -> 'a t -> 'b t

    val step : 'model -> 'model t -> ('model option* 'model t Task.t option)
  end with type 'model t =  'model EffectImpl.t
= EffectImpl

module EventSource = struct type t end

module Sub
: sig
    type 'action t
    type unsubber = unit -> unit
    type 'a callback = 'a -> unit
    val make :
      string -> ('a -> 'action) -> ('a callback -> unsubber) -> 'action t
    val run : ('action -> unit) -> 'action t -> unsubber
    val unsub : unsubber -> unit
    val id : _ t -> string
  end
= struct
    type unsubber = unit -> unit
    type 'a callback = 'a -> unit
    type 'a dispatcher = 'a -> unit
    type 'action t = {
      id: string;
      spawner: 'action dispatcher -> unsubber;}
    let make id action spawner =
      {
        id;
        spawner =
          (fun dispatch  ->
             spawner (fun value  -> (value |> action) |> dispatch))
      }
    let run dispatch sub = sub.spawner dispatch
    let unsub unsubber = unsubber ()
    let id sub = sub.id
  end 

module Time
: sig
    val every : string -> float -> (unit -> 'action) -> 'action Sub.t
  end
= struct
    let every id ms action =
      Sub.make id action
        begin fun callback ->
          let intervalId = Js.Global.setIntervalFloat callback ms in
          fun () -> Js.Global.clearInterval intervalId
        end
  end 

type 'model dispatcher = 'model Effect.t -> unit
type 'model element = 'model dispatcher -> htmlElement
and htmlElement = ReasonReact.reactElement

let _log value = Js.log2 "model updated" value

module SubMap = Belt.Map.String

let run
  ~mount:(mount : htmlElement -> unit)
  ~render:(render : htmlElement -> unit)  ~init:(init : 'arg -> 'model) 
  ?update:((update : 'action -> 'model Effect.t)= fun x  -> x) 
  ?subs:((subs : 'model -> 'action Sub.t list)= fun _  -> []) 
  ~view:(view : 'model -> 'model element) 
  (arg : 'arg)
=
  let activeSubs = ref SubMap.empty in
  let model = ref (init arg) in

  let rec updateSubs () =
    let newSubs =
      subs !model |>
        List.fold_left
          (fun subs sub -> SubMap.set subs (Sub.id sub) sub)
          SubMap.empty
      in
    let spawns = SubMap.keep newSubs (fun key _ -> not (SubMap.has !activeSubs key)) in
    let existing = SubMap.keep (!activeSubs) (fun key _ -> SubMap.has newSubs key) in
    let kills = SubMap.keep (!activeSubs) (fun key _ -> not (SubMap.has newSubs key)) in

    SubMap.forEach kills (fun _ -> Sub.unsub);

    activeSubs :=
      SubMap.reduce spawns existing
        (fun subs id sub -> SubMap.set subs id (Sub.run dispatch sub));

    Js.log2 "updateSubs" !activeSubs

  and dispatch action =
    let rec runEffect effect =
      let maybeModel, nextEffect = EffectImpl.step !model effect in
      match maybeModel with
      | Some newModel ->
        _log newModel;
        model := newModel;
        updateSubs ();
        render (view !model dispatch)
      | None -> ();
      match nextEffect with
      | Some task -> Task.run runEffect task
      | None      -> ()
      in

    action |> update |> runEffect
    in

  updateSubs ();
  mount (view !model dispatch)

let map getter setter element =
    fun dispatch ->
      element (fun effect -> dispatch (effect |> Effect.map getter setter))

let mountHtml ~at:(at : string)  =
  let render component = ReactDOMRe.renderToElementWithId component at in
  run ~mount:render ~render

module MakeHtml(T:sig type model end) =
  struct
    type event
    type attr =
      | Raw of string* string
      | Event of string* (event -> T.model Effect.t)

    module Attr =
      struct
        let className name = Raw ("className", name)
        let autofocus value = Raw ("autoFocus", Obj.magic value)
        let hidden value = Raw ("hidden", Obj.magic value)
        let name name = Raw ("name", name)
        let onClick command = Event ("onClick", fun _ -> command)
        let onDoubleClick command = Event ("onDoubleClick", fun _ -> command)
        let onChange command = Event ("onChange", fun _ -> command)
        let onBlur command = Event ("onBlur", fun _ -> command)
        let onInput callback = Event ("onInput", fun event -> callback (Obj.magic event)##target##value)
        let onKeyDown callback = Event ("onKeyDown", fun event -> callback (Obj.magic event)##keyCode)
      end

    external _setProp : < .. > Js.t -> string -> 'a -> unit = "" [@@bs.set_index]

    let _element elementName ?(id="") ?(className="") ?(attrs=[]) children =
      fun (dispatch : _ -> unit) ->
        let attrs = Raw ("id", id) :: Raw ("className", className) :: attrs in
        let attrs
          : < .. > Js.t
          = attrs |>
            List.fold_left
              begin fun attrs ->
                  function
                  | Raw (key, value) ->
                    _setProp attrs key value;
                    attrs
                  | Event (name, callback) ->
                    begin
                      _setProp attrs name (fun event -> dispatch (callback event));
                      attrs
                    end
              end
              (Js.Obj.empty ())
          in
        ReasonReact.createDomElement
          elementName
          ~props:attrs 
          (children |> List.map (fun el -> el dispatch) |> Array.of_list)

    let null _dispatch = ReasonReact.null
    let text text _dispatch = ReasonReact.string text
    let button = _element "button"
    let footer = _element "footer"
    let div = _element "div"
    let header = _element "header"
    let h1 = _element "h1"
    let section = _element "section"
    let span = _element "span"
    let ul = _element "ul"
    let li = _element "li"
    let strong = _element "strong"
    let p = _element "p"
    let a ?(href= "")  ?(attrs= [])  =
      _element "a" ~attrs:([Raw ("href", href)] @ attrs)
    let label ?(for_= "")  ?(attrs= [])  =
      _element "label" ~attrs:([Raw ("htmlFor", for_)] @ attrs)
    let input ?(placeholder= "")  ?id  ?className  ?(attrs= [])  ~value  =
      match value with
      | `Text value ->
        _element "input" ?id ?className ~attrs:([Raw ("placeholder", placeholder); Raw ("value", value)] @ attrs) []
      | `Checkbox checked ->
        _element "input" ?id ?className ~attrs:([Raw ("type", "checkbox"); Raw ("checked", Obj.magic checked)] @ attrs) []
  end

module Html = MakeHtml(struct type nonrec model = unit end)

module Core = Realm__Core