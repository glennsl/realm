
module Core = struct
  include Realm__Core

  module Future = struct
    type 'a t = ('a -> unit) -> unit

    let make f =
      f
    let const value =
      make (fun f -> f value)
    let andThen f task =
      fun resolve ->
        task (fun a -> f a resolve)
    let map f task =
      fun resolve ->
        task (fun a -> resolve (f a))
    let map2 f taskA taskB =
      taskA
      |> andThen (fun a -> taskB
      |> andThen (fun b -> const (f a b)))

    let run receiver task =
      task receiver

    let randomInt l h f =
      f ((Random.int h) + l)
  end 


  module Effect = struct
    type 'model t =
      'model node list
    and 'model node =
      | Update of ('model -> 'model)
      | Task   of ('model -> ('model -> 'model) Future.t)

    let none =
      []

    let const value =
      [ Update (fun _ -> value) ]

    let update updater =
      [ Update updater ]

    let do_ action mapper =
      [ Task (fun model -> action model |> Future.map mapper) ]

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
        Task (fun model -> model |> get |> f |> Future.map (fun f model -> model |> get |> f |> set model))
          :: map get set rest


    let step model =
      function
      | []               -> ( None, None )
      | Update f :: []   -> ( Some (f model), None )
      | Update f :: rest -> ( Some (f model), Some (Future.const rest) )
      | Task f   :: rest ->
        let next = f model |> Future.map (fun f' -> Update f' :: rest) in
        ( None, Some next )
  end


  module Sub = struct
    type unsubber = unit -> unit
    type 'a callback = 'a -> unit
    type 'a dispatcher = 'a -> unit
    type 'action t =
      { id: string
      ; spawner: 'action dispatcher -> unsubber
    }

    let make id action spawner =
      { id
      ; spawner = fun dispatch -> spawner (fun value -> value |> action |> dispatch)
      }

    let run dispatch sub =
      sub.spawner dispatch
    let unsub unsubber =
      unsubber ()
    let id sub =
      sub.id
  end 


  module Time = struct
    let every id ms action =
      Sub.make id action
        begin fun callback ->
          let intervalId = Js.Global.setIntervalFloat callback ms in
          fun () -> Js.Global.clearInterval intervalId
        end
  end 
end

open! Core


module type RuntimeConfig = sig
  type element
end

module Runtime(Config : RuntimeConfig) = struct
  type 'model dispatcher = 'model Effect.t -> unit
  type 'model element = 'model dispatcher -> Config.element

  module SubMap = Belt.Map.String

  let run
    (* :
    'action.
    mount : (htmlElement -> unit) ->
    render : (htmlElement -> unit) ->
    init : ('arg -> 'model Task.t) ->
    update : ('action -> 'model Effect.t) ->
    ?subs : ('model -> 'action Sub.t list) ->
    view : ('model -> 'model element) ->
    'arg ->
    unit *)
    = fun ~mount ~render ~init ?(update = fun x -> x) ?(subs = fun _ -> []) ~view arg ->
  (* let run
    ~mount:(mount : htmlElement -> unit)
    ~render:(render : htmlElement -> unit)
    ~init:(init : 'arg -> 'model Task.t)
    ~update:(update : 'action -> 'model Effect.t) 
    ?subs:(subs : 'model -> 'action Sub.t list = fun _ -> []) 
    ~view:(view : 'model -> 'model element) 
    (arg : 'arg)
  = *)
    let run' initialModel =
      let activeSubs = ref SubMap.empty in
      let model = ref initialModel in

      let rec updateSubs () =
        let newSubs =
          subs !model |>
            List.foldl
              (fun subs sub -> SubMap.set subs (Sub.id sub) sub)
              SubMap.empty
          in
        let spawns = SubMap.keep newSubs (fun key _ -> not (SubMap.has !activeSubs key)) in
        let existing = SubMap.keep !activeSubs (fun key _ -> SubMap.has newSubs key) in
        let kills = SubMap.keep !activeSubs (fun key _ -> not (SubMap.has newSubs key)) in

        SubMap.forEach kills (fun _ -> Sub.unsub);

        activeSubs :=
          SubMap.reduce spawns existing
            (fun subs id sub -> SubMap.set subs id (Sub.run dispatch sub));

        Js.log2 "updateSubs" !activeSubs

      and dispatch action =
        let rec runEffect effect =
          let maybeModel, nextEffect = Effect.step !model effect in
          match maybeModel with
          | Some newModel ->
            Js.log2 "model updated" newModel;
            model := newModel;
            updateSubs ();
            render (view !model dispatch)
          | None -> ();
          match nextEffect with
          | Some task -> Future.run runEffect task
          | None      -> ()
          in

        action |> update |> runEffect
        in

      updateSubs ();
      mount (view !model dispatch)
      in
    Future.run run' (init arg)
end



module React = struct

  module Runtime = Runtime(struct type element = ReasonReact.reactElement end)

  module Html = struct
    type 'model t = 'model Runtime.element

    module Attr = struct
      type event
      type 'model t =
        | Raw of string * string
        | Event of string * (event -> 'model Effect.t)

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

      external setProp : < .. > Js.t -> string -> 'a -> unit = "" [@@bs.set_index]
      let toProps dispatch attrs =
        let addProp obj attr = 
          begin match attr with
            | Raw (key, value) ->
              setProp obj key value
            | Event (name, callback) ->
              setProp obj name (fun event -> dispatch (callback event))
          end;
          obj
          in
        List.foldl addProp (Js.Obj.empty ()) attrs
    end

    let _element tag ?(id="") ?(className="") ?(attrs=[]) children =
      let attrs = Attr.Raw ("id", id) :: Attr.Raw ("className", className) :: attrs in
      fun dispatch ->
        ReasonReact.createDomElement
          tag
          ~props:(Attr.toProps dispatch attrs)
          (children |> List.map (fun el -> el dispatch) |> Array.of_list)

    let null _dispatch = ReasonReact.null
    let text text _dispatch = ReasonReact.string text
    let button ?id = _element "button" ?id
    let div ?id = _element "div" ?id
    let footer ?id = _element "footer" ?id
    let header ?id = _element "header" ?id
    let h1 ?id = _element "h1" ?id
    let section ?id = _element "section" ?id
    let span ?id = _element "span" ?id
    let ul ?id = _element "ul" ?id
    let li ?id = _element "li" ?id
    let strong ?id = _element "strong" ?id
    let p ?id = _element "p" ?id
    let a ?(href= "")  ?(attrs= [])  =
      _element "a" ~attrs:([Attr.Raw ("href", href)] @ attrs)
    let label ?(for_= "")  ?(attrs= [])  =
      _element "label" ~attrs:([Attr.Raw ("htmlFor", for_)] @ attrs)
    let input ?(placeholder= "") ~value ?id ?className ?(attrs= []) children =
      match value with
      | `Text value ->
        _element "input" ?id ?className ~attrs:([Attr.Raw ("placeholder", placeholder); Raw ("value", value)] @ attrs) children
      | `Checkbox checked ->
        _element "input" ?id ?className ~attrs:([Attr.Raw ("type", "checkbox"); Raw ("checked", Obj.magic checked)] @ attrs) children

    let map getter setter element =
        fun dispatch ->
          element (fun effect -> dispatch (effect |> Effect.map getter setter))

    let raw f =
      f
  end


  module type AppSpec = sig
    type model

    val init : unit -> model Future.t
    val update : model Effect.t -> model Effect.t
    val subs : model -> model Effect.t Sub.t list
    val view : model -> model Html.t
  end

  module type SimpleAppSpec = sig
    type model
    (* type action *)

    val init : unit -> model
    val view : model -> model Html.t
  end

  module type App = sig
    val mount : at:string -> unit
  end


  module App(Spec : AppSpec) = struct
    let mount ~at =
      let render component = ReactDOMRe.renderToElementWithId component at in
      let open Spec in
      Runtime.run ~mount:render ~render ~init ~update ~subs ~view ()
  end

  module SimpleApp(Spec : SimpleAppSpec) = struct
    include App(struct
      include Spec
      let init () = Future.const (Spec.init ())
      let update x = x
      let subs _ = []
    end)
  end
end