module Task : sig
  type 'a t

  val make : (('a -> unit) -> unit) -> 'a t
  val const : 'a -> 'a t
  val andThen : ('a -> 'b t) -> 'a t -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  val run : ('a -> unit) -> 'a t -> unit

  val randomInt : int -> int -> int t
end


module Effect : sig
  type 'model t

  val none : 'model t
  val const : 'model -> 'model t
  val update : ('model -> 'model) -> 'model t
  val do_ : ('model -> 'result Task.t) -> ('result -> 'model -> 'model) -> 'model t
  val andThen : 'model t -> 'model t -> 'model t
  val map : ('b -> 'a) -> ('b -> 'a -> 'b) -> 'a t -> 'b t

  (* val step : 'model -> 'model t -> ('model option* 'model t Task.t option) *)
end


module Sub : sig
  type 'action t
  type unsubber = unit -> unit
  type 'a callback = 'a -> unit

  val make :
    string -> ('a -> 'action) -> ('a callback -> unsubber) -> 'action t

  (* val id : _ t -> string
  val run : ('action -> unit) -> 'action t -> unsubber
  val unsub : unsubber -> unit *)
end


module Time : sig
  val every : string -> float -> (unit -> 'action) -> 'action Sub.t
end

type 'model element

val map : ('b -> 'a) -> ('b -> 'a -> 'b) -> 'a element -> 'b element

val mountHtml
  (* at: string ->
  init : ('arg -> 'model Task.t) ->
  update : ('action -> 'model Effect.t) ->
  ?subs : ('model -> 'action Sub.t list) ->
  view : ('model -> 'model element) ->
  'arg ->
  unit *)
  :  at: string
  -> init : ('arg -> 'model Task.t)
  -> ?update : ('model Effect.t -> 'model Effect.t)
  -> ?subs : ('model -> 'model Effect.t Sub.t list)
  -> view : ('model -> 'model element) 
  -> 'arg 
  -> unit

module MakeHtml : 
  functor (T : sig type model end) -> sig
    type event
    type attr
    module Attr :
      sig
        val className : string -> attr
        val autofocus : 'a -> attr
        val hidden : 'a -> attr
        val name : string -> attr
        val onClick : T.model Effect.t -> attr
        val onDoubleClick : T.model Effect.t -> attr
        val onChange : T.model Effect.t -> attr
        val onBlur : T.model Effect.t -> attr
        val onInput : ('a -> T.model Effect.t) -> attr
        val onKeyDown : ('a -> T.model Effect.t) -> attr
      end
    val null : T.model element
    val text : string -> T.model element
    val button
      :  ?id:string
      -> ?className:string
      -> ?attrs:attr list
      -> T.model element list
      -> T.model element
    val footer
      :  ?id:string
      -> ?className:string
      -> ?attrs:attr list
      -> T.model element list
      -> T.model element
    val div
      :  ?id:string
      -> ?className:string
      -> ?attrs:attr list
      -> T.model element list
      -> T.model element
    val header
      :  ?id:string
      -> ?className:string
      -> ?attrs:attr list
      -> T.model element list
      -> T.model element
    val h1
      :  ?id:string
      -> ?className:string
      -> ?attrs:attr list
      -> T.model element list
      -> T.model element
    val section
      :  ?id:string
      -> ?className:string
      -> ?attrs:attr list
      -> T.model element list
      -> T.model element
    val span
      :  ?id:string
      -> ?className:string
      -> ?attrs:attr list
      -> T.model element list
      -> T.model element
    val ul
      :  ?id:string
      -> ?className:string
      -> ?attrs:attr list
      -> T.model element list
      -> T.model element
    val li
      :  ?id:string
      -> ?className:string
      -> ?attrs:attr list
      -> T.model element list
      -> T.model element
    val strong
      :  ?id:string
      -> ?className:string
      -> ?attrs:attr list
      -> T.model element list
      -> T.model element
    val p
      :  ?id:string
      -> ?className:string
      -> ?attrs:attr list
      -> T.model element list
      -> T.model element
    val a
      :  ?href:string
      -> ?attrs:attr list
      -> ?id:string
      -> ?className:string
      -> T.model element list
      -> T.model element
    val label
      :  ?for_:string
      -> ?attrs:attr list
      -> ?id:string
      -> ?className:string
      -> T.model element list
      -> T.model element
    val input
      :  ?placeholder:string
      -> value:[< `Checkbox of 'a | `Text of string ]
      -> ?id:string
      -> ?className:string
      -> ?attrs:attr list
      -> T.model element list
      -> T.model element

    val reactComponent : ((T.model Effect.t -> unit) -> ReasonReact.reactElement) -> T.model element
  end

module Html : sig
  type event
  type attr

  module Attr : sig
    val className : string -> attr
    val autofocus : 'a -> attr
    val hidden : 'a -> attr
    val name : string -> attr
    val onClick : unit Effect.t -> attr
    val onDoubleClick : unit Effect.t -> attr
    val onChange : unit Effect.t -> attr
    val onBlur : unit Effect.t -> attr
    val onInput : ('a -> unit Effect.t) -> attr
    val onKeyDown : ('a -> unit Effect.t) -> attr
  end

  val null : unit element
  val text : string -> unit element
  val button
  :  ?id:string
  -> ?className:string
  -> ?attrs:attr list
  -> unit element list
  -> unit element
val footer
  :  ?id:string
  -> ?className:string
  -> ?attrs:attr list
  -> unit element list
  -> unit element
val div
  :  ?id:string
  -> ?className:string
  -> ?attrs:attr list
  -> unit element list
  -> unit element
val header
  :  ?id:string
  -> ?className:string
  -> ?attrs:attr list
  -> unit element list
  -> unit element
val h1
  :  ?id:string
  -> ?className:string
  -> ?attrs:attr list
  -> unit element list
  -> unit element
val section
  :  ?id:string
  -> ?className:string
  -> ?attrs:attr list
  -> unit element list
  -> unit element
val span
  :  ?id:string
  -> ?className:string
  -> ?attrs:attr list
  -> unit element list
  -> unit element
val ul
  :  ?id:string
  -> ?className:string
  -> ?attrs:attr list
  -> unit element list
  -> unit element
val li
  :  ?id:string
  -> ?className:string
  -> ?attrs:attr list
  -> unit element list
  -> unit element
val strong
  :  ?id:string
  -> ?className:string
  -> ?attrs:attr list
  -> unit element list
  -> unit element
val p
  :  ?id:string
  -> ?className:string
  -> ?attrs:attr list
  -> unit element list
  -> unit element
val a
  :  ?href:string
  -> ?attrs:attr list
  -> ?id:string
  -> ?className:string
  -> unit element list
  -> unit element
val label
  :  ?for_:string
  -> ?attrs:attr list
  -> ?id:string
  -> ?className:string
  -> unit element list
  -> unit element
val input
  :  ?placeholder:string
  -> value:[< `Checkbox of 'a | `Text of string ]
  -> ?id:string
  -> ?className:string
  -> ?attrs:attr list
  -> unit element list
  -> unit element
end

module Core : module type of Realm__Core