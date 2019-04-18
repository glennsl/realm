module Core : sig
  include module type of Realm__Core

  module Future : sig
    type 'a t

    val make : (('a -> unit) -> unit) -> 'a t
    val const : 'a -> 'a t
    val andThen : ('a -> 'b t) -> 'a t -> 'b t
    val map : ('a -> 'b) -> 'a t -> 'b t
    val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

    val all2 : 'a t -> 'b t -> ('a * 'b) t

    val run : ('a -> unit) -> 'a t -> unit

    val randomInt : int -> int -> int t
  end


  module Effect : sig
    type 'model t

    val none : 'model t
    val const : 'model -> 'model t
    val update : ('model -> 'model) -> 'model t
    val do_ : ('model -> 'result Future.t) -> ('result -> 'model -> 'model) -> 'model t
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
    type t

    val now : t Future.t
    val delay : float -> unit Future.t
    val every : string -> float -> (unit -> 'action) -> 'action Sub.t

    val toString : t -> string
  end

end

open Core

module React : sig

  module Html : sig
    type 'model t

    module Attr : sig
      type 'model t

      val className : string -> _ t
      val autofocus : 'a -> _ t
      val hidden : 'a -> _ t
      val name : string -> _ t
      val onClick : 'action -> 'action t
      val onDoubleClick : 'action -> 'action t
      val onChange : 'action -> 'action t
      val onBlur : 'action -> 'action t
      val onInput : ('a -> 'action) -> 'action t
      val onKeyDown : ('a -> 'action) -> 'action t
    end

    val null : _ t
    val text : string -> _ t
    val button
      :  ?id: string
      -> ?className: string
      -> ?attrs: 'model Attr.t list
      -> 'model t list
      -> 'model t
    val div
      :  ?id: string
      -> ?className: string
      -> ?attrs: 'model Attr.t list
      -> 'model t list
      -> 'model t
    val footer
      :  ?id: string
      -> ?className: string
      -> ?attrs: 'model Attr.t list
      -> 'model t list
      -> 'model t
    val header
      :  ?id: string
      -> ?className: string
      -> ?attrs: 'model Attr.t list
      -> 'model t list
      -> 'model t
    val h1
      :  ?id: string
      -> ?className: string
      -> ?attrs: 'model Attr.t list
      -> 'model t list
      -> 'model t
    val section
      :  ?id: string
      -> ?className: string
      -> ?attrs: 'model Attr.t list
      -> 'model t list
      -> 'model t
    val span
      :  ?id: string
      -> ?className: string
      -> ?attrs: 'model Attr.t list
      -> 'model t list
      -> 'model t
    val ul
      :  ?id: string
      -> ?className: string
      -> ?attrs: 'model Attr.t list
      -> 'model t list
      -> 'model t
    val li
      :  ?id: string
      -> ?className: string
      -> ?attrs: 'model Attr.t list
      -> 'model t list
      -> 'model t
    val strong
      :  ?id: string
      -> ?className: string
      -> ?attrs: 'model Attr.t list
      -> 'model t list
      -> 'model t
    val p
      :  ?id: string
      -> ?className: string
      -> ?attrs: 'model Attr.t list
      -> 'model t list
      -> 'model t
    val a
      :  ?href:string
      -> ?attrs: 'model Attr.t list
      -> ?id: string
      -> ?className: string
      -> 'model t list
      -> 'model t
    val label
      :  ?for_:string
      -> ?attrs: 'model Attr.t list
      -> ?id: string
      -> ?className: string
      -> 'model t list
      -> 'model t
    val input
      :  ?placeholder:string
      -> value:[< `Checkbox of 'a | `Text of string ]
      -> ?id: string
      -> ?className: string
      -> ?attrs: 'model Attr.t list
      -> 'model t list
      -> 'model t

    val map : ('a -> 'b) -> 'a t -> 'b t

    val raw : (('action -> unit) -> ReasonReact.reactElement) -> 'action t
  end

  module type AppSpec = sig
    type model
    type action

    val init : unit -> model Future.t
    val update : action -> model Effect.t
    val subs : model -> action Sub.t list
    val view : model -> action Html.t
  end

  module type SimpleAppSpec = sig
    type model

    val init : unit -> model
    val view : model -> model Effect.t Html.t
  end

  module type App = sig
    val mount : at:string -> unit
  end

  module App : functor(Spec : AppSpec) -> App
  module SimpleApp : functor(Spec : SimpleAppSpec) -> App
end