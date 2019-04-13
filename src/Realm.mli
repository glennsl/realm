module Core : sig
  include module type of Realm__Core

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
      val onClick : 'model Effect.t -> 'model t
      val onDoubleClick : 'model  Effect.t -> 'model t
      val onChange : 'model Effect.t -> 'model t
      val onBlur : 'model Effect.t -> 'model t
      val onInput : ('a -> 'model Effect.t) -> 'model t
      val onKeyDown : ('a -> 'model Effect.t) -> 'model t
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

    val map : ('b -> 'a) -> ('b -> 'a -> 'b) -> 'a t -> 'b t

    val raw : (('model Effect.t -> unit) -> ReasonReact.reactElement) -> 'model t
  end

  module type AppSpec = sig
    type model
    (* type action *)

    val init : unit -> model Task.t
    val update : model Effect.t -> model Effect.t
    val subs : model -> model Effect.t Sub.t list
    val view : model -> model Html.t
  end

  module type SimpleAppSpec = sig
    type model
    (* type action *)

    val init : unit -> model Task.t
    val view : model -> model Html.t
  end

  module type App = sig
    val mount : at:string -> unit
  end

  module App : functor(Spec : AppSpec) -> App
  module SimpleApp : functor(Spec : SimpleAppSpec) -> App
end