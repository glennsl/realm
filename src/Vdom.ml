
module Attribute = struct
  type t =
    { namespace: string option
    ; key: string
    ; value: string
    }

  let make ?namespace key value =
    { namespace; key; value }
end

module Node = struct
  type t =
    | Text of string
    | Element of element

  and element =
    { namespace: string option
    ; tagName: string
    ; key: string option
    ; attributes: Attribute.t list
    ; children: t list
    }

  let text s =
    Text s

  let element ?namespace ?key tagName attributes children =
    Element { namespace; key; tagName; attributes; children }
end

external _getElementById : string -> Dom.node = "document.getElementById" [@@bs.val]
external _createElement : string -> Dom.node = "document.createElement" [@@bs.val]
external _createElementNS : string -> string -> Dom.node = "document.createElementNS" [@@bs.val]
external _createTextNode : string -> Dom.node = "document.createTextNode" [@@bs.val]
external _appendChild : Dom.node -> unit = "appendChild" [@@bs.send.pipe: Dom.node]
external _setAttribute : string -> string -> unit = "setAttribute" [@@bs.send.pipe: Dom.node] (* element *)
external _setAttributeNS : string -> string -> string -> unit = "setAttribute" [@@bs.send.pipe: Dom.node] (* element *)

let rec append node targetNode =
  let domNode =
    match node with
    | Node.Text text -> _createTextNode text
    | Node.Element spec ->
      let el =
        match spec.namespace with 
        | Some namespace -> _createElementNS namespace spec.tagName
        | None -> _createElement spec.tagName
      in
      List.iter (fun attr ->
        match attr.Attribute.namespace with
        | Some namespace -> _setAttributeNS namespace attr.key attr.value el
        | None -> _setAttribute attr.key attr.value el
        ) spec.attributes;
      List.iter (fun child -> append child el) spec.children;
      el
  in
  _appendChild domNode targetNode

let render node targetId =
  append node (_getElementById targetId)
