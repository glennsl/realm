

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
    | Element of t element
    | KeyedElement of (string * t) element

  and property =
    | Attribute of Attribute.t

  and 'a element =
    { namespace: string option
    ; tag: string
    ; properties: property list
    ; children: 'a list
    }


let rec dekey =
  function
  | KeyedElement { namespace; tag; properties; children } ->
    Element 
      { namespace
      ; tag
      ; properties
      ; children = List.map (fun (_, child) -> dekey child) children
      }
  | node -> node


  let text s =
    Text s

  let element ?namespace tag properties children =
    Element { namespace; tag; properties; children }
end


module Dom = struct
  type node = Dom.node
  type nodelist

  external getElementById : string -> Dom.node = "document.getElementById" [@@bs.val]
  external createElement : string -> Dom.node = "document.createElement" [@@bs.val]
  external createElementNS : string -> string -> Dom.node = "document.createElementNS" [@@bs.val]
  external createTextNode : string -> Dom.node = "document.createTextNode" [@@bs.val]
  external setAttribute : string -> string -> unit = "setAttribute" [@@bs.send.pipe: Dom.node] (* element *)
  external setAttributeNS : string -> string -> string -> unit = "setAttribute" [@@bs.send.pipe: Dom.node] (* element *)

  external appendChild : Dom.node -> unit = "appendChild" [@@bs.send.pipe: Dom.node]
  external childNodes : Dom.node -> nodelist = "childNodes" [@@bs.get] (* nodelist *)
  external getChild : nodelist -> int -> Dom.node option = "" [@@bs.get_index] [@@bs.return undefined_to_opt]
  external firstChild : Dom.node -> Dom.node option = "firstChild" [@@bs.get] [@@bs.return undefined_to_opt]
end


let rec append node targetNode =
  let domNode =
    match node with
    | Node.Text text ->
      Dom.createTextNode text

    | Node.Element spec ->
      let el =
        match spec.namespace with 
        | Some namespace -> Dom.createElementNS namespace spec.tag
        | None -> Dom.createElement spec.tag
      in
      List.iter (function
        | Node.Attribute attr ->
          ( match attr.Attribute.namespace with
          | Some namespace -> Dom.setAttributeNS namespace attr.key attr.value el
          | None -> Dom.setAttribute attr.key attr.value el
          )
        ) spec.properties;
      List.iter (fun child -> append child el) spec.children;
      el
    
    | Node.KeyedElement _ ->
      failwith "todo"
  in
  Dom.appendChild domNode targetNode


let render node targetId =
  let domNode = Dom.getElementById targetId in
  append node domNode;
  domNode


type patch =
  | Rerender of Dom.node * Node.t
  | PushNodes of Dom.node * Node.t list
  | PopNodes of Dom.node * int
  | SetText of Dom.node * string
  | SetProperty of Dom.node * Node.property
  | RemoveProperty of Dom.node * Node.property


let diff
  ~(rootDomNode: Dom.node)
  ~(oldVTree: Node.t)
  ~(newVTree: Node.t)
  : patch list
  =

  let rec diffNode domNode patches oVNode nVNode =
    let open Node in
    ( match oVNode, nVNode with
    | Text oText, Text nText ->
      if oText = nText then
        patches
      else
        SetText (domNode, nText) :: patches

    | Element _, KeyedElement _ ->
      diffNode domNode patches oVNode (dekey nVNode)

    | Element o, Element n ->
      if o.tag = n.tag && o.namespace = n.namespace then
        let patches = diffProperties domNode patches o.properties n.properties in
        diffChildNodes domNode patches o.children n.children 0
      else
        Rerender (domNode, nVNode) :: patches

    | KeyedElement o, KeyedElement n ->
      if o.tag = n.tag && o.namespace = n.namespace then
        patches
      else
        Rerender (domNode, nVNode) :: patches

    | _ ->
      Rerender (domNode, nVNode) :: patches
    )

  and diffProperties domNode patches oldProperties newProperties =
    (* naive approach, pretty much assumes a small number of properties *)
    let rec oldHelper patches oldProperties' newProperties' = 
      ( match oldProperties', newProperties' with
      | [], _ ->
        patches

      | oldProperty :: oldRest, [] ->
        oldHelper (RemoveProperty (domNode, oldProperty) :: patches) oldRest newProperties

      | oldProperty :: oldRest, newProperty :: newRest ->
        ( match oldProperty, newProperty with
        | Attribute o, Attribute n when o.namespace = n.namespace && o.key = n.key ->
          if o.value = n.value then
            oldHelper patches oldRest newProperties
          else
            oldHelper (SetProperty (domNode, newProperty) :: patches) oldRest newProperties
        
        | _ ->
          oldHelper patches oldProperties' newRest
        )
      )

    and newHelper patches newProperties' oldProperties' = 
      ( match newProperties', oldProperties' with
      | [], _ ->
        patches

      | newProperty :: newRest, [] ->
        newHelper (SetProperty (domNode, newProperty) :: patches) newRest oldProperties

      | newProperty :: newRest, oldProperty :: oldRest ->
        ( match newProperty, oldProperty with
        | Attribute n, Attribute o when n.namespace = o.namespace && n.key = o.key ->
          newHelper patches newRest oldProperties
        
        | _ ->
          newHelper patches newProperties' oldRest
        )
      )
    in

    let patches = oldHelper patches oldProperties newProperties in
    newHelper patches newProperties oldProperties

  and diffChildNodes parentDomNode patches oldVNodes newVNodes index =
    ( match oldVNodes, newVNodes with
    | [], [] ->
      patches

    | [], newRest ->
      PushNodes (parentDomNode, newRest) :: patches

    | oldRest, [] ->
      PopNodes (parentDomNode, List.length oldRest) :: patches

    | oVNode :: oRest, nVNode :: nRest ->
      let childDomNodes = Dom.childNodes parentDomNode in
      let probablyDomNode = Dom.getChild childDomNodes index in
      let patches =
        ( match probablyDomNode with
        | Some domNode -> diffNode domNode patches oVNode nVNode
        | None -> failwith "well this shouldn't happen"
        )
      in
      diffChildNodes parentDomNode patches oRest nRest (index + 1)
    )

  in
  match Dom.firstChild rootDomNode with
  | Some node -> diffNode node [] oldVTree newVTree
  | None -> failwith "no dom"


let pp_node =
  function
  | Node.Text text -> {j|Text $text |j}
  | Node.Element { tag } -> {j|Element $tag |j}
  | Node.KeyedElement { tag } -> {j|KeyedElement $tag |j}

let pp_patch =
  function
  | Rerender (_, node) -> let text = pp_node node in {j|Rerender $text|j}
  | PushNodes (_, nodes) -> let length = List.length nodes in {j|PushNodes $length|j}
  | PopNodes (_, n) -> {j|PopNodes $n |j}
  | SetText (_, text) -> {j|SetText $text|j}
  | SetProperty (_, property) -> {j|SetProperty $property|j}
  | RemoveProperty (_, property) -> {j|RemoveProperty $property|j}