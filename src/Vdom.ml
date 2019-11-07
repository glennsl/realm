
module Dom = struct
  type node = Dom.node
  type event = Dom.event
  type nodelist

  external getElementById : string -> node = "document.getElementById" [@@bs.val]
  external createElement : string -> node = "document.createElement" [@@bs.val]
  external createElementNS : string -> string -> Dom.node = "document.createElementNS" [@@bs.val]
  external createTextNode : string -> node = "document.createTextNode" [@@bs.val]
  external setAttribute : string -> string -> unit = "setAttribute" [@@bs.send.pipe: node] (* element *)
  external removeAttribute : string -> unit = "removeAttribute" [@@bs.send.pipe: node] (* element *)
  external setAttributeNS : string -> string -> string -> unit = "setAttribute" [@@bs.send.pipe: node] (* element *)
  external addEventListener : string -> (event -> unit) -> unit = "addEventListener" [@@bs.send.pipe: node] (* element *)
  external removeEventListener : string -> (event -> unit) -> unit = "removeEventListener" [@@bs.send.pipe: node] (* element *)

  external appendChild : node -> unit = "appendChild" [@@bs.send.pipe: node]
  external insertBefore : node -> anchor:node -> unit = "insertBefore" [@@bs.send.pipe: node]
  external replaceChild : node -> target:node -> unit = "replaceChild" [@@bs.send.pipe: node]
  external removeChild : node -> unit = "removeChild" [@@bs.send.pipe: node]
  external childNodes : node -> nodelist = "childNodes" [@@bs.get] (* nodelist *)
  external getChild : nodelist -> int -> node option = "" [@@bs.get_index] [@@bs.return undefined_to_opt]
  external firstChild : node -> node option = "firstChild" [@@bs.get] [@@bs.return undefined_to_opt]
  external lastChild : node -> node option = "lastChild" [@@bs.get] [@@bs.return undefined_to_opt]
  external parentNode : node -> node option = "parentNode" [@@bs.get] [@@bs.return undefined_to_opt]

  external replaceData : int -> int -> string -> unit = "replaceData" [@@bs.send.pipe: node] (* CharacterData *)
  external length : node -> int = "length" [@@bs.get] (* CharacterData *)
end

module Dict = struct
  type 'a t = 'a Js.Dict.t
  let empty = Js.Dict.empty
  let get = Js.Dict.get
  let set = Js.Dict.set
  let values = Js.Dict.values
end


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
    | Event of string * (Dom.event -> unit)

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

    | node ->
      node


  let text s =
    Text s

  let element ?namespace tag properties children =
    Element { namespace; tag; properties; children }

  module Keyed = struct
  let element ?namespace tag properties children =
    KeyedElement { namespace; tag; properties; children }
  end
end


type patch =
  | Rerender of Dom.node * Node.t
  | PushNodes of Dom.node * Node.t list
  | PopNodes of Dom.node * int
  | SetText of Dom.node * string
  | SetProperty of Dom.node * Node.property
  | RemoveProperty of Dom.node * Node.property
  | Reorder of Dom.node *
      [ `Insert of Node.t * int
      | `Remove of int
      | `Move of int * int
      ] array


let diff
  ~(rootDomNode: Dom.node)
  ~(oldVTree: Node.t)
  ~(newVTree: Node.t)
  : patch list
  =

  let rec diffNode domNode patches oVNode nVNode =
    Node.( match oVNode, nVNode with
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
          let patches = diffProperties domNode patches o.properties n.properties in
          diffKeyedChildNodes domNode patches o.children n.children
        else
          Rerender (domNode, nVNode) :: patches

      | _ ->
        Rerender (domNode, nVNode) :: patches
    )

  and diffProperties domNode patches oldProperties newProperties =
    (* naive approach, assumes a small number of properties *)

    let helper isMatch onResult acc allXs allYs = 
      let rec processXs acc xs ys = 
        (* match Xs against Ys, report back whether or not a match was found *)
        ( match xs, ys with
          | [], _ ->
            (* no more Xs, return *)
            acc

          | x :: remainingXs, [] ->
            (* no more Ys, report back, move on to next X *)
            let acc = onResult acc (Some x) None in
            processXs acc remainingXs allYs

          | x :: remainingXs, y :: _ when isMatch x y ->
            (* found match, report back, move on to next X *)
            let acc = onResult acc (Some x) (Some y) in
            processXs acc remainingXs allYs

          | _, _ :: remainingYs ->
            (* no match, check next Y *)
            processXs acc xs remainingYs
        )

      and processYs acc xs ys = 
        (* match Ys against Xs, report back only when match was NOT found, since
         * matches were reported in processXs
         *)
        ( match xs, ys with
          | _, [] ->
            (* no more Ys, return *)
            acc

          | [], y :: remainingYs ->
            (* no more Xs, report back, move on to next Y *)
            let acc = onResult acc None (Some y) in
            processYs acc allXs remainingYs

          | x :: _, y :: remainingYs when isMatch x y ->
            (* found match, already reported in processXs, so just move on to next Y *)
            processYs acc xs remainingYs

          | _ :: remainingXs, _ ->
            (* no match, check next X *)
            processYs acc remainingXs ys
        )
      in

      let acc = processXs acc allXs allYs in
      let acc = processYs acc allXs allYs in
      acc
    in

    let isMatch x y =
      Node.( match x, y with
        | Attribute o, Attribute n when o.namespace = n.namespace && o.key = n.key ->
          true

        | Event (oName, _), Event (nName, _) when oName = nName ->
          true

        | _ ->
          false
      )

    and onResult patches x y =
      ( match x, y with
        | Some oldProperty, None ->
          RemoveProperty (domNode, oldProperty) :: patches

        | None, Some newProperty ->
          SetProperty (domNode, newProperty) :: patches

        | Some oldProperty, Some newProperty ->
          Node.( match oldProperty, newProperty with
            | Attribute o, Attribute n when o.value <> n.value ->
              SetProperty (domNode, newProperty) :: patches

            | Event _, Event _ ->
              (* TODO: Event delegation *)
              RemoveProperty (domNode, oldProperty)
                :: SetProperty (domNode, newProperty)
                :: patches

            | _ ->
              patches
          )

        | _ ->
          patches
      )
    in
    helper isMatch onResult patches oldProperties newProperties

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
      let patches =
        ( match Dom.getChild childDomNodes index with
          | Some domNode ->
            diffNode domNode patches oVNode nVNode

          | None ->
            failwith "well this shouldn't happen"
        )
      in
      diffChildNodes parentDomNode patches oRest nRest (index + 1)
    )

  and diffKeyedChildNodes parentDomNode patches oldVNodes newVNodes =
    let key_SUFFIX = "_rlm" in
    let changes = Dict.empty () in

    let rec insert key node index =
      ( match Dict.get changes key with
        | None ->
          Dict.set changes key (`Insert (node, index))

        | Some (`Remove fromIndex) ->
          Dict.set changes key (`Move (fromIndex, index));

        | _ ->
          insert (key ^ key_SUFFIX) node index
      )
    in

    let rec remove key node index =
      ( match Dict.get changes key with
        | None ->
          Dict.set changes key (`Remove index)

        | Some (`Insert (_, toIndex)) ->
          Dict.set changes key (`Move (index, toIndex));

        | _ ->
          remove (key ^ key_SUFFIX) node index
      )
    in

    let diffChildNode  oVNode nVNode index =
      let childDomNodes = Dom.childNodes parentDomNode in
      ( match Dom.getChild childDomNodes index with
        | Some domNode ->
          diffNode domNode patches oVNode nVNode

        | None ->
          failwith "well this shouldn't happen"
      )
    in

    let rec helper patches oldVNodes newVNodes index =
      ( match oldVNodes, newVNodes with
      | [], [] ->
        patches

      | [], newRest ->
        PushNodes (parentDomNode, List.map snd newRest) :: patches

      | oldRest, [] ->
        PopNodes (parentDomNode, List.length oldRest) :: patches

      | (oKey, oVNode) :: oRest, (nKey, nVNode) :: nRest ->
        if oKey = nKey then
          let patches = diffChildNode oVNode nVNode index in
          helper patches oRest nRest (index + 1)
        else
          ( match oRest, nRest with
            | (oNextKey, _) :: oNextRest, (nNextKey, _) :: nNextRest ->
              if oKey = nNextKey && nKey = oNextKey then
                begin
                  insert nKey nVNode index;
                  remove oKey oVNode (index + 1);
                  helper patches oNextRest nNextRest (index + 1)
                end

              else if oKey = nNextKey then
                begin
                  insert nKey nVNode index;
                  helper patches oRest nNextRest (index + 2)
                end

              else if nKey = oNextKey then
                begin
                  remove oKey oVNode index;
                  helper patches oNextRest nRest (index + 1)
                end

              else
                begin
                  remove oKey oVNode index;
                  insert nKey nVNode index;
                  helper patches oRest nRest (index + 1)
                end

            | _ ->
                helper patches oRest nRest (index + 1)
          )
      )
    in
    let patches = helper patches oldVNodes newVNodes 0 in
    Reorder (parentDomNode, Js.Dict.values changes) :: patches

  in
  match Dom.firstChild rootDomNode with
    | Some node ->
      diffNode node [] oldVTree newVTree

    | None ->
      failwith "no dom"


let rec render node =
  Node.( match node with
    | Text text ->
      Dom.createTextNode text

    | Element spec ->
      let el =
        ( match spec.namespace with 
          | Some namespace ->
            Dom.createElementNS namespace spec.tag

          | None ->
            Dom.createElement spec.tag
        )
      in
      List.iter
        ( function
          | Attribute attr ->
            ( match attr.Attribute.namespace with
              | Some namespace ->
                Dom.setAttributeNS namespace attr.key attr.value el

              | None ->
                Dom.setAttribute attr.key attr.value el
            )

          | Event (name, callback) ->
            Dom.addEventListener name callback el
        ) spec.properties;
      List.iter (fun child -> Dom.appendChild (render child) el) spec.children;
      el
    
    | KeyedElement spec ->
      let el =
        ( match spec.namespace with 
          | Some namespace ->
            Dom.createElementNS namespace spec.tag

          | None ->
            Dom.createElement spec.tag
        )
      in
      List.iter
        ( function
          | Attribute attr ->
            ( match attr.Attribute.namespace with
              | Some namespace ->
                Dom.setAttributeNS namespace attr.key attr.value el

              | None ->
                Dom.setAttribute attr.key attr.value el
            )

          | Event (name, callback) ->
            Dom.addEventListener name callback el
        ) spec.properties;
      List.iter (fun (_, child) -> Dom.appendChild (render child) el) spec.children;
      el
  )


let patch =
  List.iter
    ( function
      | Rerender (domNode, node) ->
        ( match Dom.parentNode domNode with
        | Some parent ->
          Dom.replaceChild ~target:domNode (render node) parent
        | None ->
          ()
        )

      | PushNodes (domNode, nodes) ->
        List.iter (fun node -> Dom.appendChild (render node) domNode) nodes

      | PopNodes (domNode, n) ->
        for _ = 1 to n do
          match Dom.lastChild domNode with
            | Some child ->
              Dom.removeChild child domNode

            | None ->
              ()
        done

      | SetText (domNode, text) ->
        Dom.replaceData 0 (Dom.length domNode) text domNode

      | SetProperty (domNode, property) ->
        ( match property with
          | Attribute attr ->
            Dom.setAttribute attr.key attr.value domNode

          | Event (name, callback) ->
            Dom.addEventListener name callback domNode
        )


      | RemoveProperty (domNode, property) ->
        ( match property with
          | Attribute attr ->
            Dom.removeAttribute attr.key domNode

          | Event (name, callback) ->
            Dom.removeEventListener name callback domNode
        )
      
      | Reorder (domNode, changes) ->
        Array.iter
          ( function
            | `Insert (node, index) ->
              ( match Dom.getChild (Dom.childNodes domNode) index with
                | Some anchor ->
                  Dom.insertBefore ~anchor (render node) domNode

                | None ->
                  ()
              )

            | `Remove index ->
              ( match Dom.getChild (Dom.childNodes domNode) index with
                | Some target ->
                  Dom.removeChild target domNode

                | None ->
                  ()
              )

            | `Move (fromIndex, toIndex) ->
              ( match Dom.getChild (Dom.childNodes domNode) fromIndex with
                | Some target ->
                  Dom.removeChild target domNode;
                  ( match Dom.getChild (Dom.childNodes domNode) toIndex with
                    | Some anchor ->
                      Dom.insertBefore ~anchor target domNode

                    | None ->
                      ()
                  )

                | None ->
                  ()
              )
          ) changes
    )



let pp_node =
  function
    | Node.Text text ->
      {j|Text $text |j}

    | Node.Element { tag } ->
      {j|Element $tag |j}

    | Node.KeyedElement { tag } ->
      {j|KeyedElement $tag |j}

let pp_change =
  function
    | `Insert (node, index) ->
      {j|Insert $index $node|j}

    | `Remove index ->
      {j|Remove $index|j}

    | `Move (fromIndex, toIndex) ->
      {j|Move $fromIndex $toIndex|j}


let pp_patch =
  function
    | Rerender (_, node) ->
      let text = pp_node node in
      {j|Rerender $text|j}

    | PushNodes (_, nodes) ->
      let length = List.length nodes in
      {j|PushNodes $length|j}

    | PopNodes (_, n) ->
      {j|PopNodes $n |j}

    | SetText (_, text) ->
      {j|SetText $text|j}

    | SetProperty (_, property) ->
      {j|SetProperty $property|j}

    | RemoveProperty (_, property) ->
      {j|RemoveProperty $property|j}

    | Reorder (_, changes) ->
      let changes = Array.map pp_change changes in
      {j|Reorder $changes|j}