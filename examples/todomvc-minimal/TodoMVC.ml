open Realm
open! Core



(* MODEL *)


module Entry = struct
  type t =
    { description : string
    ; completed : bool
    ; editing : bool
    ; id : int
    }

  let create description id =
    { description
    ; completed = false
    ; editing = false
    ; id
    }
end


type model =
    { entries : Entry.t list
    ; field : string
    ; uid : int
    ; visibility : string
    }


let emptyModel =
  { entries = []
  ; visibility = "All"
  ; field = ""
  ; uid = 0
  }


module LocalStorage = struct
  external getItem : string -> string option = "localStorage.getItem" [@@bs.val] [@@bs.return nullable]
  external setItem : string -> string -> unit = "localStorage.setItem" [@@bs.val]
end


module Json = struct
  external parse : string -> 'a option = "JSON.parse" [@@bs.val] [@@bs.return nullable]
  external stringify : 'a -> string option = "JSON.stringify" [@@bs.val] [@@bs.return nullable]
end


let init () =
  LocalStorage.getItem "realm-todo-save"
  |> Option.andThen Json.parse
  |> Option.withDefault emptyModel

let persist =
  Effect.map (fun model -> model) @@
    fun _ model ->
      let _: unit option =
        Json.stringify model
        |> Option.map (LocalStorage.setItem "realm-todo-save")
      in
        model



(* ACTIONS *)


let add =
  Effect.update @@
    fun model ->
      { model with
        uid = model.uid + 1
      ; field = ""
      ; entries =
          if model.field = "" then
            model.entries
          else
            model.entries @ [ Entry.create model.field model.uid ]
      }


let updateField str =
  Effect.update (fun model -> { model with field = str })


let editingEntry id isEditing =
  let
    updateEntry t =
      if t.Entry.id = id then
        { t with editing = isEditing }
      else
        t
    in
  Effect.update (fun model -> { model with entries = List.map updateEntry model.entries })


let updateEntry id task =
  let
    updateEntry t =
      if t.Entry.id = id then
        { t with description = task }
      else
        t
    in
  Effect.update (fun model -> { model with entries = List.map updateEntry model.entries })


let delete id =
  Effect.update (fun model -> { model with entries = List.filter (fun t -> t.Entry.id != id) model.entries})


let deleteComplete = 
  Effect.update (fun model -> { model with entries = List.filter (fun t -> not t.Entry.completed) model.entries})


let check id isCompleted =
  let
    updateEntry t =
      if t.Entry.id = id then
        { t with completed = isCompleted }
      else
        t
    in
  Effect.update (fun model -> { model with entries = List.map updateEntry model.entries })


let checkAll isCompleted =
  let
    updateEntry t =
      Entry.{ t with completed = isCompleted }
    in
  Effect.update (fun model -> { model with entries = List.map updateEntry model.entries })


let changeVisibility visibility =
  Effect.update (fun model -> { model with visibility })


let update cmd =
  persist cmd


(* VIEW *)


module Html = MakeHtml (struct
  type nonrec model = model
end)

open Html
open Attr


let onEnter action =
  onKeyDown <|
    fun keyCode ->
      if keyCode == 13 then
        action
      else
        Effect.none


let viewInput task =
  header
    ~className: "header"
    [ h1 [ text "todos" ]
    ; Html.input
        ~className: "new-todo"
        ~placeholder: "What needs to be done?"
        ~value: (`Text task)
        ~attrs:
          [ autofocus true
          ; name "newTodo"
          ; onInput updateField
          ; onEnter add
          ]
    ]

let viewEntry todo =
  let open Entry in
  li
    ~className: (if todo.editing then "editing" else "")
    [ div
        ~className: "view"
        [ Html.input
            ~className: "toggle"
            ~value: (`Checkbox todo.completed)
            ~attrs: [ onClick (check todo.id (not todo.completed)) ]
        ; label
            ~attrs: [ onDoubleClick (editingEntry todo.id true) ]
            [ text todo.description ]
        ; button
            ~className: "destroy"
            ~attrs: [ onClick (delete todo.id) ]
            []
        ]
    ; Html.input
        ~id: ("todo-" ^ string_of_int todo.id)
        ~className: "edit"
        ~value: (`Text todo.description)
        ~attrs:
          [ onInput (updateEntry todo.id)
          ; onBlur (editingEntry todo.id false)
          ; onEnter (editingEntry todo.id false)
          ]
    ]


let viewEntries visibility entries =
  let open Entry in
  let
    isVisible todo =
      match visibility with
      | "Completed" -> todo.completed
      | "Active"    -> not todo.completed
      | _           -> true
    in
  let
    allCompleted =
      List.all (fun t -> t.completed) entries
    in
  (* let
    cssVisibility =
      if entries = [] then
        "hidden"
      else
        "visible"
    in *)
  section
    ~className: "main"
    (* ~attrs: [style "visibility" cssVisibility] *)
    [ Html.input
        ~className: "toggle-all"
        ~value: (`Checkbox allCompleted)
        ~attrs: [ name "toggle-all"; onClick (checkAll (not allCompleted)) ]
    ; label
        ~for_: "toggle-all"
        [ text "Mark all as complete" ]
    ; ul
        ~className: "todo-list" <|
        (entries |> List.filter isVisible |> List.map viewEntry)
    ]


let viewControlsCount entriesLeft =
  let
    item =
      if entriesLeft = 1 then
        " item"
      else
        " items"
    in
  span
    ~className: "todo-count"
    [ strong [ text (String.fromInt entriesLeft) ]
    ; text (item ^ " left")
    ]


let visibilitySwap uri visibility actualVisibility =
  li
    ~attrs: [ onClick (changeVisibility visibility) ]
    [ a
        ~className: (if visibility = actualVisibility then "selected" else "")
        ~href: uri
        [ text visibility ]
    ]


let viewControlsFilters visibility =
  ul
    ~className: "filters"
    [ visibilitySwap "#/" "All" visibility
    ; text " "
    ; visibilitySwap "#/active" "Active" visibility
    ; text " "
    ; visibilitySwap "#/completed" "Completed" visibility
    ]


let viewControlsClear entriesCompleted =
  button
    ~className: "clear-completed"
    ~attrs:
      [ hidden (entriesCompleted == 0)
      ; onClick deleteComplete
      ] 
      [ text ("Clear completed (" ^ String.fromInt entriesCompleted ^ ")") ]


let viewControls visibility entries =
  let
    entriesCompleted =
      entries
      |> List.filter (fun t -> t.Entry.completed)
      |> List.length
    in
  let
    entriesLeft =
      List.length entries - entriesCompleted
    in
  footer
    ~className: "footer"
    ~attrs: [ hidden (entries = []) ]
    [ viewControlsCount entriesLeft
    ; viewControlsFilters visibility
    ; viewControlsClear entriesCompleted
    ]


let infoFooter =
  footer ~className:"info"
    [ p [ text "Double-click to edit a todo" ]
    ; p
      [ text "Written by "
      ; a ~href: "https://github.com/glennsl" [ text "Glenn Slotte" ]
      ]
    ; p
      [ text "Based on "
      ; a ~href: "https://github.com/evancz/elm-todomvc" [ text "evancz/elm-todomvc" ]
      ]
    ; p
      [ text "Part of "
      ; a ~href: "http://todomvc.com" [ text "TodoMVC" ]
      ]
    ]


let view model =
  div
    ~className: "todomvc-wrapper"
    (*~attrs: [ style "visibility" "hidden" ]*)
    [ section ~className: "todoapp"
      [ viewInput model.field
      ; viewEntries model.visibility model.entries
      ; viewControls model.visibility model.entries
      ]
    ; infoFooter
    ]


let () =
  mountHtml ~at:"todoapp" ~init ~update ~view ()