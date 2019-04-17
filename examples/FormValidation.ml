open! Realm.Core
open Realm.React


module Validation : sig

  type 'a t
  type state = Valid | Invalid of string

  val init : ('a -> state) -> 'a -> 'a t
  val get : 'a t -> 'a
  val set : 'a t -> 'a -> 'a t
  val touch : 'a t -> 'a t
  val error : _ t -> string option
  val isValid : _ t -> bool
  val isInitial : _ t -> bool

end = struct

  type 'a t =
    { validate : 'a -> state
    ; value : 'a
    ; state : state
    ; initial : bool
    }

  and state =
    | Valid
    | Invalid of string


  let init validate value =
    { validate
    ; value
    ; state = validate value
    ; initial = true
    }

  let get field =
    field.value

  let set field value =
    { field with
      value
    ; initial = false
    ; state = field.validate value
    }

  let touch field =
    { field with initial = false }

  let error field =
    match field.state with
    | Valid           -> None
    | Invalid message -> Some message

  let isValid field =
    field.state = Valid

  let isInitial field =
    field.initial

end


let isRequired value =
  let open Validation in
  match value with
  | ""  -> Invalid "Field is required"
  | _   -> Valid

let isValidEmail value =
  let open Validation in
  let re = [%re "/^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$/"] in
  if Js.Re.test_ re value then
    Valid
  else
    Invalid "Not a valid e-mail address"

let isJson value =
  let open Validation in
  match Js.Json.parseExn value with
  | exception _ -> Invalid "Not valid Json"
  | _           -> Valid

let isInteger value =
  let open Validation in
  let re = [%re "/^-?[0-9]+$/"] in
  if Js.Re.test_ re value then
    Valid
  else
    Invalid "Not an integer"


let validatedTextInput placeholder field =
  let open Html in
  let open Attr in
  div
    [ Html.input
        ~value:(`Text (Validation.get field))
        ~placeholder
        ~attrs:[ onInput (Validation.set field >> Effect.const) ]
        []
    ; if Validation.isInitial field then
        null
      else
        match Validation.error field with
        | Some message -> span [ text message ]
        | None         -> null
    ]


module App = SimpleApp(struct
  type model =
    { a : string Validation.t
    ; b : string Validation.t
    ; c : string Validation.t
    ; d : string Validation.t
    ; submitMessage : string
    }

  let init () =
    { a = Validation.init isRequired ""
    ; b = Validation.init isValidEmail ""
    ; c = Validation.init isJson ""
    ; d = Validation.init isInteger ""
    ; submitMessage = ""
    }

  let submit =
    Effect.update
      begin fun model ->
        let errors =
          List.filterMap Fn.id
            [ Validation.error model.a
            ; Validation.error model.b
            ; Validation.error model.c
            ; Validation.error model.d
            ]
          in
        if errors = [] then
          { model with submitMessage = "Success!" }
        else
          { a = Validation.touch model.a
          ; b = Validation.touch model.b
          ; c = Validation.touch model.c
          ; d = Validation.touch model.d
          ; submitMessage = "Error!"
          }
      end

  let view model =
    let open Html in
    let open Attr in
    div
      [ validatedTextInput "Required" model.a
          |> map (fun m -> m.a ) (fun m v -> { m with a = v })
      ; validatedTextInput "E-mail address" model.b
          |> map (fun m -> m.b ) (fun m v -> { m with b = v })
      ; validatedTextInput "JSON" model.c
          |> map (fun m -> m.c ) (fun m v -> { m with c = v })
      ; validatedTextInput "Whole number" model.d
          |> map (fun m -> m.d ) (fun m v -> { m with d = v })
      ; button ~attrs:[ onClick submit ] [ text "Submit" ]
      ; text model.submitMessage
      ]
end)