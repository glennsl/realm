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

let () =
  let open Jest in
  let open Expect in

  describe "Task"
    begin fun () ->

      testAsync "make"
        begin fun finish ->
          let expected = "my-value" in
          let task = Task.make (fun callback -> callback expected) in
          task |> Task.run (fun value -> expect value |> toBe expected |> finish)
        end;

      testAsync "map"
        begin fun finish ->
          let task = Task.make (fun callback  -> callback 3) in
          task
            |> Task.map (fun x -> x + 5)
            |> Task.run (fun value -> expect value |> toBe 8 |> finish)
        end;

      testAsync "const"
        begin fun finish ->
          let expected = "my-value" in
          let task = Task.const expected in
          task |> Task.run (fun value -> expect value |> toBe expected |> finish)
        end

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
  end
= struct
    type 'model t =
      | Update of ('model -> 'model)* 'model t
      | Task of ('model -> ('model -> 'model) Task.t)* 'model t
      | End

    let none =
      End

    let const value =
      Update ((fun _ -> value), End)

    let update updater =
      Update (updater, End)

    let do_ action mapper =
      Task ((fun model -> action model |> Task.map mapper), End)

    let rec andThen last =
      function
      | End ->
        last
      | Update (f, next) ->
        Update (f, andThen last next)
      | Task (f, next) ->
        Task (f, andThen last next)

    let rec map getter setter =
      function
      | End ->
        End
      | Update (f, next) ->
        Update
          ( (fun model -> model |> getter |> f |> setter model)
          , map getter setter next
          )
      | Task (f, next) ->
        Task
          ( (fun model -> model |> getter |> f |> Task.map (fun f model -> model |> getter |> f |> setter model))
          , map getter setter next
          )


    let step model =
      function
      | End ->
        (None, None)
      | Update (f, next) ->
        ( Some (f model)
        , if next = End then
            None
          else
            Some (Task.const next)
        )
      | Task (f, next) ->
        ( None
        , Some (f model |> Task.map (fun updater -> Update (updater, next)))
        )
  end


type model =
  { number: int
  ; text: string
  }

let () =
  let open Jest in
  let open Expect in

  describe "Effect"
    begin fun () ->

      let run effect model callback =

        let rec aux model result effect =
          let maybeValue, next = Effect.step model effect in
          let model, result =
            match maybeValue with
            | Some value -> value, value :: result
            | None       -> model, result
            in
          match next with
          | Some task -> task |> Task.run (aux model result)
          | None      -> callback (List.rev result)
          in
        aux model [] effect in

      test "const"
        begin fun () ->
          let effect = Effect.const 42 in
          let value, next = Effect.step 0 effect in
          expect (value, next) |> toEqual (Some 42, None)
        end;

      test "update"
        begin fun () ->
          let effect = Effect.update (fun model -> model + 1) in
          let value, next = Effect.step 2 effect in
          expect (value, next) |> toEqual (Some 3, None)
        end;

      testAsync "do_"
        begin fun finish ->
          let effect =
            Effect.do_
              (fun model -> Task.const (model + 1))
              (fun model value -> model + value)
            in
          run effect 2 (fun result -> expect result |> toEqual [5] |> finish)
        end;

      testAsync "andThen"
        begin fun finish ->
          let initial = { number = 1; text = "foo" } in
          let effect =
            Effect.(
              do_ (fun model -> Task.const (model.number + 1))
                  (fun result model -> { model with number = result })
                |> andThen (update (fun model -> { model with text = string_of_int model.number }))
            ) in
          run effect initial
            begin fun result ->
              expect result
                |> toEqual
                  [ { initial with number = 2 }
                  ; { number = 2; text = "2" }
                  ]
                |> finish
            end
        end;

      test "map"
        begin fun () ->
          let initial = { number = 4; text = "bar" } in
          let effect =
            Effect.(
              update (fun model -> model + 1)
                |> map
                    (fun model -> model.number)
                    (fun model number -> { model with number })
            ) in
          let value, next = Effect.step initial effect in
          expect (value, next) |> toEqual (Some { initial with number = 5 }, None)
        end

      end