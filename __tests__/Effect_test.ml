
module Future = Realm.Core.Future

let () =
  let open Jest in
  let open Expect in

  describe "Future"
    begin fun () ->

      testAsync "make"
        begin fun finish ->
          let expected = "my-value" in
          let task = Future.make (fun callback -> callback expected) in
          task |> Future.run (fun value -> expect value |> toBe expected |> finish)
        end;

      testAsync "const"
        begin fun finish ->
          let expected = "my-value" in
          let task = Future.const expected in
          task |> Future.run (fun value -> expect value |> toBe expected |> finish)
        end;

      testAsync "andThen"
        begin fun finish ->
          let task =
            Future.const "a"
              |> Future.andThen (fun a -> Future.const (a ^ "b"))
            in
          task |> Future.run (fun value -> expect value |> toBe "ab" |> finish)
        end;

      testAsync "map"
        begin fun finish ->
          Future.const 3
            |> Future.map (fun x -> x + 5)
            |> Future.run (fun value -> expect value |> toBe 8 |> finish)
        end;

      testAsync "map2"
        begin fun finish ->
          Future.map2
            (fun x y -> x + y) 
            (Future.const 3)
            (Future.const 6)
            |> Future.run (fun value -> expect value |> toBe 9 |> finish)
        end;

    end

module Effect
: sig
    type 'model t

    val none : 'model t
    val const : 'model -> 'model t
    val update : ('model -> 'model) -> 'model t
    val do_ : ('model -> 'result Future.t) -> ('result -> 'model -> 'model) -> 'model t
    val andThen : 'model t -> 'model t -> 'model t
    val map : ('b -> 'a) -> ('b -> 'a -> 'b) -> 'a t -> 'b t

    val step : 'model -> 'model t -> ('model option* 'model t Future.t option)
  end
= struct
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
          | Some task -> task |> Future.run (aux model result)
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
              (fun model -> Future.const (model + 1))
              (fun model value -> model + value)
            in
          run effect 2 (fun result -> expect result |> toEqual [5] |> finish)
        end;

      testAsync "andThen"
        begin fun finish ->
          let initial = { number = 1; text = "foo" } in
          let effect =
            Effect.(
              do_ (fun model -> Future.const (model.number + 1))
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