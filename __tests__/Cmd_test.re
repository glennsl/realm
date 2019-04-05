module Task : {
  type t('a);
  let make : (('a => unit) => unit) => t('a);
  let const : 'a => t('a);
  let map : (('a => 'b), t('a)) => t('b);
  let andThen : (('a => t('b)), t('a)) => t('b);

  let run : (('a => unit), t('a)) => unit;
} = {
  type t('a) = ('a => unit) => unit;
  let make = f => f;
  let const = value =>
    make (f => f(value));
  let map = (f, task) =>
    resolve => task(value => resolve(f(value)));
  let rec andThen = (f, task) =>
    resolve => task(value => run(resolve, f(value)))

  and run = (receiver, task) =>
    task(receiver);
}

let () = {
  open Jest
  open Expect

  describe("Task", () => {
    testAsync("make", finish => {
      let expected = "my-value"
      let task = Task.make(callback => callback(expected))
      task |> Task.run(value => expect(value) |> toBe(expected) |> finish)
    })

    testAsync("map", finish => {
      let task = Task.make(callback => callback(3))
      task
        |> Task.map(x => x + 5)
        |> Task.run(value => expect(value) |> toBe(8) |> finish)
    });

    testAsync("const", finish => {
      let expected = "my-value"
      let task = Task.const(expected)
      task |> Task.run(value => expect(value) |> toBe(expected) |> finish)
    })
  })
}

module Effect : {
  type t('model)
  let const : 'model => t('model)
  let update : ('model => 'model) => t('model)
  let do_ : ('model => Task.t('result), ('result, 'model) => 'model) => t('model)
  let andThen : (t('model), t('model)) => t('model)

  let step : ('model, t('model)) => (option('model), option(Task.t(t('model))))
} = {
  type t('model) =
  | Update('model => 'model, t('model))
  | Task('model => Task.t('model => 'model), t('model))
  | End

  let const = value =>
    Update(_ => value, End)
  let update = updater => 
    Update(updater, End)
  let do_ = (action, mapper) =>
    Task(model => action(model) |> Task.map(mapper), End)
  let rec andThen = last => 
    fun | End => last
        | Update(f, next) => Update(f, andThen(last, next))
        | Task(f, next) => Task(f, andThen(last, next))

  let step = model =>
    fun | End =>
          (None, None)
        | Update(f, next) =>
          (Some(f(model)),
           next == End ? None : Some(Task.const(next)))
        | Task(f, next) =>
          (None,
           Some(f(model) |> Task.map(updater => Update(updater, next))))

  // let mapModel = (mapper, command) =>
  //   model => command(model) |> Task.map(updater => value => updater |> mapper);
  // let mapResult = 
  //   model => command(model) |> Task.map(updater => b => b |> getter |> updater |> setter(b));
  // let map : (('b => 'a), (('b, 'a) => 'b), t('model, 'a)) => t('model, 'b)
  //         = (getter, setter, command) =>
  //   model => command(model) |> Task.map(updater => b => b |> getter |> updater |> setter(b));
  // let effect; let do_
  // let update
  // let andThen
  // let then_
}

let () = {
  open Jest
  open Expect

  describe("Effect", () => {

    test("const", () => {
      let effect = Effect.const(42)
      let (value, next) = Effect.step(0, effect);
      expect((value, next)) |> toEqual((Some(42), None))
    });

    test("update", () => {
      let effect = Effect.update(model => model + 1)
      let (value, next) = Effect.step(2, effect);
      expect((value, next)) |> toEqual((Some(3), None))
    });

    testAsync("do_", finish => {
      let model = 2
      let effect = Effect.do_(model => Task.const(model + 1), (model, value) => model + value)
      let (value1, next) = Effect.step(model, effect);
      switch next {
      | Some(task) =>
        task |> Task.run(effect => {
          let model = switch value1 {
          | Some(value) => value
          | None => model
          };
          let (value2, next) = Effect.step(model, effect);
          expect((value1, value2, next)) |> toEqual((None, Some(5), None)) |> finish
        })
      | None =>
        fail("should be more steps") |> finish
      }
    })

    testAsync("andThen", finish => {
      let model = 3
      let effect =
        Effect.(
          do_(
            model => Task.const(string_of_int(model) ++ "1"),
            (result, model) => model + int_of_string(result))
          |> andThen(update(model => model / 2))
        )
      let (value1, next) = Effect.step(model, effect);
      switch next {
      | Some(task) =>
        task |> Task.run(effect => {
          let model = switch value1 {
          | Some(value) => value
          | None => model
          };
          let (value2, next) = Effect.step(model, effect);
          switch next {
          | Some(task) =>
            task |> Task.run(effect => {
              let model = switch value2 {
              | Some(value) => value
              | None => model
              };
              let (value3, next) = Effect.step(model, effect);
              expect((value1, value2, value3, next)) |> toEqual((None, Some(34), Some(17), None)) |> finish
            })
          | None =>
            fail("should be more steps") |> finish
          }
        })
      | None =>
        fail("should be more steps") |> finish
      }
    })
  })
}
