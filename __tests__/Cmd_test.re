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
  let map : ('b => 'a, ('b, 'a) => 'b, t('a)) => t('b)

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
  let rec map = (getter, setter) =>
    fun | End => End
        | Update(f, next) =>
          Update(model => model |> getter |> f |> setter(model), map(getter, setter, next))
        | Task(f, next) =>
          Task(model => model |> getter |> f |> Task.map(f => model => model |> getter |> f |> setter(model)), map(getter, setter, next))

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

type model = {
  number: int,
  text: string
}

let () = {
  open Jest
  open Expect

  describe("Effect", () => {

    let run = (effect, model, callback) => {
      let rec aux = (model, result, effect) => {
        let (maybeValue, next) = Effect.step(model, effect);
        let (model, result) =
          switch maybeValue {
          | Some(value) =>
            (value, [value, ...result])
          | None =>
            (model, result)
          };
        switch next {
        | Some(task) =>
          task |> Task.run(aux(model, result))
        | None =>
          callback(List.rev(result));
        }
      }
      aux(model, [], effect);
    }

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
      let effect = Effect.do_(model => Task.const(model + 1), (model, value) => model + value)
      run(effect, 2, result => expect(result) |> toEqual([5]) |> finish)
    })

    testAsync("andThen", finish => {
      let initial = { number: 1, text: "foo" }
      let effect =
        Effect.(
          do_(
            model => Task.const(model.number + 1),
            (result, model) => { ...model, number: result }
          )
          |> andThen(update(model => { ...model, text: string_of_int(model.number) }))
        )
      run(effect, initial, result =>
        expect(result)
          |> toEqual([
            { ...initial, number: 2 },
            { number: 2, text: "2" }
          ])
          |> finish)
    })

    test("map", () => {
      let initial = { number: 4, text: "bar" }
      let effect =
        Effect.(
          update(model => model + 1)
          |> map(model => model.number, (model, number) => { ...model, number })
        )
      let (value, next) = Effect.step(initial, effect);
      expect((value, next)) |> toEqual((Some({ ...initial, number: 5 }), None))
    });
  })
}
