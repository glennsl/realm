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
  type t('model, 'result)
  let const : 'a => t(_, 'a)
  let update : ('model => 'result) => t('model, 'result)
  let do_ : ('model => Task.t('result)) => t('model, 'result)
  let andThen : ('a => t('model, 'b), t('model, 'a)) => t('model, 'b)

  let run : ('result => unit, 'model, t('model, 'result)) => unit
} = {
  type t('model, 'result) = 'model => Task.t('result);
  let const = value =>
    _ => Task.const(value)
  let update = updater => 
    model => updater(model) |> Task.const;
  let do_ = f =>
    f
  let andThen = (f, effect) =>
    model => effect(model) |> Task.andThen(value => f(value)(model))

  let run = (receiver, model, command) =>
    command(model) |> Task.run(receiver);
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

    testAsync("const", finish => {
      let effect = Effect.const(42)
      effect |> Effect.run(value => expect(value) |> toBe(42) |> finish, "foo")
    });

    testAsync("update", finish => {
      let effect = Effect.update(model => model + 1)
      effect |> Effect.run(value => expect(value) |> toBe(3) |> finish, 2)
    });

    testAsync("do_", finish => {
      let effect = Effect.do_(model => Task.const(model + 1))
      effect |> Effect.run(value => expect(value) |> toBe(3) |> finish, 2)
    })

    testAsync("andThen", finish => {
      let effect =
        Effect.(
          do_(model => Task.const(int_of_string(model) + 1))
          |> andThen(value => update(model => model ++ (string_of_int(value))))
        )
      effect |> Effect.run(value => expect(value) |> toBe("23") |> finish, "2")
    })
  })
}
