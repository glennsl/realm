open Realm;
open! Core;
open Model;

let toggle = entry =>
  Effect.update(model => { ...model, entries: List.map(it => it.Todo.id == entry.Todo.id ? { ...entry, completed: !entry.completed } : it, model.entries) })

let edit = (entry, ~editing) =>
  Effect.update(model => { ...model, entries: List.map(it => it.Todo.id == entry.Todo.id ? { ...entry, editing } : it, model.entries) })

let remove = entry =>
  Effect.update(model => { ...model, entries: List.filter(it => it.Todo.id != entry.Todo.id, model.entries) })

let clearCompleted =
  Effect.update(model => { ...model, entries: List.filter(it => !it.Todo.completed, model.entries) })

let changeVisibility = visibility =>
  Effect.update(model => { ...model, visibility })