open RealmNoUpdate;
open! Core;
open Model;

let toggle = entry =>
  Cmd.make(model => { ...model, entries: List.map(it => it.Todo.id == entry.Todo.id ? { ...entry, completed: !entry.completed } : it, model.entries) })

let edit = (entry, ~editing) =>
  Cmd.make(model => { ...model, entries: List.map(it => it.Todo.id == entry.Todo.id ? { ...entry, editing } : it, model.entries) })

let remove = entry =>
  Cmd.make(model => { ...model, entries: List.filter(it => it.Todo.id != entry.Todo.id, model.entries) })

let clearCompleted =
  Cmd.make(model => { ...model, entries: List.filter(it => !it.Todo.completed, model.entries) })

let changeVisibility = visibility =>
  Cmd.make(model => { ...model, visibility })