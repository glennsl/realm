open RealmNoUpdate;
open Model;

let init = () =>
  { items:
    [ Todo.make("Item 1")
    , Todo.make("Item 2")
    ]
  , editing: None
  };

let view = ({ items, editing }) => {
  TodoApp.view(~items, ~editing);
};

mountHtml(~at="todoapp", ~init, ~view)