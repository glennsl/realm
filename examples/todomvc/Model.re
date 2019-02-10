
module Todo = {
  type t = {
    id: string,
    title: string,
    completed: bool,
    created: Js.Date.t
  };

  let newId = () => [%bs.raw {|'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => (c === 'x' ? Math.random()*16|0 : (((Math.random()*16|0)&0x3)|0x8)).toString(16))|}];

  let make = title => {
    id: newId (),
    title,
    completed: false,
    created: Js.Date.make ()
  };
};

type model = {
  editing: option(string),
  items : list(Todo.t)
};

module Html = RealmNoUpdate.MakeHtml({
  type nonrec model = model
})

