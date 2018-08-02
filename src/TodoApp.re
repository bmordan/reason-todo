type item = {
  id: int,
  title: string,
  completed: bool
};

type state = {
  items: list(item)
};

type action =
  | AddItem(string)
  | ToggleItem(int);

let lastId = ref(0);

let str = ReasonReact.stringToElement;

let targetValue = evt : string => (
  evt
  |> ReactEventRe.Form.target
  |> ReactDOMRe.domElementToObj
)##value;

let newItem = (title) => {
  lastId := lastId^ + 1;
  {id: lastId^, title , completed: false}
};

module TodoItem = {
  let component = ReasonReact.statelessComponent("TodoItem");
  let make = (~item, ~onToggle, _children) => {
    ...component,
    render: (_) =>
      <div className="pa2" key=(string_of_int(item.id)) onClick=(_ => onToggle())>
        <input
          _type="checkbox"
          checked=(Js.Boolean.to_js_boolean(item.completed))
        />
        <span className="pl2">(str(item.title))</span>
      </div>
  };
};

module Input = {
  type state = string;
  let component = ReasonReact.reducerComponent("Input");
  let make = (~onSubmit, _children) => {
    ...component,
    initialState: () => "",
    reducer: (newText, _text) => ReasonReact.Update(newText),
    render: ({state: text, reduce}) => {
      <input
        value=text
        _type="text"
        className="mt4"
        placeholder="What do you need todo?"
        onChange=(reduce(evt => targetValue(evt)))
        onKeyDown=(evt => {
          if (ReactEventRe.Keyboard.key(evt) == "Enter") {
            onSubmit(text);
            (reduce(() => ""))();
          }
        })
      />
    }
  }
};

let component = ReasonReact.reducerComponent("TodoApp");

let make = _children => {
  ...component,
  initialState: () => {
    items: [
      {id: 1, title: "Create some things to do", completed: false}
    ]
  },
  reducer: (actions, {items}) =>
    switch(actions) {
    | AddItem(text) => ReasonReact.Update({items: [newItem(text), ...items]})
    | ToggleItem(id) => {
      let items = List.map(item => {
        item.id === id ? {...item, completed: !item.completed} : item
      }, items);

      ReasonReact.Update({items: items});
    }
    }
  ,
  render: ({state: {items}, reduce}) => {
    let numItems = List.length(items);

    <div className="flex flex-column items-start justify-start mw5 center vh-100">
      <div className="f1 flex-none pv5">(str("What to do"))</div>
      <div className="flex-none bg-washed-blue blue pa2">(
        ReasonReact.arrayToElement(Array.of_list(
          List.map(item => <TodoItem item onToggle=(reduce(() => ToggleItem(item.id))) />, items)
        ))
      )</div>
      <div className="flex-auto self-start">
        <Input onSubmit=(reduce(text => AddItem(text))) />
      </div>
      <div className="flex-none pb5">
        (str(string_of_int(numItems) ++ " item" ++ (numItems > 1 ? "s" : "")))
      </div>
    </div>
  }
};
