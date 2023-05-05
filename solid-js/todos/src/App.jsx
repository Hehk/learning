import { createSignal, createMemo, Show, For } from "solid-js";
import './App.css';

let counter = 0;

function App() {
    const [todos, setTodos] = createSignal([{
        title: 'Learn Solid',
        id: counter++,
        completed: false
    }]);
    const remainingCount = createMemo(() => todos().length - todos().filter(todo => !todo.completed).length);
    const ENTER_KEY = 13;

    const addTodo = (event) => {
        const title = event.target.value;
        if (event.keyCode === ENTER_KEY && title) {
            setTodos(todos().concat({
                title,
                id: counter++,
                completed: false
            }));
            event.target.value = '';
        }
    }

    const toggle = (todoId) => {
        setTodos(todos().map(todo => {
            if (todo.id === todoId) {
                todo.completed = !todo.completed;
            }
            return todo;
        }))
    }

    const remove = (todoId) => {
        setTodos(todos().filter(todo => todo.id !== todoId));
    }

    const toggleAll = (e) => {
        const completed = e.target.checked;
        setTodos(todos().map(todo => {
            todo.completed = completed;
            return todo;
        }))
    }

    return <section class="todoapp">
        <header class="header">
            <h1>todos</h1>
            <input id="toggle-all" class="toggle-all" checked={remainingCount() === 0} type="checkbox" onClick={toggleAll} />
            <input class="new-todo" placeholder="What needs to be done?" autofocus onKeyDown={addTodo} />
        </header>
        <Show when={todos().length > 0}>
            <ul class="todo-list">
                <For each={todos()}>
                    {todo => (<li key={todo.id} class="todo" classList={{
                        completed: todo.completed
                    }}>
                        <div class="view">
                            <input type="checkbox" class="toggle" onClick={() => toggle(todo.id)}/>
                            <label>{todo.title}</label>
                            <button class="destroy" onClick={() => remove(todo.id)} />
                        </div>
                    </li>)}
                </For>
            </ul>
        </Show>
    </section>
}

export default App;
