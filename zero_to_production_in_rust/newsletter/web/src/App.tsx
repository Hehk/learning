import type { Component } from 'solid-js';
import { Routes, Route } from '@solidjs/router';

function Form() {
    const handleSubmit = async (e: Event) => {
        e.preventDefault();
        const formElement = e.target as HTMLFormElement;
        const values = Object.fromEntries(new FormData(formElement));
        debugger
        try {
            const url = new URL('http://0.0.0.0:8000/subscriptions');
            const response = await fetch(url.toString(), {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/x-www-form-urlencoded',
                },
                body: encodeURI(`name=${values.name}&email=${values.email}`),
            });

            if (response.ok) {
                console.log("Success")
            } else {
                console.error("Failed")
            }
        } catch (e) {
            console.error(e)
        }
    };

    return <form class="flex flex-col space-y-4" onSubmit={handleSubmit}>
        <h2 class="text-2xl font-bold mb-4">Subscribe</h2>
        <label class="flex flex-col">
            <span class="text-sm">Name</span>
            <input type="text" class="border border-gray-300 rounded-md px-4 py-2" name="name" id="name" />
        </label>
        <label class="flex flex-col">
            <span class="text-sm">Email</span>
            <input type="email" class="border border-gray-300 rounded-md px-4 py-2" name="email" id="email" />
        </label>
        <button>
            Submit
        </button>
    </form>
}

const App: Component = () => {
    return (
        <div class="max-w-2xl m-auto py-16 px-4 space-y-4">
            <h1 class="text-4xl font-bold mb-8">Zero to Production in Rust</h1>
            <p>As I work through the book, I figured it was a good time to try making a website in solid js that integrates with it</p>

            <nav class="flex flex-col space-y-2">
                <a class="relative w-fit after:absolute after:left-0 after:right-0 after:bottom-0 after:content-[''] after:h-1 after:bg-green-400/30" href="/subscriptions">Subscriptions</a>
                <a class="relative w-fit after:absolute after:left-0 after:right-0 after:bottom-0 after:content-[''] after:h-1 after:bg-green-400/30" href="/about">About</a>
            </nav>

            <div class="relative">
                <div class="absolute inset-0 flex items-center" aria-hidden="true">
                    <div class="w-full border-t border-gray-300"></div>
                </div>
                <div class="relative flex justify-center">
                    <span class="bg-white px-3 text-base font-semibold leading-6 text-gray-900">Projects</span>
                </div>
            </div>

            <Routes>
                <Route path="/subscriptions" element={<Form />} />
            </Routes>
        </div>
    );
};

export default App;
