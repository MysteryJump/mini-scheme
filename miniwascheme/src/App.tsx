import React, { useEffect, useRef, useState } from "react";

function App() {
  const [result, setResult] = useState("");
  const [text, setText] = useState("");

  const executor = useRef<(lines: string) => string>();
  useEffect(() => {
    const f = async () => {
      const imp = await import("mini-scheme-wasm");
      executor.current = imp.execute_lines;
    };
    f();
  }, []);

  return (
    <div className="w-screen h-screen bg-gray-200">
      <div className="flex flex-col p-4 justify-start">
        <h1 className="text-xl">Miniwascheme - Web Playground</h1>
        <div className="flex w-full pt-4">
          <textarea
            className="flex-grow mr-2"
            value={text}
            onChange={(e) => setText(e.target.value)}
          />
          <textarea className="flex-grow ml-2" value={result} />
        </div>
        <div className="p-2">
          <button
            className="p-1.5 rounded bg-red-500 hover:bg-red-700 focus:ring-0 focus-visible:ring-2"
            onClick={() => {
              if (executor.current) {
                setResult(executor.current(text));
              }
            }}
          >
            Execute
          </button>
        </div>
      </div>
    </div>
  );
}

export default App;
