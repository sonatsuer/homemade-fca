import mermaid from "mermaid";

"use strict";

mermaid.initialize({
  startOnLoad: false,
  theme: "default",
});


export const render = function (diagramSource) {
  return function () {
    const outputDiv = document.getElementById("mermaid-output");
    const errorDiv = document.getElementById("error-message");

    outputDiv.innerHTML = "Rendering...";
    errorDiv.innerHTML = "";

    if (outputDiv) {
      mermaid
        .render("mermaid-temp-id", diagramSource)
        .then(({ svg }) => {
          outputDiv.innerHTML = svg;
        })
        .catch((error) => {
          console.error("Mermaid rendering failed:", error);
          outputDiv.innerHTML = "";
          errorDiv.innerHTML = `<pre>⚠️ Rendering Error: ${error.message}</pre>`;
        });
    } else {
      console.error("Target div #mermaid-output not found.");
    }
  };
};

export const displayError = function (message) {
  return function () {
    const errorDiv = document.getElementById("error-message");
    // Clear the output and show the error from PureScript
    document.getElementById("mermaid-output").innerHTML = "";
    errorDiv.innerHTML = `<pre>⚠️ ${message}</pre>`;
  };
};


export const setupFileUploadHandler = function (handleFn) {
  return function () {
    const fileInput = document.getElementById("diagram-upload");
    const errorDiv = document.getElementById("error-message");

    if (!fileInput) {
      console.error("File input #diagram-upload not found.");
      return;
    }

    fileInput.addEventListener("change", (event) => {
      const file = event.target.files[0];
      if (!file) {
        errorDiv.innerHTML = "No file selected.";
        return;
      }

      const reader = new FileReader();

      reader.onload = (e) => {
        const diagramSource = e.target.result;
        // The JS calls the PureScript handler with the raw file string.
        handleFn(diagramSource)();
      };

      reader.onerror = (e) => {
        // Use the new JS FFI function to display file reading errors too
        displayError(`Error reading file: ${e.target.error.name}`)();
      };

      reader.readAsText(file);
    });
  };
};
