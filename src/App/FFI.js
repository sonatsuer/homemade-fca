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
        handleFn(diagramSource)();
      };

      reader.onerror = (e) => {
        displayError(`Error reading file: ${e.target.error.name}`)();
      };

      reader.readAsText(file, 'UTF-8');
    });
  };
};

const setupModalListeners = () => {
    const modalOverlay = document.getElementById("modal-overlay");

    if (modalOverlay) {
        modalOverlay.addEventListener("click", (e) => {
            if (e.target === modalOverlay) {
                document.getElementById("modal-overlay").style.display = "none";
            }
        });
    }
}

export const setGeneratedMermaid = function (content) {
  return function () {
    const copyButton = document.getElementById("copy-button");
    const modalTextarea = document.getElementById("modal-textarea");
    const modalOverlay = document.getElementById("modal-overlay");

    copyButton.disabled = false;
    document.getElementById("copy-status").textContent = "";

    if (!copyButton.hasClickListener) {
        setupModalListeners();

        copyButton.addEventListener("click", () => {
            modalTextarea.value = content;
            modalOverlay.style.display = "flex";
            setTimeout(() => {
                modalTextarea.focus();
                modalTextarea.select();
            }, 50);
        });
        copyButton.hasClickListener = true;
    };
  };
}
