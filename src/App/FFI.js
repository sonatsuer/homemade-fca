import mermaid from "mermaid";

("use strict");

function isDarkMode() {
  return (
    window.matchMedia &&
    window.matchMedia("(prefers-color-scheme: dark)").matches
  );
}

const theme = isDarkMode() ? "dark" : "default";
mermaid.initialize({
  startOnLoad: false,
  theme: theme,
});

const exampleMermaid = `
graph BT
  Concept_3["\{Grow on Trees, Small Size, Tropical\}
\{Date\}"]
  Concept_1["\{Grow on Trees, Red Color, Small Size\}
\{Cherry\}"]
  Concept_0["\{Grow on Trees, Red Color, Small Size, Tropical\}
\{\}"]
  Concept_2["\{Grow on Trees, Red Color\}
\{Apple, Cherry\}"]
  Concept_5["\{Grow on Trees, Tropical\}
\{Banana, Date\}"]
  Concept_4["\{Grow on Trees, Small Size\}
\{Cherry, Date\}"]
  Concept_6["\{Grow on Trees\}
\{Apple, Banana, Cherry, Date\}"]
  Concept_3 --> Concept_0
  Concept_1 --> Concept_0
  Concept_2 --> Concept_1
  Concept_5 --> Concept_3
  Concept_4 --> Concept_1
  Concept_4 --> Concept_3
  Concept_6 --> Concept_2
  Concept_6 --> Concept_4
  Concept_6 --> Concept_5
`;
const exampleDiv = document.getElementById("mermaid-example");
if (exampleDiv) {
  mermaid
    .render("mermaid-example-temp-id", exampleMermaid)
    .then(({ svg }) => {
      exampleDiv.innerHTML = svg;
    })
    .catch((error) => {
      console.error("Example mermaid rendering failed:", error);
    });
} else {
  console.error("Target div #example-mermaid not found.");
}

export const renderAndDisplayCode = function (diagramSource) {
  return function () {
    const outputDiv = document.getElementById("mermaid-output");
    const errorDiv = document.getElementById("error-message");
    const codeDiv = document.getElementById("code-textarea");

    outputDiv.innerHTML = "Rendering...";
    errorDiv.innerHTML = "";
    if (codeDiv) {
      codeDiv.value = diagramSource;
    }
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
      window.location.hash = "#lattice-generator";
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

export const setupFileReadHandler = function (handleFn) {
  return function () {
    const fileInput = document.getElementById("csv-read");
    const errorDiv = document.getElementById("error-message");

    if (!fileInput) {
      console.error("File input #csv-read not found.");
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

      reader.readAsText(file, "UTF-8");
    });
  };
};
