async function renderTable(container) {
  const url = container.dataset.json; // read the data-json attribute
  try {
    const rows = await (await fetch(url)).json();
    if (!rows.length) return;

    const cols = Object.keys(rows[0]);

    const thead = `<thead><tr>${cols.map(c => `<th>${c}</th>`).join("")}</tr></thead>`;
    const tbody = `<tbody>${
      rows.map(r => `<tr>${cols.map(c => `<td>${formatCell(r[c])}</td>`).join("")}</tr>`).join("")
    }</tbody>`;

    container.querySelector("table").innerHTML = thead + tbody;

  } catch (err) {
    container.querySelector("table").innerHTML = `<tbody><tr><td>Error loading ${url}</td></tr></tbody>`;
    console.error("Failed to load JSON:", url, err);
  }

  function formatCell(v) {
    if (v == null) return "";
    if (typeof v === "number") return Number.isInteger(v) ? v : v.toFixed(3);
    return v;
  }
}

// find all .table-wrap blocks and render each one
document.addEventListener("DOMContentLoaded", () => {
  document.querySelectorAll(".table-wrap").forEach(renderTable);
});
