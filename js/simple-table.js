(async function () {
  // Find all .table-wrap blocks on the page
  const wraps = document.querySelectorAll(".table-wrap");
  for (const wrap of wraps) {
    const url = wrap.dataset.json;
    const table = wrap.querySelector("table.freeze2");

    if (!url || !table) continue;

    try {
      const rows = await (await fetch(url, { cache: "no-store" })).json();
      if (!Array.isArray(rows) || rows.length === 0) {
        table.innerHTML = "<tbody><tr><td>No data</td></tr></tbody>";
        continue;
      }

      // Columns from keys of first row (preserve order if already ordered)
      const cols = Object.keys(rows[0]);

      // Build <colgroup> so CSS can size columns cleanly (and avoid width creep)
      const colgroup = document.createElement("colgroup");
      cols.forEach((_, i) => {
        const col = document.createElement("col");
        if (i === 0) col.className = "col-1";
        if (i === 1) col.className = "col-2";
        colgroup.appendChild(col);
      });

      // Build thead
      const thead = document.createElement("thead");
      const htr = document.createElement("tr");
      cols.forEach((c, i) => {
        const th = document.createElement("th");
        th.textContent = c;
        if (i === 0) th.classList.add("sticky-col-1");
        if (i === 1) th.classList.add("sticky-col-2");
        th.title = c;
        htr.appendChild(th);
      });
      thead.appendChild(htr);

      // Build tbody
      const tbody = document.createElement("tbody");
      for (const r of rows) {
        const tr = document.createElement("tr");
        cols.forEach((c, i) => {
          const td = document.createElement("td");
          const v = r[c] ?? "";
          const s = typeof v === "object" ? JSON.stringify(v) : String(v);
          td.textContent = s;
          td.title = s;
          if (i === 0) td.classList.add("sticky-col-1");
          if (i === 1) td.classList.add("sticky-col-2");
          tr.appendChild(td);
        });
        tbody.appendChild(tr);
      }

      // Hydrate table
      table.innerHTML = "";               // clear
      table.appendChild(colgroup);
      table.appendChild(thead);
      table.appendChild(tbody);

      // After first paint, measure the first two column widths and set CSS vars
      // This ensures the second sticky column sits exactly after the first,
      // without passing any sizing params through HTML.
      requestAnimationFrame(() => {
        const firstCell = table.querySelector("tbody tr td:nth-child(1)") ||
                          table.querySelector("thead tr th:nth-child(1)");
        const secondCell = table.querySelector("tbody tr td:nth-child(2)") ||
                           table.querySelector("thead tr th:nth-child(2)");

        if (firstCell) {
          const w1 = Math.ceil(firstCell.getBoundingClientRect().width);
          wrap.style.setProperty("--col1", w1 + "px");
        }
        //// Use css to control the column width
        //if (secondCell) {
        //  const w2 = Math.ceil(secondCell.getBoundingClientRect().width);
        //  wrap.style.setProperty("--col2", w2 + "px");
        //}
      });

    } catch (err) {
      console.error("Failed to load JSON:", url, err);
      table.innerHTML = `<tbody><tr><td>Error loading ${url}</td></tr></tbody>`;
    }
  }
})();
