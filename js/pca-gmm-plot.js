// Main entry point
window.addEventListener("DOMContentLoaded", () => main());

async function main() {
  // ---- Load artifacts ----
  const scores = await (await fetch("artifacts/pca_scores_2D_ext.json")).json();
  const means  = await (await fetch("artifacts/gmm_means.json")).json();
  const covs   = await (await fetch("artifacts/gmm_covariances.json")).json();

  // ---- Coerce numeric types (defensive) ----
  scores.forEach(r => {
    r.Year    = +r.Year;
    r.PC1     = +r.PC1;
    r.PC2     = +r.PC2;
    r.Cluster = +r.Cluster;
  });
  means.forEach(m => {
    m.component = +m.component;
    m.PC1 = +m.PC1;
    m.PC2 = +m.PC2;
  });
  covs.forEach(c => {
    c.component = +c.component;
    c.PC1_PC1 = +c.PC1_PC1; c.PC1_PC2 = +c.PC1_PC2;
    c.PC2_PC1 = +c.PC2_PC1; c.PC2_PC2 = +c.PC2_PC2;
  });

  // ---- Helpers ----
  const years = [...new Set(scores.map(r => r.Year))].sort((a,b)=>a-b);
  const clustersAll = [...new Set(scores.map(r=>r.Cluster))].sort((a,b)=>a-b);
  const palette = ["#1f77b4","#ff7f0e","#2ca02c","#d62728",
                   "#9467bd","#8c564b","#e377c2","#7f7f7f",
                   "#bcbd22","#17becf"];

  // ellipse generator
  function ellipsePoints(mu, S, k=1, n=120){
    const a = S[0][0], b = S[0][1], c = S[1][1];
    const tr = a + c, det = a*c - b*b;
    const disc = Math.max(0, (tr*tr)/4 - det);
    const l1 = tr/2 + Math.sqrt(disc);
    const l2 = tr/2 - Math.sqrt(disc);

    let u1;
    if (Math.abs(b) < 1e-12) {
      u1 = (a >= c) ? [1,0] : [0,1];
    } else {
      const v1 = [b, l1 - a];
      const n1 = Math.hypot(v1[0], v1[1]) || 1;
      u1 = [v1[0]/n1, v1[1]/n1];
    }
    const u2 = [-u1[1], u1[0]];
    const r1 = Math.sqrt(Math.max(0,l1)) * k;
    const r2 = Math.sqrt(Math.max(0,l2)) * k;

    const xs=[], ys=[];
    for (let t=0; t<=n; t++){
      const th = 2*Math.PI*t/n;
      const x = mu[0] + r1*Math.cos(th)*u1[0] + r2*Math.sin(th)*u2[0];
      const y = mu[1] + r1*Math.cos(th)*u1[1] + r2*Math.sin(th)*u2[1];
      xs.push(x); ys.push(y);
    }
    return {x: xs, y: ys};
  }

  function tracesForYear(Y){
    const pts = scores.filter(r => r.Year === Y);

    // scatter per cluster
    const scatters = clustersAll.map(cl => {
      const S = pts.filter(r=>r.Cluster===cl);
      return {
        type: "scatter",
        mode: "markers+text",
        name: `Cluster ${cl}`,
        x: S.map(r=>r.PC1),
        y: S.map(r=>r.PC2),
        text: S.map(r=>r.Country),
        textposition: "top center",
        textfont: {size: 9},
        hovertemplate: "<b>%{text}</b><br>PC1=%{x:.2f}<br>PC2=%{y:.2f}<extra></extra>",
        marker: {
          size: 7, opacity: 0.9, line:{width:1},
          color: palette[(cl-1) % palette.length]
        }
      };
    });

    // ellipses from GMM params
    const ellipses = means.map(m=>{
      const C = covs.find(c=>c.component===m.component);
      const ptsE = ellipsePoints(
        [m.PC1, m.PC2],
        [[C.PC1_PC1, C.PC1_PC2],[C.PC2_PC1, C.PC2_PC2]],
        1.0, 180
      );
      return {
        type: "scatter",
        mode: "lines",
        x: ptsE.x, y: ptsE.y,
        name: `Comp ${m.component} (1σ)`,
        hoverinfo: "skip",
        line: {width: 2, color: "rgba(60,60,60,0.6)"},
        showlegend: false
      };
    });

    // GMM means
    const meanTrace = {
      type: "scatter",
      mode: "markers",
      x: means.map(m=>m.PC1),
      y: means.map(m=>m.PC2),
      marker: {symbol: "x", size: 10, line:{width:2}, color: "black"},
      name: "GMM means",
      hovertemplate: "μ: (%{x:.2f}, %{y:.2f})<extra></extra>"
    };

    return [...scatters, ...ellipses, meanTrace];
  }

  // ---- Layout & frames ----
  const layout = {
    title: "GMM on PCA (2D)",
    xaxis: {title:"PC1", zeroline:true},
    yaxis: {title:"PC2", zeroline:true, scaleanchor:"x", scaleratio:1},
    hovermode: "closest",
    legend: {orientation:"v"},
    margin: {l:60,r:20,t:60,b:60},
    updatemenus: [{
      type: "buttons", x: 0.02, y: 1.12, xanchor:"left",
      buttons: [
        {label:"Play",  method:"animate", args:[null, {fromcurrent:true, frame:{duration:700}, transition:{duration:0}}]},
        {label:"Pause", method:"animate", args:[[null], {mode:"immediate"}]}
      ]
    }],
    sliders: [{
      pad:{t:30}, currentvalue:{prefix:"Year: "},
      steps: years.map(y => ({
        label: y,
        method: "animate",
        args: [[String(y)], {mode:"immediate", transition:{duration:0}, frame:{duration:0}}]
      }))
    }]
  };

  const frames = years.map(y => ({name:String(y), data: tracesForYear(y)}));
  const init = tracesForYear(years[0]);

  Plotly.newPlot("plot", init, layout).then(()=>{
    Plotly.addFrames("plot", frames);
  });
}
