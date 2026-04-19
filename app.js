// Estado Global
let rawData = [];
let filteredData = [];

// Elementos UI
const els = {
  spinner: document.getElementById('loading-screen'),
  main: document.getElementById('main-content'),
  info: document.getElementById('info-status'),
  bar: document.getElementById('loading-bar'),
  tolSlider: document.getElementById('filter-tol'),
  tolVal: document.getElementById('tol-val'),
  yrMin: document.getElementById('filter-year-min'),
  yrMax: document.getElementById('filter-year-max'),
  sexo: document.getElementById('filter-sexo'),
  area: document.getElementById('filter-area'),
  rama: document.getElementById('filter-rama'),
  ocup: document.getElementById('filter-ocup'),
  cate: document.getElementById('filter-cate')
};

// Utilidades
const formatGs = (num) => new Intl.NumberFormat('es-PY').format(Math.round(num));
const calcWMean = (data, valKey, wKey) => {
  let sumW = 0, sumV = 0;
  for(let i=0; i<data.length; i++) {
    const v = data[i][valKey], w = data[i][wKey];
    if(v !== null && !isNaN(v) && w > 0) { sumW += w; sumV += v * w; }
  }
  return sumW > 0 ? sumV / sumW : 0;
};
const calcWShare = (data, conditionFn, wKey) => {
  let sumW = 0, sumCond = 0;
  for(let i=0; i<data.length; i++) {
    const w = data[i][wKey];
    if(w > 0) {
      sumW += w;
      if(conditionFn(data[i])) sumCond += w;
    }
  }
  return sumW > 0 ? sumCond / sumW : 0;
};

// Carga Inicial
function init() {
  Papa.parse('data/db_app.csv', {
    download: true,
    header: true,
    dynamicTyping: true,
    skipEmptyLines: true,
    step: function(results, parser) {
      // Opcional: mostrar progreso si tuviéramos un Content-Length total (difícil sin server headers)
    },
    complete: function(results) {
      processData(results.data);
    }
  });
}

function processData(data) {
  els.info.textContent = "Aplicando filtros duros...";
  // Filtros duros: NAs, inactivos y menores
  rawData = data.filter(d => 
    d.edad >= 15 && 
    d.cate_pea !== 5 && 
    d.salario !== null && d.salario !== "" && !isNaN(d.salario) &&
    d.cotiza_bin !== null && d.cotiza_bin !== "" && !isNaN(d.cotiza_bin) &&
    d.ratio_sml !== null && d.ratio_sml !== "" && !isNaN(d.ratio_sml)
  );

  populateSelects(rawData);
  
  els.spinner.style.display = 'none';
  els.main.style.display = 'block';
  
  // Asignar listeners
  [els.tolSlider, els.yrMin, els.yrMax, els.sexo, els.area, els.rama, els.ocup, els.cate].forEach(el => {
    el.addEventListener('change', updateApp);
  });
  
  els.tolSlider.addEventListener('input', (e) => {
    els.tolVal.textContent = e.target.value;
  });

  updateApp();
}

function populateSelects(data) {
  const ramas = new Set(), ocups = new Set(), cates = new Set();
  data.forEach(d => {
    if(d.rama_pea) ramas.add(d.rama_pea);
    if(d.ocup_pea) ocups.add(d.ocup_pea);
    if(d.cate_pea) cates.add(d.cate_pea);
  });

  const addOptions = (el, set) => {
    Array.from(set).sort((a,b)=>a-b).forEach(v => {
      const opt = document.createElement('option');
      opt.value = v; opt.textContent = v;
      el.appendChild(opt);
    });
  };

  addOptions(els.rama, ramas);
  addOptions(els.ocup, ocups);
  addOptions(els.cate, cates);
}

function updateApp() {
  const tol = parseFloat(els.tolSlider.value) / 100;
  const yrMin = parseInt(els.yrMin.value);
  const yrMax = parseInt(els.yrMax.value);
  const sexo = els.sexo.value;
  const area = els.area.value;
  const rama = els.rama.value;
  const ocup = els.ocup.value;
  const cate = els.cate.value;

  const lo = 1 - tol;
  const hi = 1 + tol;

  filteredData = [];
  let sumWTotal = 0;

  for(let i=0; i<rawData.length; i++) {
    const d = rawData[i];
    if (d.anio < yrMin || d.anio > yrMax) continue;
    if (sexo !== "Todos" && d.sexo !== sexo) continue;
    if (area !== "Todos" && d.area_urb != area) continue;
    if (rama !== "Todos" && d.rama_pea != rama) continue;
    if (ocup !== "Todos" && d.ocup_pea != ocup) continue;
    if (cate !== "Todos" && d.cate_pea != cate) continue;

    // Calcular franja dinámicamente
    if (d.ratio_sml < lo) d.ingoc1sml_cat = "Menos de 1 SML";
    else if (d.ratio_sml > hi) d.ingoc1sml_cat = "Más de 1 SML";
    else d.ingoc1sml_cat = "1 SML";

    filteredData.push(d);
    sumWTotal += (d.w || 0);
  }

  els.info.textContent = `Registros: ${filteredData.length.toLocaleString('es-PY')} (N Poblacional: ${formatGs(sumWTotal)})`;

  updateKPIs(sumWTotal);
  drawPlots();
}

function updateKPIs(sumWTotal) {
  document.getElementById('kpi-n').textContent = formatGs(sumWTotal);
  document.getElementById('kpi-sal').textContent = formatGs(calcWMean(filteredData, 'salario', 'w'));
  const formalPct = calcWMean(filteredData, 'cotiza_bin', 'w') * 100;
  document.getElementById('kpi-formal').textContent = formalPct.toFixed(1) + "%";
  
  const share1Pct = calcWShare(filteredData, d => d.ingoc1sml_cat === "1 SML", 'w') * 100;
  document.getElementById('kpi-share1').textContent = share1Pct.toFixed(1) + "%";
}

const COL_SML = {
  "Menos de 1 SML": "#E74C3C", // Rojo
  "1 SML": "#F1C40F",          // Amarillo
  "Más de 1 SML": "#2ECC71"    // Verde
};

function groupData(data, groupKeys, aggFn) {
  const map = {};
  data.forEach(d => {
    const key = groupKeys.map(k => d[k]).join('|');
    if(!map[key]) map[key] = { items: [], wSum: 0 };
    map[key].items.push(d);
    map[key].wSum += (d.w || 0);
  });
  const res = [];
  for(let key in map) {
    const keys = key.split('|');
    const row = {};
    groupKeys.forEach((k, i) => row[k] = keys[i]);
    row.value = aggFn(map[key].items, map[key].wSum);
    res.push(row);
  }
  return res;
}

function drawPlots() {
  if(filteredData.length === 0) return;

  // Ordenamos los trimestres (anio + q)
  const trimestres = Array.from(new Set(filteredData.map(d => d.trimestredesc))).sort((a, b) => {
    // a = "T1 2022" -> sort string by year then T
    const pa = a.split(' '); const pb = b.split(' ');
    if (pa[1] !== pb[1]) return pa[1] - pb[1];
    return pa[0].localeCompare(pb[0]);
  });

  const franjas = ["Menos de 1 SML", "1 SML", "Más de 1 SML"];

  // 1. Distribución
  const distData = groupData(filteredData, ['trimestredesc', 'ingoc1sml_cat'], (items, wSum) => wSum);
  // Calcular % por trimestre
  const trimTotals = {};
  distData.forEach(d => { trimTotals[d.trimestredesc] = (trimTotals[d.trimestredesc]||0) + d.value; });
  distData.forEach(d => { d.pct = (d.value / trimTotals[d.trimestredesc]) * 100; });

  const tracesDist = franjas.map(franja => {
    return {
      x: trimestres,
      y: trimestres.map(t => { const fd = distData.find(d => d.trimestredesc===t && d.ingoc1sml_cat===franja); return fd ? fd.pct : 0; }),
      name: franja,
      type: 'bar',
      marker: { color: COL_SML[franja] }
    };
  });
  Plotly.newPlot('plot-dist', tracesDist, { barmode: 'stack', margin: {t:20, b:40, l:40, r:10}, legend: {orientation: 'h', y: -0.2} }, {responsive: true});

  // 2. Formalidad
  const formData = groupData(filteredData, ['trimestredesc', 'ingoc1sml_cat'], (items, wSum) => calcWMean(items, 'cotiza_bin', 'w') * 100);
  const tracesForm = franjas.map(franja => {
    return {
      x: trimestres,
      y: trimestres.map(t => { const fd = formData.find(d => d.trimestredesc===t && d.ingoc1sml_cat===franja); return fd ? fd.value : null; }),
      name: franja,
      type: 'scatter', mode: 'lines+markers',
      marker: { color: COL_SML[franja], size: 8 },
      line: { width: 3 }
    };
  });
  Plotly.newPlot('plot-formal', tracesForm, { margin: {t:20, b:40, l:40, r:10}, legend: {orientation: 'h', y: -0.2} }, {responsive: true});

  // 3. Salario promedio por género y franja
  const salData = groupData(filteredData, ['trimestredesc', 'ingoc1sml_cat', 'sexo'], (items, wSum) => calcWMean(items, 'salario', 'w'));
  const tracesSal = [];
  franjas.forEach(franja => {
    ['Hombres', 'Mujeres'].forEach(sexo => {
      const isHombre = sexo === 'Hombres';
      tracesSal.push({
        x: trimestres,
        y: trimestres.map(t => { const fd = salData.find(d => d.trimestredesc===t && d.ingoc1sml_cat===franja && d.sexo===sexo); return fd ? fd.value : null; }),
        name: `${franja} (${sexo})`,
        type: 'scatter', mode: 'lines+markers',
        line: { color: COL_SML[franja], dash: isHombre ? 'solid' : 'dot', width: isHombre ? 3 : 2 },
        marker: { symbol: isHombre ? 'circle' : 'diamond', size: 8 }
      });
    });
  });
  Plotly.newPlot('plot-sal', tracesSal, { margin: {t:20, b:40, l:60, r:10}, legend: {orientation: 'h', y: -0.2} }, {responsive: true});

}

// Iniciar aplicación
document.addEventListener('DOMContentLoaded', init);
