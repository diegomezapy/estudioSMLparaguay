// Estado Global
let rawData = [];
let filteredData = [];
let espiralData = [];
let espiralReady = false;
let espiralLoading = false;
let espiralControlsReady = false;
let contextoR01 = [];
let contextoR01Ready = false;
let contextoR01Loading = false;
const espiralCache = new Map();

// Diccionario de Variables (EPHC INE)
const DICT = {
  rama_pea: {
    1: "Agro/Pesca",
    2: "Manufactura",
    3: "Energía/Agua",
    4: "Construcción",
    5: "Comercio/Hoteles",
    6: "Transporte/Almacén",
    7: "Finanzas/Inmuebles",
    8: "Servicios",
    99: "NR"
  },
  ocup_pea: {
    1: "Poder Ejecutivo y Directivos",
    2: "Profesionales y Científicos",
    3: "Técnicos Nivel Medio",
    4: "Empleados de Oficina",
    5: "Servicios y Vendedores",
    6: "Trabajadores Agropecuarios",
    7: "Oficiales y Operarios",
    8: "Operadores de Maquinarias",
    9: "Trabajadores No Calificados",
    10: "Fuerzas Armadas",
    99: "No Especificado"
  },
  cate_pea: {
    1: "Obrero público",
    2: "Obrero privado",
    3: "Empleador/patrón",
    4: "Cuenta propia",
    5: "Trabajador fam. no remun.",
    6: "Doméstico/a",
    9: "NR"
  },
  dptorep: {
    0: "Asunción", 1: "Concepción", 2: "San Pedro", 3: "Cordillera", 4: "Guairá",
    5: "Caaguazú", 6: "Caazapá", 7: "Itapúa", 8: "Misiones", 9: "Paraguarí",
    10: "Alto Paraná", 11: "Central", 12: "Ñeembucú", 13: "Amambay", 14: "Canindeyú",
    15: "Presidente Hayes", 16: "Boquerón", 17: "Alto Paraguay"
  }
};

function getLabel(type, code) {
  if(code === "Todos") return "Todos";
  const num = parseInt(code);
  if(DICT[type] && DICT[type][num]) return `${num} - ${DICT[type][num]}`;
  return `${code}`;
}

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
  dpto: document.getElementById('filter-dpto'),
  rama: document.getElementById('filter-rama-group'),
  ocup: document.getElementById('filter-ocup-group'),
  cate: document.getElementById('filter-cate-group'),
  internet: document.getElementById('filter-internet'),
  agua: document.getElementById('filter-agua'),
  piso: document.getElementById('filter-piso')
};

// Utilidades
const formatGs = (num) => new Intl.NumberFormat('es-PY').format(Math.round(num));
const toNumber = (v) => {
  if (v === null || v === undefined || v === "") return null;
  const n = Number(v);
  return Number.isFinite(n) ? n : null;
};
const mean = (arr) => {
  if (!arr || arr.length === 0) return null;
  let s = 0;
  for (let i = 0; i < arr.length; i++) s += arr[i];
  return s / arr.length;
};
const sum = (arr) => {
  if (!arr || arr.length === 0) return null;
  let s = 0;
  for (let i = 0; i < arr.length; i++) s += arr[i];
  return s;
};
const clamp = (v, lo, hi) => Math.min(Math.max(v, lo), hi);
const formatPP = (v) => (v === null || !Number.isFinite(v) ? "-" : `${v >= 0 ? "+" : ""}${v.toFixed(2)} pp`);

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
  els.info.textContent = "Descargando datos...";
  Papa.parse('data/db_app.csv', {
    download: true,
    header: true,
    dynamicTyping: true,
    skipEmptyLines: true,
    error: function(err, file, inputElem, reason) {
      console.error(err);
      els.info.textContent = "Error de carga.";
      els.spinner.innerHTML = `
        <div class="text-center text-danger">
          <i class="fas fa-exclamation-triangle fa-3x mb-3"></i>
          <h4>Error al cargar datos</h4>
          <p>No se pudo descargar db_app.csv.</p>
          <p><small>Asegúrate de estar viendo la página en GitHub Pages (https://...) y no desde tu computadora (file://).</small></p>
        </div>`;
    },
    complete: function(results) {
      try {
        processData(results.data);
      } catch (e) {
        console.error("Error procesando datos:", e);
        els.info.textContent = "Error en procesamiento.";
        els.spinner.innerHTML = `
          <div class="text-center text-danger">
            <i class="fas fa-bug fa-3x mb-3"></i>
            <h4>Error interno</h4>
            <p>${e.message}</p>
          </div>`;
      }
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
  
  const infoCard = document.getElementById('info-card');
  if (infoCard) infoCard.style.display = 'none';
  els.spinner.remove(); // Elimina el cartel central por completo
  els.main.style.display = 'block';
  
  // Asignar listeners
  [els.tolSlider, els.yrMin, els.yrMax].forEach(el => {
    el.addEventListener('change', updateApp);
  });
  
  document.querySelectorAll('.filter-q').forEach(el => el.addEventListener('change', updateApp));
  document.querySelectorAll('input[type="radio"], input[type="checkbox"]').forEach(el => {
    if (el.id !== 'toggle-real' && !el.classList.contains('esp-ctx')) {
      el.addEventListener('change', updateApp);
    }
  });
  
  document.getElementById('toggle-real').addEventListener('change', updateApp);
  document.querySelectorAll('input[name="filter-area"]').forEach(el => el.addEventListener('change', updateApp));
  if(els.rama) els.rama.addEventListener('click', updateApp);
  if(els.ocup) els.ocup.addEventListener('click', updateApp);
  if(els.cate) els.cate.addEventListener('click', updateApp);
  if(els.internet) els.internet.addEventListener('change', updateApp);
  if(els.agua) els.agua.addEventListener('change', updateApp);
  if(els.piso) els.piso.addEventListener('change', updateApp);
  
  document.getElementById('btn-reset').addEventListener('click', () => {
    els.tolSlider.value = 10;
    els.tolVal.textContent = "10";
    els.yrMin.value = 2022;
    els.yrMax.value = 2025;
    document.querySelectorAll('.filter-q').forEach(el => el.checked = true);
    document.getElementById('sexo-todos').checked = true;
    document.getElementById('area-todos').checked = true;
    document.getElementById('rama-todos').checked = true;
    document.getElementById('ocup-todos').checked = true;
    document.getElementById('cate-todos').checked = true;
    document.getElementById('toggle-real').checked = false;
    updateApp();
  });

  els.tolSlider.addEventListener('input', (e) => {
    els.tolVal.textContent = e.target.value;
  });

  setupEspiralControls();
  setupEspiralTabResize();
  loadEspiralData();
  loadContextoR01Data();

  updateApp();
}

function populateSelects(data) {
  const ramas = new Set(), ocups = new Set(), cates = new Set();
  const dptos = new Set();
  
  window.geojsonData = null;
  fetch('data/departamentos.geojson').then(r => r.json()).then(d => window.geojsonData = d).catch(e => console.error("No se pudo cargar el geojson", e));

  data.forEach(d => {
    if(d.rama_pea) ramas.add(d.rama_pea);
    if(d.ocup_pea) ocups.add(d.ocup_pea);
    if(d.cate_pea) cates.add(d.cate_pea);
    if(d.dptorep !== undefined && d.dptorep !== null && d.dptorep !== "") dptos.add(d.dptorep);
  });

  els.dpto.innerHTML = '<option value="Todos">Todos los Departamentos</option>';
  const sortedDptos = Array.from(dptos).sort((a,b)=>a-b);
  sortedDptos.forEach(v => {
    let name = v;
    if (DICT.dptorep && DICT.dptorep[v]) name = DICT.dptorep[v];
    els.dpto.innerHTML += `<option value="${v}">${name}</option>`;
  });

  const addButtons = (el, set, type, name) => {
    Array.from(set).sort((a,b)=>a-b).forEach(v => {
      const input = document.createElement('input');
      input.type = 'radio'; input.className = 'btn-check';
      input.name = name; input.id = `${name}-${v}`; input.value = v;
      input.addEventListener('change', updateApp);
      
      const label = document.createElement('label');
      label.className = 'btn btn-outline-secondary btn-sm flex-fill text-start';
      label.htmlFor = `${name}-${v}`;
      label.textContent = getLabel(type, v);
      
      el.appendChild(input);
      el.appendChild(label);
    });
  };

  addButtons(els.rama, ramas, 'rama_pea', 'filter-rama');
  addButtons(els.ocup, ocups, 'ocup_pea', 'filter-ocup');
  addButtons(els.cate, cates, 'cate_pea', 'filter-cate');
}

function updateApp() {
  const tol = parseFloat(els.tolSlider.value) / 100;
  const yrMin = parseInt(els.yrMin.value);
  const yrMax = parseInt(els.yrMax.value);
  const sexo = document.querySelector('input[name="filter-sexo"]:checked').value;
  const area = document.querySelector('input[name="filter-area"]:checked').value;
  const dpto = els.dpto.value;
  
  const fInternet = els.internet ? els.internet.value : "Todos";
  const fAgua = els.agua ? els.agua.value : "Todos";
  const fPiso = els.piso ? els.piso.value : "Todos";
  
  const rama = document.querySelector('input[name="filter-rama"]:checked').value;
  const ocup = document.querySelector('input[name="filter-ocup"]:checked').value;
  const cate = document.querySelector('input[name="filter-cate"]:checked').value;
  
  const selectedQs = Array.from(document.querySelectorAll('.filter-q:checked')).map(el => parseInt(el.value));
  const isReal = document.getElementById('toggle-real').checked;

  const lo = 1 - tol;
  const hi = 1 + tol;

  filteredData = [];
  let sumWTotal = 0;

  for(let i=0; i<rawData.length; i++) {
    const d = rawData[i];
    if (d.anio < yrMin || d.anio > yrMax) continue;
    if (!selectedQs.includes(d.q)) continue;
    if (sexo !== "Todos" && d.sexo !== sexo) continue;
    if (area !== 'Todos' && String(d.area_urb) !== area) continue;
    if (dpto !== 'Todos' && String(d.dptorep) !== dpto) continue;
    if (rama !== 'Todos' && String(d.rama_pea) !== rama) continue;
    if (ocup !== "Todos" && String(d.ocup_pea) !== String(ocup)) continue;
    if (cate !== "Todos" && String(d.cate_pea) !== String(cate)) continue;

    // Filtros de Vivienda
    if (fInternet !== "Todos" && String(d.internet) !== fInternet) continue;
    if (fAgua !== "Todos" && String(d.agua_potable) !== fAgua) continue;
    if (fPiso !== "Todos" && String(d.piso_bueno) !== fPiso) continue;

    // Calcular variables en tiempo de ejecución (Real vs Nominal)
    d.salario_plot = isReal ? d.salario / (d.ipc / 100) : d.salario;
    d.sml_plot = isReal ? d.sml_real : d.sml;

    // Actualizar ratio_sml en base al SML
    d.ratio_sml_plot = d.salario_plot / d.sml_plot;

    // Calcular franja dinámicamente
    if (d.ratio_sml_plot < lo) d.ingoc1sml_cat = "Menos de 1 SML";
    else if (d.ratio_sml_plot > hi) d.ingoc1sml_cat = "Más de 1 SML";
    else d.ingoc1sml_cat = "1 SML";

    filteredData.push(d);
    sumWTotal += (d.w || 0);
  }

  els.info.textContent = `Registros: ${filteredData.length.toLocaleString('es-PY')} (N: ${formatGs(sumWTotal)})`;

  updateKPIs(sumWTotal);
  drawPlots();
  drawHousingPlots();
  updateEspiralDashboard();
  updateMetodologiaMetadata();
}

function updateKPIs(sumWTotal) {
  document.getElementById('kpi-n').textContent = formatGs(sumWTotal);
  
  const salGen = calcWMean(filteredData, 'salario_plot', 'w');
  document.getElementById('kpi-sal').textContent = formatGs(salGen);
  
  const formalPct = calcWMean(filteredData, 'cotiza_bin', 'w') * 100;
  document.getElementById('kpi-formal').textContent = formalPct.toFixed(1) + "%";
  
  const share1Pct = calcWShare(filteredData, d => d.ingoc1sml_cat === "1 SML", 'w') * 100;
  document.getElementById('kpi-share1').textContent = share1Pct.toFixed(1) + "%";

  // Brecha Salarial
  const salH = calcWMean(filteredData.filter(d => d.sexo === "Hombres"), 'salario_plot', 'w');
  const salM = calcWMean(filteredData.filter(d => d.sexo === "Mujeres"), 'salario_plot', 'w');
  if(salH > 0 && salM > 0) {
    const brecha = ((salH - salM) / salH) * 100;
    document.getElementById('kpi-brecha').textContent = brecha.toFixed(1) + "%";
  } else {
    document.getElementById('kpi-brecha').textContent = "-";
  }

  // Informalidad Crítica (<1 SML y sin IPS)
  const pctCritico = calcWShare(filteredData, d => d.ingoc1sml_cat === "Menos de 1 SML" && d.cotiza_bin === 0, 'w') * 100;
  document.getElementById('kpi-critico').textContent = pctCritico.toFixed(1) + "%";
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
  if(filteredData.length === 0) {
    Plotly.purge('plot-dist');
    Plotly.purge('plot-formal');
    Plotly.purge('plot-sal');
    Plotly.purge('plot-dens-rel-sml');
    document.getElementById('summary-table-body').innerHTML = '';
    return;
  }

  const trimestres = Array.from(new Set(filteredData.map(d => d.trimestredesc))).sort();
  const franjas = ["Menos de 1 SML", "1 SML", "Más de 1 SML"];
  const generos = ["Hombres", "Mujeres"];

  // 1. Distribución Segmentada por Género
  const distData = groupData(filteredData, ['trimestredesc', 'ingoc1sml_cat', 'sexo'], (items, wSum) => wSum);
  const trimSexoTotals = {};
  distData.forEach(d => {
    const key = d.trimestredesc + '|' + d.sexo;
    trimSexoTotals[key] = (trimSexoTotals[key]||0) + d.value;
  });
  distData.forEach(d => { 
    const key = d.trimestredesc + '|' + d.sexo;
    d.pct = trimSexoTotals[key] > 0 ? (d.value / trimSexoTotals[key]) * 100 : 0; 
  });

  const tracesDist = [];
  franjas.forEach(franja => {
    generos.forEach(sexo => {
      tracesDist.push({
        x: trimestres.map(t => `${t}<br>${sexo.charAt(0)}`),
        y: trimestres.map(t => { const fd = distData.find(d => d.trimestredesc===t && d.ingoc1sml_cat===franja && d.sexo===sexo); return fd ? fd.pct : 0; }),
        name: `${franja} (${sexo})`,
        type: 'bar',
        marker: { color: COL_SML[franja], opacity: sexo === 'Hombres' ? 1.0 : 0.6 }
      });
    });
  });
  Plotly.newPlot('plot-dist', tracesDist, { barmode: 'stack', margin: {t:20, b:60, l:40, r:10}, legend: {orientation: 'h', y: -0.3} }, {responsive: true});

  // 2. Formalidad
  const formData = groupData(filteredData, ['trimestredesc', 'ingoc1sml_cat', 'sexo'], (items, wSum) => calcWMean(items, 'cotiza_bin', 'w') * 100);
  const tracesForm = [];
  franjas.forEach(franja => {
    generos.forEach(sexo => {
      tracesForm.push({
        x: trimestres.map(t => `${t}<br>${sexo.charAt(0)}`),
        y: trimestres.map(t => { const fd = formData.find(d => d.trimestredesc===t && d.ingoc1sml_cat===franja && d.sexo===sexo); return fd ? fd.value : 0; }),
        name: `${franja} (${sexo})`,
        type: 'bar',
        marker: { color: COL_SML[franja], opacity: sexo === 'Hombres' ? 1.0 : 0.6 }
      });
    });
  });
  Plotly.newPlot('plot-formal', tracesForm, { barmode: 'group', margin: {t:20, b:60, l:40, r:10}, legend: {orientation: 'h', y: -0.3} }, {responsive: true});

  // 3. Salario Promedio
  const salData = groupData(filteredData, ['trimestredesc', 'sexo'], (items, wSum) => calcWMean(items, 'salario_plot', 'w'));
  
  const isReal = document.getElementById('toggle-real').checked;
  const smlName = isReal ? "Salario Mínimo Real (Base Dic 2021)" : "Salario Mínimo Nominal";
  
  const smlLine = {
    name: smlName,
    x: trimestres,
    y: trimestres.map(t => {
      const p = filteredData.find(d => d.trimestredesc === t);
      return p ? p.sml_plot : null;
    }),
    type: 'scatter', mode: 'lines', line: { color: '#7f8c8d', dash: 'dash' }
  };

  const tracesSal = generos.map(sexo => {
    return {
      x: trimestres,
      y: trimestres.map(t => { const fd = salData.find(d => d.trimestredesc===t && d.sexo===sexo); return fd ? fd.value : null; }),
      name: `Salario General (${sexo})`,
      type: 'scatter', mode: 'lines+markers',
      line: { color: sexo === 'Hombres' ? '#3498DB' : '#E74C3C', width: 4, shape: 'spline' },
      marker: { size: 10 }
    };
  });
  tracesSal.push(smlLine);
  Plotly.newPlot('plot-sal', tracesSal, { margin: {t:20, b:40, l:60, r:10}, legend: {orientation: 'h', y: -0.2} }, {responsive: true});

  // 4. Densidad relativa al SML (idea del Rmd)
  drawDensityRelSML();

  // ================= TABLA RESUMEN ==================
  drawTable(trimestres, generos);
  
  // ================= TAB DEMOGRAFIA ==================
  drawDemografia(filteredData, trimestres);
  
  // ================= TAB MAPAS ==================
  drawMap(filteredData);
}

function drawDensityRelSML() {
  const plotId = 'plot-dens-rel-sml';
  if (!document.getElementById(plotId)) return;

  const tol = parseFloat(els.tolSlider.value) / 100;
  const lo = 1 - tol;
  const hi = 1 + tol;

  const rows = filteredData
    .map(d => ({ x: d.ratio_sml_plot, w: d.w || 0 }))
    .filter(d => Number.isFinite(d.x) && d.x > 0 && d.x < 2.5 && d.w > 0);

  if (rows.length === 0) {
    renderEmptyPlot(plotId, "Distribución relativa al SML", "No hay datos válidos para la densidad.");
    return;
  }

  const inside = rows.filter(d => d.x >= lo && d.x <= hi);
  const outside = rows.filter(d => d.x < lo || d.x > hi);

  const traces = [
    {
      x: outside.map(d => d.x),
      y: outside.map(d => d.w),
      type: 'histogram',
      histfunc: 'sum',
      nbinsx: 80,
      name: 'Fuera de banda',
      marker: { color: '#9ecae1' },
      opacity: 0.85
    },
    {
      x: inside.map(d => d.x),
      y: inside.map(d => d.w),
      type: 'histogram',
      histfunc: 'sum',
      nbinsx: 80,
      name: 'Dentro de banda ±tol',
      marker: { color: '#2ca25f' },
      opacity: 0.90
    }
  ];

  Plotly.newPlot(plotId, traces, {
    barmode: 'overlay',
    title: { text: `Ingreso / SML con banda ±${Math.round(tol * 100)}%`, font: { size: 14 } },
    xaxis: { title: 'Ingreso relativo al SML', range: [0, 2.5] },
    yaxis: { title: 'Frecuencia ponderada' },
    shapes: [
      { type: 'rect', x0: lo, x1: hi, y0: 0, y1: 1, yref: 'paper', fillcolor: '#2ca25f', opacity: 0.08, line: { width: 0 } },
      { type: 'line', x0: 1, x1: 1, y0: 0, y1: 1, yref: 'paper', line: { color: '#1b7837', dash: 'dash', width: 2 } },
      { type: 'line', x0: lo, x1: lo, y0: 0, y1: 1, yref: 'paper', line: { color: '#1b7837', dash: 'dot', width: 1 } },
      { type: 'line', x0: hi, x1: hi, y0: 0, y1: 1, yref: 'paper', line: { color: '#1b7837', dash: 'dot', width: 1 } }
    ],
    margin: { t: 45, b: 45, l: 60, r: 20 },
    legend: { orientation: 'h', y: -0.2 }
  }, { responsive: true, displayModeBar: false });
}

function drawDemografia(data, trimestres) {
  // Función auxiliar para nivel educativo
  const getEducNivel = (anios) => {
    if (anios < 7) return "Educación Básica";
    if (anios < 13) return "Educación Media";
    return "Educación Superior";
  };
  
  const getEdadFranja = (edad) => {
    if (edad < 25) return "15-24 años";
    if (edad < 35) return "25-34 años";
    if (edad < 45) return "35-44 años";
    if (edad < 55) return "45-54 años";
    return "55+ años";
  };

  const educStats = {}; // {Trimestre: { "Educación Básica": { w:0, wSal:0 }... }}
  const edadStats = {}; // {Trimestre: { "15-24 años": w, ... }}
  
  data.forEach(d => {
    const t = d.trimestredesc;
    if (!educStats[t]) {
      educStats[t] = { "Educación Básica": {w:0, wSal:0}, "Educación Media": {w:0, wSal:0}, "Educación Superior": {w:0, wSal:0} };
      edadStats[t] = { "15-24 años": 0, "25-34 años": 0, "35-44 años": 0, "45-54 años": 0, "55+ años": 0 };
    }
    
    // Educacion
    if (d.educacion !== undefined && d.educacion !== null) {
      const niv = getEducNivel(d.educacion);
      educStats[t][niv].w += d.w;
      educStats[t][niv].wSal += d.salario_plot * d.w;
    }
    // Edad
    if (d.edad !== undefined && d.edad !== null) {
      const franjaE = getEdadFranja(d.edad);
      edadStats[t][franjaE] += d.w;
    }
  });

  const educLabels = ["Educación Básica", "Educación Media", "Educación Superior"];
  const colsEduc = ['#e67e22', '#f1c40f', '#2c3e50'];
  const franjasEdad = ["15-24 años", "25-34 años", "35-44 años", "45-54 años", "55+ años"];
  const colsEdad = ['#ff9999', '#66b3ff', '#99ff99', '#ffcc99', '#c2c2f0'];

  // Plot Distribución Nivel Educativo (líneas de tendencia % o absoluto)
  // Vamos a usar absoluto ponderado
  const tracesEducDist = educLabels.map((l, i) => {
    const ys = trimestres.map(t => educStats[t] ? educStats[t][l].w : null);
    return { name: l, x: trimestres, y: ys, type: 'scatter', mode: 'lines+markers', line: { color: colsEduc[i], shape: 'spline', width: 3 }, marker: {size: 6} };
  });
  Plotly.newPlot('plot-educacion', tracesEducDist, { margin: { t: 20, b: 40, l: 60, r: 20 } }, { responsive: true, displayModeBar: false });

  // Plot Salario por Nivel Educativo
  const tracesEducSal = educLabels.map((l, i) => {
    const ys = trimestres.map(t => educStats[t] && educStats[t][l].w > 0 ? (educStats[t][l].wSal / educStats[t][l].w) : null);
    return { name: l, x: trimestres, y: ys, type: 'scatter', mode: 'lines+markers', line: { color: colsEduc[i], shape: 'spline', width: 3 }, marker: {size: 6} };
  });
  Plotly.newPlot('plot-sal-educacion', tracesEducSal, {
    margin: { t: 20, b: 40, l: 60, r: 20 }, yaxis: { tickformat: ',.0f' }
  }, { responsive: true, displayModeBar: false });

  // Plot Franjas de Edad
  const tracesEdad = franjasEdad.map((fe, i) => {
    const ys = trimestres.map(t => edadStats[t] ? edadStats[t][fe] : null);
    return { name: fe, x: trimestres, y: ys, type: 'scatter', mode: 'lines+markers', line: { color: colsEdad[i], shape: 'spline', width: 3 }, marker: {size: 6} };
  });
  Plotly.newPlot('plot-edad', tracesEdad, {
    margin: { t: 20, b: 40, l: 60, r: 20 }
  }, { responsive: true, displayModeBar: false });
}

function drawMap(data) {
  Plotly.purge('plot-mapa');
  const dptoStats = {};
  data.forEach(d => {
    if (d.dptorep === undefined || d.dptorep === null) return;
    const dp = String(d.dptorep);
    if (!dptoStats[dp]) dptoStats[dp] = { w:0, wSal:0 };
    dptoStats[dp].w += d.w;
    dptoStats[dp].wSal += d.salario * d.w;
  });

  const locations = [];
  const z = [];
  const text = [];
  
  // Enlazar con GeoJSON (NAME_1)
  if (window.geojsonData && window.geojsonData.features) {
    window.geojsonData.features.forEach(f => {
      const name = f.properties.NAME_1;
      // Buscar id de diccionario
      let dptoId = null;
      for (const [id, label] of Object.entries(DICT.dptorep)) {
        if (name.includes(label) || label.includes(name)) {
          dptoId = id; break;
        }
      }
      
      locations.push(name);
      if (dptoId && dptoStats[dptoId] && dptoStats[dptoId].w > 0) {
        const avg = dptoStats[dptoId].wSal / dptoStats[dptoId].w;
        z.push(avg);
        text.push(`${name}<br>Salario Promedio: ${avg.toLocaleString('es-ES', {maximumFractionDigits:0})} Gs.<br>Trabajadores ponderados: ${dptoStats[dptoId].w.toLocaleString('es-ES', {maximumFractionDigits:0})}`);
      } else {
        z.push(null);
        text.push(`${name}<br>Sin datos`);
      }
    });

    const trace = {
      type: "choropleth",
      geojson: window.geojsonData,
      locations: locations,
      featureidkey: "properties.NAME_1",
      z: z,
      text: text,
      hoverinfo: "text",
      colorscale: "Viridis",
      marker: { opacity: 0.9, line: { width: 0.5, color: "white" } }
    };

    const layout = {
      geo: {
        fitbounds: "locations",
        visible: false,
        projection: { type: "mercator" }
      },
      margin: { t: 0, b: 0, l: 0, r: 0 }
    };

    Plotly.newPlot('plot-mapa', [trace], layout, { responsive: true, displayModeBar: false });
  } else {
    // Retry in 500ms
    setTimeout(updateApp, 500);
  }
}

function drawTable(trimestres, generos) {
  const tbody = document.getElementById('summary-table-body');
  let html = '';
  const franjas = ["Menos de 1 SML", "1 SML", "Más de 1 SML"];
  
  generos.forEach(sexo => {
    let rowSpanAdded = false;
    franjas.forEach(franja => {
      const items = filteredData.filter(d => d.sexo === sexo && d.ingoc1sml_cat === franja);
      let sumW = 0; items.forEach(d => sumW += (d.w||0));
      const sal = calcWMean(items, 'salario_plot', 'w');
      const form = calcWMean(items, 'cotiza_bin', 'w') * 100;
      
      html += `<tr>`;
      if(!rowSpanAdded) {
        html += `<td rowspan="${franjas.length}" class="fw-bold align-middle">${sexo}</td>`;
        rowSpanAdded = true;
      }
      html += `
        <td><span style="display:inline-block; width:12px; height:12px; background-color:${COL_SML[franja]}; margin-right:5px; border-radius:2px;"></span>${franja}</td>
        <td class="text-end">${formatGs(sumW)}</td>
        <td class="text-end">${formatGs(sal)}</td>
        <td class="text-end">${form.toFixed(1)}%</td>
      </tr>`;
    });
  });
  tbody.innerHTML = html;
}

function drawHousingPlots() {
  const trimestres = Array.from(new Set(filteredData.map(d => `${d.anio}-Q${d.q}`))).sort();
  const franjas = ["Menos de 1 SML", "1 SML", "Más de 1 SML"];
  const cols = ["#808080", "#2ca02c", "#1f77b4"]; // Gris, Verde, Azul
  const tol = parseFloat(els.tolVal.textContent) / 100;

  // Initialize accumulators
  const stats = {};
  trimestres.forEach(t => {
    stats[t] = {};
    franjas.forEach(f => {
      stats[t][f] = { internet: 0, agua: 0, piso: 0, w: 0 };
    });
  });

  filteredData.forEach(d => {
    const t = `${d.anio}-Q${d.q}`;
    let cat = "";
    const r = d.ratio_sml;
    if (r >= (1 - tol)) {
      if (r <= (1 + tol)) cat = "1 SML";
      else cat = "Más de 1 SML";
    } else {
      cat = "Menos de 1 SML";
    }
    
    if (d.w > 0) {
      stats[t][cat].w += d.w;
      if (d.internet === 1) stats[t][cat].internet += d.w;
      if (d.agua_potable === 1) stats[t][cat].agua += d.w;
      if (d.piso_bueno === 1) stats[t][cat].piso += d.w;
    }
  });

  const getTraces = (indicatorKey) => {
    return franjas.map((fe, i) => {
      const ys = trimestres.map(t => {
        const s = stats[t][fe];
        return s.w > 0 ? (s[indicatorKey] / s.w) * 100 : null;
      });
      return { 
        name: fe, 
        x: trimestres, 
        y: ys, 
        type: 'scatter', 
        mode: 'lines+markers', 
        line: { color: cols[i], shape: 'spline', width: 3 }, 
        marker: {size: 6} 
      };
    });
  };

  const layoutTpl = (title) => ({
    title: { text: title, font: { size: 14 } },
    yaxis: { title: "% de Trabajadores", rangemode: 'tozero', tickformat: '.0f' },
    margin: { t: 40, b: 40, l: 60, r: 20 },
    legend: { orientation: 'h', y: -0.2 }
  });

  if(document.getElementById('plot-internet')) {
    Plotly.newPlot('plot-internet', getTraces('internet'), layoutTpl('Acceso a Internet en el Hogar (%)'), { responsive: true, displayModeBar: false });
  }
  if(document.getElementById('plot-agua')) {
    Plotly.newPlot('plot-agua', getTraces('agua'), layoutTpl('Vivienda con Agua Segura (%)'), { responsive: true, displayModeBar: false });
  }
  if(document.getElementById('plot-piso')) {
    Plotly.newPlot('plot-piso', getTraces('piso'), layoutTpl('Vivienda con Pisos de Materiales Aptos (%)'), { responsive: true, displayModeBar: false });
  }
}

function setupEspiralControls() {
  if (espiralControlsReady) return;
  const hEl = document.getElementById('esp-h');
  const minAdjEl = document.getElementById('esp-min-adj');
  const overlapEl = document.getElementById('esp-overlap');
  const repsEl = document.getElementById('esp-placebo-reps');
  const modeEl = document.getElementById('esp-mode');
  const storyStepEl = document.getElementById('esp-story-step');
  if (!hEl || !minAdjEl || !overlapEl || !repsEl || !modeEl || !storyStepEl) return;

  espiralControlsReady = true;

  hEl.addEventListener('input', () => {
    syncEspiralLabels();
    updateEspiralDashboard();
  });
  minAdjEl.addEventListener('input', () => {
    syncEspiralLabels();
    updateEspiralDashboard();
  });
  overlapEl.addEventListener('change', updateEspiralDashboard);

  repsEl.addEventListener('change', () => {
    const clean = clamp(parseInt(repsEl.value || "1000", 10), 200, 5000);
    repsEl.value = clean;
    updateEspiralDashboard();
  });

  modeEl.addEventListener('change', () => {
    setEspiralStoryPresetIfNeeded();
    syncEspiralLabels();
    updateEspiralDashboard();
  });

  storyStepEl.addEventListener('input', () => {
    setEspiralStoryPresetIfNeeded();
    syncEspiralLabels();
    updateEspiralDashboard();
  });

  document.querySelectorAll('.esp-ctx').forEach(el => {
    el.addEventListener('change', updateEspiralDashboard);
  });

  setEspiralStoryPresetIfNeeded();
  syncEspiralLabels();
}

function setupEspiralTabResize() {
  const tabBtn = document.getElementById('espiral-tab');
  if (!tabBtn || tabBtn.dataset.resizeBound === "1") return;
  tabBtn.dataset.resizeBound = "1";
  tabBtn.addEventListener('shown.bs.tab', () => {
    ['esp-plot-event', 'esp-plot-placebo', 'esp-plot-sim', 'esp-plot-context'].forEach(id => {
      const el = document.getElementById(id);
      if (el && typeof Plotly !== "undefined") Plotly.Plots.resize(el);
    });
  });
}

function syncEspiralLabels() {
  const hEl = document.getElementById('esp-h');
  const minAdjEl = document.getElementById('esp-min-adj');
  const storyStepEl = document.getElementById('esp-story-step');
  const hVal = document.getElementById('esp-h-val');
  const minAdjVal = document.getElementById('esp-min-adj-val');
  const stepVal = document.getElementById('esp-story-step-val');
  if (hEl && hVal) hVal.textContent = hEl.value;
  if (minAdjEl && minAdjVal) minAdjVal.textContent = minAdjEl.value;
  if (storyStepEl && stepVal) stepVal.textContent = storyStepEl.value;
}

function setEspiralStoryPresetIfNeeded() {
  const modeEl = document.getElementById('esp-mode');
  const stepEl = document.getElementById('esp-story-step');
  const hEl = document.getElementById('esp-h');
  const minAdjEl = document.getElementById('esp-min-adj');
  const overlapEl = document.getElementById('esp-overlap');
  if (!modeEl || !stepEl || !hEl || !minAdjEl || !overlapEl) return;

  const inPresentation = modeEl.value === "presentacion";
  [hEl, minAdjEl, overlapEl].forEach(el => el.disabled = inPresentation);
  if (!inPresentation) return;

  const presets = {
    1: { h: 3, minAdj: 0, overlap: "all", ctx: ["pct_1sml"] },
    2: { h: 6, minAdj: 0, overlap: "all", ctx: ["pct_1sml", "pct_cotiza"] },
    3: { h: 6, minAdj: 3, overlap: "clean", ctx: ["pct_1sml", "pct_cotiza"] },
    4: { h: 9, minAdj: 4, overlap: "clean", ctx: ["pct_1sml", "pct_cotiza", "pct_hogares_internet", "pct_hogares_material_apto"] },
    5: { h: 12, minAdj: 0, overlap: "clean", ctx: ["pct_1sml", "pct_cotiza", "pct_hogares_internet", "pct_hogares_material_apto", "pct_hogares_movilidad"] }
  };

  const step = clamp(parseInt(stepEl.value || "1", 10), 1, 5);
  const p = presets[step] || presets[1];
  hEl.value = p.h;
  minAdjEl.value = p.minAdj;
  overlapEl.value = p.overlap;
  document.querySelectorAll('.esp-ctx').forEach(chk => {
    chk.checked = p.ctx.includes(chk.value);
  });
}

function getEspiralParams() {
  const hEl = document.getElementById('esp-h');
  const minAdjEl = document.getElementById('esp-min-adj');
  const overlapEl = document.getElementById('esp-overlap');
  const repsEl = document.getElementById('esp-placebo-reps');
  const modeEl = document.getElementById('esp-mode');
  const stepEl = document.getElementById('esp-story-step');
  const contextSeries = Array.from(document.querySelectorAll('.esp-ctx:checked')).map(el => el.value);
  return {
    h: clamp(parseInt(hEl ? hEl.value : "6", 10), 2, 12),
    minAdj: clamp(parseFloat(minAdjEl ? minAdjEl.value : "0"), 0, 20),
    overlap: overlapEl ? overlapEl.value : "all",
    reps: clamp(parseInt(repsEl ? repsEl.value : "1000", 10), 200, 5000),
    mode: modeEl ? modeEl.value : "analitico",
    step: clamp(parseInt(stepEl ? stepEl.value : "1", 10), 1, 5),
    contextSeries
  };
}

function setEspiralStatusMessage(msg, isError = false) {
  const el = document.getElementById('esp-story-text');
  if (!el) return;
  el.classList.toggle('text-danger', isError);
  el.classList.toggle('text-muted', !isError);
  el.textContent = msg;
}

function loadEspiralData() {
  if (espiralReady || espiralLoading) return;
  espiralLoading = true;
  setEspiralStatusMessage("Cargando serie macro de IPC y ajustes de salario mínimo...");

  Papa.parse('data/espiral_salario_precios.csv', {
    download: true,
    header: true,
    dynamicTyping: true,
    skipEmptyLines: true,
    error: function(err) {
      espiralLoading = false;
      console.error("No se pudo cargar espiral_salario_precios.csv:", err);
      setEspiralStatusMessage("No se pudo cargar la base de espiral (data/espiral_salario_precios.csv).", true);
    },
    complete: function(results) {
      espiralLoading = false;
      const parsed = (results.data || []).map(row => {
        const d = row && row.fecha ? new Date(`${row.fecha}T00:00:00`) : null;
        if (!d || Number.isNaN(d.getTime())) return null;
        const year = d.getFullYear();
        const q = Math.floor(d.getMonth() / 3) + 1;
        return {
          fecha: d,
          periodo: row.periodo || "",
          ajuste_pp: toNumber(row.ajuste_pp),
          ipc_general_m: toNumber(row.ipc_general_m),
          ipc_alim_m: toNumber(row.ipc_alim_m),
          quarter: `${year}-Q${q}`
        };
      }).filter(Boolean).sort((a, b) => a.fecha - b.fecha);

      espiralData = parsed;
      espiralReady = parsed.length > 0;
      espiralCache.clear();

      if (!espiralReady) {
        setEspiralStatusMessage("La base de espiral se cargó vacía.", true);
        updateMetodologiaMetadata();
        return;
      }
      updateEspiralDashboard();
      updateMetodologiaMetadata();
    }
  });
}

function loadContextoR01Data() {
  if (contextoR01Ready || contextoR01Loading) return;
  contextoR01Loading = true;

  Papa.parse('data/contexto_r01.csv', {
    download: true,
    header: true,
    dynamicTyping: true,
    skipEmptyLines: true,
    error: function(err) {
      contextoR01Loading = false;
      console.warn("No se pudo cargar data/contexto_r01.csv:", err);
    },
    complete: function(results) {
      contextoR01Loading = false;
      contextoR01 = (results.data || []).map(row => {
        const year = parseInt(row.year, 10);
        const quarter = parseInt(row.quarter, 10);
        const trimestredesc = row.trimestredesc || (Number.isFinite(year) && Number.isFinite(quarter) ? `${year}Trim${quarter}` : null);
        if (!trimestredesc) return null;
        return {
          trimestredesc,
          pct_hogares_internet: toNumber(row.pct_hogares_internet),
          pct_hogares_material_apto: toNumber(row.pct_hogares_material_apto),
          pct_hogares_movilidad: toNumber(row.pct_hogares_movilidad),
          n_hogares: toNumber(row.n_hogares)
        };
      }).filter(Boolean);
      contextoR01Ready = contextoR01.length > 0;
      updateEspiralDashboard();
      updateMetodologiaMetadata();
    }
  });
}

function updateMetodologiaMetadata() {
  const r02El = document.getElementById('met-r02-cov');
  const r01El = document.getElementById('met-r01-cov');
  const espEl = document.getElementById('met-esp-cov');
  const evtEl = document.getElementById('met-events');
  if (!r02El || !r01El || !espEl || !evtEl) return;

  if (rawData.length > 0) {
    const years = rawData.map(d => parseInt(d.anio, 10)).filter(v => Number.isFinite(v));
    const minY = years.length ? Math.min(...years) : null;
    const maxY = years.length ? Math.max(...years) : null;
    const trimSet = new Set(rawData.map(d => `${d.anio}Trim${d.q}`));
    r02El.textContent = minY && maxY ? `${minY}Trim1 a ${maxY}Trim4 (${trimSet.size} trimestres)` : `${trimSet.size} trimestres`;
  } else {
    r02El.textContent = "Sin datos";
  }

  if (contextoR01.length > 0) {
    const keys = contextoR01.map(d => d.trimestredesc).filter(Boolean).sort((a, b) => {
      const ma = a.match(/^(\d{4})Trim(\d)$/);
      const mb = b.match(/^(\d{4})Trim(\d)$/);
      if (!ma || !mb) return a.localeCompare(b);
      const ya = parseInt(ma[1], 10), qa = parseInt(ma[2], 10);
      const yb = parseInt(mb[1], 10), qb = parseInt(mb[2], 10);
      return ya === yb ? qa - qb : ya - yb;
    });
    r01El.textContent = `${keys[0]} a ${keys[keys.length - 1]} (${keys.length} trimestres)`;
  } else if (contextoR01Loading) {
    r01El.textContent = "Cargando...";
  } else {
    r01El.textContent = "No disponible";
  }

  if (espiralData.length > 0) {
    const minD = espiralData[0].fecha;
    const maxD = espiralData[espiralData.length - 1].fecha;
    const fmt = (d) => d.toISOString().slice(0, 7);
    espEl.textContent = `${fmt(minD)} a ${fmt(maxD)} (${espiralData.length} meses)`;
    const nEvents = espiralData.filter(d => d.ajuste_pp !== null && d.ajuste_pp > 0).length;
    evtEl.textContent = `${nEvents} eventos`;
  } else if (espiralLoading) {
    espEl.textContent = "Cargando...";
    evtEl.textContent = "Cargando...";
  } else {
    espEl.textContent = "No disponible";
    evtEl.textContent = "No disponible";
  }
}

function computeEspiralMetric(index, h) {
  if (index - h < 0 || index + h >= espiralData.length) return null;
  const row0 = espiralData[index];
  if (!row0) return null;

  const preG = [];
  const postG = [];
  const preF = [];
  const postF = [];
  const relGeneral = [];
  const relFood = [];

  for (let k = -h; k <= h; k++) {
    const r = espiralData[index + k];
    const g = toNumber(r ? r.ipc_general_m : null);
    const f = toNumber(r ? r.ipc_alim_m : null);
    relGeneral.push(g);
    relFood.push(f);
    if (k < 0) {
      if (g !== null) preG.push(g);
      if (f !== null) preF.push(f);
    }
    if (k > 0) {
      if (g !== null) postG.push(g);
      if (f !== null) postF.push(f);
    }
  }

  const preMeanG = mean(preG);
  const preMeanF = mean(preF);
  const postMeanG = mean(postG);
  const postMeanF = mean(postF);

  return {
    date: row0.fecha,
    ajuste: toNumber(row0.ajuste_pp),
    eiGeneral: preMeanG === null || postMeanG === null ? null : (postMeanG - preMeanG),
    eiFood: preMeanF === null || postMeanF === null ? null : (postMeanF - preMeanF),
    ctGeneral: preG.length === 0 || postG.length === 0 ? null : (sum(postG) - sum(preG)),
    ctFood: preF.length === 0 || postF.length === 0 ? null : (sum(postF) - sum(preF)),
    relGeneral: relGeneral.map(v => (v === null || preMeanG === null ? null : (v - preMeanG))),
    relFood: relFood.map(v => (v === null || preMeanF === null ? null : (v - preMeanF)))
  };
}

function seededRng(seed) {
  let t = seed >>> 0;
  return function() {
    t += 0x6D2B79F5;
    let r = Math.imul(t ^ (t >>> 15), 1 | t);
    r ^= r + Math.imul(r ^ (r >>> 7), 61 | r);
    return ((r ^ (r >>> 14)) >>> 0) / 4294967296;
  };
}

function sampleWithoutReplacement(pool, n, rng) {
  if (n >= pool.length) return pool.slice();
  const copy = pool.slice();
  for (let i = 0; i < n; i++) {
    const j = i + Math.floor(rng() * (copy.length - i));
    const tmp = copy[i];
    copy[i] = copy[j];
    copy[j] = tmp;
  }
  return copy.slice(0, n);
}

function fitLine(xs, ys) {
  if (!xs || xs.length === 0 || xs.length !== ys.length) return null;
  if (xs.length === 1) return { a: ys[0], b: 0 };
  const mx = mean(xs);
  const my = mean(ys);
  let cov = 0;
  let varX = 0;
  for (let i = 0; i < xs.length; i++) {
    const dx = xs[i] - mx;
    cov += dx * (ys[i] - my);
    varX += dx * dx;
  }
  if (varX === 0) return { a: my, b: 0 };
  const b = cov / varX;
  return { a: my - b * mx, b };
}

function computeEspiralResults(params) {
  if (!espiralReady || espiralData.length === 0) return null;

  const cacheKey = [params.h, params.minAdj.toFixed(1), params.overlap, params.reps].join("|");
  if (espiralCache.has(cacheKey)) return espiralCache.get(cacheKey);

  const n = espiralData.length;
  const allEventIdx = [];
  const selectedEventIdx = [];
  for (let i = params.h; i < n - params.h; i++) {
    const adj = toNumber(espiralData[i].ajuste_pp);
    if (adj !== null && adj > 0) allEventIdx.push(i);
    if (adj !== null && adj >= params.minAdj) selectedEventIdx.push(i);
  }

  let cleanEventIdx = selectedEventIdx;
  if (params.overlap === "clean") {
    cleanEventIdx = selectedEventIdx.filter(i => selectedEventIdx.every(j => i === j || Math.abs(i - j) > params.h));
  }

  const metricMemo = new Map();
  const getMetric = (idx) => {
    if (!metricMemo.has(idx)) metricMemo.set(idx, computeEspiralMetric(idx, params.h));
    return metricMemo.get(idx);
  };

  const events = cleanEventIdx.map(getMetric).filter(Boolean);
  const ks = Array.from({ length: params.h * 2 + 1 }, (_, i) => i - params.h);

  const eiGeneral = mean(events.map(e => e.eiGeneral).filter(v => v !== null));
  const eiFood = mean(events.map(e => e.eiFood).filter(v => v !== null));
  const ctGeneral = mean(events.map(e => e.ctGeneral).filter(v => v !== null));
  const ctFood = mean(events.map(e => e.ctFood).filter(v => v !== null));

  const relGeneral = ks.map((_, idx) => mean(events.map(e => e.relGeneral[idx]).filter(v => v !== null)));
  const relFood = ks.map((_, idx) => mean(events.map(e => e.relFood[idx]).filter(v => v !== null)));

  const realEventSet = new Set(allEventIdx);
  const candidates = [];
  for (let i = params.h; i < n - params.h; i++) {
    if (realEventSet.has(i)) continue;
    if (params.overlap === "clean" && allEventIdx.some(ev => Math.abs(ev - i) <= params.h)) continue;
    candidates.push(i);
  }

  const placeboDist = [];
  const drawSize = Math.min(events.length, candidates.length);
  if (drawSize > 0) {
    const rng = seededRng(20260423 + params.h * 11 + Math.round(params.minAdj * 10) * 31 + (params.overlap === "clean" ? 73 : 41));
    for (let r = 0; r < params.reps; r++) {
      const picked = sampleWithoutReplacement(candidates, drawSize, rng);
      const vals = picked.map(getMetric).filter(Boolean).map(m => m.eiGeneral).filter(v => v !== null);
      if (vals.length > 0) placeboDist.push(mean(vals));
    }
  }

  const pValueGeneral = placeboDist.length > 0 && eiGeneral !== null
    ? (placeboDist.filter(v => Math.abs(v) >= Math.abs(eiGeneral)).length / placeboDist.length)
    : null;

  const fitSample = events.filter(e => e.ajuste !== null && e.eiGeneral !== null && e.eiFood !== null);
  const xs = fitSample.map(e => e.ajuste);
  const yg = fitSample.map(e => e.eiGeneral);
  const yf = fitSample.map(e => e.eiFood);
  const fitG = fitLine(xs, yg);
  const fitF = fitLine(xs, yf);
  const simLevels = [3, 5, 8, 12, 15];
  const simScenarios = simLevels.map(level => ({
    adj: level,
    general: fitG ? (fitG.a + fitG.b * level) : null,
    food: fitF ? (fitF.a + fitF.b * level) : null
  }));

  const result = {
    events,
    ks,
    relGeneral,
    relFood,
    eiGeneral,
    eiFood,
    ctGeneral,
    ctFood,
    placeboDist,
    pValueGeneral,
    simScenarios
  };
  espiralCache.set(cacheKey, result);
  return result;
}

function buildEspiralContextRows() {
  if (filteredData.length === 0) return [];

  const labor = {};
  filteredData.forEach(d => {
    const key = `${d.anio}Trim${d.q}`;
    if (!labor[key]) labor[key] = { w: 0, w1sml: 0, wCot: 0 };
    const w = d.w || 0;
    if (w <= 0) return;
    labor[key].w += w;
    if (d.ingoc1sml_cat === "1 SML") labor[key].w1sml += w;
    if (d.cotiza_bin === 1) labor[key].wCot += w;
  });

  const r01Map = {};
  contextoR01.forEach(r => {
    r01Map[r.trimestredesc] = r;
  });

  const macro = {};
  espiralData.forEach(r => {
    if (r.ipc_general_m === null || !r.fecha) return;
    const y = r.fecha.getFullYear();
    const q = Math.floor(r.fecha.getMonth() / 3) + 1;
    const key = `${y}Trim${q}`;
    if (!macro[key]) macro[key] = { s: 0, n: 0 };
    macro[key].s += r.ipc_general_m;
    macro[key].n += 1;
  });

  const keys = Object.keys(labor).sort((a, b) => {
    const ma = a.match(/^(\d{4})Trim(\d)$/);
    const mb = b.match(/^(\d{4})Trim(\d)$/);
    if (!ma || !mb) return a.localeCompare(b);
    const ya = parseInt(ma[1], 10), qa = parseInt(ma[2], 10);
    const yb = parseInt(mb[1], 10), qb = parseInt(mb[2], 10);
    return ya === yb ? qa - qb : ya - yb;
  });

  return keys.map(k => ({
    quarter: k,
    pct_1sml: labor[k].w > 0 ? (labor[k].w1sml / labor[k].w) * 100 : null,
    pct_cotiza: labor[k].w > 0 ? (labor[k].wCot / labor[k].w) * 100 : null,
    pct_hogares_internet: r01Map[k] ? r01Map[k].pct_hogares_internet : null,
    pct_hogares_material_apto: r01Map[k] ? r01Map[k].pct_hogares_material_apto : null,
    pct_hogares_movilidad: r01Map[k] ? r01Map[k].pct_hogares_movilidad : null,
    ipc_general_q: macro[k] && macro[k].n > 0 ? (macro[k].s / macro[k].n) : null
  }));
}

function renderEmptyPlot(id, title, message) {
  Plotly.newPlot(id, [], {
    title: { text: title, font: { size: 14 } },
    xaxis: { visible: false },
    yaxis: { visible: false },
    annotations: [{
      x: 0.5, y: 0.5, xref: "paper", yref: "paper",
      text: message, showarrow: false, font: { size: 13, color: "#6c757d" }
    }],
    margin: { t: 40, b: 20, l: 20, r: 20 }
  }, { responsive: true, displayModeBar: false });
}

function renderEspiralEventPlot(results) {
  if (!results || results.events.length === 0) {
    renderEmptyPlot('esp-plot-event', 'Event Study', 'No hay eventos válidos con estos filtros.');
    return;
  }

  const traces = [
    {
      x: results.ks,
      y: results.relGeneral,
      name: "IPC general",
      type: "scatter",
      mode: "lines+markers",
      line: { color: "#2E86DE", width: 3, shape: "spline" },
      marker: { size: 7 }
    },
    {
      x: results.ks,
      y: results.relFood,
      name: "IPC alimentos",
      type: "scatter",
      mode: "lines+markers",
      line: { color: "#E67E22", width: 3, shape: "spline" },
      marker: { size: 7 }
    }
  ];

  Plotly.newPlot('esp-plot-event', traces, {
    title: { text: "Inflación mensual relativa al promedio pre-evento (pp)", font: { size: 14 } },
    xaxis: { title: "Meses relativos al ajuste (k)" },
    yaxis: { title: "Diferencia vs pre-evento (pp)" },
    shapes: [
      { type: "line", x0: 0, x1: 0, y0: 0, y1: 1, yref: "paper", line: { color: "#7f8c8d", dash: "dot" } },
      { type: "line", x0: Math.min(...results.ks), x1: Math.max(...results.ks), y0: 0, y1: 0, line: { color: "#bdc3c7", dash: "dash" } }
    ],
    margin: { t: 45, b: 45, l: 60, r: 20 },
    legend: { orientation: "h", y: -0.25 }
  }, { responsive: true, displayModeBar: false });
}

function renderEspiralPlaceboPlot(results) {
  if (!results || results.placeboDist.length === 0) {
    renderEmptyPlot('esp-plot-placebo', 'Placebo Monte Carlo', 'Sin simulaciones disponibles para esta configuración.');
    return;
  }

  const obs = results.eiGeneral;
  Plotly.newPlot('esp-plot-placebo', [{
    x: results.placeboDist,
    type: "histogram",
    nbinsx: 35,
    marker: { color: "#95a5a6", opacity: 0.85 },
    name: "Distribución placebo"
  }], {
    title: { text: "Distribución placebo del EI (IPC general)", font: { size: 14 } },
    xaxis: { title: "EI placebo (pp)" },
    yaxis: { title: "Frecuencia" },
    shapes: obs === null ? [] : [{
      type: "line", x0: obs, x1: obs, y0: 0, y1: 1, yref: "paper",
      line: { color: "#C0392B", width: 2 }
    }],
    annotations: obs === null ? [] : [{
      x: obs, y: 1.05, xref: "x", yref: "paper", showarrow: false,
      text: `EI observado = ${formatPP(obs)}`
    }],
    margin: { t: 45, b: 45, l: 55, r: 20 }
  }, { responsive: true, displayModeBar: false });
}

function renderEspiralSimPlot(results) {
  const rows = results ? results.simScenarios.filter(r => r.general !== null || r.food !== null) : [];
  if (!rows || rows.length === 0) {
    renderEmptyPlot('esp-plot-sim', 'Simulador', 'No hay suficiente variación de eventos para estimar escenarios.');
    return;
  }

  const labels = rows.map(r => `+${r.adj} pp`);
  Plotly.newPlot('esp-plot-sim', [
    {
      x: labels,
      y: rows.map(r => r.general),
      type: "bar",
      name: "IPC general estimado",
      marker: { color: "#2E86DE" }
    },
    {
      x: labels,
      y: rows.map(r => r.food),
      type: "bar",
      name: "IPC alimentos estimado",
      marker: { color: "#E67E22" }
    }
  ], {
    title: { text: "Simulación: respuesta esperada del EI por tamaño del ajuste", font: { size: 14 } },
    xaxis: { title: "Ajuste hipotético del SML" },
    yaxis: { title: "EI esperado (pp)" },
    barmode: "group",
    margin: { t: 45, b: 45, l: 60, r: 20 },
    legend: { orientation: "h", y: -0.25 }
  }, { responsive: true, displayModeBar: false });
}

function renderEspiralContextPlot(params) {
  const rows = buildEspiralContextRows();
  if (rows.length === 0) {
    renderEmptyPlot('esp-plot-context', 'Contexto social y laboral', 'No hay datos en el filtro actual para construir contexto.');
    return;
  }

  const labelMap = {
    pct_1sml: "% en 1 SML (R02)",
    pct_cotiza: "% formalidad laboral (R02)",
    pct_hogares_internet: "% hogares con internet (R01)",
    pct_hogares_material_apto: "% hogares con material apto (R01)",
    pct_hogares_movilidad: "% hogares con movilidad (R01)"
  };
  const colMap = {
    pct_1sml: "#117a65",
    pct_cotiza: "#7b241c",
    pct_hogares_internet: "#1f618d",
    pct_hogares_material_apto: "#884ea0",
    pct_hogares_movilidad: "#ca6f1e"
  };

  const traces = [];
  params.contextSeries.forEach(key => {
    if (!labelMap[key]) return;
    traces.push({
      x: rows.map(r => r.quarter),
      y: rows.map(r => r[key]),
      name: labelMap[key],
      type: "scatter",
      mode: "lines+markers",
      line: { width: 3, shape: "spline", color: colMap[key] },
      marker: { size: 7 }
    });
  });

  const hasIpc = rows.some(r => r.ipc_general_q !== null);
  if (hasIpc) {
    traces.push({
      x: rows.map(r => r.quarter),
      y: rows.map(r => r.ipc_general_q),
      name: "IPC general trimestral (promedio mensual)",
      type: "scatter",
      mode: "lines+markers",
      line: { color: "#E74C3C", dash: "dot", width: 2 },
      marker: { size: 6 },
      yaxis: "y2"
    });
  }

  const contextVals = rows.flatMap(r => params.contextSeries.map(k => r[k])).filter(v => v !== null && Number.isFinite(v));
  const markY = contextVals.length > 0 ? (Math.max(...contextVals) + 2) : 98;

  const eventTrims = new Set(
    espiralData
      .filter(r => r.ajuste_pp !== null && r.ajuste_pp > 0)
      .map(r => `${r.fecha.getFullYear()}Trim${Math.floor(r.fecha.getMonth() / 3) + 1}`)
  );
  const marks = rows.filter(r => eventTrims.has(r.quarter));
  if (marks.length > 0) {
    traces.push({
      x: marks.map(r => r.quarter),
      y: marks.map(() => markY),
      name: "Trimestres con ajuste SML",
      type: "scatter",
      mode: "markers",
      marker: { symbol: "diamond", size: 9, color: "#1f618d" }
    });
  }

  if (traces.length === 0) {
    renderEmptyPlot('esp-plot-context', 'Contexto social y laboral', 'Activa al menos una serie de contexto.');
    return;
  }

  Plotly.newPlot('esp-plot-context', traces, {
    title: { text: "Contexto laboral y de hogares con marcas de ajuste SML", font: { size: 14 } },
    xaxis: { title: "Trimestre" },
    yaxis: { title: "Indicadores de contexto (%)", rangemode: "tozero" },
    yaxis2: {
      title: "IPC general mensual promedio (%)",
      overlaying: "y",
      side: "right",
      showgrid: false
    },
    margin: { t: 45, b: 45, l: 60, r: 70 },
    legend: { orientation: "h", y: -0.3 }
  }, { responsive: true, displayModeBar: false });
}

function renderEspiralTable(results) {
  const tbody = document.getElementById('esp-table-body');
  if (!tbody) return;
  if (!results || results.events.length === 0) {
    tbody.innerHTML = '<tr><td colspan="7" class="text-center text-muted">Sin eventos válidos para esta configuración.</td></tr>';
    return;
  }

  const rows = results.events.map(ev => {
    const dateTxt = ev.date ? ev.date.toISOString().slice(0, 10) : "-";
    return `<tr>
      <td>Ajuste SML</td>
      <td>${dateTxt}</td>
      <td class="text-end">${formatPP(ev.ajuste)}</td>
      <td class="text-end">${formatPP(ev.eiGeneral)}</td>
      <td class="text-end">${formatPP(ev.eiFood)}</td>
      <td class="text-end">${formatPP(ev.ctGeneral)}</td>
      <td class="text-end">${formatPP(ev.ctFood)}</td>
    </tr>`;
  }).join("");
  tbody.innerHTML = rows;
}

function renderEspiralStory(results, params) {
  const el = document.getElementById('esp-story-text');
  if (!el) return;
  const maxR02 = rawData.length > 0 ? Math.max(...rawData.map(d => parseInt(d.anio, 10) || 0)) : null;
  const maxR01 = contextoR01.length > 0
    ? Math.max(...contextoR01.map(d => parseInt((d.trimestredesc || "").slice(0, 4), 10) || 0))
    : null;
  const covR02 = maxR02 ? `R02 hasta ${maxR02}T4.` : "";
  const covR01 = maxR01 ? `R01 hasta ${maxR01}T4.` : "";
  const coverage = [covR02, covR01].filter(Boolean).join(" ");

  if (!results || results.events.length === 0) {
    el.innerHTML = `No hay eventos válidos con esta configuración. Probá bajar el umbral de ajuste o ampliar el horizonte. ${coverage}`;
    return;
  }

  const base = `Eventos: <strong>${results.events.length}</strong> | EI IPC general: <strong>${formatPP(results.eiGeneral)}</strong> | EI alimentos: <strong>${formatPP(results.eiFood)}</strong>.`;
  if (params.mode === "analitico") {
    el.innerHTML = `${base} El p-value placebo para IPC general es <strong>${results.pValueGeneral === null ? "-" : results.pValueGeneral.toFixed(3)}</strong>. ${coverage}`;
    return;
  }

  const stories = {
    1: "Paso 1: observamos la trayectoria promedio de inflación antes y después de ajustes del salario mínimo.",
    2: "Paso 2: medimos el EI (diferencia post-pre) y verificamos magnitud en IPC general y alimentos.",
    3: "Paso 3: limpiamos eventos solapados y exigimos ajustes más grandes para robustez.",
    4: "Paso 4: conectamos el fenómeno con contexto social (franja 1 SML, formalidad e internet).",
    5: "Paso 5: usamos la curva estimada para simular escenarios alternativos de ajuste."
  };
  el.innerHTML = `${stories[params.step] || stories[1]} ${base} ${coverage}`;
}

function updateEspiralDashboard() {
  const kEvents = document.getElementById('esp-kpi-events');
  if (!kEvents) return;
  syncEspiralLabels();

  const params = getEspiralParams();
  if (!espiralReady) {
    kEvents.textContent = "-";
    document.getElementById('esp-kpi-ei-gen').textContent = "-";
    document.getElementById('esp-kpi-ei-food').textContent = "-";
    document.getElementById('esp-kpi-pgen').textContent = "-";
    if (!espiralLoading) setEspiralStatusMessage("Esperando carga de la base macro de espiral...");
    return;
  }

  const results = computeEspiralResults(params);
  const pText = results && results.pValueGeneral !== null ? results.pValueGeneral.toFixed(3) : "-";
  kEvents.textContent = results ? String(results.events.length) : "-";
  document.getElementById('esp-kpi-ei-gen').textContent = results ? formatPP(results.eiGeneral) : "-";
  document.getElementById('esp-kpi-ei-food').textContent = results ? formatPP(results.eiFood) : "-";
  document.getElementById('esp-kpi-pgen').textContent = pText;

  renderEspiralEventPlot(results);
  renderEspiralPlaceboPlot(results);
  renderEspiralSimPlot(results);
  renderEspiralContextPlot(params);
  renderEspiralTable(results);
  renderEspiralStory(results, params);
}
// Iniciar aplicación
document.addEventListener('DOMContentLoaded', init);
