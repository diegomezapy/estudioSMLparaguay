// Estado Global
let rawData = [];
let filteredData = [];

// Diccionario de Variables (EPHC INE)
const DICT = {
  rama_pea: {
    1: "Agricultura y Ganadería",
    2: "Industria y Minería",
    3: "Electricidad, Gas y Agua",
    4: "Construcción",
    5: "Comercio y Hoteles",
    6: "Transporte y Comunicaciones",
    7: "Finanzas e Inmuebles",
    8: "Servicios Sociales y Personales",
    9: "No Especificado"
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
    1: "1 - Empleado/a", 2: "2 - Obrero/a", 3: "3 - Patrón/a",
    4: "4 - Empleado/a público", 5: "5 - Cuenta propia", 6: "6 - Familiar no remun.",
    7: "7 - Empleado/a doméstico", 8: "8 - Trabajador/a no remun.", 9: "9 - Otros"
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
  cate: document.getElementById('filter-cate-group')
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
  
  els.bar.parentElement.style.display = 'none';
  els.spinner.remove(); // Elimina el cartel central por completo
  els.main.style.display = 'block';
  
  // Asignar listeners
  [els.tolSlider, els.yrMin, els.yrMax].forEach(el => {
    el.addEventListener('change', updateApp);
  });
  
  document.querySelectorAll('.filter-q').forEach(el => el.addEventListener('change', updateApp));
  document.querySelectorAll('input[name="filter-sexo"]').forEach(el => el.addEventListener('change', updateApp));
  document.querySelectorAll('input[name="filter-area"]').forEach(el => el.addEventListener('change', updateApp));
  
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
    updateApp();
  });

  els.tolSlider.addEventListener('input', (e) => {
    els.tolVal.textContent = e.target.value;
  });

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
  const rama = document.querySelector('input[name="filter-rama"]:checked').value;
  const ocup = document.querySelector('input[name="filter-ocup"]:checked').value;
  const cate = document.querySelector('input[name="filter-cate"]:checked').value;
  
  const selectedQs = Array.from(document.querySelectorAll('.filter-q:checked')).map(el => parseInt(el.value));

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

    // Calcular franja dinámicamente
    if (d.ratio_sml < lo) d.ingoc1sml_cat = "Menos de 1 SML";
    else if (d.ratio_sml > hi) d.ingoc1sml_cat = "Más de 1 SML";
    else d.ingoc1sml_cat = "1 SML";

    filteredData.push(d);
    sumWTotal += (d.w || 0);
  }

  els.info.textContent = `Registros: ${filteredData.length.toLocaleString('es-PY')} (N: ${formatGs(sumWTotal)})`;

  updateKPIs(sumWTotal);
  drawPlots();
}

function updateKPIs(sumWTotal) {
  document.getElementById('kpi-n').textContent = formatGs(sumWTotal);
  
  const salGen = calcWMean(filteredData, 'salario', 'w');
  document.getElementById('kpi-sal').textContent = formatGs(salGen);
  
  const formalPct = calcWMean(filteredData, 'cotiza_bin', 'w') * 100;
  document.getElementById('kpi-formal').textContent = formalPct.toFixed(1) + "%";
  
  const share1Pct = calcWShare(filteredData, d => d.ingoc1sml_cat === "1 SML", 'w') * 100;
  document.getElementById('kpi-share1').textContent = share1Pct.toFixed(1) + "%";

  // Brecha Salarial
  const salH = calcWMean(filteredData.filter(d => d.sexo === "Hombres"), 'salario', 'w');
  const salM = calcWMean(filteredData.filter(d => d.sexo === "Mujeres"), 'salario', 'w');
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

  // 2. Formalidad (ahora como barras agrupadas, al igual que distribución)
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
  const salData = groupData(filteredData, ['trimestredesc', 'sexo'], (items, wSum) => calcWMean(items, 'salario', 'w'));
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
  Plotly.newPlot('plot-sal', tracesSal, { margin: {t:20, b:40, l:60, r:10}, legend: {orientation: 'h', y: -0.2} }, {responsive: true});

  // ================= TABLA RESUMEN ==================
  drawTable(trimestres, generos);
  
  // ================= TAB DEMOGRAFIA ==================
  drawDemografia(filteredData, trimestres);
  
  // ================= TAB MAPAS ==================
  drawMap(filteredData);
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
      educStats[t][niv].wSal += d.salario * d.w;
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
  document.getElementById('plot-mapa').innerHTML = '<div class="d-flex align-items-center justify-content-center h-100 text-muted"><p>En desarrollo...</p></div>';
  return;
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
      text.push(`${name}<br>Sin datos suficientes`);
    }
  });

  const trace = {
    type: "choroplethmapbox",
    geojson: window.geojsonData,
    locations: locations,
    featureidkey: "properties.NAME_1",
    z: z,
    text: text,
    hoverinfo: "text",
    colorscale: "Viridis",
    marker: { opacity: 0.7, line: { width: 1, color: "white" } }
  };

  const layout = {
    mapbox: {
      style: "carto-positron",
      center: { lon: -58.0, lat: -23.5 },
      zoom: 4.5
    },
    margin: { t: 0, b: 0, l: 0, r: 0 }
  };

  Plotly.newPlot('plot-mapa', [trace], layout, { responsive: true, displayModeBar: false });
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
      const sal = calcWMean(items, 'salario', 'w');
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

// Iniciar aplicación
document.addEventListener('DOMContentLoaded', init);
