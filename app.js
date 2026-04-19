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
    1: "Empleador o Patrón",
    2: "Empleado Público",
    3: "Empleado Privado",
    4: "Cuenta Propia",
    5: "Familiar No Remunerado",
    6: "Empleado Doméstico",
    7: "Jornalero",
    8: "No Especificado",
    9: "No Especificado"
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
  [els.tolSlider, els.yrMin, els.yrMax, els.rama, els.ocup, els.cate].forEach(el => {
    el.addEventListener('change', updateApp);
  });
  
  document.querySelectorAll('input[name="filter-sexo"]').forEach(el => el.addEventListener('change', updateApp));
  document.querySelectorAll('input[name="filter-area"]').forEach(el => el.addEventListener('change', updateApp));
  
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

  const addOptions = (el, set, type) => {
    Array.from(set).sort((a,b)=>a-b).forEach(v => {
      const opt = document.createElement('option');
      opt.value = v; opt.textContent = getLabel(type, v);
      el.appendChild(opt);
    });
  };

  addOptions(els.rama, ramas, 'rama_pea');
  addOptions(els.ocup, ocups, 'ocup_pea');
  addOptions(els.cate, cates, 'cate_pea');
}

function updateApp() {
  const tol = parseFloat(els.tolSlider.value) / 100;
  const yrMin = parseInt(els.yrMin.value);
  const yrMax = parseInt(els.yrMax.value);
  const sexo = document.querySelector('input[name="filter-sexo"]:checked').value;
  const area = document.querySelector('input[name="filter-area"]:checked').value;
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
    if (area !== "Todos" && String(d.area_urb) !== String(area)) continue;
    if (rama !== "Todos" && String(d.rama_pea) !== String(rama)) continue;
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
      line: { color: sexo === 'Hombres' ? '#3498DB' : '#E74C3C', width: 4 },
      marker: { size: 10 }
    };
  });
  Plotly.newPlot('plot-sal', tracesSal, { margin: {t:20, b:40, l:60, r:10}, legend: {orientation: 'h', y: -0.2} }, {responsive: true});

  // 4. Llenar Tabla Resumen
  drawTable(franjas, generos);
}

function drawTable(franjas, generos) {
  const tbody = document.getElementById('summary-table-body');
  let html = '';
  
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
