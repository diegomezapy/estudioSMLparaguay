# Carpeta Lista Para Overleaf: Articulo Espiral Salario-Precios

Esta carpeta esta preparada para subir directamente a Overleaf y compilar un manuscrito tecnico centrado en el fenomeno de la espiral salario-precios en Paraguay.

## Contenido

- `main.tex`: manuscrito principal (compilable).
- `sections/`: secciones del articulo.
- `references.bib`: bibliografia en formato BibTeX.
- `figures/`: figuras en PDF y PNG.
- `tables/`: tablas en LaTeX (`booktabs`) y macros numericas.
- `data/`: archivos CSV de salida para auditoria de resultados.
- `scripts/generate_assets.py`: script reproducible para regenerar tablas/figuras.

## Como usar en Overleaf

1. Crear un proyecto nuevo en Overleaf.
2. Subir el contenido completo de `articulo_overleaf_espiral/`.
3. Verificar que el archivo principal sea `main.tex`.
4. Compilar con `pdfLaTeX` + `BibTeX` (Overleaf lo hace automaticamente al detectar citas).

## Checklist de publicabilidad incluido

- Estructura IMRyD (introduccion, metodos, resultados, discusion).
- Declaraciones de conflicto de interes, datos y limitaciones.
- Figuras y tablas numeradas con trazabilidad.
- Bibliografia y citas en formato academico.
- Texto enfocado en interpretacion prudente y evidencia reproducible.

## Reproducibilidad local (opcional)

Desde el repo principal, regenerar activos con:

```bash
cd articulo_overleaf_espiral
MPLCONFIGDIR=/tmp/mpl python3 scripts/generate_assets.py
```

El script usa:

- `../data/espiral_salario_precios.csv`
- `../data/db_app.csv`
- `../data/contexto_r01.csv`

## Nota de colaboracion

La metodologia del articulo incorpora colaboracion con el trabajo original del estadistico Diego Daniel Sanabria.
