import pandas as pd
xl = pd.ExcelFile('../Anexo_estadístico_2025.xlsx')
found = False
for s in xl.sheet_names:
    df = pd.read_excel(xl, sheet_name=s)
    for c in df.columns:
        if df[c].astype(str).str.contains('IPC|Inflación|inflacion|Precio|precio', case=False, na=False).any():
            print('Found in sheet:', s, 'column:', c)
            found = True
if not found:
    print('IPC not found')
