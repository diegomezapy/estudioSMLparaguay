import pandas as pd
xl = pd.ExcelFile('../Anexo_estadístico_2025.xlsx')
for s in xl.sheet_names:
    df = pd.read_excel(xl, sheet_name=s, nrows=10, header=None)
    title = df.dropna(how='all').head(3).to_dict('records')
    print(f'Sheet {s}:', title)
