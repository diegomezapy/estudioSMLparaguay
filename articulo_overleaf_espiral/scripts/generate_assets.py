#!/usr/bin/env python3
from __future__ import annotations

import math
import re
from pathlib import Path

import numpy as np
import pandas as pd

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt


ROOT = Path(__file__).resolve().parents[1]
REPO_ROOT = ROOT.parent
OUT_FIG = ROOT / "figures"
OUT_TAB = ROOT / "tables"
OUT_DATA = ROOT / "data"

for p in (OUT_FIG, OUT_TAB, OUT_DATA):
    p.mkdir(parents=True, exist_ok=True)

ESP_PATH = REPO_ROOT / "data" / "espiral_salario_precios.csv"
DB_APP_PATH = REPO_ROOT / "data" / "db_app.csv"
CTX_R01_PATH = REPO_ROOT / "data" / "contexto_r01.csv"


def weighted_mean(x: pd.Series, w: pd.Series) -> float:
    m = x.notna() & w.notna()
    if not m.any():
        return float("nan")
    xx = x[m].astype(float)
    ww = w[m].astype(float)
    s = ww.sum()
    if not np.isfinite(s) or s <= 0:
        return float("nan")
    return float(np.average(xx, weights=ww))


def weighted_share(mask: pd.Series, w: pd.Series) -> float:
    m = mask.notna() & w.notna()
    if not m.any():
        return float("nan")
    vv = mask[m].astype(float)
    ww = w[m].astype(float)
    s = ww.sum()
    if not np.isfinite(s) or s <= 0:
        return float("nan")
    return float(100.0 * np.sum(vv * ww) / s)


def parse_trim_key(x: str) -> tuple[int, int]:
    m = re.match(r"^(\d{4})Trim([1-4])$", str(x))
    if not m:
        return (0, 0)
    return (int(m.group(1)), int(m.group(2)))


def valid_event_indices(df: pd.DataFrame, h: int) -> list[int]:
    idx = df.index[df["ajuste_pp"].notna() & (df["ajuste_pp"] > 0)].tolist()
    return [i for i in idx if i - h >= 0 and i + h < len(df)]


def exclude_overlap(indices: list[int], h: int) -> list[int]:
    indices = sorted(indices)
    keep: list[int] = []
    for i in indices:
        if all(abs(i - j) > h for j in keep):
            keep.append(i)
    return keep


def ei_for_index(df: pd.DataFrame, idx: int, h: int, col: str) -> float:
    pre = df.iloc[idx - h:idx][col].dropna()
    post = df.iloc[idx + 1:idx + h + 1][col].dropna()
    if len(pre) == 0 or len(post) == 0:
        return float("nan")
    return float(post.mean() - pre.mean())


def event_metrics(df: pd.DataFrame, indices: list[int], h: int) -> tuple[pd.DataFrame, pd.DataFrame]:
    rows = []
    prof_rows = []
    for idx in indices:
        win = df.iloc[idx - h:idx + h + 1].copy().reset_index(drop=True)
        pre = win.iloc[:h]
        post = win.iloc[h + 1:]

        pre_g = pre["ipc_general_m"].dropna().mean()
        post_g = post["ipc_general_m"].dropna().mean()
        pre_f = pre["ipc_alim_m"].dropna().mean()
        post_f = post["ipc_alim_m"].dropna().mean()

        ei_g = (post_g - pre_g) if (np.isfinite(pre_g) and np.isfinite(post_g)) else float("nan")
        ei_f = (post_f - pre_f) if (np.isfinite(pre_f) and np.isfinite(post_f)) else float("nan")

        ct_g = ei_g / df.loc[idx, "ajuste_pp"] if np.isfinite(ei_g) and np.isfinite(df.loc[idx, "ajuste_pp"]) and df.loc[idx, "ajuste_pp"] != 0 else float("nan")
        ct_f = ei_f / df.loc[idx, "ajuste_pp"] if np.isfinite(ei_f) and np.isfinite(df.loc[idx, "ajuste_pp"]) and df.loc[idx, "ajuste_pp"] != 0 else float("nan")

        rows.append(
            {
                "fecha_evento": df.loc[idx, "fecha"],
                "ajuste_pp": df.loc[idx, "ajuste_pp"],
                "ei_general_pp": ei_g,
                "ei_alimentos_pp": ei_f,
                "ct_general": ct_g,
                "ct_alimentos": ct_f,
            }
        )

        ks = list(range(-h, h + 1))
        for i_k, k in enumerate(ks):
            g = win.loc[i_k, "ipc_general_m"]
            f = win.loc[i_k, "ipc_alim_m"]
            prof_rows.append(
                {
                    "k": k,
                    "rel_general": (g - pre_g) if np.isfinite(g) and np.isfinite(pre_g) else float("nan"),
                    "rel_alimentos": (f - pre_f) if np.isfinite(f) and np.isfinite(pre_f) else float("nan"),
                }
            )

    return pd.DataFrame(rows), pd.DataFrame(prof_rows)


def placebo_distribution(df: pd.DataFrame, event_indices: list[int], h: int, reps: int = 2000, seed: int = 20260423) -> tuple[np.ndarray, np.ndarray, float, float]:
    n = len(df)
    size = len(event_indices)
    if size == 0:
        return np.array([]), np.array([]), float("nan"), float("nan")

    event_set = set(event_indices)
    candidates = [i for i in range(h, n - h) if i not in event_set]
    if len(candidates) < size:
        return np.array([]), np.array([]), float("nan"), float("nan")

    obs_g = np.nanmean([ei_for_index(df, i, h, "ipc_general_m") for i in event_indices])
    obs_f = np.nanmean([ei_for_index(df, i, h, "ipc_alim_m") for i in event_indices])

    rng = np.random.default_rng(seed + h + size)
    dist_g = np.empty(reps)
    dist_f = np.empty(reps)

    for b in range(reps):
        picked = rng.choice(candidates, size=size, replace=False)
        vals_g = [ei_for_index(df, int(i), h, "ipc_general_m") for i in picked]
        vals_f = [ei_for_index(df, int(i), h, "ipc_alim_m") for i in picked]
        dist_g[b] = np.nanmean(vals_g)
        dist_f[b] = np.nanmean(vals_f)

    p_g = float(np.mean(np.abs(dist_g) >= abs(obs_g))) if np.isfinite(obs_g) else float("nan")
    p_f = float(np.mean(np.abs(dist_f) >= abs(obs_f))) if np.isfinite(obs_f) else float("nan")
    return dist_g, dist_f, p_g, p_f


def tex_escape(s: str) -> str:
    repl = {
        "&": r"\&",
        "%": r"\%",
        "_": r"\_",
        "#": r"\#",
        "$": r"\$",
    }
    out = str(s)
    for k, v in repl.items():
        out = out.replace(k, v)
    return out


def write_latex_table(df: pd.DataFrame, path: Path, col_labels: list[str], col_formats: list[str]) -> None:
    lines = []
    lines.append(r"\begin{tabular}{" + "".join(col_formats) + "}")
    lines.append(r"\toprule")
    lines.append(" & ".join(tex_escape(c) for c in col_labels) + r"\\")
    lines.append(r"\midrule")
    for _, row in df.iterrows():
        vals = []
        for v in row.tolist():
            if isinstance(v, float):
                if np.isnan(v):
                    vals.append("--")
                else:
                    vals.append(f"{v:,.3f}")
            else:
                vals.append(tex_escape(v))
        lines.append(" & ".join(vals) + r"\\")
    lines.append(r"\bottomrule")
    lines.append(r"\end{tabular}")
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")


def main() -> None:
    esp = pd.read_csv(ESP_PATH)
    esp["fecha"] = pd.to_datetime(esp["fecha"], errors="coerce")
    for c in ["ipc_general_m", "ipc_alim_m", "ajuste_pp"]:
        esp[c] = pd.to_numeric(esp[c], errors="coerce")
    esp = esp.dropna(subset=["fecha"]).sort_values("fecha").reset_index(drop=True)

    db = pd.read_csv(DB_APP_PATH, usecols=["trimestredesc", "ingoc1sml_cat", "cotiza_bin", "w", "ratio_sml"])
    db["w"] = pd.to_numeric(db["w"], errors="coerce")
    db["cotiza_bin"] = pd.to_numeric(db["cotiza_bin"], errors="coerce")
    db["ratio_sml"] = pd.to_numeric(db["ratio_sml"], errors="coerce")

    ctx_r01 = pd.read_csv(CTX_R01_PATH)

    # Main configuration
    h_main = 6
    idx_all = valid_event_indices(esp, h_main)
    idx_clean = exclude_overlap(idx_all, h_main)

    met_all, prof_all = event_metrics(esp, idx_all, h_main)
    met_clean, prof_clean = event_metrics(esp, idx_clean, h_main)

    dist_g, dist_f, p_g, p_f = placebo_distribution(esp, idx_clean, h_main, reps=2500)

    # Figure 1: event profile
    prof_plot = prof_clean.groupby("k", as_index=False).mean(numeric_only=True).sort_values("k")
    plt.figure(figsize=(8.6, 4.6))
    if len(prof_plot) > 0:
        plt.plot(prof_plot["k"], prof_plot["rel_general"], marker="o", linewidth=2.3, label="IPC general")
        plt.plot(prof_plot["k"], prof_plot["rel_alimentos"], marker="o", linewidth=2.3, label="IPC alimentos")
    plt.axvline(0, color="#555555", linestyle="--", linewidth=1)
    plt.axhline(0, color="#aaaaaa", linestyle=":", linewidth=1)
    plt.xlabel("Mes relativo al ajuste (k)")
    plt.ylabel("Diferencia vs promedio pre-evento (pp)")
    plt.title("Perfil promedio del evento (h=6, eventos no solapados)")
    plt.legend(frameon=False)
    plt.tight_layout()
    plt.savefig(OUT_FIG / "fig_event_profile.pdf")
    plt.savefig(OUT_FIG / "fig_event_profile.png", dpi=220)
    plt.close()

    # Figure 2: placebo histogram
    plt.figure(figsize=(8.2, 4.8))
    if len(dist_g) > 0:
        plt.hist(dist_g, bins=35, alpha=0.75, color="#8fa6b8", edgecolor="white")
        obs = float(np.nanmean(met_clean["ei_general_pp"])) if len(met_clean) > 0 else float("nan")
        if np.isfinite(obs):
            plt.axvline(obs, color="#c0392b", linewidth=2.2, label=f"EI observado = {obs:.3f} pp")
            plt.legend(frameon=False)
    plt.xlabel("EI placebo (IPC general, pp)")
    plt.ylabel("Frecuencia")
    plt.title("Distribucion placebo Monte Carlo (h=6, reps=2500)")
    plt.tight_layout()
    plt.savefig(OUT_FIG / "fig_placebo_hist.pdf")
    plt.savefig(OUT_FIG / "fig_placebo_hist.png", dpi=220)
    plt.close()

    # Context quarterly
    ctx_r02_rows = []
    for t, g in db.groupby("trimestredesc"):
        w = g["w"]
        pct_1sml = weighted_share((g["ingoc1sml_cat"] == "1 SML"), w)
        pct_cot = weighted_share((g["cotiza_bin"] == 1), w)
        ctx_r02_rows.append({"trimestredesc": t, "pct_1sml": pct_1sml, "pct_cotiza": pct_cot})
    ctx_r02 = pd.DataFrame(ctx_r02_rows)

    esp["trimestredesc"] = esp["fecha"].dt.year.astype(str) + "Trim" + (((esp["fecha"].dt.month - 1) // 3) + 1).astype(str)
    macro_q = esp.groupby("trimestredesc", as_index=False)["ipc_general_m"].mean().rename(columns={"ipc_general_m": "ipc_general_q"})

    ctx = ctx_r02.merge(ctx_r01, on="trimestredesc", how="left").merge(macro_q, on="trimestredesc", how="left")
    ctx = ctx.drop_duplicates(subset=["trimestredesc"]).copy()
    ctx["year"] = ctx["trimestredesc"].apply(lambda x: parse_trim_key(x)[0])
    ctx["quarter"] = ctx["trimestredesc"].apply(lambda x: parse_trim_key(x)[1])
    ctx = ctx.sort_values(["year", "quarter"]).reset_index(drop=True)
    ctx.to_csv(OUT_DATA / "contexto_trimestral_para_articulo.csv", index=False)

    # Figure 3: contexto
    x = np.arange(len(ctx))
    labels = ctx["trimestredesc"].tolist()
    fig, ax1 = plt.subplots(figsize=(10.4, 4.8))
    for col, color, lbl in [
        ("pct_1sml", "#117a65", "% en 1 SML (R02)"),
        ("pct_cotiza", "#7b241c", "% formalidad (R02)"),
        ("pct_hogares_internet", "#1f618d", "% internet hogar (R01)"),
        ("pct_hogares_material_apto", "#884ea0", "% material apto (R01)"),
        ("pct_hogares_movilidad", "#ca6f1e", "% movilidad hogar (R01)"),
    ]:
        if col in ctx.columns and ctx[col].notna().any():
            ax1.plot(x, ctx[col], marker="o", linewidth=2, label=lbl, color=color)

    ax1.set_ylabel("Indicadores de contexto (%)")
    ax1.set_xticks(x)
    ax1.set_xticklabels(labels, rotation=45, ha="right")

    ax2 = ax1.twinx()
    if ctx["ipc_general_q"].notna().any():
        ax2.plot(x, ctx["ipc_general_q"], color="#2e86de", linestyle="--", linewidth=1.8, label="IPC general trimestral (promedio mensual)")
        ax2.set_ylabel("IPC general mensual promedio (%)")

    events_q = esp.loc[esp["ajuste_pp"].notna() & (esp["ajuste_pp"] > 0), "trimestredesc"].drop_duplicates().tolist()
    idx_marks = [i for i, t in enumerate(labels) if t in events_q]
    if len(idx_marks) > 0:
        y_top = np.nanmax(pd.concat([ctx.get("pct_1sml", pd.Series(dtype=float)), ctx.get("pct_cotiza", pd.Series(dtype=float)), ctx.get("pct_hogares_internet", pd.Series(dtype=float)), ctx.get("pct_hogares_material_apto", pd.Series(dtype=float)), ctx.get("pct_hogares_movilidad", pd.Series(dtype=float))], axis=0).values)
        if np.isfinite(y_top):
            ax1.scatter(idx_marks, np.repeat(y_top + 2, len(idx_marks)), marker="D", color="#1f618d", s=35, label="Trimestres con ajuste SML")

    h1, l1 = ax1.get_legend_handles_labels()
    h2, l2 = ax2.get_legend_handles_labels()
    ax1.legend(h1 + h2, l1 + l2, ncol=2, frameon=False, loc="upper left", fontsize=8)
    ax1.set_title("Contexto laboral y de hogares con marcas de ajuste")
    fig.tight_layout()
    fig.savefig(OUT_FIG / "fig_contexto_trimestral.pdf")
    fig.savefig(OUT_FIG / "fig_contexto_trimestral.png", dpi=220)
    plt.close(fig)

    # Figure 4: density around 1 SML
    dens = db.loc[(db["ratio_sml"].notna()) & (db["w"].notna()) & (db["w"] > 0) & (db["ratio_sml"] > 0) & (db["ratio_sml"] < 2)].copy()
    bins = np.linspace(0, 2, 81)
    inside = dens[(dens["ratio_sml"] >= 0.9) & (dens["ratio_sml"] <= 1.1)]
    outside = dens[(dens["ratio_sml"] < 0.9) | (dens["ratio_sml"] > 1.1)]

    plt.figure(figsize=(8.6, 4.8))
    plt.axvspan(0.9, 1.1, alpha=0.08, color="#2ca25f")
    plt.hist(outside["ratio_sml"], bins=bins, weights=outside["w"], color="#9ecae1", edgecolor="white", linewidth=0.2, label="Fuera de banda")
    plt.hist(inside["ratio_sml"], bins=bins, weights=inside["w"], color="#2ca25f", edgecolor="white", linewidth=0.2, label="Dentro de banda")
    plt.axvline(1.0, linestyle="--", color="#1b7837", linewidth=1.2)
    plt.axvline(0.9, linestyle=":", color="#1b7837", linewidth=1.0)
    plt.axvline(1.1, linestyle=":", color="#1b7837", linewidth=1.0)
    plt.xlim(0, 2)
    plt.xlabel("Ingreso / SML")
    plt.ylabel("Frecuencia ponderada")
    plt.title("Distribucion de ingreso relativo al SML (banda +-10%)")
    plt.legend(frameon=False)
    plt.tight_layout()
    plt.savefig(OUT_FIG / "fig_density_rel_sml.pdf")
    plt.savefig(OUT_FIG / "fig_density_rel_sml.png", dpi=220)
    plt.close()

    # Table A: event metrics main
    met_tab = met_clean.copy()
    met_tab = met_tab.sort_values("fecha_evento").reset_index(drop=True)
    met_tab["fecha_evento"] = met_tab["fecha_evento"].dt.strftime("%Y-%m-%d")
    write_latex_table(
        met_tab[["fecha_evento", "ajuste_pp", "ei_general_pp", "ei_alimentos_pp", "ct_general", "ct_alimentos"]],
        OUT_TAB / "table_event_metrics.tex",
        ["Fecha", "Ajuste (pp)", "EI IPC gen", "EI IPC alim", "CT gen", "CT alim"],
        ["l", "r", "r", "r", "r", "r"],
    )

    # Table B: robustness
    rob_rows = []
    for h in [3, 6, 9]:
        idx_h = valid_event_indices(esp, h)
        for overlap in ["all", "clean"]:
            idx_use = idx_h if overlap == "all" else exclude_overlap(idx_h, h)
            m_h, _ = event_metrics(esp, idx_use, h)
            _, _, pg, pf = placebo_distribution(esp, idx_use, h, reps=1500)
            rob_rows.append(
                {
                    "h": h,
                    "solapamiento": overlap,
                    "n_eventos": len(idx_use),
                    "ei_gen": float(np.nanmean(m_h["ei_general_pp"])) if len(m_h) else float("nan"),
                    "ei_alim": float(np.nanmean(m_h["ei_alimentos_pp"])) if len(m_h) else float("nan"),
                    "p_gen": pg,
                    "p_alim": pf,
                }
            )
    rob = pd.DataFrame(rob_rows)
    write_latex_table(
        rob[["h", "solapamiento", "n_eventos", "ei_gen", "ei_alim", "p_gen", "p_alim"]],
        OUT_TAB / "table_robustness.tex",
        ["h", "Solap.", "N", "EI gen", "EI alim", "p gen", "p alim"],
        ["r", "l", "r", "r", "r", "r", "r"],
    )

    # Table C: contexto 2025
    ctx_2025 = ctx.loc[ctx["year"] == 2025, ["trimestredesc", "pct_1sml", "pct_cotiza", "pct_hogares_internet", "pct_hogares_material_apto", "pct_hogares_movilidad"]].copy()
    if len(ctx_2025) == 0:
        ctx_2025 = pd.DataFrame(
            [{
                "trimestredesc": "--",
                "pct_1sml": float("nan"),
                "pct_cotiza": float("nan"),
                "pct_hogares_internet": float("nan"),
                "pct_hogares_material_apto": float("nan"),
                "pct_hogares_movilidad": float("nan"),
            }]
        )
    write_latex_table(
        ctx_2025,
        OUT_TAB / "table_context_2025.tex",
        ["Trimestre", "% 1 SML", "% formalidad", "% internet", "% material apto", "% movilidad"],
        ["l", "r", "r", "r", "r", "r"],
    )

    # Table D: key results for abstract
    key_rows = [
        ("Eventos identificados (total)", float(len(valid_event_indices(esp, h_main)))),
        ("Eventos usados (h=6, no solapados)", float(len(idx_clean))),
        ("EI IPC general (pp)", float(np.nanmean(met_clean["ei_general_pp"])) if len(met_clean) else float("nan")),
        ("EI IPC alimentos (pp)", float(np.nanmean(met_clean["ei_alimentos_pp"])) if len(met_clean) else float("nan")),
        ("CT general (EI/ajuste)", float(np.nanmean(met_clean["ct_general"])) if len(met_clean) else float("nan")),
        ("CT alimentos (EI/ajuste)", float(np.nanmean(met_clean["ct_alimentos"])) if len(met_clean) else float("nan")),
        ("p-value placebo IPC general", p_g),
        ("p-value placebo IPC alimentos", p_f),
    ]
    key_df = pd.DataFrame(key_rows, columns=["Indicador", "Valor"])
    write_latex_table(key_df, OUT_TAB / "table_key_results.tex", ["Indicador", "Valor"], ["l", "r"])

    # Macros
    macros = {
        "nEventosTotal": int(len(valid_event_indices(esp, h_main))),
        "nEventosMain": int(len(idx_clean)),
        "eiGeneralMain": float(np.nanmean(met_clean["ei_general_pp"])) if len(met_clean) else float("nan"),
        "eiAlimentosMain": float(np.nanmean(met_clean["ei_alimentos_pp"])) if len(met_clean) else float("nan"),
        "pGeneralMain": float(p_g),
        "pAlimentosMain": float(p_f),
    }

    macro_lines = []
    macro_lines.append("% Auto-generated by scripts/generate_assets.py")
    for k, v in macros.items():
        if isinstance(v, (int, np.integer)):
            macro_lines.append(rf"\newcommand\{k}{{{int(v)}}}")
        else:
            if np.isfinite(v):
                macro_lines.append(rf"\newcommand\{k}{{{v:.3f}}}")
            else:
                macro_lines.append(rf"\newcommand\{k}{{--}}")
    (OUT_TAB / "macros.tex").write_text("\n".join(macro_lines) + "\n", encoding="utf-8")

    # Save event and robustness CSV for transparency
    met_clean.to_csv(OUT_DATA / "event_metrics_main.csv", index=False)
    rob.to_csv(OUT_DATA / "robustness_grid.csv", index=False)

    print("Assets generated in:", ROOT)


if __name__ == "__main__":
    main()
