
from __future__ import annotations

import os
from pathlib import Path

import numpy as np
import pandas as pd
import xarray as xr

ROOT = Path(__file__).resolve().parents[1]
WIND_DIR = ROOT / "environmental-datasets" / "wind"
INPUT_CSV = ROOT / "altered-datasets" / "final_chick_data_pre_env_vars.csv"
OUTPUT_CSV = ROOT / "altered-datasets" / "final_chick_data_with_wind.csv"

U_VAR = "u10"
V_VAR = "v10"
TIME_DIM = "valid_time"


def wind_nc_path(year: int, wind_dir: Path = WIND_DIR) -> Path | None:
    """Project wind folder first, then ~/Downloads (common CDS output location)."""
    candidates = [
        wind_dir / f"wind_{year}.nc",
        Path.home() / "Downloads" / f"wind_{year}.nc",
    ]
    for path in candidates:
        if path.is_file():
            return path
    return None


def lon_to_360(lon: np.ndarray) -> np.ndarray:
    return np.where(lon < 0, lon + 360.0, lon)


def month_starts(dt: pd.Series) -> np.ndarray:
    return dt.values.astype("datetime64[M]")


def extract_wind_for_year(
    df_year: pd.DataFrame, ds: xr.Dataset
) -> tuple[np.ndarray, np.ndarray]:
    valid_time = month_starts(df_year["datetime"])
    lat = df_year["latitude"].to_numpy()
    lon = lon_to_360(df_year["longitude"].to_numpy())

    points = {"points": np.arange(len(df_year))}
    sel_kw = dict(
        method="nearest",
        latitude=xr.DataArray(lat, dims="points"),
        longitude=xr.DataArray(lon, dims="points"),
    )
    sel_kw[TIME_DIM] = xr.DataArray(valid_time, dims="points")

    wind_u = ds[U_VAR].sel(**sel_kw).values
    wind_v = ds[V_VAR].sel(**sel_kw).values
    return wind_u, wind_v


def add_wind_columns(df: pd.DataFrame, wind_dir: Path = WIND_DIR) -> pd.DataFrame:
    out = df.copy()
    out["datetime"] = pd.to_datetime(out["datetime"])
    out["wind_u"] = np.nan
    out["wind_v"] = np.nan

    years = sorted(out["datetime"].dt.year.unique())
    missing_years: list[int] = []

    for year in years:
        nc_path = wind_nc_path(year, wind_dir)
        if nc_path is None:
            missing_years.append(year)
            continue

        mask = out["datetime"].dt.year == year
        idx = out.index[mask]
        sub = out.loc[mask]

        with xr.open_dataset(nc_path) as ds:
            if TIME_DIM not in ds.dims and "time" in ds.dims:
                ds = ds.rename({"time": TIME_DIM})
            wind_u, wind_v = extract_wind_for_year(sub, ds)

        out.loc[idx, "wind_u"] = wind_u
        out.loc[idx, "wind_v"] = wind_v

    if missing_years:
        print(
            "Missing wind files (rows left as NaN): "
            + ", ".join(f"wind_{y}.nc" for y in missing_years)
        )
        print(f"Place files in: {wind_dir}")

    return out


def main() -> None:
    if not INPUT_CSV.is_file():
        raise FileNotFoundError(f"Input not found: {INPUT_CSV}")

    df = pd.read_csv(INPUT_CSV, parse_dates=["datetime", "fledge_date"])
    result = add_wind_columns(df)
    result.to_csv(OUTPUT_CSV, index=False)

    n_ok = result["wind_u"].notna().sum()
    print(f"Wrote {len(result):,} rows to {OUTPUT_CSV}")
    print(f"wind_u / wind_v populated: {n_ok:,} ({100 * n_ok / len(result):.1f}%)")


if __name__ == "__main__":
    main()
