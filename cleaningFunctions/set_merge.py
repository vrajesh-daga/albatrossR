import os

import pandas as pd

CANONICAL_COLS = [
    "Tag",
    "Point number",
    "datetime",
    "type",
    "latitude",
    "longitude",
    "prev_lat",
    "prev_long",
    "colony_lat",
    "colony_long",
    "correct_step_distance",
]


def _series_to_iso_date_strings(s: pd.Series) -> pd.Series:
    """Parse datetimes (mixed ISO precision etc.), output YYYY-MM-DD strings."""
    ts = pd.to_datetime(s, utc=True, errors="coerce", format="mixed")
    if isinstance(ts.dtype, pd.DatetimeTZDtype):
        ts = ts.dt.tz_convert("UTC").dt.tz_localize(None)
    midnight = ts.dt.normalize()
    out = midnight.dt.strftime("%Y-%m-%d")
    return out.where(midnight.notna(), pd.NA)


def standardize_left(df: pd.DataFrame) -> pd.DataFrame:
    out = df.loc[:, [c for c in CANONICAL_COLS if c in df.columns]].copy()
    missing = set(CANONICAL_COLS) - set(out.columns)
    if missing:
        raise ValueError(f"left frame missing columns: {sorted(missing)}")
    out = out.reindex(columns=CANONICAL_COLS)
    out["datetime"] = _series_to_iso_date_strings(out["datetime"])
    return out


def standardize_og7(df: pd.DataFrame) -> pd.DataFrame:
    rename_map = {
        "BirdID": "Tag",
        "Data point #": "Point number",
        "Fix type": "type",
        "compensatedlat": "latitude",
        "long": "longitude",
    }
    missing_src = set(rename_map) - set(df.columns)
    if missing_src:
        raise ValueError(f"og7 frame missing columns: {sorted(missing_src)}")
    out = df.rename(columns=rename_map)
    required_before_datetime = [c for c in CANONICAL_COLS if c != "datetime"]
    for c in required_before_datetime:
        if c not in out.columns:
            raise ValueError(f"og7 frame missing column after rename: {c}")
    if "date" not in df.columns or "midvalue" not in df.columns:
        raise ValueError("og7 frame needs date and midvalue to build datetime")

    date_part = out["date"].astype(str).str.strip()
    time_part = out["midvalue"].astype(str).str.strip()
    combined = date_part + " " + time_part
    parsed = pd.to_datetime(combined, dayfirst=True, errors="coerce")
    out = out.reindex(columns=CANONICAL_COLS)
    out["datetime"] = _series_to_iso_date_strings(parsed)
    return out


def merge_chick_step_datasets(
    left_path: str,
    right_path: str,
) -> pd.DataFrame:
    left_df = pd.read_csv(left_path)
    right_df = pd.read_csv(right_path)
    left_std = standardize_left(left_df)
    right_std = standardize_og7(right_df)
    combined = pd.concat([left_std, right_std], ignore_index=True)
    combined["Tag"] = pd.to_numeric(combined["Tag"], errors="coerce").astype("Int64")
    combined["Point number"] = pd.to_numeric(combined["Point number"], errors="coerce").astype(
        "Int64"
    )
    return combined


def _default_paths() -> tuple[str, str]:
    root = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
    left = os.path.join(root, "original-datasets", "2013-2024correct_step_size.csv")
    right = os.path.join(root, "original-datasets", "og7chicks_nofilter_correctstepdistance.csv")
    return left, right


if __name__ == "__main__":
    left_p, right_p = _default_paths()
    combined = merge_chick_step_datasets(left_p, right_p)
    out_root = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
    out_xlsx = os.path.join(out_root, "mergedchickdata(o7&2013-2014).xlsx")
    combined.to_excel(out_xlsx, index=False)
