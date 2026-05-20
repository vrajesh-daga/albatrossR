"""
Input:  altered-datasets/all_chick_data_final_filter.csv
Output: altered-datasets/all_chick_data_final_filter_enriched.csv
"""

from __future__ import annotations

import calendar
import os

import numpy as np
import pandas as pd

COLONY_LAT = 21.5752667
COLONY_LONG = -158.2733528
EARTH_RADIUS_KM = 6371

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
INPUT_CSV = os.path.join(ROOT, "altered-datasets", "all_chick_data_final_filter.csv")
OUTPUT_CSV = os.path.join(
    ROOT, "altered-datasets", "all_chick_data_final_filter_enriched.csv"
)

MONTH_NAMES = [calendar.month_name[i] for i in range(1, 13)]


def haversine_km(lat1, lon1, lat2, lon2) -> np.ndarray:
    lat1_rad = np.radians(lat1)
    lon1_rad = np.radians(lon1)
    lat2_rad = np.radians(lat2)
    lon2_rad = np.radians(lon2)
    dlat = np.abs(lat1_rad - lat2_rad)
    dlon = np.abs(lon1_rad - lon2_rad)
    return (2 * EARTH_RADIUS_KM) * np.arcsin(
        np.sqrt(
            (np.sin(dlat / 2) ** 2)
            + np.cos(lat1_rad) * np.cos(lat2_rad) * (np.sin(dlon / 2) ** 2)
        )
    )


def months_post_fledge(fledge: pd.Series, dt: pd.Series) -> pd.Series:
    """Whole months elapsed since fledge (0 if < 1 full month)."""
    months = (dt.dt.year - fledge.dt.year) * 12 + (dt.dt.month - fledge.dt.month)
    return months - (dt.dt.day < fledge.dt.day).astype(int)


def years_post_fledge_bin(months: pd.Series) -> pd.Series:
    labels = np.select(
        [
            months <= 12,
            (months > 12) & (months <= 36),
            (months > 36) & (months <= 60),
        ],
        [
            "year 1",
            "year 2-3",
            "year 4-5",
        ],
        default=pd.NA,
    )
    return pd.Series(labels, index=months.index, dtype="string")


def prepare_final_filter(df: pd.DataFrame) -> pd.DataFrame:
    out = df.copy()

    out["datetime"] = pd.to_datetime(out["datetime"])
    fledge = pd.to_datetime(out["Start_Date"])

    out = out.drop(columns=["date_parsed"])

    out["Month"] = out["datetime"].dt.month.map(
        lambda m: MONTH_NAMES[int(m) - 1]
    )
    out["Day"] = out["datetime"].dt.dayofyear

    out = out.rename(columns={"Start_Date": "fledge_date"})
    out["fledge_date"] = fledge

    out["cohort"] = out["fledge_date"].dt.year
    out["months_post_fledge"] = months_post_fledge(out["fledge_date"], out["datetime"])
    out["years_post_fledge"] = years_post_fledge_bin(out["months_post_fledge"])

    out["distance_from_colony"] = haversine_km(
        out["latitude"],
        out["longitude"],
        out["colony_lat"].fillna(COLONY_LAT),
        out["colony_long"].fillna(COLONY_LONG),
    )

    return out


def main() -> None:
    df = pd.read_csv(INPUT_CSV)
    result = prepare_final_filter(df)
    result.to_csv(OUTPUT_CSV, index=False)
    print(f"Wrote {len(result):,} rows to {OUTPUT_CSV}")


if __name__ == "__main__":
    main()
