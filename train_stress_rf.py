#!/usr/bin/env python3
"""
Train a binary **stress‑detection** model on the WESAD dataset using only
five HRV features (SDNN, RMSSD, LF power, HF power, LF/HF ratio).

Key choices made to match the official WESAD README (excerpted in the
conversation):

* Only the **synchronised RespiBAN chest ECG** contained in each `SX.pkl`
  file is used (700 Hz, already time‑aligned with the labels).
* Subjects **S1** and **S12** are **excluded** because their data is
  missing according to the README.
* Label mapping follows the README’s protocol IDs (III.1):
  * `2 → stress (positive class = 1)`
  * `1 → baseline`, everything else (`0, 3, 4`) is treated as
    *non‑stress* (`0`). Windows containing IDs 5‑7 are discarded.
* 60 s windows with 30 s overlap are used, which is standard for
  frequency‑domain HRV, giving ~50 % correlation but enough samples.
* R‑peak detection and HRV computation are done with NeuroKit2 **0.2.10
  or later**. The PSD is obtained via `psd_method="welch"`, which is the
  fully supported API.
* Windows where R‑peak detection fails or returns fewer than three R‑R
  intervals are skipped.
* **Balanced** `RandomForestClassifier` is trained with
  **Leave‑One‑Subject‑Out** (LOSO) external CV and nested 3‑fold grid
  search on the training folds.
* The full preprocessing‑plus‑estimator pipeline is serialised to
  `stress_rf.pkl` so the *same* scaling is applied at inference time.

Run:
```
python3 train_stress_rf.py --data_dir ~/datasets --skip-download
```
The script will print a LOSO summary (Acc, AUC, F1 ± SD) and save the
final model.
"""

from __future__ import annotations

import argparse
import pickle
import warnings
import zipfile
from collections import defaultdict
from pathlib import Path
from typing import Iterable, List

import joblib
import neurokit2 as nk
import numpy as np
import pandas as pd
from scipy.signal import welch  # just to ensure SciPy is present
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import (
    accuracy_score,
    classification_report,
    roc_auc_score,
)
from sklearn.model_selection import GridSearchCV, LeaveOneGroupOut
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler
from tqdm import tqdm

# -----------------------------------------------------------------------------
# Constants & Dataset info
# -----------------------------------------------------------------------------
DATA_URL = "https://ubicomp.eti.uni-siegen.de/home/datasets/icmi18/WESAD.zip"
SEG_LEN_S = 60        # window length in seconds
STEP_LEN_S = 30       # step (overlap 50 %)
FS = 700              # ECG sampling frequency (Hz)
INVALID_SUBJECTS = {"S1", "S12"}  # folders absent per README
STRESS_LABEL = 2      # protocol ID 2 → stress
IGNORED_LABELS = {5, 6, 7}  # per README, ignore these completely

# -----------------------------------------------------------------------------
# Utility functions
# -----------------------------------------------------------------------------

def download_dataset(target_dir: Path) -> None:
    """Download & unzip WESAD (≈1.1 GB) unless already present."""
    zip_path = target_dir / "WESAD.zip"
    wesad_root = target_dir / "WESAD"
    if wesad_root.exists():
        print("✔ WESAD already present – skipping download.")
        return
    target_dir.mkdir(parents=True, exist_ok=True)
    print(f"⬇ Downloading WESAD → {zip_path}")
    import urllib.request

    urllib.request.urlretrieve(DATA_URL, zip_path)
    print("✔ Downloaded. Extracting…")
    with zipfile.ZipFile(zip_path, "r") as zf:
        zf.extractall(target_dir)
    print("✔ Extraction complete.")


def window_start_stop(n_samples: int, seg_len: int, step_len: int) -> Iterable[tuple[int, int]]:
    for start in range(0, n_samples - seg_len + 1, step_len):
        yield start, start + seg_len


def majority_label(labels: np.ndarray) -> int:
    """Return the most frequent label in a 1‑D int array."""
    uniq, counts = np.unique(labels, return_counts=True)
    return int(uniq[np.argmax(counts)])


# -----------------------------------------------------------------------------
# Feature extraction
# -----------------------------------------------------------------------------

def extract_features_for_subject(
    ecg: np.ndarray, labels: np.ndarray, subject_id: str
) -> list[dict]:
    seg_len = SEG_LEN_S * FS
    step_len = STEP_LEN_S * FS
    feature_rows: List[dict] = []

    for s, e in window_start_stop(len(ecg), seg_len, step_len):
        win_labels = labels[s:e]
        proto_label = majority_label(win_labels)
        if proto_label in IGNORED_LABELS:
            continue  # skip meditation, etc.
        is_stress = 1 if proto_label == STRESS_LABEL else 0

        # Extract HRV from this window
        try:
            _, info = nk.ecg_process(ecg[s:e], sampling_rate=FS)
            rpeaks = info["ECG_R_Peaks"]
            if len(rpeaks) < 3:
                raise ValueError("Too few R‑peaks")
            hrv_time = nk.hrv_time(rpeaks, sampling_rate=FS, show=False)
            hrv_freq = nk.hrv_frequency(
                rpeaks, sampling_rate=FS, psd_method="welch", show=False
            )
            row = {
                "SDNN": float(hrv_time["HRV_SDNN"][0]),
                "RMSSD": float(hrv_time["HRV_RMSSD"][0]),
                "LF": float(hrv_freq["HRV_LF"][0]),
                "HF": float(hrv_freq["HRV_HF"][0]),
                "LF_HF": float(hrv_freq["HRV_LF"][0] / hrv_freq["HRV_HF"][0])
                if hrv_freq["HRV_HF"][0] > 0
                else np.nan,
                "label": is_stress,
                "subject": subject_id,
            }
            feature_rows.append(row)
        except Exception as err:
            warnings.warn(f"{subject_id}: bad window ({s}:{e}) → {err}")
    return feature_rows


# -----------------------------------------------------------------------------
# End‑to‑end pipeline
# -----------------------------------------------------------------------------

def build_feature_dataframe(wesad_root: Path) -> pd.DataFrame:
    all_rows: List[dict] = []
    for subj_dir in sorted(wesad_root.glob("S*")):
        if subj_dir.name in INVALID_SUBJECTS:
            continue
        pkl_path = subj_dir / f"{subj_dir.name}.pkl"
        with open(pkl_path, "rb") as f:
            data = pickle.load(f, encoding="bytes")
        ecg = data[b"signal"][b"chest"][b"ECG"][:, 0]  # (n_samples, 1) → 1‑D
        labels = data[b"label"]  # already at 700 Hz
        rows = extract_features_for_subject(ecg, labels, subj_dir.name)
        all_rows.extend(rows)

    df = pd.DataFrame(all_rows)
    df.dropna(inplace=True)
    return df


def train_and_evaluate(df: pd.DataFrame) -> None:
    X = df[["SDNN", "RMSSD", "LF", "HF", "LF_HF"]].values
    y = df["label"].values
    groups = df["subject"].values

    logo = LeaveOneGroupOut()
    pipe = Pipeline(
        [
            ("scaler", StandardScaler()),
            (
                "rf",
                RandomForestClassifier(
                    class_weight="balanced", random_state=42, n_estimators=300
                ),
            ),
        ]
    )
    param_grid = {
        "rf__max_depth": [None, 8, 16],
        "rf__max_features": ["sqrt", 0.8],
    }

    scores = defaultdict(list)
    for train_idx, test_idx in tqdm(logo.split(X, y, groups), desc="LOSO"):
        X_tr, X_te = X[train_idx], X[test_idx]
        y_tr, y_te = y[train_idx], y[test_idx]
        gscv = GridSearchCV(pipe, param_grid, cv=3, n_jobs=-1, scoring="f1")
        gscv.fit(X_tr, y_tr)
        y_pred = gscv.predict(X_te)
        y_prob = gscv.predict_proba(X_te)[:, 1]
        scores["acc"].append(accuracy_score(y_te, y_pred))
        scores["auc"].append(roc_auc_score(y_te, y_prob))
        scores["f1"].append(
            classification_report(y_te, y_pred, output_dict=True)["weighted avg"][
                "f1-score"
            ]
        )

    print("\n==== LOSO summary ====")
    for k, v in scores.items():
        print(f"{k.upper():4}: {np.mean(v):.3f} ± {np.std(v):.3f}")

    # Train final model on the *entire* dataset with best params
    final_gs = GridSearchCV(pipe, param_grid, cv=3, n_jobs=-1, scoring="f1")
    final_gs.fit(X, y)
    joblib.dump(final_gs.best_estimator_, "stress_rf.pkl")
    print("\n✔ Saved full pipeline to stress_rf.pkl")


# -----------------------------------------------------------------------------
# Main entry‑point
# -----------------------------------------------------------------------------

def main(data_dir: str, skip_download: bool = True):
    data_path = Path(data_dir).expanduser()
    if not skip_download:
        download_dataset(data_path)
    df = build_feature_dataframe(data_path / "WESAD")
    print(f"Feature matrix: {df.shape[0]} windows × {df.shape[1]-2} features")
    train_and_evaluate(df)


if __name__ == "__main__":
    ap = argparse.ArgumentParser(description="WESAD stress‑detection trainer")
    ap.add_argument("--data_dir", default="~/datasets", help="WESAD root directory")
    ap.add_argument("--skip-download", action="store_true", help="Do not (re)download data")
    args = ap.parse_args()
    main(args.data_dir, args.skip_download)
