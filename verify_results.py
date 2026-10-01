#!/usr/bin/env python3
"""Compare reproduced model result CSVs against the archived reference ZIPs."""

import argparse
import csv
import io
import json
import math
from pathlib import Path
from zipfile import ZipFile


def selected(archive):
    data = {}
    for name in archive.namelist():
        parts = name.split("/", 1)
        if len(parts) != 2:
            continue
        relative = parts[1]
        if not relative.endswith(".csv"):
            continue
        if any(skip in relative for skip in (
            "/model_input.csv", "/risk_intervals.csv", "/r_execution.log",
        )):
            continue
        # Result and support tables are small. Prepared intermediate data are
        # checked by the original pipeline's own input and cohort assertions.
        if archive.getinfo(name).file_size > 2_000_000:
            continue
        data[relative] = list(csv.DictReader(io.StringIO(archive.read(name).decode("utf-8-sig"))))
    return data


def equal_cell(a, b):
    if a == b:
        return True
    try:
        x, y = float(a), float(b)
    except (ValueError, TypeError):
        return False
    if not math.isfinite(x) or not math.isfinite(y):
        return False
    return math.isclose(x, y, rel_tol=1e-7, abs_tol=1e-8)


def compare(reference, new):
    differences = []
    with ZipFile(reference) as original, ZipFile(new) as reproduced:
        if original.testzip() or reproduced.testzip():
            raise ValueError("Corrupt reference or reproduced ZIP")
        expected, actual = selected(original), selected(reproduced)
        if not expected or not actual:
            raise ValueError("No comparable result CSVs in reference or reproduced ZIP")
        for name in sorted(set(expected) | set(actual)):
            if name not in expected or name not in actual:
                differences.append(f"{name}: missing in {'reference' if name not in expected else 'new'} ZIP")
                continue
            rows_a, rows_b = expected[name], actual[name]
            if len(rows_a) != len(rows_b):
                differences.append(f"{name}: {len(rows_a)} vs {len(rows_b)} rows")
                continue
            for number, (row_a, row_b) in enumerate(zip(rows_a, rows_b), 2):
                if row_a.keys() != row_b.keys():
                    differences.append(f"{name}:{number}: columns differ")
                    break
                for column in row_a:
                    if not equal_cell(row_a[column], row_b[column]):
                        differences.append(f"{name}:{number}:{column}: {row_a[column]!r} vs {row_b[column]!r}")
                        break
                if len(differences) >= 30:
                    break
            if len(differences) >= 30:
                break
    return {"compared_csvs": len(expected), "mismatches": differences}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ("reference_refit", "new_refit", "reference_closure", "new_closure", "report"):
        parser.add_argument("--" + name.replace("_", "-"), type=Path, required=True)
    args = parser.parse_args()
    result = {
        "refit": compare(args.reference_refit, args.new_refit),
        "closure": compare(args.reference_closure, args.new_closure),
        "interpretation": "Numeric cells use rel_tol=1e-7 and abs_tol=1e-8; no manuscript text was parsed.",
    }
    args.report.parent.mkdir(parents=True, exist_ok=True)
    args.report.write_text(json.dumps(result, indent=2) + "\n", encoding="utf-8")
    for label in ("refit", "closure"):
        info = result[label]
        print(label, "compared CSV files:", info["compared_csvs"], "mismatches:", len(info["mismatches"]))
        for difference in info["mismatches"][:10]:
            print("  ", difference)
    if any(result[label]["mismatches"] for label in ("refit", "closure")):
        raise SystemExit(1)


if __name__ == "__main__":
    main()
