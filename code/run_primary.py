#!/usr/bin/env python3
"""Refit RQ1 Model A and RQ2 after audited Claude-provenance corrections."""

from __future__ import annotations

import argparse
import collections
import csv
import gzip
import hashlib
import io
import json
import math
from pathlib import Path
import subprocess
import sys
from datetime import datetime, timezone
from zipfile import ZipFile, ZIP_DEFLATED


DESKTOP = Path.home() / "Desktop"
ROOT = DESKTOP / "Courses" / "trust_paper_aidev76_v5"
CORRECTION = DESKTOP / "trust_claude_correction_scenarios_20260924_002827.zip"
AUDIT = DESKTOP / "trust_rq2_design_audit_20260921_191552_128113.zip"
RQ1_LABEL = "687ac9d28d78"
VERSION = "1.0.1"
EXPECTED = {
    "expanded_primary": {
        "exclusion_keys": 1337,
        "rq1_all_rows": 101276,
        "rq1_excluded_human_rows": 1337,
        "rq1_model_a_resolved_rows": 92978,
        "rq2_resolved_rows": 92958,
        "rq2_resolved_human_rows_excluded": 1250,
        "rq2_time_aware_cohort_prs": 101207,
        "rq2_time_aware_open_censored_prs": 8249,
        "rq2_time_aware_risk_intervals": 146013,
    },
    "core_sensitivity": {
        "exclusion_keys": 169,
        "rq1_all_rows": 102444,
        "rq1_excluded_human_rows": 169,
        "rq1_model_a_resolved_rows": 94089,
        "rq2_resolved_rows": 94069,
        "rq2_resolved_human_rows_excluded": 139,
        "rq2_time_aware_cohort_prs": 102375,
        "rq2_time_aware_open_censored_prs": 8306,
        "rq2_time_aware_risk_intervals": 147242,
    },
}


def sha(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as f:
        for block in iter(lambda: f.read(1024 * 1024), b""):
            h.update(block)
    return h.hexdigest()


def member(z: ZipFile, suffix: str) -> str:
    found = [name for name in z.namelist() if name.endswith("/" + suffix)]
    if len(found) != 1:
        raise ValueError(f"Expected one archive member ending in {suffix}; found {len(found)}")
    return found[0]


def zip_csv(z: ZipFile, suffix: str, compressed: bool = False):
    with z.open(member(z, suffix)) as raw:
        if compressed:
            with gzip.GzipFile(fileobj=raw) as decoded:
                yield from csv.DictReader(io.TextIOWrapper(decoded, encoding="utf-8", newline=""))
        else:
            yield from csv.DictReader(io.TextIOWrapper(raw, encoding="utf-8", newline=""))


def truth(value) -> bool:
    if isinstance(value, bool):
        return value
    value = str(value).strip().lower()
    if value in {"true", "1", "yes", "y"}:
        return True
    if value in {"false", "0", "no", "n"}:
        return False
    raise ValueError(f"Unexpected Boolean: {value!r}")


def timestamp(value: str) -> datetime:
    parsed = datetime.fromisoformat(value.replace("Z", "+00:00"))
    if parsed.tzinfo is None:
        raise ValueError("Timestamp has no UTC offset")
    return parsed


def load_csv(path: Path):
    with path.open(newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))


def write_csv(path: Path, rows, fields=None):
    rows = list(rows)
    if fields is None:
        if not rows:
            raise ValueError(f"Cannot infer fields for empty output: {path}")
        fields = list(rows[0])
    with path.open("w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=fields, extrasaction="ignore")
        w.writeheader()
        w.writerows(rows)


def run_r(script: Path, input_path: Path, output: Path, log_name: str, prepare_only: bool):
    if prepare_only:
        return
    result = subprocess.run(
        ["Rscript", str(script), str(input_path), str(output)],
        stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True,
    )
    (output / log_name).write_text(result.stdout, encoding="utf-8")
    if result.returncode:
        print(result.stdout, file=sys.stderr)
        raise RuntimeError(f"R model failed; prepared files retained at {output}")


def load_corrections(correction_zip: Path):
    with ZipFile(correction_zip) as z:
        if z.testzip() is not None:
            raise ValueError("Correction ZIP integrity failure")
        manifest = json.loads(z.read(member(z, "MANIFEST.json")))
        core_rows = list(zip_csv(z, "02_core_exclusions.csv"))
        branch_rows = list(zip_csv(z, "03_expanded_branch_only.csv"))
        inventory = list(zip_csv(z, "05_model_input_inventory.csv"))
    core = {row["canonical_pr_key"] for row in core_rows}
    branch = {row["canonical_pr_key"] for row in branch_rows}
    if len(core) != 169 or len(branch) != 1168 or core & branch:
        raise ValueError("Correction scenario counts or disjointness changed")
    if len(core | branch) != 1337:
        raise ValueError("Expanded correction count changed")
    inv = {row["model_input"]: row for row in inventory}
    return {
        "core": core,
        "expanded": core | branch,
        "inventory": inv,
        "manifest": manifest,
        "core_rows": core_rows,
        "branch_rows": branch_rows,
    }


def model_path(root: Path, label: str) -> Path:
    path = root / "data" / "model_inputs" / ("rq1_prs.csv" if label == RQ1_LABEL else "rq2_prs.csv")
    if not path.is_file():
        raise ValueError(f"Model input missing: {path}")
    return path


def validate_model(path: Path, inventory_row: dict):
    if sha(path) != inventory_row["file_sha256"]:
        raise ValueError(f"Model input SHA mismatch: {path}")


def prepare_rq1(input_path: Path, exclusions: set[str], destination: Path):
    rows = load_csv(input_path)
    keys = {row["pr_key"] for row in rows}
    missing = exclusions - keys
    if missing:
        raise ValueError(f"RQ1 input lacks {len(missing)} correction keys")
    filtered = [row for row in rows if not (row["source"] == "human" and row["pr_key"] in exclusions)]
    removed = len(rows) - len(filtered)
    if removed != len(exclusions):
        raise ValueError("RQ1 correction removed an unexpected number of rows")
    write_csv(destination, filtered, list(rows[0]))
    resolved = sum(truth(r["resolved"]) and int(r["merged_resolved_only"]) >= 0 and truth(r["candidate_spec_a_complete_case"]) for r in filtered)
    return {"all_rows": len(filtered), "excluded_human_rows": removed, "model_a_resolved_rows": resolved}


def load_rq2_audit(audit_zip: Path, rq2_input: Path):
    original_rows = load_csv(rq2_input)
    original = {row["pr_key"]: row for row in original_rows}
    if len(original) != len(original_rows):
        raise ValueError("Duplicate PR in RQ2 model input")

    with ZipFile(audit_zip) as z:
        if z.testzip() is not None:
            raise ValueError("RQ2 audit ZIP integrity failure")
        summary = json.loads(z.read(member(z, "summary.json")))
        if summary["provenance"]["input_sha256"] != sha(rq2_input):
            raise ValueError("RQ2 audit and frozen model input do not match")
        audited_rows = list(zip_csv(z, "pr_design_inventory.csv.gz", compressed=True))
        event_rows = list(zip_csv(z, "event_timing_inventory.csv.gz", compressed=True))
    if len(audited_rows) != len(original_rows):
        raise ValueError("RQ2 audit/model row count mismatch")

    resolved_rows = []
    boundaries = {}
    audited_by_key = {}
    for audited in audited_rows:
        key = audited["pr_key"]
        source = original.get(key)
        if source is None:
            raise ValueError(f"Audited PR missing in frozen input: {key}")
        audited_by_key[key] = audited
        for col in ("repository", "source", "historical_status_at_cutoff", "created_at"):
            if audited[col] != source[col]:
                raise ValueError(f"Frozen/audit mismatch for {key}: {col}")
        if not truth(audited["candidate_formal_resolution_plus_A_available"]):
            continue
        if not truth(source["candidate_spec_a_complete_case"]):
            raise ValueError(f"Incomplete Model A covariates: {key}")
        status = source["historical_status_at_cutoff"]
        if status not in {"MERGED", "CLOSED_UNMERGED"}:
            raise ValueError(f"Unresolved PR in resolved analysis: {key}")
        outcome = int(status == "MERGED")
        if int(source["merged_resolved_only"]) != outcome:
            raise ValueError(f"Outcome mismatch: {key}")
        formal_count = int(audited["formal_pre"])
        start = timestamp(audited["raw_created_at"])
        decision = timestamp(audited["candidate_resolution_timestamp"])
        if not start < decision:
            raise ValueError(f"Invalid resolution boundary: {key}")
        boundaries[key] = (start, decision)
        resolved_rows.append({
            "pr_key": key,
            "repository": source["repository"],
            "source": source["source"],
            "agentic": int(source["source"] == "agentic"),
            "merged": outcome,
            "formal_review": int(formal_count > 0),
            "audited_formal_count": formal_count,
            "inline_review_comment": 0,
            "log1p_files_changed": source["log1p_files_changed"],
            "test_file_presence": int(truth(source["test_file_presence"])),
            "log1p_description_word_count": source["log1p_description_word_count"],
            "duration_hours": (decision - start).total_seconds() / 3600.0,
        })

    formal_counts = collections.Counter()
    inline_counts = collections.Counter()
    event_ids = set()
    for event in event_rows:
        key = event["pr_key"]
        if key not in boundaries or event["actor_category"] != "EXTERNAL_NONBOT" or not event["effective_timestamp"]:
            continue
        component = event["component"]
        if component == "reviews" and event["current_snapshot_state"] == "PENDING":
            continue
        if component == "review_comments" and event["parent_status"] != "submitted":
            continue
        if component not in {"reviews", "review_comments"}:
            continue
        stamp = timestamp(event["effective_timestamp"])
        start, decision = boundaries[key]
        if not start <= stamp < decision:
            continue
        identity = (key, component, event["event_id"])
        if not event["event_id"] or identity in event_ids:
            raise ValueError("Empty or duplicate event ID")
        event_ids.add(identity)
        if component == "reviews":
            formal_counts[key] += 1
        else:
            inline_counts[key] += 1

    for row in resolved_rows:
        key = row["pr_key"]
        if formal_counts[key] != row["audited_formal_count"]:
            raise ValueError(f"Formal event reconstruction mismatch: {key}")
        row["inline_review_comment"] = int(inline_counts[key] > 0)
        if row["inline_review_comment"] > row["formal_review"]:
            raise ValueError(f"Inline signal without formal review: {key}")

    return summary, original, audited_rows, event_rows, resolved_rows


def filter_rq2(rows, exclusions: set[str]):
    keys = {row["pr_key"] for row in rows if row["source"] == "human"}
    relevant = exclusions & keys
    filtered = [row for row in rows if not (row["source"] == "human" and row["pr_key"] in exclusions)]
    return filtered, len(relevant)


def prepare_time_aware(summary, audited_rows, event_rows, resolved_keys: set[str], exclusions: set[str], destination: Path):
    cutoff = timestamp(summary["cutoff"])
    cohort = {}
    audited_counts = {}
    open_count = 0
    for row in audited_rows:
        key = row["pr_key"]
        status = row["historical_status_at_cutoff"]
        selected_resolved = key in resolved_keys
        if not selected_resolved and status != "OPEN":
            continue
        if not all(truth(row[name]) for name in (
            "metadata_identity_and_time_valid", "manifest_raw_author_agree",
            "formal_measure_available", "inline_measure_available",
        )):
            if selected_resolved:
                raise ValueError(f"Resolved PR lacks time-aware audit support: {key}")
            continue
        if row["source"] == "human" and key in exclusions:
            continue
        created = timestamp(row["raw_created_at"])
        end = cutoff if status == "OPEN" else timestamp(row["candidate_resolution_timestamp"])
        if not created < end <= cutoff:
            raise ValueError(f"Invalid time-aware period: {key}")
        cohort[key] = {
            "created": created,
            "end": end,
            "status": status,
            "repository": row["repository"],
            "agentic": int(row["source"] == "agentic"),
            "log1p_files_changed": row["log1p_files_changed"],
            "test_file_presence": int(truth(row["test_file_presence"])),
            "log1p_description_word_count": row["log1p_description_word_count"],
        }
        if status == "OPEN":
            open_count += 1
        audited_counts[key] = (
            int(row["formal_by_cutoff"] if status == "OPEN" else row["formal_pre"]),
            int(row["inline_by_cutoff"] if status == "OPEN" else row["inline_pre"]),
        )

    counts = collections.defaultdict(collections.Counter)
    first = collections.defaultdict(dict)
    ids = set()
    for row in event_rows:
        key = row["pr_key"]
        if key not in cohort or row["component"] not in {"reviews", "review_comments"}:
            continue
        if row["actor_category"] != "EXTERNAL_NONBOT" or not row["effective_timestamp"]:
            continue
        if row["component"] == "reviews" and row["current_snapshot_state"] == "PENDING":
            continue
        if row["component"] == "review_comments" and row["parent_status"] != "submitted":
            continue
        stamp = timestamp(row["effective_timestamp"])
        sample = cohort[key]
        in_window = sample["created"] <= stamp <= sample["end"] if sample["status"] == "OPEN" else sample["created"] <= stamp < sample["end"]
        if not in_window:
            continue
        identity = (key, row["component"], row["event_id"])
        if not row["event_id"] or identity in ids:
            raise ValueError("Empty or duplicate time-aware event ID")
        ids.add(identity)
        counts[key][row["component"]] += 1
        if stamp < sample["end"]:
            previous = first[key].get(row["component"])
            if previous is None or stamp < previous:
                first[key][row["component"]] = stamp

    for key in cohort:
        observed = (counts[key]["reviews"], counts[key]["review_comments"])
        if observed != audited_counts[key]:
            raise ValueError(f"Time-aware event count mismatch: {key}")
        if "review_comments" in first[key] and (
            "reviews" not in first[key] or first[key]["review_comments"] < first[key]["reviews"]
        ):
            raise ValueError(f"Inline event precedes formal review: {key}")

    population = collections.Counter()
    transitions = collections.Counter()
    n_intervals = 0
    with gzip.open(destination, "wt", encoding="utf-8", newline="") as f:
        fields = [
            "pr_key", "repository", "agentic", "log1p_files_changed",
            "test_file_presence", "log1p_description_word_count",
            "tstart_seconds", "tstop_seconds", "formal_t", "inline_t", "terminal_event",
        ]
        w = csv.DictWriter(f, fieldnames=fields)
        w.writeheader()
        for key, sample in cohort.items():
            end_seconds = (sample["end"] - sample["created"]).total_seconds()
            changes = {kind: (stamp - sample["created"]).total_seconds() for kind, stamp in first[key].items()}
            points = sorted({0.0, end_seconds, *changes.values()})
            population[(sample["agentic"], sample["status"])] += 1
            transitions[(sample["agentic"], "formal", int("reviews" in changes))] += 1
            transitions[(sample["agentic"], "inline", int("review_comments" in changes))] += 1
            for left, right in zip(points, points[1:]):
                if right <= left:
                    raise ValueError(f"Empty risk interval: {key}")
                formal = int("reviews" in changes and changes["reviews"] <= left)
                inline = int("review_comments" in changes and changes["review_comments"] <= left)
                event = ({"OPEN": 0, "MERGED": 1, "CLOSED_UNMERGED": 2}[sample["status"]]
                         if right == end_seconds else 0)
                w.writerow({
                    "pr_key": key, "repository": sample["repository"], "agentic": sample["agentic"],
                    "log1p_files_changed": sample["log1p_files_changed"],
                    "test_file_presence": sample["test_file_presence"],
                    "log1p_description_word_count": sample["log1p_description_word_count"],
                    "tstart_seconds": left, "tstop_seconds": right,
                    "formal_t": formal, "inline_t": inline, "terminal_event": event,
                })
                n_intervals += 1
    return {
        "cohort_prs": len(cohort), "resolved_prs": len(cohort) - open_count,
        "open_censored_prs": open_count, "risk_intervals": n_intervals,
        "population": population, "transitions": transitions,
    }


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--project-root", type=Path, default=ROOT)
    ap.add_argument("--correction-zip", type=Path, default=CORRECTION)
    ap.add_argument("--audit-zip", type=Path, default=AUDIT)
    ap.add_argument("--output-parent", type=Path, default=DESKTOP)
    ap.add_argument("--prepare-only", action="store_true")
    args = ap.parse_args()
    root = args.project_root.expanduser().resolve()
    correction_zip = args.correction_zip.expanduser().resolve()
    audit_zip = args.audit_zip.expanduser().resolve()
    for path in (root, correction_zip, audit_zip):
        if not path.exists():
            ap.error(f"Required input missing: {path}")

    correction = load_corrections(correction_zip)
    if RQ1_LABEL not in correction["inventory"]:
        raise ValueError("RQ1 model input is absent from correction inventory")
    rq1_path = model_path(root, RQ1_LABEL)
    validate_model(rq1_path, correction["inventory"][RQ1_LABEL])

    with ZipFile(audit_zip) as az:
        audit_summary = json.loads(az.read(member(az, "summary.json")))
    rq2_sha = audit_summary["provenance"]["input_sha256"]
    candidates = []
    for path in sorted((root / "data" / "model_inputs").glob("*_prs.csv")):
        if sha(path) == rq2_sha:
            candidates.append(path)
    if len(candidates) != 1:
        raise ValueError(f"Expected one RQ2 frozen input; found {len(candidates)}")
    rq2_path = candidates[0]

    audit_summary, original, audited_rows, event_rows, rq2_base = load_rq2_audit(audit_zip, rq2_path)

    out_parent = args.output_parent.expanduser().resolve()
    out_parent.mkdir(parents=True, exist_ok=True)
    stamp = datetime.now(timezone.utc).strftime("%Y%m%d_%H%M%S")
    work = out_parent / f"trust_corrected_rq1_rq2_refit_{stamp}"
    work.mkdir(exist_ok=False)
    scripts = Path(__file__).resolve().parent
    scenario_summaries = []

    scenarios = [
        ("expanded_primary", correction["expanded"]),
        ("core_sensitivity", correction["core"]),
    ]
    for scenario_name, exclusions in scenarios:
        scenario_dir = work / scenario_name
        scenario_dir.mkdir()

        rq1_dir = scenario_dir / "rq1_model_a"
        rq1_dir.mkdir()
        rq1_input = rq1_dir / "model_input.csv"
        rq1_summary = prepare_rq1(rq1_path, exclusions, rq1_input)
        run_r(scripts / "fit_rq1_pr_characteristics.R", rq1_input, rq1_dir, "r_execution.log", args.prepare_only)

        rq2_dir = scenario_dir / "rq2_review_merge"
        rq2_dir.mkdir()
        rq2_rows, rq2_removed = filter_rq2(rq2_base, exclusions)
        rq2_input = rq2_dir / "model_input.csv"
        write_csv(rq2_input, rq2_rows)
        run_r(scripts / "fit_rq2_review_association.R", rq2_input, rq2_dir, "r_execution.log", args.prepare_only)

        cells = collections.Counter((r["agentic"], r["merged"], r["formal_review"], r["inline_review_comment"]) for r in rq2_rows)
        write_csv(rq2_dir / "review_signal_cells.csv", (
            {"agentic": a, "merged": y, "formal_review": f, "inline_review_comment": i, "prs": n}
            for (a, y, f, i), n in sorted(cells.items())
        ), ["agentic", "merged", "formal_review", "inline_review_comment", "prs"])

        time_dir = scenario_dir / "rq2_time_aware"
        time_dir.mkdir()
        risk_path = time_dir / "risk_intervals.csv.gz"
        time_summary = prepare_time_aware(
            audit_summary, audited_rows, event_rows,
            {row["pr_key"] for row in rq2_base}, exclusions, risk_path,
        )
        run_r(scripts / "fit_rq2_time_aware.R", risk_path, time_dir, "r_execution.log", args.prepare_only)
        write_csv(time_dir / "population.csv", (
            {"agentic": a, "status": status, "prs": n}
            for (a, status), n in sorted(time_summary["population"].items())
        ), ["agentic", "status", "prs"])
        write_csv(time_dir / "event_transitions.csv", (
            {"agentic": a, "signal": signal, "observed_before_terminal": yes, "prs": n}
            for (a, signal, yes), n in sorted(time_summary["transitions"].items())
        ), ["agentic", "signal", "observed_before_terminal", "prs"])

        scenario_summary = {
            "scenario": scenario_name,
            "exclusion_keys": len(exclusions),
            **{f"rq1_{k}": v for k, v in rq1_summary.items()},
            "rq2_resolved_rows": len(rq2_rows),
            "rq2_resolved_human_rows_excluded": rq2_removed,
            "rq2_time_aware_cohort_prs": time_summary["cohort_prs"],
            "rq2_time_aware_open_censored_prs": time_summary["open_censored_prs"],
            "rq2_time_aware_risk_intervals": time_summary["risk_intervals"],
        }
        if scenario_summary != {"scenario": scenario_name, **EXPECTED[scenario_name]}:
            raise ValueError(
                f"Prepared population changed for {scenario_name}: "
                f"observed={scenario_summary}, expected={EXPECTED[scenario_name]}"
            )
        scenario_summaries.append(scenario_summary)
        (scenario_dir / "provenance.json").write_text(json.dumps({
            "scenario": scenario_name,
            "correction_zip_sha256": sha(correction_zip),
            "audit_zip_sha256": sha(audit_zip),
            "rq1_input_sha256": sha(rq1_path),
            "rq2_input_sha256": sha(rq2_path),
            "agentic_rows_reclassified": 0,
            "human_rows_excluded_only": True,
            "summary": scenario_summary,
        }, indent=2) + "\n", encoding="utf-8")

    write_csv(work / "scenario_population_summary.csv", scenario_summaries)
    (work / "README.md").write_text(
        "Corrected RQ1/RQ2 refit. Expanded Claude provenance exclusion is the primary analysis. "
        "Core direct-disclosure exclusion is the sensitivity analysis. Agentic rows were not relabelled.\n",
        encoding="utf-8",
    )
    provenance = {
        "tool_version": VERSION,
        "generated_at_utc": datetime.now(timezone.utc).isoformat(),
        "project_root": str(root),
        "correction_zip_sha256": sha(correction_zip),
        "audit_zip_sha256": sha(audit_zip),
        "rq1_model_label": RQ1_LABEL,
        "rq1_input_sha256": sha(rq1_path),
        "rq2_input_sha256": sha(rq2_path),
        "models_fitted": not args.prepare_only,
        "primary_scenario": "expanded_primary",
        "sensitivity_scenario": "core_sensitivity",
        "scenario_summaries": scenario_summaries,
    }
    (work / "provenance.json").write_text(json.dumps(provenance, indent=2) + "\n", encoding="utf-8")

    target = work.with_suffix(".zip")
    with ZipFile(target, "x", compression=ZIP_DEFLATED, compresslevel=4) as z:
        for path in sorted(work.rglob("*")):
            if path.is_file():
                z.write(path, arcname=str(Path(work.name) / path.relative_to(work)))
        for script in ("run_primary.py", "fit_rq1_pr_characteristics.R", "fit_rq2_review_association.R", "fit_rq2_time_aware.R"):
            z.write(scripts / script, arcname=f"{work.name}/code/{script}")
    with ZipFile(target) as z:
        if z.testzip() is not None:
            raise ValueError("Output ZIP integrity failure")
    print("Primary scenario: expanded Claude provenance exclusion")
    print("Sensitivity scenario: direct-disclosure core exclusion")
    for row in scenario_summaries:
        print(row["scenario"], "RQ1 Model A rows:", row["rq1_model_a_resolved_rows"],
              "RQ2 resolved rows:", row["rq2_resolved_rows"],
              "time-aware cohort:", row["rq2_time_aware_cohort_prs"])
    print("Models fitted:", not args.prepare_only)
    print("UPLOAD THIS ZIP:", target)


if __name__ == "__main__":
    try:
        main()
    except Exception as exc:
        print("FAILED:", exc, file=sys.stderr)
        sys.exit(1)
