#!/usr/bin/env python3
"""Run the remaining prespecified RQ1/RQ2 interpretation and sensitivity analyses."""

from __future__ import annotations

import argparse
import collections
import csv
import gzip
import hashlib
import io
import json
import re
import subprocess
import sys
import tempfile
from datetime import datetime, timezone
from pathlib import Path
from zipfile import ZipFile, ZIP_DEFLATED


DESKTOP = Path.home() / "Desktop"
DEFAULT_REFIT = DESKTOP / "trust_corrected_rq1_rq2_refit_20260924_064727.zip"
EXPECTED_VERSION = "1.0.1"
SCENARIOS = ("expanded_primary", "core_sensitivity")
TIME_CUTS = (86400.0, 259200.0, 604800.0, 2592000.0)
TIME_BANDS = ("d00_01", "d01_03", "d03_07", "d07_30", "d30_plus")
VERSION = "1.0.0"


def member(z: ZipFile, suffix: str) -> str:
    found = [name for name in z.namelist() if name.endswith("/" + suffix)]
    if len(found) != 1:
        raise ValueError(f"Expected one member ending in {suffix}; found {len(found)}")
    return found[0]


def root_member(z: ZipFile, filename: str) -> str:
    found = [name for name in z.namelist() if name.endswith("/" + filename) and name.count("/") == 1]
    if len(found) != 1:
        raise ValueError(f"Expected one top-level {filename}; found {len(found)}")
    return found[0]


def sha(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as f:
        for block in iter(lambda: f.read(1024 * 1024), b""):
            h.update(block)
    return h.hexdigest()


def truth(value: str) -> bool:
    value = str(value).strip().lower()
    if value in {"true", "1"}:
        return True
    if value in {"false", "0"}:
        return False
    raise ValueError(f"Unexpected Boolean: {value!r}")


def read_zip_csv(z: ZipFile, suffix: str, compressed: bool = False):
    with z.open(member(z, suffix)) as raw:
        if compressed:
            with gzip.GzipFile(fileobj=raw) as decoded:
                yield from csv.DictReader(io.TextIOWrapper(decoded, encoding="utf-8", newline=""))
        else:
            yield from csv.DictReader(io.TextIOWrapper(raw, encoding="utf-8", newline=""))


def write_csv(path: Path, rows, fields=None):
    rows = list(rows)
    if fields is None:
        if not rows:
            raise ValueError(f"Cannot infer columns for empty output: {path}")
        fields = list(rows[0])
    with path.open("w", encoding="utf-8", newline="") as f:
        w = csv.DictWriter(f, fieldnames=fields, extrasaction="ignore")
        w.writeheader()
        w.writerows(rows)


def extract_member(z: ZipFile, suffix: str, destination: Path):
    with z.open(member(z, suffix)) as src, destination.open("wb") as dst:
        for block in iter(lambda: src.read(1024 * 1024), b""):
            dst.write(block)


def run_r(script: Path, args: list[Path], output_dir: Path, prepare_only: bool):
    if prepare_only:
        return
    command = ["Rscript", str(script), *map(str, args), str(output_dir)]
    result = subprocess.run(command, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
    (output_dir / "r_execution.log").write_text(result.stdout, encoding="utf-8")
    if result.returncode:
        print(result.stdout, file=sys.stderr)
        raise RuntimeError(f"R analysis failed; files retained at {output_dir}")


def safe_group(label: str) -> str:
    value = re.sub(r"[^a-z0-9]+", "_", label.lower()).strip("_")
    if not value:
        raise ValueError(f"Cannot create family identifier for {label!r}")
    return "agent_" + value


def prepare_family_inputs(rq1_rows: list[dict], rq2_rows: list[dict], work: Path):
    by_key = {}
    labels = {}
    rq1_model = []
    for row in rq1_rows:
        key = row["pr_key"]
        if key in by_key:
            raise ValueError(f"Duplicate RQ1 PR: {key}")
        if row["source"] == "human":
            gid, label = "human", "Human"
        else:
            label = row["agent_family"].strip()
            if not label:
                raise ValueError(f"Agentic PR lacks family: {key}")
            gid = safe_group(label)
        if gid in labels and labels[gid] != label:
            raise ValueError(f"Family identifier collision: {gid}")
        labels[gid] = label
        by_key[key] = {
            "group_id": gid,
            "group_label": label,
            "created_quarter": row["calendar_quarter"],
            "created_at": row["created_at"],
            "source": row["source"],
        }
        if truth(row["resolved"]) and truth(row["candidate_spec_a_complete_case"]) and int(row["merged_resolved_only"]) in (0, 1):
            rq1_model.append({
                "pr_key": key,
                "repository": row["repository"],
                "group_id": gid,
                "group_label": label,
                "source": row["source"],
                "merged": row["merged_resolved_only"],
                "log1p_files_changed": row["log1p_files_changed"],
                "test_file_presence": int(truth(row["test_file_presence"])),
                "log1p_description_word_count": row["log1p_description_word_count"],
                "created_quarter": row["calendar_quarter"],
            })

    agent_groups = sorted(g for g in labels if g != "human")
    if len(agent_groups) != 6:
        raise ValueError(f"Expected six agent families; found {len(agent_groups)}")

    rq2_model = []
    for row in rq2_rows:
        meta = by_key.get(row["pr_key"])
        if meta is None:
            raise ValueError(f"RQ2 PR missing from RQ1 model input: {row['pr_key']}")
        if meta["source"] != row["source"]:
            raise ValueError(f"Source mismatch: {row['pr_key']}")
        rq2_model.append({
            **row,
            "group_id": meta["group_id"],
            "group_label": meta["group_label"],
            "created_quarter": meta["created_quarter"],
        })

    mapping = [{"group_id": "human", "group_label": labels["human"], "group_order": 0}]
    mapping.extend(
        {"group_id": gid, "group_label": labels[gid], "group_order": i}
        for i, gid in enumerate(agent_groups, start=1)
    )
    write_csv(work / "rq1_family_input.csv", rq1_model)
    write_csv(work / "rq2_family_input.csv", rq2_model)
    write_csv(work / "family_mapping.csv", mapping)
    return rq1_model, rq2_model, mapping


def family_support(rows: list[dict], mapping: list[dict], destination: Path, rq2: bool):
    counters = collections.defaultdict(collections.Counter)
    repos = collections.defaultdict(set)
    human_repos = {r["repository"] for r in rows if r["group_id"] == "human"}
    for row in rows:
        gid = row["group_id"]
        c = counters[gid]
        c["prs"] += 1
        c["merged"] += int(row["merged"])
        c["closed_unmerged"] += 1 - int(row["merged"])
        if rq2:
            c["formal_review"] += int(row["formal_review"])
            c["inline_review_comment"] += int(row["inline_review_comment"])
        repos[gid].add(row["repository"])
    output = []
    labels = {m["group_id"]: m["group_label"] for m in mapping}
    for gid in [m["group_id"] for m in mapping]:
        row = {
            "group_id": gid,
            "group_label": labels[gid],
            "prs": counters[gid]["prs"],
            "repositories": len(repos[gid]),
            "repositories_also_with_human_prs": len(repos[gid] & human_repos),
            "merged": counters[gid]["merged"],
            "closed_unmerged": counters[gid]["closed_unmerged"],
        }
        if rq2:
            row.update({
                "formal_review": counters[gid]["formal_review"],
                "inline_review_comment": counters[gid]["inline_review_comment"],
            })
        output.append(row)
    write_csv(destination, output)


def calendar_support(rows: list[dict], destination: Path, summary_path: Path):
    quarter_rows = collections.defaultdict(list)
    for row in rows:
        quarter = row["created_quarter"]
        if not re.fullmatch(r"\d{4}-Q[1-4]", quarter):
            raise ValueError(f"Invalid quarter: {quarter!r}")
        quarter_rows[quarter].append(row)
    output = []
    for quarter in sorted(quarter_rows):
        data = quarter_rows[quarter]
        h = [r for r in data if r["source"] == "human"]
        a = [r for r in data if r["source"] == "agentic"]
        h_repos = {r["repository"] for r in h}
        a_repos = {r["repository"] for r in a}
        row = {
            "created_quarter": quarter,
            "human_prs": len(h),
            "agentic_prs": len(a),
            "human_repositories": len(h_repos),
            "agentic_repositories": len(a_repos),
            "repositories_with_both_sources": len(h_repos & a_repos),
            "human_merged": sum(int(r["merged"]) for r in h),
            "human_closed_unmerged": sum(1 - int(r["merged"]) for r in h),
            "agentic_merged": sum(int(r["merged"]) for r in a),
            "agentic_closed_unmerged": sum(1 - int(r["merged"]) for r in a),
        }
        if "formal_review" in data[0]:
            row.update({
                "human_formal_review": sum(int(r["formal_review"]) for r in h),
                "agentic_formal_review": sum(int(r["formal_review"]) for r in a),
                "human_inline_review_comment": sum(int(r["inline_review_comment"]) for r in h),
                "agentic_inline_review_comment": sum(int(r["inline_review_comment"]) for r in a),
            })
        output.append(row)
    write_csv(destination, output)
    both = [r for r in output if r["human_prs"] and r["agentic_prs"]]
    outcome_support = [r for r in both if all(r[k] > 0 for k in (
        "human_merged", "human_closed_unmerged", "agentic_merged", "agentic_closed_unmerged"
    ))]
    cox_stuart_pairs = len(output) // 2
    minimum_two_sided_p = min(1.0, 2.0 * (0.5 ** cox_stuart_pairs))
    summary_path.write_text(json.dumps({
        "quarters": len(output),
        "first_quarter": output[0]["created_quarter"],
        "last_quarter": output[-1]["created_quarter"],
        "quarters_with_both_sources": len(both),
        "quarters_with_both_sources_and_both_outcomes": len(outcome_support),
        "minimum_repositories_with_both_sources_in_supported_quarters": min(
            (r["repositories_with_both_sources"] for r in both), default=0
        ),
        "cox_stuart_pairs_if_all_quarters_used": cox_stuart_pairs,
        "minimum_attainable_two_sided_cox_stuart_p": minimum_two_sided_p,
        "cox_stuart_recommended": False,
        "trend_model_fitted": False,
        "reason": "The short series cannot provide a useful exact two-sided Cox-Stuart test, and repository coverage changes sharply across quarters.",
    }, indent=2) + "\n", encoding="utf-8")


def time_band(left: float) -> str:
    for cut, label in zip(TIME_CUTS, TIME_BANDS):
        if left < cut:
            return label
    return TIME_BANDS[-1]


def prepare_piecewise(z: ZipFile, scenario: str, destination: Path, support_path: Path):
    support = collections.Counter()
    with z.open(member(z, f"{scenario}/rq2_time_aware/risk_intervals.csv.gz")) as raw:
        with gzip.GzipFile(fileobj=raw) as decoded:
            reader = csv.DictReader(io.TextIOWrapper(decoded, encoding="utf-8", newline=""))
            fields = list(reader.fieldnames) + ["time_band"]
            with gzip.open(destination, "wt", encoding="utf-8", newline="") as out:
                writer = csv.DictWriter(out, fieldnames=fields)
                writer.writeheader()
                for row in reader:
                    left, right = float(row["tstart_seconds"]), float(row["tstop_seconds"])
                    cuts = [c for c in TIME_CUTS if left < c < right]
                    points = [left, *cuts, right]
                    for start, stop in zip(points, points[1:]):
                        item = dict(row)
                        item["tstart_seconds"] = start
                        item["tstop_seconds"] = stop
                        item["time_band"] = time_band(start)
                        if stop != right:
                            item["terminal_event"] = "0"
                        writer.writerow(item)
                        key = (
                            item["time_band"], item["agentic"], item["formal_t"],
                            item["inline_t"], item["terminal_event"]
                        )
                        support[key] += 1
    write_csv(support_path, (
        {
            "time_band": band,
            "agentic": agentic,
            "formal_t": formal,
            "inline_t": inline,
            "terminal_event": event,
            "risk_intervals": n,
        }
        for (band, agentic, formal, inline, event), n in sorted(support.items())
    ), ["time_band", "agentic", "formal_t", "inline_t", "terminal_event", "risk_intervals"])


def validate_refit(provenance: dict):
    if provenance.get("tool_version") != EXPECTED_VERSION or not provenance.get("models_fitted"):
        raise ValueError("Input is not the completed corrected refit v1.0.1")
    if provenance.get("primary_scenario") != "expanded_primary":
        raise ValueError("Unexpected primary scenario")
    summaries = {r["scenario"]: r for r in provenance.get("scenario_summaries", [])}
    if set(summaries) != set(SCENARIOS):
        raise ValueError("Correction scenarios are incomplete")
    expected = {
        "expanded_primary": (92978, 92958, 101207),
        "core_sensitivity": (94089, 94069, 102375),
    }
    for scenario, values in expected.items():
        observed = summaries[scenario]
        actual = (
            observed["rq1_model_a_resolved_rows"], observed["rq2_resolved_rows"],
            observed["rq2_time_aware_cohort_prs"],
        )
        if actual != values:
            raise ValueError(f"Population mismatch for {scenario}: {actual}")


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--refit-zip", type=Path, default=DEFAULT_REFIT)
    ap.add_argument("--output-parent", type=Path, default=DESKTOP)
    ap.add_argument("--prepare-only", action="store_true")
    args = ap.parse_args()
    refit = args.refit_zip.expanduser().resolve()
    if not refit.is_file():
        ap.error(f"Refit ZIP not found: {refit}")
    scripts = Path(__file__).resolve().parent
    stamp = datetime.now(timezone.utc).strftime("%Y%m%d_%H%M%S")
    output = args.output_parent.expanduser().resolve() / f"trust_analysis_closure_{stamp}"
    output.mkdir(parents=True, exist_ok=False)

    with ZipFile(refit) as z, tempfile.TemporaryDirectory(prefix="trust_analysis_closure_") as temp_name:
        if z.testzip() is not None:
            raise ValueError("Refit ZIP integrity failure")
        provenance = json.loads(z.read(root_member(z, "provenance.json")))
        validate_refit(provenance)
        temp = Path(temp_name)
        primary_inputs = None
        for scenario in SCENARIOS:
            scenario_out = output / scenario
            scenario_out.mkdir()
            scenario_work = temp / scenario
            scenario_work.mkdir()
            rq1_path = scenario_work / "rq1.csv"
            rq2_path = scenario_work / "rq2.csv"
            extract_member(z, f"{scenario}/rq1_model_a/model_input.csv", rq1_path)
            extract_member(z, f"{scenario}/rq2_review_merge/model_input.csv", rq2_path)
            with rq1_path.open(encoding="utf-8", newline="") as f:
                rq1_rows = list(csv.DictReader(f))
            with rq2_path.open(encoding="utf-8", newline="") as f:
                rq2_rows = list(csv.DictReader(f))

            rq1_family, rq2_family, mapping = prepare_family_inputs(rq1_rows, rq2_rows, scenario_work)
            family_dir = scenario_out / "family_interpretation"
            family_dir.mkdir()
            family_support(rq1_family, mapping, family_dir / "rq1_family_support.csv", rq2=False)
            family_support(rq2_family, mapping, family_dir / "rq2_family_support.csv", rq2=True)
            (family_dir / "family_mapping.csv").write_bytes((scenario_work / "family_mapping.csv").read_bytes())
            run_r(
                scripts / "fit_rq1_rq2_agent_families.R",
                [scenario_work / "rq1_family_input.csv", scenario_work / "rq2_family_input.csv", scenario_work / "family_mapping.csv"],
                family_dir, args.prepare_only,
            )

            calendar_dir = scenario_out / "calendar_support"
            calendar_dir.mkdir()
            calendar_support(rq1_family, calendar_dir / "rq1_quarter_support.csv", calendar_dir / "rq1_support_summary.json")
            calendar_support(rq2_family, calendar_dir / "rq2_quarter_support.csv", calendar_dir / "rq2_support_summary.json")

            piecewise_dir = scenario_out / "rq2_piecewise_time"
            piecewise_dir.mkdir()
            piecewise_input = scenario_work / "piecewise_intervals.csv.gz"
            prepare_piecewise(z, scenario, piecewise_input, piecewise_dir / "band_support.csv")
            run_r(scripts / "fit_rq2_piecewise_time.R", [piecewise_input], piecewise_dir, args.prepare_only)

            if scenario == "expanded_primary":
                primary_inputs = (
                    scenario_work / "rq1_family_input.csv",
                    scenario_work / "rq2_family_input.csv",
                )
                influence_dir = scenario_out / "repository_influence"
                influence_dir.mkdir()
                run_r(scripts / "fit_repository_influence.R", list(primary_inputs), influence_dir, args.prepare_only)

        required = [] if args.prepare_only else [
            output / scenario / "family_interpretation" / "rq1_family_omnibus_tests.csv"
            for scenario in SCENARIOS
        ] + [
            output / scenario / "rq2_piecewise_time" / "merge_rate_band_contrasts.csv"
            for scenario in SCENARIOS
        ] + [
            output / "expanded_primary" / "repository_influence" / "rq2_influence.csv"
        ]
        missing = [str(p) for p in required if not p.is_file()]
        if missing:
            raise ValueError(f"Expected analysis outputs missing: {missing}")

    (output / "provenance.json").write_text(json.dumps({
        "tool_version": VERSION,
        "generated_at_utc": datetime.now(timezone.utc).isoformat(),
        "source_refit_zip_sha256": sha(refit),
        "source_refit_provenance": provenance,
        "time_band_cutpoints_seconds": list(TIME_CUTS),
        "time_band_labels": list(TIME_BANDS),
        "models_fitted": not args.prepare_only,
        "calendar_trend_model_fitted": False,
    }, indent=2) + "\n", encoding="utf-8")
    (output / "README.md").write_text(
        "Remaining RQ1/RQ2 analyses: agent-family interpretation, piecewise time-aware "
        "sensitivity, calendar support audit, and repository-influence sensitivity. "
        "Calendar trend models are not fitted until the support audit is reviewed.\n",
        encoding="utf-8",
    )
    target = output.with_suffix(".zip")
    with ZipFile(target, "x", compression=ZIP_DEFLATED, compresslevel=6) as outzip:
        for path in sorted(output.rglob("*")):
            if path.is_file():
                outzip.write(path, arcname=str(Path(output.name) / path.relative_to(output)))
        for name in ("run_additional.py", "fit_rq1_rq2_agent_families.R", "fit_rq2_piecewise_time.R", "fit_repository_influence.R"):
            outzip.write(scripts / name, arcname=f"{output.name}/code/{name}")
    with ZipFile(target) as check:
        if check.testzip() is not None:
            raise ValueError("Output ZIP integrity failure")
    print("Family interpretation prepared for RQ1 and RQ2: True")
    print("Piecewise time-aware sensitivity prepared: True")
    print("Calendar-time support audit prepared: True")
    print("Repository-influence sensitivity prepared: True")
    print("Models fitted:", not args.prepare_only)
    print("UPLOAD THIS ZIP:", target)


if __name__ == "__main__":
    try:
        main()
    except Exception as exc:
        print("FAILED:", exc, file=sys.stderr)
        sys.exit(1)
