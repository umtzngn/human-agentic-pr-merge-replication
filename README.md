# Pull request merge study: replication package

This package reproduces the primary and sensitivity models for RQ1 and RQ2.
The frozen data and archived reference results are copied from the local
analysis; see `config/source_manifest.json` for the original paths and hashes.

## Run

Install Python 3 and R with the `survival` package. From this directory:

```bash
bash run_replication.sh
```

On a Mac that may sleep during the run, use `caffeinate -i bash run_replication.sh`.
The script checks SHA-256 hashes, fits both scenarios, and compares every
small model result CSV with the reference archives using the stated numerical
tolerance. Outputs go to `results_reproduced/`. An existing output directory
is never overwritten.

## Where things are

| Folder | Contents |
|---|---|
| `code/` | `run_primary.py`, `run_additional.py`, and clearly named R models |
| `data/model_inputs/` | Two frozen input tables, named for their analysis role |
| `data/repository_sample.csv` | Selected 341 repositories |
| `data/repository_descriptives.csv` | Optional repository summary from the original package |
| `data/provenance/` | Audited Claude exclusions and review events |
| `results_reference/expanded_primary/` | Readable tables for the paper's main analysis, grouped by RQ |
| `results_reference/core_sensitivity/` | The direct disclosure sensitivity results |
| `results_reference/archives/` | Complete original result ZIPs used for verification |
| `docs/` | Model and data guidance, scope, and limits |

The source labels `687ac9d28d78` and `9559840112a4` remain only in
the provenance manifest and internal validation. The main analyses do not
relabel AIDev agentic PRs. The main scenario excludes 1,337 human candidates;
the narrower sensitivity excludes 169.

This package starts with frozen model inputs. It does not re-collect GitHub
data. Supplementary manuscript checks listed in `docs/REPLICATION_SCOPE.md`
may have separate scripts or only archived outputs; they are not silently
presented as products of `run_replication.sh`.
