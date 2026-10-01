# Analysis map

| Question | Code | Main reference tables |
|---|---|---|
| RQ1 PR characteristics and merging | `code/fit_rq1_pr_characteristics.R` | `results_reference/expanded_primary/rq1/primary/` |
| RQ2 formal and inline review | `code/fit_rq2_review_association.R` | `results_reference/expanded_primary/rq2/review/` |
| RQ2 timing | `code/fit_rq2_time_aware.R`, `code/fit_rq2_piecewise_time.R` | `results_reference/expanded_primary/rq2/time_aware/`, `piecewise_time/` |
| Agent families | `code/fit_rq1_rq2_agent_families.R` | `results_reference/expanded_primary/rq1/agent_families/` and `rq2/agent_families/` |
| Repository influence | `code/fit_repository_influence.R` | `results_reference/expanded_primary/rq1/repository_influence/` and `rq2/repository_influence/` |

Run `code/run_primary.py` and then `code/run_additional.py` through
`run_replication.sh`. The two runners prepare the required model-specific
inputs and execute the R scripts. They retain their original mathematical
definitions. The changes in their packaged copies affect input paths and
script filenames only.

`expanded_primary` excludes 169 direct text disclosures and 1,168
`claude/` branch matches from the human candidate group. `core_sensitivity`
excludes only the 169 direct text disclosures. Neither adds PRs to AIDev's
agentic group. Full audit records are in `data/provenance/`.
