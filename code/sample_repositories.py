#!/usr/bin/env python3
"""Freeze the one-draw Phase B3 repository sample and complete PR universe."""
from __future__ import annotations

import collections
import csv
import datetime as dt
import hashlib
import json
import math
import os
import random
import sqlite3
from pathlib import Path
from typing import Any,Iterable,Sequence

import phase_b2_core as b2
from parquet_lite import LocalSource,ParquetFileLite

ROOT=Path("/Users/umutzengin/Desktop/Courses/trust_paper_aidev76_v5")
CONTRACT=ROOT/"docs/59_phase_b3_final_repository_sampling_contract.md"
CONTRACT_HASH=ROOT/"manifests/phase_b3_final_sampling_contract_hash.json"
SOURCE=ROOT/"data/03_staged/phase_b3a/b3a_repository_sampling_diagnostics.parquet"
REPO_FRAME=ROOT/"data/03_staged/phase_b22/b22_authoritative_b3_repository_frame.parquet"
AGENT_FRAME=ROOT/"data/03_staged/phase_b22/b22_authoritative_b3_agent_frame.parquet"
HUMAN_PRIMARY=ROOT/"data/03_staged/phase_b22/b22_authoritative_b3_human_primary.parquet"
AGENT_DETAIL=ROOT/"data/03_staged/phase_b2/b2_known_aidev_agents_discovered.parquet"
WINGET_DETAIL=ROOT/"data/03_staged/phase_b21/winget_repaired_s3_prs.parquet"
B2_DB=ROOT/"data/02_github_raw/phase_b2/checkpoints/phase_b2_state.sqlite"
COHORT=ROOT/"data/04_cohorts/phase_b3";AUDIT=ROOT/"outputs/audits/phase_b3";DESC=ROOT/"outputs/descriptive/phase_b3";MANIFESTS=ROOT/"manifests";LOG=ROOT/"logs/phase_b3"
ELIGIBLE=COHORT/"b3_eligible_repository_population_100_1000.parquet";SAMPLE_PQ=COHORT/"b3_final_repository_sample_341.parquet";SAMPLE_CSV=COHORT/"b3_final_repository_sample_341.csv";PR_MANIFEST=COHORT/"b3_final_pr_hydration_manifest.parquet"
SEED=20260817;N=341;POPULATION=2990;CUTOFF="2026-03-31T23:59:59Z"
UPSTREAM_SHA={"data/03_staged/phase_b3a/b3a_repository_sampling_diagnostics.parquet":"22f02c21695faa993c379a9909578ec60e4c3487a5ff19e69231ce1366ac860f","data/03_staged/phase_b22/b22_authoritative_b3_repository_frame.parquet":"4800f9269aecfabf2ed91d8d159e1d4fd4449c63bedb618fef791c0f968d01c7","data/03_staged/phase_b22/b22_authoritative_b3_agent_frame.parquet":"e89999ab49fffc991d083e380ada88f3e9b7689dd067cb47c6f23cfe5b84d337","data/03_staged/phase_b22/b22_authoritative_b3_human_primary.parquet":"c4236c9b3111aa66dc317e0fe099d6c8146aa718297c1bf6e2dbc489db75e66a"}

def utc_now():return dt.datetime.now(dt.timezone.utc).replace(microsecond=0).isoformat().replace("+00:00","Z")
def sha(path:Path)->str:
    h=hashlib.sha256()
    with path.open("rb") as f:
        for chunk in iter(lambda:f.read(1024*1024),b""):h.update(chunk)
    return h.hexdigest()
def prows(path:Path,cols:Sequence[str]):yield from b2.parquet_rows(path,cols)
def ensure_dirs():
    for p in [COHORT,AUDIT,DESC,MANIFESTS,LOG]:p.mkdir(parents=True,exist_ok=True)
def write_parquet_atomic(path:Path,schema:list[tuple[str,str]],rows:Iterable[dict[str,Any]]):
    temp=path.with_name(f".{path.name}.tmp.{os.getpid()}")
    if temp.exists():temp.unlink()
    with b2.StreamingParquetWriter(temp,schema) as writer:
        batch=[]
        for row in rows:
            batch.append(row)
            if len(batch)>=10000:writer.write_rows(batch);batch=[]
        writer.write_rows(batch)
    os.replace(temp,path)

def verify_contract():
    data=json.loads(CONTRACT_HASH.read_text())
    if sha(CONTRACT)!=data["contract_sha256"] or data["rng_seed"]!=SEED or data["sample_size"]!=N or data["eligible_population"]!=POPULATION or data["stratified"] is not False:raise RuntimeError("frozen B3 sampling contract mismatch")
    for rel,want in UPSTREAM_SHA.items():
        if sha(ROOT/rel)!=want:raise RuntimeError(f"upstream mutation: {rel}")
    return data

def eligible_rows()->tuple[list[dict[str,Any]],list[dict[str,Any]]]:
    repo_frame={str(r["repository"]):r for r in prows(REPO_FRAME,["repository","github_repo_id","s3_window_start"])};all_rows=[]
    for r in prows(SOURCE,["normalized_repository","github_repository_id","agent_pr_count","primary_human_candidate_count","total_s3_candidate_pr_count"]):
        name=str(r["normalized_repository"]);up=repo_frame[name];total=int(r["total_s3_candidate_pr_count"])
        all_rows.append({"normalized_repository":name,"github_repository_id":int(r["github_repository_id"]),"repository_url":f"https://github.com/{name}","s3_window_start":str(up["s3_window_start"]),"cutoff":CUTOFF,"agent_pr_count":int(r["agent_pr_count"]),"primary_human_pr_count":int(r["primary_human_candidate_count"]),"total_s3_pr_count":total})
    eligible=sorted((r for r in all_rows if 100<=r["total_s3_pr_count"]<=1000),key=lambda r:(r["github_repository_id"],r["normalized_repository"]))
    bands=(sum(r["total_s3_pr_count"]<100 for r in all_rows),len(eligible),sum(r["total_s3_pr_count"]>1000 for r in all_rows))
    if len(all_rows)!=8160 or bands!=(4574,2990,596):raise RuntimeError(f"eligibility reproduction mismatch: all={len(all_rows)} bands={bands}")
    return all_rows,eligible

REPO_SCHEMA=[("normalized_repository","string"),("github_repository_id","int64"),("repository_url","string"),("s3_window_start","string"),("cutoff","string"),("agent_pr_count","int64"),("primary_human_pr_count","int64"),("total_s3_pr_count","int64")]
SAMPLE_SCHEMA=[("draw_order","int64")]+REPO_SCHEMA+[("inclusion_probability_ppb","int64"),("inverse_probability_weight_ppb","int64"),("sampling_contract_sha256","string"),("sample_frozen_at_utc","string")]

def make_draw(eligible,contract)->list[dict[str,Any]]:
    rng=random.Random(SEED)
    indices=rng.sample(range(len(eligible)),N) # The one and only final draw.
    frozen_at=utc_now();selected=[]
    for order,index in enumerate(indices,1):selected.append({"draw_order":order,**eligible[index],"inclusion_probability_ppb":round(1_000_000_000*N/len(eligible)),"inverse_probability_weight_ppb":round(1_000_000_000*len(eligible)/N),"sampling_contract_sha256":contract["contract_sha256"],"sample_frozen_at_utc":frozen_at})
    if len(selected)!=N or len({r["normalized_repository"] for r in selected})!=N:raise RuntimeError("invalid SRS draw")
    return selected

def existing_sample_matches(selected)->bool:
    if not SAMPLE_CSV.exists() and not SAMPLE_PQ.exists():return False
    if not SAMPLE_CSV.exists() or not SAMPLE_PQ.exists():raise RuntimeError("partial pre-existing final sample; manual review required")
    existing=list(csv.DictReader(SAMPLE_CSV.open()))
    expected=[(str(r["draw_order"]),r["normalized_repository"],str(r["github_repository_id"])) for r in selected]
    actual=[(r["draw_order"],r["normalized_repository"],r["github_repository_id"]) for r in existing]
    if actual!=expected:raise RuntimeError("pre-existing final sample differs; refusing alternate draw")
    return True

def detail_maps(selected_names:set[str]):
    agent={}
    cols=["canonical_pr_key","requested_aidev_repository","pr_number","github_pr_id","html_url","user_login","created_at","known_agent_family","raw_body_path"]
    for r in prows(AGENT_DETAIL,cols):
        if str(r["requested_aidev_repository"]) in selected_names:agent[str(r["canonical_pr_key"])]=dict(r)
    for r in prows(WINGET_DETAIL,["repository","pr_number","github_pr_id","github_node_id","created_at","user_login","classification","known_agent_family"]):
        repo=str(r["repository"]);key=f"{repo}#{int(r['pr_number'])}"
        if repo in selected_names and str(r["classification"])=="KNOWN_AIDEV_AGENT":agent[key]={"canonical_pr_key":key,"requested_aidev_repository":repo,"pr_number":int(r["pr_number"]),"github_pr_id":int(r["github_pr_id"] or 0),"github_node_id":str(r["github_node_id"] or ""),"html_url":f"https://github.com/{repo}/pull/{int(r['pr_number'])}","user_login":str(r["user_login"] or ""),"created_at":str(r["created_at"]),"known_agent_family":str(r["known_agent_family"]),"raw_body_path":"data/03_staged/phase_b21/winget_repaired_s3_prs.parquet"}
    conn=sqlite3.connect(B2_DB);conn.row_factory=sqlite3.Row
    for r in conn.execute("SELECT canonical_pr_key,repository,pr_number,aidev_pr_id,html_url,agent_family,created_at FROM known_agent_keys"):
        if r["repository"] not in selected_names:continue
        current=agent.setdefault(r["canonical_pr_key"],{})
        current.setdefault("canonical_pr_key",r["canonical_pr_key"]);current.setdefault("requested_aidev_repository",r["repository"]);current.setdefault("pr_number",int(r["pr_number"]));current.setdefault("github_pr_id",int(r["aidev_pr_id"]));current.setdefault("html_url",r["html_url"] or f"https://github.com/{r['repository']}/pull/{int(r['pr_number'])}");current.setdefault("known_agent_family",r["agent_family"]);current.setdefault("created_at",r["created_at"]);current.setdefault("raw_body_path","data/02_github_raw/phase_b2/checkpoints/phase_b2_state.sqlite")
    conn.close()
    return agent

PR_SCHEMA=[("canonical_pr_key","string"),("repository","string"),("github_repository_id","int64"),("repository_url","string"),("pr_number","int64"),("github_pr_id","int64"),("github_node_id","string"),("html_url","string"),("pr_source_class","string"),("agent_family","string"),("primary_human_candidate","bool"),("strict_human_candidate","bool"),("broad_human_candidate","bool"),("user_login","string"),("created_at","string"),("s3_window_start","string"),("cutoff","string"),("discovery_source_artifact","string"),("sampled_repository","bool"),("hydration_status","string")]
def build_pr_manifest(selected)->tuple[list[dict[str,Any]],dict[str,int]]:
    repo={r["normalized_repository"]:r for r in selected};names=set(repo);details=detail_maps(names);out=[];family=collections.Counter()
    for r in prows(AGENT_FRAME,["canonical_pr_key","repository","pr_number","agent_family","created_at"]):
        name=str(r["repository"])
        if name not in names:continue
        key=str(r["canonical_pr_key"]);d=details.get(key,{});fam=str(r["agent_family"]);family[fam]+=1
        out.append({"canonical_pr_key":key,"repository":name,"github_repository_id":repo[name]["github_repository_id"],"repository_url":repo[name]["repository_url"],"pr_number":int(r["pr_number"]),"github_pr_id":int(d.get("github_pr_id") or 0),"github_node_id":str(d.get("github_node_id") or ""),"html_url":str(d.get("html_url") or f"https://github.com/{name}/pull/{int(r['pr_number'])}"),"pr_source_class":"EXACT_AIDEV_AGENT","agent_family":fam,"primary_human_candidate":False,"strict_human_candidate":False,"broad_human_candidate":False,"user_login":str(d.get("user_login") or ""),"created_at":str(r["created_at"]),"s3_window_start":repo[name]["s3_window_start"],"cutoff":CUTOFF,"discovery_source_artifact":"data/03_staged/phase_b22/b22_authoritative_b3_agent_frame.parquet","sampled_repository":True,"hydration_status":"NOT_STARTED"})
    for r in prows(HUMAN_PRIMARY,["canonical_pr_key","requested_aidev_repository","pr_number","github_pr_id","html_url","user_login","created_at","primary_human_candidate","strict_human_candidate","broad_human_candidate","source_layer"]):
        name=str(r["requested_aidev_repository"])
        if name not in names:continue
        out.append({"canonical_pr_key":str(r["canonical_pr_key"]),"repository":name,"github_repository_id":repo[name]["github_repository_id"],"repository_url":repo[name]["repository_url"],"pr_number":int(r["pr_number"]),"github_pr_id":int(r["github_pr_id"] or 0),"github_node_id":"","html_url":str(r["html_url"] or f"https://github.com/{name}/pull/{int(r['pr_number'])}"),"pr_source_class":"PRIMARY_HUMAN_CANDIDATE","agent_family":"","primary_human_candidate":bool(r["primary_human_candidate"]),"strict_human_candidate":bool(r["strict_human_candidate"]),"broad_human_candidate":bool(r["broad_human_candidate"]),"user_login":str(r["user_login"] or ""),"created_at":str(r["created_at"]),"s3_window_start":repo[name]["s3_window_start"],"cutoff":CUTOFF,"discovery_source_artifact":"data/03_staged/phase_b22/b22_authoritative_b3_human_primary.parquet","sampled_repository":True,"hydration_status":"NOT_STARTED"})
    out.sort(key=lambda r:(r["github_repository_id"],r["pr_number"],r["pr_source_class"]))
    keys=[r["canonical_pr_key"] for r in out]
    expected_agent=sum(r["agent_pr_count"] for r in selected);expected_human=sum(r["primary_human_pr_count"] for r in selected)
    actual_agent=sum(r["pr_source_class"]=="EXACT_AIDEV_AGENT" for r in out);actual_human=sum(r["pr_source_class"]=="PRIMARY_HUMAN_CANDIDATE" for r in out)
    if len(keys)!=len(set(keys)) or (actual_agent,actual_human)!=(expected_agent,expected_human):raise RuntimeError(f"PR universe mismatch keys={len(keys)}/{len(set(keys))} actual={actual_agent}/{actual_human} expected={expected_agent}/{expected_human}")
    if any(not (r["s3_window_start"]<=r["created_at"]<=CUTOFF) for r in out):raise RuntimeError("PR outside frozen S3 window")
    return out,dict(family)

def accounting(selected,prs,families):
    agents=sum(r["pr_source_class"]=="EXACT_AIDEV_AGENT" for r in prs);humans=len(prs)-agents;totals=np_stats([r["total_s3_pr_count"] for r in selected]);rows=[{"section":"SUMMARY","metric":"repositories","value":len(selected)},{"section":"SUMMARY","metric":"agent_prs","value":agents},{"section":"SUMMARY","metric":"primary_human_prs","value":humans},{"section":"SUMMARY","metric":"total_hydration_prs","value":len(prs)}]
    for family,count in sorted(families.items()):rows.append({"section":"AGENT_FAMILY","metric":family,"value":count,"denominator":agents,"percent":100*count/agents})
    for metric,value in totals.items():rows.append({"section":"PRS_PER_REPOSITORY","metric":metric,"value":value})
    months=collections.Counter(r["created_at"][:7] for r in prs)
    for month,count in sorted(months.items()):rows.append({"section":"CALENDAR_MONTH","metric":month,"value":count,"denominator":len(prs),"percent":100*count/len(prs)})
    b2.write_csv(DESC/"01_final_sample_population_accounting.csv",rows)
def np_stats(values):
    v=sorted(values);n=len(v)
    def quant(p):
        x=(n-1)*p;lo=math.floor(x);hi=math.ceil(x);return float(v[lo] if lo==hi else v[lo]+(v[hi]-v[lo])*(x-lo))
    return {"min":v[0],"q1":quant(.25),"median":quant(.5),"mean":sum(v)/n,"q3":quant(.75),"p90":quant(.9),"p95":quant(.95),"p99":quant(.99),"max":v[-1]}

def main()->int:
    ensure_dirs();contract=verify_contract();all_rows,eligible=eligible_rows();write_parquet_atomic(ELIGIBLE,REPO_SCHEMA,eligible)
    b2.write_csv(AUDIT/"01_eligibility_reproduction.csv",[{"band":"<100","repositories":4574},{"band":"100–1000 inclusive","repositories":2990},{"band":">1000","repositories":596},{"band":"TOTAL","repositories":8160}])
    selected=make_draw(eligible,contract);already=existing_sample_matches(selected)
    if not already:
        write_parquet_atomic(SAMPLE_PQ,SAMPLE_SCHEMA,selected);b2.write_csv(SAMPLE_CSV,selected)
    prs,families=build_pr_manifest(selected);write_parquet_atomic(PR_MANIFEST,PR_SCHEMA,prs);accounting(selected,prs,families)
    sample_hash={"phase":"B3_FINAL_SAMPLE","created_at_utc":utc_now(),"contract_sha256":contract["contract_sha256"],"seed":SEED,"eligible_population":POPULATION,"sample_size":N,"draw_count":1,"sample_csv_sha256":sha(SAMPLE_CSV),"sample_parquet_sha256":sha(SAMPLE_PQ),"pr_hydration_manifest_sha256":sha(PR_MANIFEST),"selected_agent_prs":sum(families.values()),"selected_primary_humans":len(prs)-sum(families.values()),"total_hydration_prs":len(prs),"agent_family_counts":families,"sample_redrawn":False,"hydration_started":False};b2.write_json(MANIFESTS/"phase_b3_final_sample_hash.json",sample_hash)
    b2.write_text(LOG/"phase_b3_sampling_log.md",f"# Phase B3 final sampling log\n\n- `{utc_now()}` — frozen contract verified before draw; eligible population 2,990; one SRSWOR draw with seed 20260817; 341 repositories; sample redraw false; hydration not started.\n")
    print(f"B3_FINAL_SAMPLE_FROZEN repositories={N} agent_prs={sample_hash['selected_agent_prs']} primary_humans={sample_hash['selected_primary_humans']} total_prs={len(prs)} redraw=false hydration_started=false")
    return 0
if __name__=="__main__":raise SystemExit(main())
