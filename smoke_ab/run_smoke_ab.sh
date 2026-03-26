#!/usr/bin/env bash
set -euo pipefail

if ! command -v opensafely >/dev/null 2>&1; then
  echo "Error: 'opensafely' not found on PATH." >&2
  exit 127
fi

if ! command -v python3 >/dev/null 2>&1; then
  echo "Error: 'python3' not found on PATH." >&2
  exit 127
fi

if ! command -v rsync >/dev/null 2>&1; then
  echo "Error: 'rsync' not found on PATH." >&2
  exit 127
fi

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

backup="project.yaml.smoke_backup"
reports_dir="smoke_ab/reports"

cleanup() {
  if [[ -f "$backup" ]]; then
    mv -f "$backup" project.yaml
  fi
}
trap cleanup EXIT

cp -f project.yaml "$backup"
mkdir -p smoke_ab/v1 smoke_ab/v2 "$reports_dir"

switch_r_tag() {
  local tag="$1" # v1 or v2
  cp -f "$backup" project.yaml
  python3 - "$tag" <<'PY'
from pathlib import Path
import sys

tag = sys.argv[1]
p = Path("project.yaml")
txt = p.read_text()

if tag == "v1":
    txt = txt.replace("r:v2", "r:v1")
else:
    txt = txt.replace("r:v1", "r:v2")

p.write_text(txt)
PY
}

run_smoke() {
  local tag="$1"
  echo "=== Running A/B smoke (${tag}) ==="
  switch_r_tag "$tag"

  opensafely run process_flow_chart_data_older_adults_s1
  opensafely run describe_dataset_older_adults_s1_sensitive_primary
  opensafely run get_counts_older_adults_s1_sensitive_primary
  opensafely run calculate_rates_rolling_older_adults_s1_sensitive_primary
  opensafely run calculate_all_rates_over_time_older_adults_s1_sensitive_primary
  opensafely run analyse_dataset_older_adults_rsv_s1_sensitive_primary

  mkdir -p "smoke_ab/${tag}/output"
  rsync -a --delete output/flow_chart/ "smoke_ab/${tag}/output/flow_chart/"
  rsync -a --delete output/results/rates/ "smoke_ab/${tag}/output/results/rates/"
  rsync -a --delete output/results/counts/ "smoke_ab/${tag}/output/results/counts/"
  rsync -a --delete output/results/models/rsv_primary/ "smoke_ab/${tag}/output/results/models/rsv_primary/"
}

run_smoke v1
run_smoke v2

echo "=== Writing report ==="
python3 - <<'PY'
from pathlib import Path
import hashlib

root = Path("smoke_ab")
v1 = root / "v1" / "output"
v2 = root / "v2" / "output"
report = root / "reports" / "ab_report.txt"
report.parent.mkdir(parents=True, exist_ok=True)

def sha256(p: Path) -> str:
    h = hashlib.sha256()
    with p.open("rb") as f:
        for chunk in iter(lambda: f.read(8192), b""):
            h.update(chunk)
    return h.hexdigest()

v1_files = {p.relative_to(v1) for p in v1.rglob("*") if p.is_file()}
v2_files = {p.relative_to(v2) for p in v2.rglob("*") if p.is_file()}

only_v1 = sorted(v1_files - v2_files)
only_v2 = sorted(v2_files - v1_files)
common = sorted(v1_files & v2_files)

diff_hash = [r for r in common if sha256(v1 / r) != sha256(v2 / r)]

lines = []
lines.append("A/B smoke test report (r:v1 vs r:v2)")
lines.append("")
lines.append(f"only in v1: {len(only_v1)}")
for r in only_v1[:200]:
    lines.append(f"- {r}")
if len(only_v1) > 200:
    lines.append("- ...")

lines.append("")
lines.append(f"only in v2: {len(only_v2)}")
for r in only_v2[:200]:
    lines.append(f"- {r}")
if len(only_v2) > 200:
    lines.append("- ...")

lines.append("")
lines.append(f"common files: {len(common)}")
lines.append(f"different hash: {len(diff_hash)}")
for r in diff_hash[:500]:
    lines.append(f"- {r}")
if len(diff_hash) > 500:
    lines.append("- ...")

report.write_text("\n".join(lines) + "\n")
print(f"Wrote {report}")
PY

echo "Done. See ${reports_dir}/ab_report.txt"

