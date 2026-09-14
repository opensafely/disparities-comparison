This folder contains scripts which are used to format data which has been released following output checking. The primary aim is to create plots for publication in addition to some tables and supplementary material.

## Recreate all outputs

From the repo root, after collated inputs are in `post_check/output/collated/`:

```bash
Rscript post_check/recreate_all_outputs.R
```

Useful variants:

```bash
Rscript post_check/recreate_all_outputs.R --dry-run
Rscript post_check/recreate_all_outputs.R --only=condensed
Rscript post_check/recreate_all_outputs.R --only=reformat,primary,models
Rscript post_check/recreate_all_outputs.R --include-scrape   # optional web scrapers
```

Stages run in order: **reformat → primary → models → condensed → exploratory → dashboard** (scrapers off by default). Each script is launched in its own `Rscript` process; the summary at the end lists any failures.
