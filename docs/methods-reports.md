# Methods reports

`cdrgam methods` writes a methods section, with citations and a reference
list, describing the selected work items and the datasets they use. The
selectors are the same as for `cdrgam run`; `-P` accepts several projects,
which are described together in one report.

```sh
cdrgam methods -P brown
cdrgam methods -P brown -m main -t main --preview --format md
cdrgam methods -P brown brown-replication -o methods --format tex
```

## Realized and preview reports

By default the report describes only work items with a complete artifact for
their current resolved identity. It uses the settings recorded in each
artifact, such as the smoothing criterion actually used, and adds results
recorded by the artifacts, such as convergence and automatic-simplification
edits. Stale and missing work items are omitted; the command counts them in a
message and lists them in `methods.json`. The command fails when no selected
work item has a complete artifact.

With `--preview`, the report describes the resolved definitions without
reading artifacts. Omitted settings take the defaults of the installed
`cdrgam`, and sentences that need results, such as convergence or the number
of tested terms, are left out.

## Output files

The output directory defaults to `results/methods` in the selected project.
`-o DIR` is required when several projects are selected. Files of the same
names are replaced.

| File | Contents |
| --- | --- |
| `methods.rtf`, `methods.md`, or `methods.tex` | The methods section and, except for tex, its reference list. |
| `methods.bib` | With `--format tex` only: the cited references as BibTeX. |
| `methods.json` | The selection, the state of each work item, and the template, context values, and citations behind each section. |
| `supplementary_*.tsv` | Per-item settings for each artifact kind, including verbatim model formulas and effect definitions. |
| `supplementary_software.tsv` | R, `cdrgam`, and `cdrgam.cli` versions recorded by the artifacts, and the versions that wrote the report, including `mgcv`. |

`--format` selects `rtf` (the default), `md`, or `tex`. RTF and Markdown use
APA-style author–year citations and reference entries. The tex output uses
`\citep` and `\citet` and needs natbib; include it with `\input{methods}` and
add `methods.bib` to the document's bibliography.

## Content

Facts shared by all described items are stated once. When items differ, the
report names the differing settings and refers to the supplementary table that
gives the values for each item. The per-item tables have a `project` column.

The sections cover:

- **Data:** response filters, preprocessing, constructed interactions, and
  the impulse and response streams.
- **Model specification and estimation:** the CDR-GAM framework, bases and
  lag windows, predictor scaling and centering, the family and link, the
  estimator and smoothing criterion, rank handling, automatic simplification,
  and, for realized reports, convergence.
- **Predictions** and **Model comparison:** the scale of predictions and the
  comparison metrics and evaluation data.
- **Statistical tests:** term tests and effect estimands, with interval level,
  covariance correction, null values, and simultaneous inference.
- **Figures:** interval level and type for effect plots, and default plot
  booklets.

The estimator and smoothing-criterion descriptions, and the list of term
tests, come from `cdrgam::methods_facts()`, so they follow the installed
core's defaults. Formulas are not paraphrased; they appear verbatim in
`supplementary_models.tsv`.

Every definition field of each artifact kind is either reported or recorded
as deliberately omitted, such as chunk sizes and other execution settings that
do not change the estimates. A package test fails when a new field is neither.
