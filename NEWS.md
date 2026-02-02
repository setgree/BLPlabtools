# BLPlabtools 0.3.0

## Documentation

* Expanded d-calc vignette with clearer explanation of difference-in-differences calculations
* Added links to meta-analysis learning resources (Experimentology, Lakens, Cooper et al.) in overview vignette
* Replaced numbered section references with semantic anchors for resilience to reorganization
* Fixed typos and incorrect examples across vignettes:
  - d-calc: DiTullio value (3.0798 → 3.0789), Boisjoly variable names, column name docs (study → Study)
  - meta-analysis: "wrapped around" → "wrapper around", `bind_cols` → `bind_rows`, LaTeX spelling
  - table-formatting: corrected `kable_resize()` and `stargazer_rowname()` example parameters

## Bug fixes

* Fixed `stargazer_rowname()`: corrected typo (`rownumber` → `row_number`)
* Fixed `kable_resize()`: corrected reference to undefined variable (`table5_rough` → `table`)
* Fixed Boisjoly example: variance calculation was using wrong effect size

## Internal

* Added missing `@importFrom` declarations to resolve R CMD check NOTEs
* Added `.claude` to `.Rbuildignore`

# BLPlabtools 0.2.0

* Initial public release with vignettes for meta-analysis and experimental data workflows
