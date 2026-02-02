# BLPlabtools 0.3.0

## Documentation

* Expanded d-calc vignette with clearer explanation of difference-in-differences calculations
* Added links to meta-analysis learning resources (Experimentology, Lakens, Cooper et al.) in overview vignette
* Fixed variable naming inconsistency in d-calc vignette (Clunies-Ross example)

## Bug fixes

* Fixed `stargazer_rowname()`: corrected typo (`rownumber` → `row_number`)
* Fixed `kable_resize()`: corrected reference to undefined variable (`table5_rough` → `table`)

## Internal

* Added missing `@importFrom` declarations to resolve R CMD check NOTEs
* Added `.claude` to `.Rbuildignore`

# BLPlabtools 0.2.0

* Initial public release with vignettes for meta-analysis and experimental data workflows
