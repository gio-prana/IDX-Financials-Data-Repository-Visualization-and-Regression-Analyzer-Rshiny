# STATCAL ONLINE - IDX Financials Data Repository, Visualization, and Regression Analyzer

## New features in this version

1. Flexible decimal digits for correlation values displayed inside the correlation heatmap.
2. Export descriptive statistics tables to Excel (`.xlsx`), including:
   - Export Info
   - Filtered Data
   - Univariate Descriptive
   - Grouped Descriptive
3. Existing PNG export via `www/statcal_exports/` is retained.

## Required packages

```r
install.packages(c(
  "shiny", "shinydashboard", "DT", "readxl", "dplyr", "ggplot2",
  "shinycssloaders", "lmtest", "car", "moments", "scales", "openxlsx"
))
```

## Run

```r
shiny::runApp(".")
```
