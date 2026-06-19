# STATCAL ONLINE IDX Financials Visualization and Regression Shiny App

This version adds:

1. Correlation heatmap color palette options suitable for publication-style figures.
2. Flexible mean-value label positioning in the multi-panel line chart.
3. Existing static PNG export through `www/statcal_exports/` is retained for Chrome, Opera, and RStudio Viewer compatibility.

Run in RStudio:

```r
install.packages(c(
  "shiny", "shinydashboard", "DT", "readxl", "dplyr", "ggplot2",
  "shinycssloaders", "lmtest", "car", "moments", "scales"
))

shiny::runApp(".")
```
