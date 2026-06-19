# STATCAL ONLINE IDX Financials Data Repository, Visualization, and Regression Analyzer

This R Shiny version includes robust PNG download handlers for line chart, correlation heatmap, scatterplot, and regression diagnostics.

## Install packages

```r
install.packages(c(
  "shiny", "shinydashboard", "DT", "readxl", "dplyr", "ggplot2",
  "shinycssloaders", "lmtest", "car", "moments", "scales"
))
```

## Run app

```r
shiny::runApp(".")
```

## Notes

The PNG export now uses `contentType = "image/png"`, a temporary `.png` file, and a fallback error PNG to avoid browser download errors such as `download_line_png.txt` or `file wasn't available on site`.
