# albatrossR

R package for analyzing juvenile albatross GLS tracking data with environmental and fisheries datasets.

## Setup

```r
devtools::install_github("vrajesh-daga/albatrossR")
library(albatrossR)
```

## Project Structure

* `R/` → functions
* `data-raw/` → data processing scripts
* `inst/extdata/` → sample datasets
* `data/` → processed outputs
* `vignettes/` → tutorials

## Goal

Build a reproducible pipeline to:

* process GLS tracking data
* attach environmental variables (SST, chlorophyll, etc.)
* quantify movement ecology and fisheries risk

## Current Status

Setting up core data pipeline (GLS ingestion + feature engineering).

## Contribution

Work will be split across:

* GLS processing
* environmental extraction
* spatial overlays
* fisheries analysis
