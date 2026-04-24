# ADaM TLF Generator (Proof of Concept)

A small R Shiny app that lets a user upload an ADaM dataset (or use a built-in
fake ADVS) and build a customized **Table**, **Listing**, or **Figure** from it.

This is a **proof of concept** - it is not validated and not production ready.

## Features

- **Data input**
  - Built-in fake ADVS (Vital Signs Analysis Dataset), generated on the fly
  - Upload your own ADaM file in `.csv`, `.sas7bdat`, `.xpt`, or `.rds`
- **Table** - descriptive statistics (`n`, mean, SD, median, min, max) by
  treatment arm and visit for a chosen parameter
- **Listing** - subject-level listing with user-selected columns, visits,
  and treatments
- **Figure** - choose between:
  - Mean profile plot (+/- SE)
  - Box plot by visit
  - Spaghetti (per-subject) plot
  - Mean change from baseline
- Download the table/listing as CSV or the figure as PNG

## Project layout

```
.
|-- app.R                   # Shiny entry point
|-- R/
|   |-- generate_advs.R     # Fake ADVS generator
|   `-- tlf_functions.R     # Table / Listing / Figure builders
|-- data/
|   `-- make_demo_advs.R    # Writes demo CSV / RDS files to test upload flow
`-- README.md
```

## Running

```r
install.packages(c("shiny", "dplyr", "tidyr", "tibble",
                   "readr", "haven", "DT", "ggplot2"))

shiny::runApp("app.R")
```

Or from a terminal:

```bash
Rscript -e 'shiny::runApp("app.R", launch.browser = TRUE)'
```

To generate standalone demo files to test the upload flow:

```bash
Rscript data/make_demo_advs.R
```

## Expected dataset shape

The app assumes an ADaM BDS structure - at minimum:

| Variable   | Purpose                                  |
|------------|------------------------------------------|
| `USUBJID`  | Unique subject identifier                |
| `PARAMCD`  | Parameter code (filter target)           |
| `PARAM`    | Parameter label                          |
| `AVISIT`   | Analysis visit label                     |
| `AVISITN`  | Analysis visit number (for ordering)     |
| `AVAL`     | Analysis value                           |
| `BASE`     | Baseline value (for change-from-baseline)|
| `CHG`      | Change from baseline                     |
| `TRT01P`   | Planned treatment (default grouping)     |
