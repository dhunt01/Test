## Standalone helper - regenerate the demo ADVS files under /data so
## users can test the "Upload" flow without running the generator.
## Run from the project root:  Rscript data/make_demo_advs.R

source("R/generate_advs.R")

advs <- generate_advs()

readr::write_csv(advs, "data/advs_demo.csv")
saveRDS(advs,       "data/advs_demo.rds")

message("Wrote data/advs_demo.csv and data/advs_demo.rds (",
        nrow(advs), " rows).")
