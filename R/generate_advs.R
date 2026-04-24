## ------------------------------------------------------------------
## generate_advs.R
##
## Produce a fake ADVS (Vital Signs Analysis Dataset) that follows
## the CDISC ADaM structure closely enough for a proof-of-concept TLF
## generator. Not validated or production-ready.
## ------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(tibble)

generate_advs <- function(n_subjects = 60, seed = 42) {
  set.seed(seed)

  arms <- c("Placebo", "Drug Low Dose", "Drug High Dose")
  visits <- c("Screening", "Baseline", "Week 2", "Week 4", "Week 8", "Week 12")
  visit_nums <- c(0, 1, 2, 4, 8, 12)

  params <- tibble::tribble(
    ~PARAMCD, ~PARAM,                           ~baseline_mean, ~baseline_sd, ~unit,
    "SYSBP",  "Systolic Blood Pressure (mmHg)",  130,            10,           "mmHg",
    "DIABP",  "Diastolic Blood Pressure (mmHg)", 82,             8,            "mmHg",
    "PULSE",  "Pulse Rate (beats/min)",          72,             8,            "beats/min",
    "TEMP",   "Temperature (C)",                 36.8,           0.3,          "C",
    "RESP",   "Respiratory Rate (breaths/min)",  16,             2,            "breaths/min"
  )

  subjects <- tibble(
    USUBJID = sprintf("STUDY01-%03d", seq_len(n_subjects)),
    SUBJID  = sprintf("%03d", seq_len(n_subjects)),
    STUDYID = "STUDY01",
    TRT01P  = sample(arms, n_subjects, replace = TRUE),
    TRT01A  = NA_character_,
    AGE     = round(rnorm(n_subjects, mean = 55, sd = 12)),
    AGEU    = "YEARS",
    SEX     = sample(c("M", "F"), n_subjects, replace = TRUE),
    RACE    = sample(
      c("WHITE", "BLACK OR AFRICAN AMERICAN", "ASIAN", "OTHER"),
      n_subjects, replace = TRUE, prob = c(0.6, 0.2, 0.15, 0.05)
    )
  ) %>%
    mutate(TRT01A = TRT01P)

  advs <- subjects %>%
    tidyr::crossing(params) %>%
    tidyr::crossing(tibble(AVISIT = visits, AVISITN = visit_nums)) %>%
    rowwise() %>%
    mutate(
      trt_effect = case_when(
        TRT01P == "Placebo"         ~ 0,
        TRT01P == "Drug Low Dose"   ~ -0.02 * AVISITN,
        TRT01P == "Drug High Dose"  ~ -0.04 * AVISITN
      ),
      AVAL = round(
        baseline_mean * (1 + trt_effect) + rnorm(1, 0, baseline_sd * 0.15),
        1
      ),
      AVALU = unit,
      ADT   = as.Date("2024-01-01") + AVISITN * 7 + sample(0:3, 1),
      ABLFL = if_else(AVISIT == "Baseline", "Y", NA_character_)
    ) %>%
    ungroup() %>%
    group_by(USUBJID, PARAMCD) %>%
    mutate(
      BASE  = AVAL[AVISIT == "Baseline"][1],
      CHG   = round(AVAL - BASE, 1),
      PCHG  = round((AVAL - BASE) / BASE * 100, 2),
      ANL01FL = "Y"
    ) %>%
    ungroup() %>%
    select(
      STUDYID, USUBJID, SUBJID, TRT01P, TRT01A, AGE, AGEU, SEX, RACE,
      PARAMCD, PARAM, AVISIT, AVISITN, ADT, AVAL, AVALU,
      BASE, CHG, PCHG, ABLFL, ANL01FL
    )

  advs
}
