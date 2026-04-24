## ------------------------------------------------------------------
## tlf_functions.R
##
## Functions that build a customized Table, Listing, or Figure from
## an ADaM-shaped data frame (ADVS in this POC).
## ------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)

## -------- Table -------------------------------------------------------

build_summary_table <- function(data,
                                param_cd,
                                group_var      = "TRT01P",
                                visits         = NULL,
                                statistics     = c("n", "mean", "sd", "median", "min", "max")) {
  df <- data %>% filter(PARAMCD == param_cd)
  if (!is.null(visits) && length(visits) > 0) {
    df <- df %>% filter(AVISIT %in% visits)
  }

  group_syms <- c(group_var, "AVISIT")

  stat_funs <- list(
    n      = ~ sum(!is.na(.x)),
    mean   = ~ round(mean(.x, na.rm = TRUE), 2),
    sd     = ~ round(sd(.x,   na.rm = TRUE), 2),
    median = ~ round(median(.x, na.rm = TRUE), 2),
    min    = ~ round(min(.x,  na.rm = TRUE), 2),
    max    = ~ round(max(.x,  na.rm = TRUE), 2)
  )

  selected <- stat_funs[statistics]

  df %>%
    group_by(across(all_of(group_syms))) %>%
    summarise(across(AVAL, selected, .names = "{.fn}"), .groups = "drop") %>%
    arrange(across(all_of(group_syms)))
}

## -------- Listing -----------------------------------------------------

build_listing <- function(data,
                          param_cd,
                          columns = c("USUBJID", "TRT01P", "AVISIT", "ADT",
                                      "AVAL", "AVALU", "BASE", "CHG"),
                          visits  = NULL,
                          treatments = NULL) {
  df <- data %>% filter(PARAMCD == param_cd)
  if (!is.null(visits) && length(visits) > 0) {
    df <- df %>% filter(AVISIT %in% visits)
  }
  if (!is.null(treatments) && length(treatments) > 0) {
    df <- df %>% filter(TRT01P %in% treatments)
  }
  df %>%
    select(all_of(columns)) %>%
    arrange(across(any_of(c("USUBJID", "AVISIT"))))
}

## -------- Figure ------------------------------------------------------

build_figure <- function(data,
                         param_cd,
                         plot_type  = c("mean_profile", "boxplot", "spaghetti", "change_from_baseline"),
                         group_var  = "TRT01P",
                         visits     = NULL) {
  plot_type <- match.arg(plot_type)

  df <- data %>% filter(PARAMCD == param_cd)
  param_label <- unique(df$PARAM)[1]
  if (!is.null(visits) && length(visits) > 0) {
    df <- df %>% filter(AVISIT %in% visits)
  }

  df <- df %>%
    mutate(AVISIT = factor(AVISIT, levels = unique(AVISIT[order(AVISITN)])))

  p <- switch(
    plot_type,
    mean_profile = {
      summ <- df %>%
        group_by(.data[[group_var]], AVISIT, AVISITN) %>%
        summarise(
          mean_val = mean(AVAL, na.rm = TRUE),
          se_val   = sd(AVAL, na.rm = TRUE) / sqrt(sum(!is.na(AVAL))),
          .groups = "drop"
        )
      ggplot(summ, aes(x = AVISIT, y = mean_val,
                       colour = .data[[group_var]],
                       group  = .data[[group_var]])) +
        geom_line(size = 1) +
        geom_point(size = 2.5) +
        geom_errorbar(aes(ymin = mean_val - se_val, ymax = mean_val + se_val),
                      width = 0.15) +
        labs(title = paste("Mean Profile Plot -", param_label),
             x = "Visit", y = "Mean (+/- SE)", colour = group_var)
    },
    boxplot = {
      ggplot(df, aes(x = AVISIT, y = AVAL, fill = .data[[group_var]])) +
        geom_boxplot(outlier.size = 1) +
        labs(title = paste("Distribution by Visit -", param_label),
             x = "Visit", y = param_label, fill = group_var)
    },
    spaghetti = {
      ggplot(df, aes(x = AVISIT, y = AVAL,
                     group = USUBJID,
                     colour = .data[[group_var]])) +
        geom_line(alpha = 0.35) +
        labs(title = paste("Individual Subject Profiles -", param_label),
             x = "Visit", y = param_label, colour = group_var)
    },
    change_from_baseline = {
      summ <- df %>%
        filter(AVISIT != "Baseline") %>%
        group_by(.data[[group_var]], AVISIT, AVISITN) %>%
        summarise(mean_chg = mean(CHG, na.rm = TRUE), .groups = "drop")
      ggplot(summ, aes(x = AVISIT, y = mean_chg,
                       colour = .data[[group_var]],
                       group  = .data[[group_var]])) +
        geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
        geom_line(size = 1) +
        geom_point(size = 2.5) +
        labs(title = paste("Mean Change from Baseline -", param_label),
             x = "Visit", y = "Mean Change from Baseline", colour = group_var)
    }
  )

  p + theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold"),
          axis.text.x = element_text(angle = 20, hjust = 1))
}
