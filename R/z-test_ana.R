#' ## Preparation
#'
#' I reproduced the 5 first lines of table 1 for HIV cummulative incidence
#'
library(tidyverse)
df <- readRDS(here::here("analysis/data/df.rds"))

prep_start <- 52 * (65 + 1) + 1
ana_beg <- prep_start + 5 * 52

dfn <- df %>%
  filter(
    time >= ana_beg,
    scenario %in% c("base", "net_all_025", "net_all_05", "net_all_09"),
  ) %>%
  group_by(scenario, batch, sim)

#' ## Actual calculations

#+ message                 = FALSE
dfa <- dfn %>%
  summarise(
    inc_100pyar            = mean(hiv_inc),
    sams                   = (sum(hiv_inc_raw) / sum(hiv_s_raw)) * 5200,
    inc_100kpyar           = mean(hiv_inc) * 1e3,
    hiv_cum_inc_100k       = sum(hiv_inc_raw) / mean(hiv_s_raw) * 1e5,
    hiv_cum_inc_100k_base  = sum(hiv_inc) / 5200 * 1e5,
    hiv_cum_inc_100k_pyar  = sum(hiv_inc) / 5200 * 1e5 / 5
  )

#' ## Cleanup and table

#+ message = FALSE
dfa %>%
  ungroup() %>%
  select(-c(batch, sim)) %>%
  group_by(scenario) %>%
  summarise(across(everything(), ~ round(median(.x), 1))) %>%
  knitr::kable() %>%
  kableExtra::kable_styling(bootstrap_options = "striped")

#' *There are some discrepencies between the values calculated using the raw
#' vectors and `hiv_inc` due to when the rounding is done.*
#' </br>
#' </br>
#' </br>
#'
#' Here is what I understand and need your opinion on:
#'
#' | calculation | my understanding |
#' |--|--|
#' | `mean(hiv_inc)`                            |  is the MEAN incidence for 100 PYAR over 5 years |
#' | `sum(hiv_inc_raw) / sum(hiv_s_raw) * 5200` |  is the same thing |
#' | `mean(hiv_inc) * 1e3 `                     |  is the MEAN incidence for 100k PYAR over 5 years |
#' | `sum(hiv_inc_raw) / mean(hiv_s_raw) * 1e5` |  is the CUMULATIVE incidence for 100k person at risk over 5 years |
#' | `sum(hiv_inc) / 5200 * 1e5`                |  is the same thing |
#' | `sum(hiv_inc) / 5200 * 1e5 / 5`            |  dividing the previous value by 5 give us again the MEAN incidence for 100k PYAR over 5 years |
#'
