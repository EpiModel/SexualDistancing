scenario_names <- c(
  "base"         = "Base Scenario",
  "base_no_prep" = "Base Scenario, counterfactual with no PrEP ever",
  "comb_025_05"  = "Reduce Casl/OO Net Behavior by 25%, Combined Service Reduction of 50%",
  "comb_025_09"  = "Reduce Casl/OO Net Behavior by 25%, Combined Service Reduction of 90%",
  "comb_05_05"   = "Reduce Casl/OO Net Behavior by 50%, Combined Service Reduction of 50%",
  "comb_05_09"   = "Reduce Casl/OO Net Behavior by 50%, Combined Service Reduction of 90%",
  "comb_075_05"  = "Reduce Casl/OO Net Behavior by 75%, Combined Service Reduction of 50%",
  "comb_075_09"  = "Reduce Casl/OO Net Behavior by 75%, Combined Service Reduction of 90%",
  "comb_09_05"   = "Reduce Casl/OO Net Behavior by 90%, Combined Service Reduction of 50%",
  "comb_09_09"   = "Reduce Casl/OO Net Behavior by 90%, Combined Service Reduction of 90%",
  "net_all_025"   = "Changes in Total Partnership Network - Reduction by 25%",
  "net_all_05"   = "Changes in Total Partnership Network - Reduction by 50%",
  "net_all_09"   = "Changes in Total Partnership Network - Reduction by 90%",
  "net_casl_025"  = "Changes in Casual Partnership Network - Reduction by 25%",
  "net_casl_05"  = "Changes in Casual Partnership Network - Reduction by 50%",
  "net_casl_09"  = "Changes in Casual Partnership Network - Reduction by 90%",
  "net_casl_1"   = "Changes in Casual Partnership Network - Reduction by 100%",
  "net_ot_025"    = "Changes in One-Time Partnership Network - Reduction by 25%",
  "net_ot_05"    = "Changes in One-Time Partnership Network - Reduction by 50%",
  "net_ot_09"    = "Changes in One-Time Partnership Network - Reduction by 90%",
  "net_ot_1"     = "Changes in One-Time Partnership Network - Reduction by 100%",
  "ser_all_025"   = "Combined Serviced Reduction - Reduction by 25%",
  "ser_all_05"   = "Combined Serviced Reduction - Reduction by 50%",
  "ser_all_09"   = "Combined Serviced Reduction - Reduction by 90%",
  "ser_art_025"   = "Reduction in ART Retention Rates - Reduction by 25%",
  "ser_art_05"   = "Reduction in ART Retention Rates - Reduction by 50%",
  "ser_art_09"   = "Reduction in ART Retention Rates - Reduction by 90%",
  "ser_prep_025"  = "Reduction in PrEP Utilization - Reduction by 25%",
  "ser_prep_05"  = "Reduction in PrEP Utilization - Reduction by 50%",
  "ser_prep_09"  = "Reduction in PrEP Utilization - Reduction by 90%",
  "ser_scre_025"  = "Reduction in HIV Screening Rates - Reduction by 25%",
  "ser_scre_05"  = "Reduction in HIV Screening Rates - Reduction by 50%",
  "ser_scre_09"  = "Reduction in HIV Screening Rates - Reduction by 90%",
  "ser_stitx_025" = "Reduction in STI Treatment Proportion - Reduction by 25%",
  "ser_stitx_05" = "Reduction in STI Treatment Proportion - Reduction by 50%",
  "ser_stitx_09" = "Reduction in STI Treatment Proportion - Reduction by 90%"
)

dts <- purrr::cross(list(seq(0, 18, 3), seq(0, 18, 3)))
dts <- purrr::transpose(dts)
dts <- purrr::map(dts, as.numeric)

sensi_scenario_names <- purrr::pmap_chr(dts, ~ glue::glue(
  "Sensitivity Analysis, {..1} Months Sexual Distancing,",
  " {..2} Months Service reduction"
))

names(sensi_scenario_names) <- purrr::pmap_chr(dts, ~ glue::glue(
  "sensi_net{..1}_ser_{..2}"
))

scenarios_3_18_names <- c(
  "base_318" = "Base Scenario",
  "net_only_318" = "Changes in Total Partnership Network - Reduction by 50% (3 Months)",
  "ser_only_318" = "Combined Serviced Reduction - Reduction by 50% (18 Months)",
  "comb_318"  = "Reduce Casl/OO Net Behavior by 50% (3 Months), Combined Service Reduction of 50% (18 Months)"
)
