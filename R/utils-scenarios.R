scenarios <- list(
  base = list(),
  base_no_prep = list(
    list(
      at = param$prep.start,
      param = list(
        prep.start.prob = 0
      )
    )
  ),
  net_all_025 = list(
    list(
      at = step_interv_start,
      param = list(
        netresim.form.rr = c(1, 0.75, 0.75),
        netresim.disl.rr = c(1, 1/0.75)
        )
    )
  ),
  net_all_05 = list(
    list(
      at = step_interv_start,
      param = list(
        netresim.form.rr = c(1, 0.5, 0.5),
        netresim.disl.rr = c(1, 1/0.5)
        )
    )
  ),
  net_all_09 = list(
    list(
      at = step_interv_start,
      param = list(
        netresim.form.rr = c(1, 0.1, 0.1),
        netresim.disl.rr = c(1, 1/0.1)
      )
    )
  ),
  net_casl_005 = list(
    list(
      at = step_interv_start,
      param = list(
        netresim.form.rr = c(1, 0.95, 1),
        netresim.disl.rr = c(1, 1/0.95)
      )
    )
  ),
  net_casl_01 = list(
    list(
      at = step_interv_start,
      param = list(
        netresim.form.rr = c(1, 0.9, 1),
        netresim.disl.rr = c(1, 1/0.9)
      )
    )
  ),
  net_casl_025 = list(
    list(
      at = step_interv_start,
      param = list(
        netresim.form.rr = c(1, 0.75, 1),
        netresim.disl.rr = c(1, 1/0.75)
      )
    )
  ),
  net_casl_05 = list(
    list(
      at = step_interv_start,
      param = list(
        netresim.form.rr = c(1, 0.5, 1),
        netresim.disl.rr = c(1, 1/0.5)
      )
    )
  ),
  net_casl_09 = list(
    list(
      at = step_interv_start,
      param = list(
        netresim.form.rr = c(1, 0.1, 1),
        netresim.disl.rr = c(1, 1/0.1)
      )
    )
  ),
  net_casl_1 = list(
    list(
      at = step_interv_start,
      param = list(
        netresim.form.rr = c(1, 0, 1),
        netresim.disl.rr = c(1, 10)
      )
    )
  ),
  net_ot_025 = list(
    list(
      at = step_interv_start,
      param = list(
        netresim.form.rr = c(1, 1, 0.75)
      )
    )
  ),
  net_ot_05 = list(
    list(
      at = step_interv_start,
      param = list(
        netresim.form.rr = c(1, 1, 0.5)
      )
    )
  ),
  net_ot_09 = list(
    list(
      at = step_interv_start,
      param = list(
        netresim.form.rr = c(1, 1, 0.1)
      )
    )
  ),
  net_ot_1 = list(
    list(
      at = step_interv_start,
      param = list(
        netresim.form.rr = c(1, 1, 0)
      )
    )
  ),
  ser_prep_025 = list(
    list(
      at = step_interv_start,
      param = list(
        prep.start.prob = param$prep.start.prob * 0.75,
        prep.discont.rate = param$prep.discont.rate / 0.75
      )
    )
  ),
  ser_prep_05 = list(
    list(
      at = step_interv_start,
      param = list(
        prep.start.prob = param$prep.start.prob * 0.5,
        prep.discont.rate = param$prep.discont.rate / 0.5
      )
    )
  ),
  ser_prep_09 = list(
    list(
      at = step_interv_start,
      param = list(
        prep.start.prob = param$prep.start.prob * 0.1,
        prep.discont.rate = param$prep.discont.rate / 0.1
      )
    )
  ),
  ser_scre_025 = list(
    list(
      at = step_interv_start,
      param = list(
        hiv.test.rate = param$hiv.test.rate * 0.75
      )
    )
  ),
  ser_scre_05 = list(
    list(
      at = step_interv_start,
      param = list(
        hiv.test.rate = param$hiv.test.rate * 0.5
      )
    )
  ),
  ser_scre_09 = list(
    list(
      at = step_interv_start,
      param = list(
        hiv.test.rate = param$hiv.test.rate * 0.1
      )
    )
  ),
  ser_art_025 = list(
    list(
      at = step_interv_start,
      param = list(
        tx.halt.part.prob = param$tx.halt.part.prob / 0.75
      )
    )
  ),
  ser_art_05 = list(
    list(
      at = step_interv_start,
      param = list(
        tx.halt.part.prob = param$tx.halt.part.prob / 0.5
      )
    )
  ),
  ser_art_09 = list(
    list(
      at = step_interv_start,
      param = list(
        tx.halt.part.prob = param$tx.halt.part.prob / 0.1
      )
    )
  ),
  ser_stitx_025 = list(
    list(
      at = step_interv_start,
      param = list(
        gc.sympt.prob.tx = param$gc.sympt.prob.tx * 0.75,
        gc.asympt.prob.tx = param$gc.asympt.prob.tx * 0.75,
        ct.sympt.prob.tx = param$ct.sympt.prob.tx * 0.75,
        ct.asympt.prob.tx = param$ct.asympt.prob.tx * 0.75
      )
    )
  ),
  ser_stitx_05 = list(
    list(
      at = step_interv_start,
      param = list(
        gc.sympt.prob.tx = param$gc.sympt.prob.tx * 0.5,
        gc.asympt.prob.tx = param$gc.asympt.prob.tx * 0.5,
        ct.sympt.prob.tx = param$ct.sympt.prob.tx * 0.5,
        ct.asympt.prob.tx = param$ct.asympt.prob.tx * 0.5
      )
    )
  ),
  ser_stitx_09 = list(
    list(
      at = step_interv_start,
      param = list(
        gc.sympt.prob.tx = param$gc.sympt.prob.tx * 0.1,
        gc.asympt.prob.tx = param$gc.asympt.prob.tx * 0.1,
        ct.sympt.prob.tx = param$ct.sympt.prob.tx * 0.1,
        ct.asympt.prob.tx = param$ct.asympt.prob.tx * 0.1
      )
    )
  ),
  ser_all_025 = list(
    list(
      at = step_interv_start,
      param = list(
        prep.start.prob = param$prep.start.prob * 0.75,
        prep.discont.rate = param$prep.discont.rate / 0.75,
        hiv.test.rate = param$hiv.test.rate * 0.75,
        tx.halt.part.prob = param$tx.halt.part.prob / 0.75,
        gc.sympt.prob.tx = param$gc.sympt.prob.tx * 0.75,
        gc.asympt.prob.tx = param$gc.asympt.prob.tx * 0.75,
        ct.sympt.prob.tx = param$ct.sympt.prob.tx * 0.75,
        ct.asympt.prob.tx = param$ct.asympt.prob.tx * 0.75
      )
    )
  ),
  ser_all_05 = list(
    list(
      at = step_interv_start,
      param = list(
        prep.start.prob = param$prep.start.prob * 0.5,
        prep.discont.rate = param$prep.discont.rate / 0.5,
        hiv.test.rate = param$hiv.test.rate * 0.5,
        tx.halt.part.prob = param$tx.halt.part.prob / 0.5,
        gc.sympt.prob.tx = param$gc.sympt.prob.tx * 0.5,
        gc.asympt.prob.tx = param$gc.asympt.prob.tx * 0.5,
        ct.sympt.prob.tx = param$ct.sympt.prob.tx * 0.5,
        ct.asympt.prob.tx = param$ct.asympt.prob.tx * 0.5
      )
    )
  ),
  ser_all_09 = list(
    list(
      at = step_interv_start,
      param = list(
        prep.start.prob = param$prep.start.prob * 0.1,
        prep.discont.rate = param$prep.discont.rate / 0.1,
        hiv.test.rate = param$hiv.test.rate * 0.1,
        tx.halt.part.prob = param$tx.halt.part.prob / 0.1,
        gc.sympt.prob.tx = param$gc.sympt.prob.tx * 0.1,
        gc.asympt.prob.tx = param$gc.asympt.prob.tx * 0.1,
        ct.sympt.prob.tx = param$ct.sympt.prob.tx * 0.1,
        ct.asympt.prob.tx = param$ct.asympt.prob.tx * 0.1
      )
    )
  ),
  comb_025_025 = list(
    list(
      at = step_interv_start,
      param = list(
        netresim.form.rr = c(1, 0.75, 0.75),
        netresim.disl.rr = c(1, 1/0.75),
        prep.start.prob = param$prep.start.prob * 0.75,
        prep.discont.rate = param$prep.discont.rate / 0.75,
        hiv.test.rate = param$hiv.test.rate * 0.75,
        tx.halt.part.prob = param$tx.halt.part.prob / 0.75,
        gc.sympt.prob.tx = param$gc.sympt.prob.tx * 0.75,
        gc.asympt.prob.tx = param$gc.asympt.prob.tx * 0.75,
        ct.sympt.prob.tx = param$ct.sympt.prob.tx * 0.75,
        ct.asympt.prob.tx = param$ct.asympt.prob.tx * 0.75
      )
    )
  ),
  comb_025_05 = list(
    list(
      at = step_interv_start,
      param = list(
        netresim.form.rr = c(1, 0.75, 0.75),
        netresim.disl.rr = c(1, 1/0.75),
        prep.start.prob = param$prep.start.prob * 0.5,
        prep.discont.rate = param$prep.discont.rate / 0.5,
        hiv.test.rate = param$hiv.test.rate * 0.5,
        tx.halt.part.prob = param$tx.halt.part.prob / 0.5,
        gc.sympt.prob.tx = param$gc.sympt.prob.tx * 0.5,
        gc.asympt.prob.tx = param$gc.asympt.prob.tx * 0.5,
        ct.sympt.prob.tx = param$ct.sympt.prob.tx * 0.5,
        ct.asympt.prob.tx = param$ct.asympt.prob.tx * 0.5
      )
    )
  ),
  comb_025_09 = list(
    list(
      at = step_interv_start,
      param = list(
        netresim.form.rr = c(1, 0.75, 0.75),
        netresim.disl.rr = c(1, 1/0.75),
        prep.start.prob = param$prep.start.prob * 0.1,
        prep.discont.rate = param$prep.discont.rate / 0.1,
        hiv.test.rate = param$hiv.test.rate * 0.1,
        tx.halt.part.prob = param$tx.halt.part.prob / 0.1,
        gc.sympt.prob.tx = param$gc.sympt.prob.tx * 0.1,
        gc.asympt.prob.tx = param$gc.asympt.prob.tx * 0.1,
        ct.sympt.prob.tx = param$ct.sympt.prob.tx * 0.1,
        ct.asympt.prob.tx = param$ct.asympt.prob.tx * 0.1
      )
    )
  ),
  comb_05_025 = list(
    list(
      at = step_interv_start,
      param = list(
        netresim.form.rr = c(1, 0.5, 0.5),
        netresim.disl.rr = c(1, 1/0.5),
        prep.start.prob = param$prep.start.prob * 0.75,
        prep.discont.rate = param$prep.discont.rate / 0.75,
        hiv.test.rate = param$hiv.test.rate * 0.75,
        tx.halt.part.prob = param$tx.halt.part.prob / 0.75,
        gc.sympt.prob.tx = param$gc.sympt.prob.tx * 0.75,
        gc.asympt.prob.tx = param$gc.asympt.prob.tx * 0.75,
        ct.sympt.prob.tx = param$ct.sympt.prob.tx * 0.75,
        ct.asympt.prob.tx = param$ct.asympt.prob.tx * 0.75
      )
    )
  ),
  comb_05_05 = list(
    list(
      at = step_interv_start,
      param = list(
        netresim.form.rr = c(1, 0.5, 0.5),
        netresim.disl.rr = c(1, 1/0.5),
        prep.start.prob = param$prep.start.prob * 0.5,
        prep.discont.rate = param$prep.discont.rate / 0.5,
        hiv.test.rate = param$hiv.test.rate * 0.5,
        tx.halt.part.prob = param$tx.halt.part.prob / 0.5,
        gc.sympt.prob.tx = param$gc.sympt.prob.tx * 0.5,
        gc.asympt.prob.tx = param$gc.asympt.prob.tx * 0.5,
        ct.sympt.prob.tx = param$ct.sympt.prob.tx * 0.5,
        ct.asympt.prob.tx = param$ct.asympt.prob.tx * 0.5
      )
    )
  ),
  comb_05_09 = list(
    list(
      at = step_interv_start,
      param = list(
        netresim.form.rr = c(1, 0.5, 0.5),
        netresim.disl.rr = c(1, 1/0.5),
        prep.start.prob = param$prep.start.prob * 0.1,
        prep.discont.rate = param$prep.discont.rate / 0.1,
        hiv.test.rate = param$hiv.test.rate * 0.1,
        tx.halt.part.prob = param$tx.halt.part.prob / 0.1,
        gc.sympt.prob.tx = param$gc.sympt.prob.tx * 0.1,
        gc.asympt.prob.tx = param$gc.asympt.prob.tx * 0.1,
        ct.sympt.prob.tx = param$ct.sympt.prob.tx * 0.1,
        ct.asympt.prob.tx = param$ct.asympt.prob.tx * 0.1
      )
    )
  ),
  comb_075_025 = list(
    list(
      at = step_interv_start,
      param = list(
        netresim.form.rr = c(1, 0.25, 0.25),
        netresim.disl.rr = c(1, 1/0.25),
        prep.start.prob = param$prep.start.prob * 0.75,
        prep.discont.rate = param$prep.discont.rate / 0.75,
        hiv.test.rate = param$hiv.test.rate * 0.75,
        tx.halt.part.prob = param$tx.halt.part.prob / 0.75,
        gc.sympt.prob.tx = param$gc.sympt.prob.tx * 0.75,
        gc.asympt.prob.tx = param$gc.asympt.prob.tx * 0.75,
        ct.sympt.prob.tx = param$ct.sympt.prob.tx * 0.75,
        ct.asympt.prob.tx = param$ct.asympt.prob.tx * 0.75
      )
    )
  ),
  comb_075_05 = list(
    list(
      at = step_interv_start,
      param = list(
        netresim.form.rr = c(1, 0.25, 0.25),
        netresim.disl.rr = c(1, 1/0.25),
        prep.start.prob = param$prep.start.prob * 0.5,
        prep.discont.rate = param$prep.discont.rate / 0.5,
        hiv.test.rate = param$hiv.test.rate * 0.5,
        tx.halt.part.prob = param$tx.halt.part.prob / 0.5,
        gc.sympt.prob.tx = param$gc.sympt.prob.tx * 0.5,
        gc.asympt.prob.tx = param$gc.asympt.prob.tx * 0.5,
        ct.sympt.prob.tx = param$ct.sympt.prob.tx * 0.5,
        ct.asympt.prob.tx = param$ct.asympt.prob.tx * 0.5
      )
    )
  ),
  comb_075_09 = list(
    list(
      at = step_interv_start,
      param = list(
        netresim.form.rr = c(1, 0.25, 0.25),
        netresim.disl.rr = c(1, 1/0.25),
        prep.start.prob = param$prep.start.prob * 0.1,
        prep.discont.rate = param$prep.discont.rate / 0.1,
        hiv.test.rate = param$hiv.test.rate * 0.1,
        tx.halt.part.prob = param$tx.halt.part.prob / 0.1,
        gc.sympt.prob.tx = param$gc.sympt.prob.tx * 0.1,
        gc.asympt.prob.tx = param$gc.asympt.prob.tx * 0.1,
        ct.sympt.prob.tx = param$ct.sympt.prob.tx * 0.1,
        ct.asympt.prob.tx = param$ct.asympt.prob.tx * 0.1
      )
    )
  ),
  comb_09_025 = list(
    list(
      at = step_interv_start,
      param = list(
        netresim.form.rr = c(1, 0.1, 0.1),
        netresim.disl.rr = c(1, 1/0.1),
        prep.start.prob = param$prep.start.prob * 0.75,
        prep.discont.rate = param$prep.discont.rate / 0.75,
        hiv.test.rate = param$hiv.test.rate * 0.75,
        tx.halt.part.prob = param$tx.halt.part.prob / 0.75,
        gc.sympt.prob.tx = param$gc.sympt.prob.tx * 0.75,
        gc.asympt.prob.tx = param$gc.asympt.prob.tx * 0.75,
        ct.sympt.prob.tx = param$ct.sympt.prob.tx * 0.75,
        ct.asympt.prob.tx = param$ct.asympt.prob.tx * 0.75
      )
    )
  ),
  comb_09_05 = list(
    list(
      at = step_interv_start,
      param = list(
        netresim.form.rr = c(1, 0.1, 0.1),
        netresim.disl.rr = c(1, 1/0.1),
        prep.start.prob = param$prep.start.prob * 0.5,
        prep.discont.rate = param$prep.discont.rate / 0.5,
        hiv.test.rate = param$hiv.test.rate * 0.5,
        tx.halt.part.prob = param$tx.halt.part.prob / 0.5,
        gc.sympt.prob.tx = param$gc.sympt.prob.tx * 0.5,
        gc.asympt.prob.tx = param$gc.asympt.prob.tx * 0.5,
        ct.sympt.prob.tx = param$ct.sympt.prob.tx * 0.5,
        ct.asympt.prob.tx = param$ct.asympt.prob.tx * 0.5
      )
    )
  ),
  comb_09_09 = list(
    list(
      at = step_interv_start,
      param = list(
        netresim.form.rr = c(1, 0.1, 0.1),
        netresim.disl.rr = c(1, 1/0.1),
        prep.start.prob = param$prep.start.prob * 0.1,
        prep.discont.rate = param$prep.discont.rate / 0.1,
        hiv.test.rate = param$hiv.test.rate * 0.1,
        tx.halt.part.prob = param$tx.halt.part.prob / 0.1,
        gc.sympt.prob.tx = param$gc.sympt.prob.tx * 0.1,
        gc.asympt.prob.tx = param$gc.asympt.prob.tx * 0.1,
        ct.sympt.prob.tx = param$ct.sympt.prob.tx * 0.1,
        ct.asympt.prob.tx = param$ct.asympt.prob.tx * 0.1
      )
    )
  )
)

sens_base <- scenarios["comb_05_05"][[1]][[1]]
sens_end_net <- list(
  at = step_interv_start,
  param = list(
    netresim.form.rr = rep(1, 3),
    netresim.disl.rr = rep(1, 2)
  )
)
sens_end_ser <- list(
  at = step_interv_start,
  param = list(
    prep.start.prob = param$prep.start.prob,
    prep.discont.rate = param$prep.discont.rate,
    tx.halt.part.prob = param$tx.halt.part.prob,
    hiv.test.rate = param$hiv.test.rate,
    gc.sympt.prob.tx = param$gc.sympt.prob.tx,
    gc.asympt.prob.tx = param$gc.asympt.prob.tx,
    ct.sympt.prob.tx = param$ct.sympt.prob.tx,
    ct.asympt.prob.tx = param$ct.asympt.prob.tx
  )
)

mk_sens_scenar <- function(at_net, at_ser) {
  ll <- list(sens_base, sens_end_net, sens_end_ser)
  ll[[2]]$at <- ll[[2]]$at + at_net / 12 * 52
  ll[[3]]$at <- ll[[3]]$at + at_ser / 12 * 52
  ll
}

dts <- purrr::cross(list(seq(0, 18, 3), seq(0, 18, 3)))
dts <- purrr::transpose(dts)
dts <- purrr::map(dts, as.numeric)

sens_scenarios <- purrr::pmap(dts, ~ mk_sens_scenar(..1, ..2))

names(sens_scenarios) <- purrr::pmap_chr(dts, ~ paste0("sensi_net", ..1, "_ser", ..2))
