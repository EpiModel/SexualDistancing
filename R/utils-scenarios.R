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
        netresim.disl.rr = c(1, 10)
        )
    )
  ),
  net_all_05 = list(
    list(
      at = step_interv_start,
      param = list(
        netresim.form.rr = c(1, 0.5, 0.5),
        netresim.disl.rr = c(1, 10)
        )
    )
  ),
  net_all_09 = list(
    list(
      at = step_interv_start,
      param = list(
        netresim.form.rr = c(1, 0.1, 0.1),
        netresim.disl.rr = c(1, 10)
      )
    )
  ),
  ## net_casl_025 = list(
  ##   list(
  ##     at = step_interv_start,
  ##     param = list(
  ##       netresim.form.rr = c(1, 0.75, 1),
  ##       netresim.disl.rr = c(1, 10)
  ##     )
  ##   )
  ## ),
  ## net_casl_05 = list(
  ##   list(
  ##     at = step_interv_start,
  ##     param = list(
  ##       netresim.form.rr = c(1, 0.5, 1),
  ##       netresim.disl.rr = c(1, 10)
  ##     )
  ##   )
  ## ),
  ## net_casl_09 = list(
  ##   list(
  ##     at = step_interv_start,
  ##     param = list(
  ##       netresim.form.rr = c(1, 0.1, 1),
  ##       netresim.disl.rr = c(1, 10)
  ##     )
  ##   )
  ## ),
  ## net_casl_1 = list(
  ##   list(
  ##     at = step_interv_start,
  ##     param = list(
  ##       netresim.form.rr = c(1, 0, 1),
  ##       netresim.disl.rr = c(1, 10)
  ##     )
  ##   )
  ## ),
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
        netresim.disl.rr = c(1, Inf)
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
  ## comb_025_05 = list(
  ##   list(
  ##     at = step_interv_start,
  ##     param = list(
  ##       netresim.form.rr = c(1, 0.25, 0.25),
  ##       netresim.disl.rr = c(1, 10),
  ##       prep.start.prob = param$prep.start.prob * 0.5,
  ##       prep.discont.rate = param$prep.discont.rate / 0.5,
  ##       hiv.test.rate = param$hiv.test.rate * 0.5,
  ##       tx.halt.part.prob = param$tx.halt.part.prob / 0.5,
  ##       gc.sympt.prob.tx = param$gc.sympt.prob.tx * 0.5,
  ##       gc.asympt.prob.tx = param$gc.asympt.prob.tx * 0.5,
  ##       ct.sympt.prob.tx = param$ct.sympt.prob.tx * 0.5,
  ##       ct.asympt.prob.tx = param$ct.asympt.prob.tx * 0.5
  ##     )
  ##   )
  ## ),
  ## comb_025_09 = list(
  ##   list(
  ##     at = step_interv_start,
  ##     param = list(
  ##       netresim.form.rr = c(1, 0.25, 0.25),
  ##       netresim.disl.rr = c(1, 10),
  ##       prep.start.prob = param$prep.start.prob * 0.1,
  ##       prep.discont.rate = param$prep.discont.rate / 0.1,
  ##       hiv.test.rate = param$hiv.test.rate * 0.1,
  ##       tx.halt.part.prob = param$tx.halt.part.prob / 0.1,
  ##       gc.sympt.prob.tx = param$gc.sympt.prob.tx * 0.1,
  ##       gc.asympt.prob.tx = param$gc.asympt.prob.tx * 0.1,
  ##       ct.sympt.prob.tx = param$ct.sympt.prob.tx * 0.1,
  ##       ct.asympt.prob.tx = param$ct.asympt.prob.tx * 0.1
  ##     )
  ##   )
  ## ),
  ## comb_05_05 = list(
  ##   list(
  ##     at = step_interv_start,
  ##     param = list(
  ##       netresim.form.rr = c(1, 0.5, 0.5),
  ##       netresim.disl.rr = c(1, 10),
  ##       prep.start.prob = param$prep.start.prob * 0.5,
  ##       prep.discont.rate = param$prep.discont.rate / 0.5,
  ##       hiv.test.rate = param$hiv.test.rate * 0.5,
  ##       tx.halt.part.prob = param$tx.halt.part.prob / 0.5,
  ##       gc.sympt.prob.tx = param$gc.sympt.prob.tx * 0.5,
  ##       gc.asympt.prob.tx = param$gc.asympt.prob.tx * 0.5,
  ##       ct.sympt.prob.tx = param$ct.sympt.prob.tx * 0.5,
  ##       ct.asympt.prob.tx = param$ct.asympt.prob.tx * 0.5
  ##     )
  ##   )
  ## ),
  ## comb_05_09 = list(
  ##   list(
  ##     at = step_interv_start,
  ##     param = list(
  ##       netresim.form.rr = c(1, 0.5, 0.5),
  ##       netresim.disl.rr = c(1, 10),
  ##       prep.start.prob = param$prep.start.prob * 0.1,
  ##       prep.discont.rate = param$prep.discont.rate / 0.1,
  ##       hiv.test.rate = param$hiv.test.rate * 0.1,
  ##       tx.halt.part.prob = param$tx.halt.part.prob / 0.1,
  ##       gc.sympt.prob.tx = param$gc.sympt.prob.tx * 0.1,
  ##       gc.asympt.prob.tx = param$gc.asympt.prob.tx * 0.1,
  ##       ct.sympt.prob.tx = param$ct.sympt.prob.tx * 0.1,
  ##       ct.asympt.prob.tx = param$ct.asympt.prob.tx * 0.1
  ##     )
  ##   )
  ## ),
  ## comb_075_05 = list(
  ##   list(
  ##     at = step_interv_start,
  ##     param = list(
  ##       netresim.form.rr = c(1, 0.25, 0.25),
  ##       netresim.disl.rr = c(1, 10),
  ##       prep.start.prob = param$prep.start.prob * 0.5,
  ##       prep.discont.rate = param$prep.discont.rate / 0.5,
  ##       hiv.test.rate = param$hiv.test.rate * 0.5,
  ##       tx.halt.part.prob = param$tx.halt.part.prob / 0.5,
  ##       gc.sympt.prob.tx = param$gc.sympt.prob.tx * 0.5,
  ##       gc.asympt.prob.tx = param$gc.asympt.prob.tx * 0.5,
  ##       ct.sympt.prob.tx = param$ct.sympt.prob.tx * 0.5,
  ##       ct.asympt.prob.tx = param$ct.asympt.prob.tx * 0.5
  ##     )
  ##   )
  ## ),
  ## comb_075_09 = list(
  ##   list(
  ##     at = step_interv_start,
  ##     param = list(
  ##       netresim.form.rr = c(1, 0.25, 0.25),
  ##       netresim.disl.rr = c(1, 10),
  ##       prep.start.prob = param$prep.start.prob * 0.1,
  ##       prep.discont.rate = param$prep.discont.rate / 0.1,
  ##       hiv.test.rate = param$hiv.test.rate * 0.1,
  ##       tx.halt.part.prob = param$tx.halt.part.prob / 0.1,
  ##       gc.sympt.prob.tx = param$gc.sympt.prob.tx * 0.1,
  ##       gc.asympt.prob.tx = param$gc.asympt.prob.tx * 0.1,
  ##       ct.sympt.prob.tx = param$ct.sympt.prob.tx * 0.1,
  ##       ct.asympt.prob.tx = param$ct.asympt.prob.tx * 0.1
  ##     )
  ##   )
  ## ),
  ## comb_09_05 = list(
  ##   list(
  ##     at = step_interv_start,
  ##     param = list(
  ##       netresim.form.rr = c(1, 0.1, 0.1),
  ##       netresim.disl.rr = c(1, 10),
  ##       prep.start.prob = param$prep.start.prob * 0.5,
  ##       prep.discont.rate = param$prep.discont.rate / 0.5,
  ##       hiv.test.rate = param$hiv.test.rate * 0.5,
  ##       tx.halt.part.prob = param$tx.halt.part.prob / 0.5,
  ##       gc.sympt.prob.tx = param$gc.sympt.prob.tx * 0.5,
  ##       gc.asympt.prob.tx = param$gc.asympt.prob.tx * 0.5,
  ##       ct.sympt.prob.tx = param$ct.sympt.prob.tx * 0.5,
  ##       ct.asympt.prob.tx = param$ct.asympt.prob.tx * 0.5
  ##     )
  ##   )
  ## ),
  ## comb_09_09 = list(
  ##   list(
  ##     at = step_interv_start,
  ##     param = list(
  ##       netresim.form.rr = c(1, 0.1, 0.1),
  ##       netresim.disl.rr = c(1, 10),
  ##       prep.start.prob = param$prep.start.prob * 0.1,
  ##       prep.discont.rate = param$prep.discont.rate / 0.1,
  ##       hiv.test.rate = param$hiv.test.rate * 0.1,
  ##       tx.halt.part.prob = param$tx.halt.part.prob / 0.1,
  ##       gc.sympt.prob.tx = param$gc.sympt.prob.tx * 0.1,
  ##       gc.asympt.prob.tx = param$gc.asympt.prob.tx * 0.1,
  ##       ct.sympt.prob.tx = param$ct.sympt.prob.tx * 0.1,
  ##       ct.asympt.prob.tx = param$ct.asympt.prob.tx * 0.1
  ##     )
  ##   )
  ## ),
  comb_025_05 = list(
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
  comb_025_09 = list(
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
