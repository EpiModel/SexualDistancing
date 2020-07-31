library(EpiModelHIV)
## devtools::load_all("../EpiModelHIV-p/")

orig <- readRDS("out/est/netest.rds")
netstats <- readRDS("out/est/netstats.rds")
epistats <- readRDS("out/est/epistats.rds")

full_tx_eff <- rep(1, 3)

param <- param_msm(
  netstats = netstats,
  epistats = epistats,
  hiv.test.rate = c(0.00385, 0.00385, 0.0069),
  hiv.test.late.prob = rep(0, 3),
  tx.init.prob = c(0.1775, 0.19, 0.2521),
  tt.part.supp = 1 - full_tx_eff,
  tt.full.supp = full_tx_eff,
  tt.dur.supp = rep(0, 3),
  tx.halt.part.prob = c(0.0065, 0.0053, 0.003),
  tx.halt.full.rr = rep(0.45, 3),
  tx.halt.dur.rr = rep(0.45, 3),
  tx.reinit.part.prob = rep(0.00255, 3),
  tx.reinit.full.rr = rep(1, 3),
  tx.reinit.dur.rr = rep(1, 3),
  max.time.off.tx.full.int = 52 * 15,
  max.time.on.tx.part.int = 52 * 10,
  max.time.off.tx.part.int = 52 * 10,
  aids.mr = 1 / 250,
  trans.scale = c(2.68, 0.4, 0.27), #c(2.21, 0.405, 0.255),
  acts.scale = 1.00,
  acts.aids.vl = 5.75,
  circ.prob = c(0.874, 0.874, 0.918),
  a.rate = 0.00052,
  prep.start = (52 * 60) + 1,
  riskh.start = 52 * 59,
  prep.adhr.dist = c(0.089, 0.127, 0.784),
  prep.adhr.hr = c(0.69, 0.19, 0.01),
  prep.start.prob =  0.71, # 0.00896,
  prep.discont.rate = 0.02138792, # 1 - (2^(-1/(224.4237/7)))
  ## prep.tst.int = 90/7,         # do I need that?
  ## prep.risk.int = 182/7,       # do I need that?
  ## prep.sti.screen.int = 182/7,
  ## prep.sti.prob.tx = 1,
  prep.risk.reassess.method = "year",
  prep.require.lnt = TRUE, # FALSE -> start with random PrEP initiation

  ## STI PARAMS (default: from combprev2, make it gaps)
  ## Using values in prep-race: scripts/burnin/sim.burn.R
  ## If not mentionned -> default from prep disparities
  ## for H : mean(c(B, W))
  #ok
  rgc.tprob = 0.357,  # gaps appendix 9.4
  ugc.tprob = 0.248,  # gaps appendix 9.4
  rct.tprob = 0.3216, # gaps appendix 9.3
  uct.tprob = 0.213,  # gaps appendix 9.3
  rgc.sympt.prob = 0.077, # gaps appendix 10.3
  ugc.sympt.prob = 0.824, # gaps appendix 10.3
  rct.sympt.prob = 0.1035,# gaps appendix 10.2
  uct.sympt.prob = 0.885, # gaps appendix 10.2
  rgc.ntx.int = 35.11851, # gaps appendix 11.2
  ugc.ntx.int = 35.11851, # gaps appendix 11.2
  gc.tx.int   = 2, # gaps appendix 11.2 - mentionned, not explicit
  rct.ntx.int = 44.24538, # gaps appendix 11.1
  uct.ntx.int = 44.24538, # gaps appendix 11.1
  ct.tx.int   = 2, # gaps appendix 11.1 - mentionned, not explicit

  gc.sympt.prob.tx =  rep(0.9, 3),  #c(0.86, 0.91, 0.96),
  ct.sympt.prob.tx =  rep(0.9, 3),  #c(0.72, 0.785, 0.85),
  gc.asympt.prob.tx = rep(0.1, 3), #c(0.10, 0.145, 0.19),
  ct.asympt.prob.tx = rep(0.1, 3), #c(0.05, 0.525, 0.10),
  # gaps appendix 9.3 - 9.4 (not explained this way but similar result)
  sti.cond.eff = 0.95,
  sti.cond.fail = c(0.39, 0.3, 0.21),
  # gaps appendix 9.2
  hiv.rgc.rr = 2.78,
  hiv.ugc.rr = 1.73,
  hiv.rct.rr = 2.78,
  hiv.uct.rr = 1.73,
 # if both ct + gc -> log(RRgc) + 0.2 * log(RRct) | swap ct and gc if RRct > RRgc
  hiv.dual.rr = 0.2 # not mentionned in appendix
)

## must be set by the calling script
if (lnt == FALSE) {
  param$prep.require.lnt = FALSE
  param$prep.start.prob = 0.00411
}

init <- init_msm(
  prev.ugc = 0,
  prev.rct = 0,
  prev.rgc = 0,
  prev.uct = 0
)
