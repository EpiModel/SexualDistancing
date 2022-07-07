# EM toy  ----------------------------------------------------------------------
library(EpiModel)

n <- 1e5
nw <- network.initialize(n = n)
nw <- set.vertex.attribute(nw, "race", rbinom(n, 1, 0.5))
est <- netest(
  nw,
  formation = ~edges + nodematch("race"), target.stats = c(25, 10),
  coef.diss = dissolution_coefs(~offset(edges), 10, 0),
  verbose = FALSE
)

param <- param.net(inf.prob = 0.3, act.rate = 0.5)
init <- init.net(i.num = n / 5)
control <- control.net(
  type = "SI",
  nsims = 1, nsteps = 100,
  verbose = FALSE, tergmLite = TRUE
)

options(browser = "firefox")

profvis::profvis({
  mod <- netsim(est, param, init, control)
})

set.seed(1)
microbenchmark::microbenchmark(
  time_it = {
  status <- rbinom(n, 1, 0.3)
  nw <- activate.vertex.attribute( nw,
    prefix = "testatus", value = status,
    onset = 1,
    terminus = Inf
  )
})

# EM simple  -------------------------------------------------------------------
library(EpiModel)

n <- 1e5
nw <- network.initialize(n = n)
nw <- set.vertex.attribute(nw, "race", rbinom(n, 1, 0.5))
est <- netest(
  nw,
  formation = ~edges + nodematch("race"), target.stats = c(25, 10),
  coef.diss = dissolution_coefs(~offset(edges), 10, 0),
  verbose = FALSE
)

param <- param.net(inf.prob = 0.3, act.rate = 0.5)
init <- init.net(i.num = n / 5)
control <- control.net(
  type = "SI",
  nsims = 1, nsteps = 100,
  verbose = FALSE, tergmLite = TRUE
)

options(browser = "firefox")

profvis::profvis({
  mod <- netsim(est, param, init, control)
})

# test EMHIV -------------------------------------------------------------------
lnt <- TRUE # if FALSE: set `require.lnt` to FALSE and adjust ` prep.start.prob`
source("R/utils-sim_calib_params.R")

control <- control_msm(
  nsteps = 52 * 4,
  nsims = 1,
  ncores = 1,
  save.nwstats = FALSE,
  save.clin.hist = FALSE,
  verbose = TRUE,
)

init <- init_msm(
  prev.ugc = 0.0,
  prev.rct = 0.0,
  prev.rgc = 0.0,
  prev.uct = 0.0
)

dat <- initialize_msm(orig, param, init, control, s = 1)
dat2 <- dat

options(browser = "firefox")

profvis::profvis({
  for (at in 2:104) {
    print(at)
    at = 2
    # dat <- aging_msm(dat, at)
    # dat <- departure_msm(dat, at)
    # dat <- arrival_msm(dat, at)
    # dat <- hivtest_msm(dat, at)
    # dat <- hivtx_msm(dat, at)
    # dat <- hivprogress_msm(dat, at)
    # dat <- hivvl_msm(dat, at)
    dat <- simnet_msm(dat, at)
    # dat <- acts_msm(dat, at)
    # dat <- condoms_msm(dat, at)
    # dat <- position_msm(dat, at)
    # dat <- prep_msm(dat, at)
    # dat <- hivtrans_msm(dat, at)
    # dat <- stitrans_msm(dat, at)
    # dat <- stirecov_msm(dat, at)
    # dat <- stitx_msm(dat, at)
    # dat <- prevalence_msm(dat, at)
  }
})

edges_correct_msm <- function(dat, at) {

  old.num <- dat$epi$num[at - 1]
  new.num <- sum(dat$attr$active == 1, na.rm = TRUE)
  adjust <- log(old.num) - log(new.num)

  coef.form.m <- get_nwparam(dat, network = 1)$coef.form
  coef.form.m[1] <- coef.form.m[1] + adjust
  dat$nwparam[[1]]$coef.form <- coef.form.m

  coef.form.p <- get_nwparam(dat, network = 2)$coef.form
  coef.form.p[1] <- coef.form.p[1] + adjust
  dat$nwparam[[2]]$coef.form <- coef.form.p

  coef.form.i <- get_nwparam(dat, network = 3)$coef.form
  coef.form.i[1] <- coef.form.i[1] + adjust
  dat$nwparam[[3]]$coef.form <- coef.form.i

  return(dat)
}

update_plist <- function(dat, at, ptype) {
  # pull existing partner type specific list
  plist1 <- dat$temp$plist[dat$temp$plist[, "ptype"] == ptype, ]

  # look up dissolutions, update stop time
  uid <- dat$attr$uid
  news <- attr(dat$el[[ptype]], "changes")
  news_uid <- cbind(matrix(uid[news[, 1:2]], ncol = 2), news[, 3])
  news_uid_stop <- news_uid[news_uid[, 3] == 0, , drop = FALSE]
  pid_plist1 <- plist1[, 1]*1e7 + plist1[, 2]
  pid_stop <- news_uid_stop[, 1]*1e7 + news_uid_stop[, 2]
  matches_stop <- match(pid_stop, pid_plist1)
  plist1[matches_stop, "stop"] <- at

  # look up new formations, row bind them
  news_uid_start <- news_uid[news_uid[, 3] == 1, , drop = FALSE]
  if (nrow(news_uid_start) > 0) {
    plist1 <- rbind(plist1, cbind(news_uid_start[, 1:2, drop = FALSE], ptype, at, NA))
  }

  return(plist1)
}


calc_nwstats <- function(dat, at) {

  for (nw in 1:3) {
    n <- attr(dat$el[[nw]], "n")
    edges <- nrow(dat$el[[nw]])
    meandeg <- round(edges * (2/n), 3)
    concurrent <- round(mean(get_degree(dat$el[[nw]]) > 1), 3)
    mat <- matrix(c(edges, meandeg, concurrent), ncol = 3, nrow = 1)
    if (at == 1) {
      dat$stats$nwstats[[nw]] <- mat
      colnames(dat$stats$nwstats[[nw]]) <- c("edges", "mdeg", "conc")
    }
    if (at > 1) {
      dat$stats$nwstats[[nw]] <- rbind(dat$stats$nwstats[[nw]], mat)
    }
  }

  return(dat)
}


profvis::profvis({
  for (at in 2:104) {
    print(at)
    at = 2
    dat <- dat2

    ## Edges correction
    dat <- edges_correct_msm(dat, at)

    ## Main network
    nwparam.m <- EpiModel::get_nwparam(dat, network = 1)

    nwparam.m$coef.form[1] <-
      nwparam.m$coef.form[1] + log(dat$param$netresim.form.rr[1])
    nwparam.m$coef.diss$coef.adj[1] <-
      nwparam.m$coef.diss$coef.adj[1] - log(dat$param$netresim.disl.rr[1])

    dat$attr$deg.casl <- get_degree(dat$el[[2]])
    dat <- tergmLite::updateModelTermInputs(dat, network = 1)

    dat$el[[1]] <- tergmLite::simulate_network(p = dat$p[[1]],
      el = dat$el[[1]],
      coef.form = nwparam.m$coef.form,
      coef.diss = nwparam.m$coef.diss$coef.adj,
      save.changes = TRUE)

    plist1 <- update_plist(dat, at, ptype = 1)

    ## Casual network
    nwparam.p <- EpiModel::get_nwparam(dat, network = 2)

    nwparam.p$coef.form[1] <-
      nwparam.p$coef.form[1] + log(dat$param$netresim.form.rr[2])
    nwparam.p$coef.diss$coef.adj[1] <-
      nwparam.p$coef.diss$coef.adj[1] - log(dat$param$netresim.disl.rr[2])

    dat$attr$deg.main <- get_degree(dat$el[[1]])
    dat <- tergmLite::updateModelTermInputs(dat, network = 2)

    dat$el[[2]] <- tergmLite::simulate_network(p = dat$p[[2]],
      el = dat$el[[2]],
      coef.form = nwparam.p$coef.form,
      coef.diss = nwparam.p$coef.diss$coef.adj,
      save.changes = TRUE)

    plist2 <- update_plist(dat, at, ptype = 2)

    dat$temp$plist <- rbind(plist1, plist2)
    if (dat$control$truncate.plist == TRUE) {
      to.keep <- which(is.na(dat$temp$plist[, "stop"]))
      dat$temp$plist <- dat$temp$plist[to.keep, ]
    }

    ## One-off network
    nwparam.i <- EpiModel::get_nwparam(dat, network = 3)
    nwparam.i$coef.form[1] <-
      nwparam.i$coef.form[1] + log(dat$param$netresim.form.rr[3])

    dat$attr$deg.tot <- pmin(dat$attr$deg.main + get_degree(dat$el[[2]]), 3)
    dat <- tergmLite::updateModelTermInputs(dat, network = 3)

    dat$el[[3]] <- tergmLite::simulate_ergm(p = dat$p[[3]],
      el = dat$el[[3]],
      coef = nwparam.i$coef.form)
  }
})

# Only simulate ----------------------------------------------------------------
lnt <- TRUE # if FALSE: set `require.lnt` to FALSE and adjust ` prep.start.prob`
source("R/utils-sim_calib_params.R")

control <- control_msm(
  nsteps = 52 * 4,
  nsims = 1, ncores = 1,
  save.nwstats = FALSE, save.clin.hist = FALSE,
  verbose = FALSE,
)

init <- init_msm()

dat <- initialize_msm(orig, param, init, control, s = 1)
nwparam.m <- EpiModel::get_nwparam(dat, network = 1)

nwparam.m$coef.form[1] <-
  nwparam.m$coef.form[1] + log(dat$param$netresim.form.rr[1])
nwparam.m$coef.diss$coef.adj[1] <-
  nwparam.m$coef.diss$coef.adj[1] - log(dat$param$netresim.disl.rr[1])

dat$attr$deg.casl <- get_degree(dat$el[[2]])
dat2 <- dat
microbenchmark::microbenchmark( {
    dat <- dat2
    dat <- tergmLite::updateModelTermInputs(dat, network = 1)

    dat$el[[1]] <- tergmLite::simulate_network(p = dat$p[[1]],
      el = dat$el[[1]],
      coef.form = nwparam.m$coef.form,
      coef.diss = nwparam.m$coef.diss$coef.adj,
      save.changes = TRUE)
  })

