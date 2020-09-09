
suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("tidyr"))

create_var_df <- function(df, scen, var) {
  tt <- filter(df, scenario == scen)
  tt <- select(tt, c("sim", "batch", "time", var))
  tt <- pivot_wider(tt,
                    id_cols = time,
                    names_from = c(sim, batch),
                    values_from = var,
                    names_prefix = "s",
                    names_sep = "")
  tt <- as.data.frame(tt)
  return(tt)
}

create_quants_df <- function(df, low = 0.25, high = 0.75) {
  df <- select(df, -time)
  out <- t(apply(df, 1, quantile, c(0.5, low, high)))
  return(out)
}

draw_quants <- function(x, col) {
  xx <- c(1:(nrow(x)), (nrow(x)):1)
  yy <- c(x[, 2], rev(x[, 3]))
  polygon(xx, yy, col = col, border = NA)
}

apply_roll <- function(x, n) {
  for (j in 1:ncol(x)) {
    x[, j] <- RcppRoll::roll_meanr(x[, j], n = n)
  }
  return(x)
}


calc_quants_prev <- function(x, var, at = 520, mult = 1, round = 1,
                             qnt.low = 0.025, qnt.high = 0.975) {
  if (is.null(x$epi[[var]])) {
    stop("var ", var, " does not exist on x", call. = FALSE)
  }
  out <- as.numeric(x$epi[[var]][at, ])*mult
  out <- quantile(out, c(0.5, qnt.low, qnt.high), names = FALSE)
  format <- paste0("%.", round, "f")
  out <- sprintf(format, out)
  out <- paste0(out[1], " (", out[2], ", ", out[3], ")")
  return(out)
}


calc_quants_ir <- function(x, scen, var, t.start, t.end,
                           qnt.low = 0.025, qnt.high = 0.975, round = 2) {
  if (is.null(x[[var]])) {
    stop("var ", var, " does not exist on x", call. = FALSE)
  }
  x <- create_var_df(x, scen, var)
  row.start <- which(x$time == t.start)
  row.end <- which(x$time == t.end)
  x <- x[row.start:row.end, -1]
  out <- as.numeric(colMeans(x))
  out <- quantile(out, c(0.5, qnt.low, qnt.high), names = FALSE)
  format <- paste0("%.", round, "f")
  out <- sprintf(format, out)
  out <- paste0(out[1], " (", out[2], ", ", out[3], ")")
  return(out)
}

calc_quants_ci <- function(x, scen, var, t.start, t.end,
                           qnt.low = 0.025, qnt.high = 0.975, round = 2) {
  if (is.null(x[[var]])) {
    stop("var ", var, " does not exist on x", call. = FALSE)
  }
  x <- create_var_df(x, scen, var)
  row.start <- which(x$time == t.start)
  row.end <- which(x$time == t.end)
  x <- x[row.start:row.end, -1]
  out <- as.numeric(colSums(x))
  out <- quantile(out, c(0.5, qnt.low, qnt.high), names = FALSE)
  format <- paste0("%.", round, "f")
  out <- sprintf(format, out)
  out <- paste0(out[1], " (", out[2], ", ", out[3], ")")
  return(out)
}


calc_quants_ia <- function(x, base.scen, comp.scen, var,
                           t.start, t.end,
                           qnt.low = 0.025, qnt.high = 0.975,
                           nsims = 1000, round.nia = 1, round.pia = 1,
                           ) {
  vec.nia <- rep(NA, nsims)
  vec.pia <- rep(NA, nsims)

  x.base <- create_var_df(x, base.scen, var)
  x.comp <- create_var_df(x, comp.scen, var)

  row.start.base <- which(x.base$time == t.start)
  row.end.base <- which(x.base$time == t.end)
  row.start.comp <- which(x.comp$time == t.start)
  row.end.comp <- which(x.comp$time == t.end)

  x.base <- x.base[row.start.base:row.end.base, -1]
  x.comp <- x.comp[row.start.comp:row.end.comp, -1]

  incid.comp.start <- unname(colSums(x.comp))
  incid.base.start <- unname(colSums(x.base))

  for (i in 1:nsims) {
    incid.comp <- sample(incid.comp.start)
    incid.base <- sample(incid.base.start)
    vec.nia[i] <- median(incid.base - incid.comp)
    vec.pia[i] <- median((incid.base - incid.comp) / incid.base)
  }
  nia <- quantile(vec.nia, c(0.5, qnt.low, qnt.high), names = FALSE)
  format <- paste0("%.", round.nia, "f")
  nia <- sprintf(format, nia)
  nia <- paste0(nia[1], " (", nia[2], ", ", nia[3], ")")

  pia <- quantile(vec.pia, c(0.5, qnt.low, qnt.high), names = FALSE)*100
  format <- paste0("%.", round.pia, "f")
  pia <- sprintf(format, pia)
  pia <- paste0(pia[1], " (", pia[2], ", ", pia[3], ")")

  out <- list()
  out$nia <- nia
  out$pia <- pia

  return(out)
}


epi_stats <- function(sim.base,
                      sim.comp = NULL,
                      otable,
                      at = 520,
                      qnt.low = 0.025,
                      qnt.high = 0.975) {

  if (is.null(sim.comp)) {
    x <- sim.base
  } else {
    x <- sim.comp
    # hazard ratio
    hr <- calc_quants_hr(sim.base, sim.comp, "ir100", qnt.low, qnt.high)
    hr.B <- calc_quants_hr(sim.base, sim.comp, "ir100.B", qnt.low, qnt.high)
    hr.H <- calc_quants_hr(sim.base, sim.comp, "ir100.H", qnt.low, qnt.high)
    hr.W <- calc_quants_hr(sim.base, sim.comp, "ir100.W", qnt.low, qnt.high)

    # infections averted
    ia <- calc_quants_ia(sim.base, sim.comp, "incid", qnt.low, qnt.high)
    ia.B <- calc_quants_ia(sim.base, sim.comp, "incid.B", qnt.low, qnt.high)
    ia.H <- calc_quants_ia(sim.base, sim.comp, "incid.H", qnt.low, qnt.high)
    ia.W <- calc_quants_ia(sim.base, sim.comp, "incid.W", qnt.low, qnt.high)

    # nnt
    nnt <- calc_quants_nnt(sim.base, sim.comp, "tot.tests", "incid")
    nnt.B <- calc_quants_nnt(sim.base, sim.comp, "tot.tests.B", "incid.B")
    nnt.H <- calc_quants_nnt(sim.base, sim.comp, "tot.tests.H", "incid.H")
    nnt.W <- calc_quants_nnt(sim.base, sim.comp, "tot.tests.W", "incid.W")
  }

  # HIV- tests per year
  testspy <- calc_quants_prev(x, "mean.neg.tests", at, 1/10, 2, qnt.low, qnt.high)
  testspy.B <- calc_quants_prev(x, "mean.neg.tests.B", at, 1/10, 2, qnt.low, qnt.high)
  testspy.H <- calc_quants_prev(x, "mean.neg.tests.H", at, 1/10, 2, qnt.low, qnt.high)
  testspy.W <- calc_quants_prev(x, "mean.neg.tests.W", at, 1/10, 2, qnt.low, qnt.high)

  # HIV- tested in past year
  pytest <- calc_quants_prev(x, "test.past.year", at, 100, 1, qnt.low, qnt.high)
  pytest.B <- calc_quants_prev(x, "test.past.year.B", at, 100, 1, qnt.low, qnt.high)
  pytest.H <- calc_quants_prev(x, "test.past.year.H", at, 100, 1, qnt.low, qnt.high)
  pytest.W <- calc_quants_prev(x, "test.past.year.W", at, 100, 1, qnt.low, qnt.high)

  # PrEP coverage
  x <- mutate_epi(x, pFrac = prepCurr / prepElig,
                     pFrac.B = prepCurr.B / prepElig.B,
                     pFrac.H = prepCurr.H / prepElig.H,
                     pFrac.W = prepCurr.W / prepElig.W)
  prep <- calc_quants_prev(x, "pFrac", at, 100, 1, qnt.low, qnt.high)
  prep.B <- calc_quants_prev(x, "pFrac.B", at, 100, 1, qnt.low, qnt.high)
  prep.H <- calc_quants_prev(x, "pFrac.H", at, 100, 1, qnt.low, qnt.high)
  prep.W <- calc_quants_prev(x, "pFrac.W", at, 100, 1, qnt.low, qnt.high)

  # HIV+ diagnosed
  dx <- calc_quants_prev(x, "cc.dx", at, 100, 1, qnt.low, qnt.high)
  dx.B <- calc_quants_prev(x, "cc.dx.B", at, 100, 1, qnt.low, qnt.high)
  dx.H <- calc_quants_prev(x, "cc.dx.H", at, 100, 1, qnt.low, qnt.high)
  dx.W <- calc_quants_prev(x, "cc.dx.W", at, 100, 1, qnt.low, qnt.high)

  # HIV+ diagnostic delay
  dx.delay <- calc_quants_prev(x, "cc.dx.delay.int.med", at, 1/52, 2, qnt.low, qnt.high)
  dx.delay.B <- calc_quants_prev(x, "cc.dx.delay.int.B.med", at, 1/52, 2, qnt.low, qnt.high)
  dx.delay.H <- calc_quants_prev(x, "cc.dx.delay.int.H.med", at, 1/52, 2, qnt.low, qnt.high)
  dx.delay.W <- calc_quants_prev(x, "cc.dx.delay.int.W.med", at, 1/52, 2, qnt.low, qnt.high)

  # HIV+ viral suppression
  vl.supp <- calc_quants_prev(x, "cc.vsupp", at, 100, 1, qnt.low, qnt.high)
  vl.supp.B <- calc_quants_prev(x, "cc.vsupp.B", at, 100, 1, qnt.low, qnt.high)
  vl.supp.H <- calc_quants_prev(x, "cc.vsupp.H", at, 100, 1, qnt.low, qnt.high)
  vl.supp.W <- calc_quants_prev(x, "cc.vsupp.W", at, 100, 1, qnt.low, qnt.high)

  vl.supp.all <- calc_quants_prev(x, "cc.vsupp.all", at, 100, 1, qnt.low, qnt.high)
  vl.supp.all.B <- calc_quants_prev(x, "cc.vsupp.all.B", at, 100, 1, qnt.low, qnt.high)
  vl.supp.all.H <- calc_quants_prev(x, "cc.vsupp.all.H", at, 100, 1, qnt.low, qnt.high)
  vl.supp.all.W <- calc_quants_prev(x, "cc.vsupp.all.W", at, 100, 1, qnt.low, qnt.high)

  # Linked in 1 month
  link1m <- calc_quants_prev(x, "cc.linked1m.int", at, 100, 1, qnt.low, qnt.high)
  link1m.B <- calc_quants_prev(x, "cc.linked1m.int.B", at, 100, 1, qnt.low, qnt.high)
  link1m.H <- calc_quants_prev(x, "cc.linked1m.int.H", at, 100, 1, qnt.low, qnt.high)
  link1m.W <- calc_quants_prev(x, "cc.linked1m.int.W", at, 100, 1, qnt.low, qnt.high)

  # prevalence
  # prev <- calc_quants_prev(x, "i.prev", at, 100, 1, qnt.low, qnt.high)
  # prev.B <- calc_quants_prev(x, "i.prev.B", at, 100, 1, qnt.low, qnt.high)
  # prev.H <- calc_quants_prev(x, "i.prev.H", at, 100, 1, qnt.low, qnt.high)
  # prev.W <- calc_quants_prev(x, "i.prev.W", at, 100, 1, qnt.low, qnt.high)

  # diagnosed prevalence
  # prev.dx <- calc_quants_prev(x, "i.prev.dx", at, 100, 1, qnt.low, qnt.high)
  # prev.dx.B <- calc_quants_prev(x, "i.prev.dx.B", at, 100, 1, qnt.low, qnt.high)
  # prev.dx.H <- calc_quants_prev(x, "i.prev.dx.H", at, 100, 1, qnt.low, qnt.high)
  # prev.dx.W <- calc_quants_prev(x, "i.prev.dx.W", at, 100, 1, qnt.low, qnt.high)

  # raw incidence
  # incid <- calc_quants_ir(x, "incid", qnt.low, qnt.high)
  # incid.B <- calc_quants_ir(x, "incid.B", qnt.low, qnt.high)
  # incid.H <- calc_quants_ir(x, "incid.H", qnt.low, qnt.high)
  # incid.W <- calc_quants_ir(x, "incid.W", qnt.low, qnt.high)

  # incidence rate
  ir100 <- calc_quants_ir(x, "ir100", qnt.low, qnt.high)
  ir100.B <- calc_quants_ir(x, "ir100.B", qnt.low, qnt.high)
  ir100.H <- calc_quants_ir(x, "ir100.H", qnt.low, qnt.high)
  ir100.W <- calc_quants_ir(x, "ir100.W", qnt.low, qnt.high)


  # Table Output -------------------------------------------------

  if (otable %in% 1:2) {
    if (is.null(sim.comp)) {
      dat <- cbind(ir100, pia = NA,
                   ir100.B, pia.B = NA,
                   ir100.H, pia.H = NA,
                   ir100.W, pia.W = NA,
                   testspy, pytest, prep, dx, dx.delay, vl.supp, vl.supp.all,
                   testspy.B, pytest.B, prep.B, dx.B, dx.delay.B, vl.supp.B, vl.supp.all.B,
                   testspy.H, pytest.H, prep.H, dx.H, dx.delay.H, vl.supp.H, vl.supp.all.H,
                   testspy.W, pytest.W, prep.W, dx.W, dx.delay.W, vl.supp.W, vl.supp.all.W)
    } else {
      dat <- cbind(ir100, pia = ia$pia,
                   ir100.B, pia.B = ia.B$pia,
                   ir100.H, pia.H = ia.H$pia,
                   ir100.W, pia.W = ia.W$pia,
                   testspy, pytest, prep, dx, dx.delay, vl.supp, vl.supp.all,
                   testspy.B, pytest.B, prep.B, dx.B, dx.delay.B, vl.supp.B, vl.supp.all.B,
                   testspy.H, pytest.H, prep.H, dx.H, dx.delay.H, vl.supp.H, vl.supp.all.H,
                   testspy.W, pytest.W, prep.W, dx.W, dx.delay.W, vl.supp.W, vl.supp.all.W)
    }
  }

  if (otable %in% 3:4) {
    if (is.null(sim.comp)) {
      dat <- cbind(ir100, pia = NA,
                   ir100.B, pia.B = NA,
                   ir100.H, pia.H = NA,
                   ir100.W, pia.W = NA,
                   link1m, vl.supp, vl.supp.all,
                   link1m.B, vl.supp.B, vl.supp.all.B,
                   link1m.H, vl.supp.H, vl.supp.all.H,
                   link1m.W, vl.supp.W, vl.supp.all.W)
    } else {
      dat <- cbind(ir100, pia = ia$pia,
                   ir100.B, pia.B = ia.B$pia,
                   ir100.H, pia.H = ia.H$pia,
                   ir100.W, pia.W = ia.W$pia,
                   link1m, vl.supp, vl.supp.all,
                   link1m.B, vl.supp.B, vl.supp.all.B,
                   link1m.H, vl.supp.H, vl.supp.all.H,
                   link1m.W, vl.supp.W, vl.supp.all.W)
    }
  }

  out <- as.data.frame(dat, stringsAsFactors = FALSE)

  return(out)
}


gather_netsim <- function(fn) {
  for (i in seq_along(fn)) {
    if (i == 1) {
      out <- list()
    }
    load(fn[i])
    out[[i]] <- sim
    cat("\nFile", fn[i], "complete")
  }
  class(out) <- "netsim.list"
  return(out)
}

plot_netsim_list <- function(x, var, ylim) {

  pal <- brewer_ramp(n = length(x), plt = "Set1", delete.lights = TRUE)

  if (class(x) == "netsim.list") {
    for (i in seq_along(x)) {
      if (i == 1) {
        plot(x[[i]], y = var, mean.col = pal[i], qnts = FALSE, ylim = ylim)
      } else {
        plot(x[[i]], y = var, mean.col = pal[i], qnts = FALSE, add = TRUE)
      }
    }
  } else {
    for (i in seq_along(x)) {
      load(x[i])
      if (i == 1) {
        plot(sim, y = var, mean.col = pal[i], qnts = FALSE, ylim = ylim)
      } else {
        plot(sim, y = var, mean.col = pal[i], qnts = FALSE, add = TRUE)
      }
    }
  }
  legend("topleft", legend = paste("Run", seq_along(x)), lty = 1, lwd = 2,
         cex = 0.8, bty = "n", col = pal)
}
