## Exploratory and Data Processing

fn <- list.files("analysis/data", pattern = "df_sim", full.names = TRUE, recursive = TRUE)
# df1 <- readRDS(fn[1])
fn <- fn[grepl("sensi_", fn)] # only sensitivity analysis

# To process only a subset:
## fn <- fn[grepl("comb_025", fn)]

btch <- 0
for (i in seq_along(fn)) {
  btch <- btch + 1
  dft <- readRDS(fn[i])
  # dft <- dplyr::select(dft, var.names)
  prep_cov = dft$prepCurr / dft$prepElig
  hiv_diag = dft$cc.dx
  hiv_suppr = dft$cc.vsupp
  sti_tx = (dft$gc.tx + dft$ct.tx) / (dft$gc + dft$ct)
  sti_inc = dft$ir100.sti
  sti_gc_inc = dft$ir100.gc
  sti_ct_inc = dft$ir100.ct
  hiv_inc = dft$ir100
  deg_main = dft$main.deg
  deg_casl = dft$casl.deg
  deg_inst = dft$inst.deg
  sti.n.tx = dft$gc.tx + dft$ct.tx
  sti.n.tot = dft$gc + dft$ct
  dft2 <- data.frame(sim = dft$sim,
                     batch = btch,
                     time = dft$time,
                     scenario = dft$scenario,
                     prep_cov, hiv_diag, hiv_suppr, sti_tx, sti_inc, sti_gc_inc,
                     sti_ct_inc, hiv_inc, deg_main, deg_casl, deg_inst,
                     sti.n.tot, sti.n.tx)
  if (i == 1) {
    df <- dft2
  } else {
    df <- rbind(df, dft2)
  }
  cat(i, "/", length(fn), "...\n ", sep = "")
}
dim(df)

saveRDS(df, file = "analysis/data/df_sensi.rds")


df_sensi <- readRDS("analysis/data/df_sensi.rds")
dim(df_sensi)
head(df_sensi)
table(df_sensi$scenario)
unique(table(df_sensi$scenario))
