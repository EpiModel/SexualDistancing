## Exploratory and Data Processing
library(future.apply)
plan(multiprocess)

fn <- list.files("analysis/data/main/", pattern = "df_sim", full.names = TRUE, recursive = TRUE)
# df1 <- readRDS(fn[1])

# To process only a subset:
## fn <- fn[grepl("318", fn)]

batch2df <- function(f_name, btch) {
  dft <- readRDS(f_name)
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

  dft2
}

ldf <- future_Map(batch2df, fn, seq_along(fn))
df <- dplyr::bind_rows(ldf)

dim(df)

saveRDS(df, file = "analysis/data/df.rds")


df <- readRDS("analysis/data/df.rds")
dim(df)
head(df)
table(df$scenario)
unique(table(df$scenario))
