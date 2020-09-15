library(tidyverse)

df_files <- fs::dir_ls("out/data/", regex = "df_sim")

for (df_f in df_files) {
  df <- readRDS(df_f)
  scenario <- unique(df$scenario)

  sc_dir <- fs::path("analysis/data/", scenario)
  if (!fs::dir_exists(sc_dir))
    fs::dir_create(sc_dir)

  fs::file_move(df_f, sc_dir)
}
