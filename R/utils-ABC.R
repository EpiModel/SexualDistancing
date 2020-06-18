#' Create the base abc/ folder
#'
#' Create the abc/ folder containing the sub directories and files that do not
#' change between each calibration
#'
#' @param force if TRUE remove exiting abc/ folder instead of throwing an error
create_abc_folder <- function(force = FALSE) {
  if (dir.exists("abc")) {
    if (force) {
      unlink("abc", recursive = T)
    } else {
      stop("abc/ folder already exists. Delete it first or use `force = TRUE`")
    }
  }

  file.copy("inst/abc/", "./", recursive = T)
}
