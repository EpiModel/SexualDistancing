update_list <- function(x, new_x) {
  for (n in names(new_x)) {
    if (is.list(new_x[[n]])) {
      x[[n]] <- update_list(x[[n]], new_x[[n]])
    } else if (is.function(new_x[[n]]) && ! is.function(x[[n]])) {
      x[[n]] <- new_x[[n]](x[[n]])
    } else {
      x[[n]] <- new_x[[n]]
    }
  }

  return(x)
}

x <- list(
  a = list(
    a1 = 1,
    a2 = 2,
    a3 = 3
    ),
  b = "b",
  c = list(
    cc = list(
      cc1 = 1,
      cc2 = 2
    ),
    c2 = 2
  )
)

new_x <- list(
  a = list(
    a1 = 3
    ),
  b = "r",
  c = list(
    cc = list(
      cc1 = 3
    ),
    c2 = 5
  )
)

new_x <- list(
  a = list(a1 = function(x) x * 30)
)

y <- update_list(x, new_x)
