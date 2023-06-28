library(rbenchmark)


benchmark(
  "vectorizing" = {
    x <- rep(2, length = 10000)
    vec <- c()
    for (i in seq_along(x)) {
      vec <- c(vec, x[i])
    }
    out <- paste0(vec, collapse="")
    print(length(out))
  },
  "pasting" = {
    x <- rep(2, length = 10000)
    vec <- ""
    for (i in seq_along(x)) {
      vec <- paste0(vec, x[i])
    }
    print(length(vec))
  },
  "trolling" = {
    x <- rep(2, length = 10000)
    vec <- vector(mode = "numeric", length = 10^7)
    for (i in seq_along(x)) {
      vec[i] <- x[i]
    }
    vec <- vec[1:i]
    vec <- paste0(vec, collapse = "")
    # vec <- paste0(vec[-seq((i+1), 10000)], collapse = "")
    print(length(vec))
  }
)
