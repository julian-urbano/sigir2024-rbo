source("src/common.R")
source("src/simulate.R")
source("rbo/rbo.R")

future_lapply(1:100000, future.seed = 0, function(i) {
  len <- sample(10:100, 2, replace = TRUE)
  xy <- simulate_rankings(min(len), max(len),
                          n = 1000, tau = runif(1,.5,1),
                          frac_ties_x = runif(1, .1, 1), frac_ties_y = runif(1, .1, 1))
  x <- xy$x
  y <- xy$y

  n_x <- length(unlist(x))
  n_t_x <- sum(lengths(x)[lengths(x)>1])
  n_y <- length(unlist(y))
  n_t_y <- sum(lengths(y)[lengths(y)>1])

  x_rand <- unlist(sapply(x, sample)) # break ties at random
  y_rand <- unlist(sapply(y, sample)) # break ties at random
  x_docid <- unlist(sapply(x, sort)) # break ties by docid
  y_docid <- unlist(sapply(y, sort)) # break ties by docid

  lapply(.P, function(p) {
    data.frame(i,
               n_x, n_y, n_t_x, n_t_y,
               p,
               rbo_rand = rbo(x_rand, y_rand, p, "w", "ext"),
               rbo_docid = rbo(x_docid, y_docid, p, "w", "ext"),
               rbow = rbo(x, y, p, "w", "ext"),
               rboa = rbo(x, y, p, "a", "ext"),
               rbob = rbo(x, y, p, "b", "ext"))
  })
}) |> bind_rows() -> d

path <- glue("output/rbo-synthetic/")
dir.create(path, recursive = TRUE, showWarnings = FALSE)
rio::export(d, glue("{path}/rbo.csv"))
