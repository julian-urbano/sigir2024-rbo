simulate_rankings <- function(len_x, len_y, enforce_ties = TRUE, ...) {
  repeat {
    xy <- simulate_scores(...)
    X <- score2id(xy$x)
    Y <- score2id(xy$y)
    X <- truncate_ranking(X, len_x)
    Y <- truncate_ranking(Y, len_y)

    if(!enforce_ties || (has_ties(X) && has_ties(Y)))
      break
  }
  list(x = X, y = Y)
}

## From ircor-devel
simulate_scores <- function(n = sample(10:100, 1),
                            tau = runif(1, -.99, .99),
                            frac_ties_x = NULL, n_groups_x = NULL,
                            frac_ties_y = NULL, n_groups_y = NULL,
                            .returnParams = FALSE) {
  if(n < 2)
    stop("'n' must be at least 2")
  if(tau < -1 || tau > 1)
    stop("'tau' must be between -1 and 1")
  if(missing(frac_ties_x) && !missing(n_groups_x))
    stop("'frac_ties_x' needed when setting 'n_groups_x'")
  if(missing(frac_ties_y) && !missing(n_groups_y))
    stop("'frac_ties_y' needed when setting 'n_groups_y'")

  # if we don't have frac_ties, set randomly
  if(missing(frac_ties_x))
    frac_ties_x <- runif(1)
  if(missing(frac_ties_y))
    frac_ties_y <- runif(1)

  if(frac_ties_x < 0 || frac_ties_x > 1)
    stop("'frac_ties_x' must be between 0 and 1")
  if(frac_ties_y < 0 || frac_ties_y > 1)
    stop("'frac_ties_y' must be between 0 and 1")

  # number of elements tied in each ranking
  # note that it can be 0, 2, ..., n (excluding only one item tied)
  n_ties_x <- floor((n-1) * frac_ties_x)
  n_ties_x <- min(ifelse(n_ties_x == 0, 0, n_ties_x+1), n)
  n_ties_y <- round((n-1) * frac_ties_y)
  n_ties_y <- min(ifelse(n_ties_y == 0, 0, n_ties_y+1), n)

  # at most, we can have as many groups as n_ties/2
  if(n_ties_x > 1) {
    if(missing(n_groups_x))
      n_groups_x <- sample(seq_len(floor(n_ties_x / 2)), 1)
    if(n_groups_x < 1 || n_groups_x > floor(n_ties_x / 2))
      stop(glue::glue("'n_groups_x' must be between 1 and {floor(n_ties_x/2)} with these params"))
  }else
    n_groups_x <- 0
  if(n_ties_y > 1){
    if(missing(n_groups_y))
      n_groups_y <- sample(seq_len(floor(n_ties_y / 2)), 1)
    if(n_groups_y < 1 || n_groups_y > floor(n_ties_y / 2))
      stop(glue::glue("'n_groups_y' must be between 1 and {floor(n_ties_y/2)} with these params"))
  }else
    n_groups_y <- 0

  # generate from bivariate guassian copula (ensures [0,1])
  # first we need to turn kendall tau into pearson r
  r <- sin(pi * tau/2)
  u <- mvtnorm::rmvnorm(n, sigma = matrix(c(1,r,r,1), 2, 2))
  x <- pnorm(u[,1])
  y <- pnorm(u[,2])

  # make ties in x and y
  x <- make_ties(x, n_ties_x, n_groups_x)
  y <- make_ties(y, n_ties_y, n_groups_y)

  # package results and params if requested
  rankings <- list(x = x, y = y)
  if(.returnParams) {
    rankings$.params <- list(n = n,
                             tau = tau,
                             frac_ties_x = frac_ties_x,
                             frac_ties_y = frac_ties_y,
                             n_groups_x = n_groups_x,
                             n_groups_y = n_groups_y)
  }
  rankings
}
make_ties <- function(x, n_ties, n_groups) {
  n_untied <- length(x)-n_ties

  if(n_ties > 1) {
    # create n_ties labels identifying groups of ties

    # first, we enforce at least 2 items per group so that we always end up with n_groups
    l <- sample(rep(seq_len(n_groups), each = 2))
    # and then randomly select group labels until we reach n_ties items
    if(length(l) < n_ties) {
      if(n_groups > 1){
        # randomly select using a random Dirichlet distribution to generate labels
        # otherwise, all labels would be equally likely, resulting in uniform group sizes
        l <- c(l,
               sample(seq_len(n_groups), n_ties - length(l), replace = TRUE,
                      prob = extraDistr::rdirichlet(1, alpha = runif(n_groups, max = 10))))
      }else{
        # only one group, so just fill in with 1s
        l <- c(l,
               rep(1, n_ties - length(l)))
      }
    }
    # randomly place the groups along the ranking
    w <- c(rep(1, n_untied), table(l))
    w <- w[sample(length(w))] # to avoid funny behavior of R's sample
    # for each group, grow its size so that it contains as many indexes as needed
    w <- unlist(sapply(seq_along(w), function(ww) rep(ww,w[ww])))
    # expand indexes so they span from 1 to n
    w <- rank(w, ties.method = "min")
    # final, sorted ranking
    w <- sort(x)[w]
    # re-sort to the same order as the original
    x <- w[rank(x, ties.method = "first")]
  }
  x
}
score2id <- function(r) {
  domain <- paste0("i", seq_along(r))

  # re-rank so that ties have the same, integer rank
  r <- rank(r, ties.method = "min")
  i <- sapply(seq_along(r), function(i) {
    domain[which(r==i)]
  })
  # if i is a list, that means there were ties and we have to clean up NULLs
  if(is.list(i))
    i <- i[lengths(i) > 0]

  i
}
truncate_ranking <- function(X, n) {
  l <- lengths(X)
  i <- which(cumsum(l)>=n)[1] # at what group can we stop counting to reach n items?
  X <- X[seq(i)]
  n0 <- max(cumsum(l[seq(i)])) # if we ended up with more than n items, remove from the end
  if(n0 > n) {
    Xlast <- X[[length(X)]]
    X[[length(X)]] <- Xlast[seq(1, length(Xlast)-n0+n)]
  }
  X
}
has_ties <- function(x) {
  any(lengths(x) > 1)
}
